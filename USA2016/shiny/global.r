#Sys.setlocale(locale="English_US")
setwd("C:/Users/yoni/Documents/GitHub/Elections/USA2016/shiny")

library(plotly);library(ggplot2);library(rvest);library(reshape2);library(zoo);library(stringr);library(plyr);library(shinyAce);library(dplyr)

remove_geom <- function(p, geom) {
  layers <- lapply(p$layers, function(x) if(any(grepl(paste0('(?i)',geom),class(x$geom)))) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  p$layers <- layers
  p
}

# load("poll_name.rdata")
# df.in=data.frame(Poll.Party=c("Democrat","Republican"),
#                  Poll.URL=c('http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html',
#                             'http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html'),stringsAsFactors = F)
# 
# poll=ddply(df.in,.(Poll.Party),.fun = function(df){
#   x=read_html(df$Poll.URL)
#   x1=x%>%html_table(header=T)
#   xt=x1[[4]]
#   if(!any(names(xt)%in%"MoE")) xt=xt%>%mutate(MoE=NA)
#   xt$start.Date=unlist(lapply(strsplit(xt$Date," - "),'[',1))
#   xt$end.Date=unlist(lapply(strsplit(xt$Date," - "),'[',2))
#   xt$yr=NA
#   xt$m=str_pad(unlist(lapply(strsplit(xt$end.Date,"/"),'[',1)),width = 2,side = "left",pad = "0")
#   xt$d=str_pad(unlist(lapply(strsplit(xt$end.Date,"/"),'[',2)),width = 2,side = "left",pad = "0")
#   xind=which(lapply(strsplit(xt$end.Date,"/"),'[',1)=='12')
#   xind=c(1,xind[1],xind[which(xind-lag(xind)>1)])
#   xt$yr[xind]=seq(2016,2012,-1)
#   xt$yr=na.locf(xt$yr)
#   xt$Poll.Date=as.Date(paste(xt$yr,xt$m,xt$d),format("%Y%m%d"))
#   xt=xt%>%select(-c(Date,start.Date,end.Date))
#   xt=xt%>%melt(.,c("Poll.Date","Poll","Sample","MoE","yr","m","d"))
#   return(xt)
# })%>%distinct

# poll=poll%>%mutate_each(funs(as.character),-c(Poll.Date))%>%
#   mutate_each(funs(ifelse(.=="--",NA,.)),-c(Poll.Date))%>%filter(variable!="Spread")
# poll=poll%>%mutate_each(funs(as.numeric),value)
# poll=poll%>%left_join(poll.name,by=c("Poll"="old"))

whichWeek <- function(aDate) paste0("0",ceiling(as.numeric(format(aDate, "%d"))/7 ))


#State Polls
url.in=data.frame(Party=c("Republican","Democrat"),
                  URL=c("http://www.realclearpolitics.com/epolls/latest_polls/gop_pres_primary/",
                        "http://www.realclearpolitics.com/epolls/latest_polls/dem_pres_primary/"),stringsAsFactors = F)

State.Polls=ddply(url.in,.(Party),.fun = function(df){
  x=read_html(df$URL)
  x1=x%>%html_table(header = T)
  names(x1)=seq(1,length(x1))
  x2=ldply(x1[seq(2,60,2)],.fun = function(y) y%>%select(-Spread),.id = "id")%>%mutate(id=as.numeric(id))
  x3=ldply(x1[seq(1,59,2)],.fun = function(y) data.frame(Date=strptime(names(y[1]),"%A, %B %d")),.id="id")%>%mutate(id=as.numeric(id))
  x3=x3%>%left_join(x2,"id")%>%select(-id)
  names(x3)[2]="Race"
  x3$State=stringi::stri_trim_both(gsub('Democratic|Republican|Presidential|Primary|Caucus|[0-9]','',x3$Race))
  x3$State[grepl('Nomination',x3$State)]='General Election'
  x3$id=seq(1,nrow(x3))
  
  x4=ddply(x3,.(id),.fun = function(x.in){
    Candidate=unlist(lapply(strsplit(x.in$Results,", ")[[1]],function(x) gsub('[0-9 ]','',x[1])))
    Results=unlist(lapply(strsplit(x.in$Results,", ")[[1]],function(x) as.numeric(gsub('[aA-zZ ]','',x[1]))))
    x.out=data.frame(Candidate,Results)%>%do(.,filter(.,complete.cases(.)))%>%mutate(Date=x.in$Date,Poll=x.in$Poll,State=x.in$State)
    return(x.out)
  })%>%select(-id)
  
  return(x4)
})

State.Polls=State.Polls%>%left_join(State.Polls%>%filter(State=="General Election")%>%group_by(Candidate)%>%do(head(.,5))%>%summarise(GenMean=mean(Results),GenSd=sd(Results)),by="Candidate")%>%
  left_join(data.frame(State=state.name,State.Abb=state.abb,stringsAsFactors = F),by="State")%>%
  mutate(State.Abb=ifelse(is.na(State.Abb),"US",State.Abb))

poll.shiny=State.Polls%>%select(-contains("Gen"))%>%rename(Pollster=Poll)%>%mutate(
    #Sample.Type=gsub('[^A-Z]','',Sample),Sample=as.numeric(gsub('[^0-9]','',Sample)),
          Date=as.Date(Date),Month=format(Date,"%b"),Weekday=format(Date,"%A"),MonthWeek=whichWeek(Date),
         DaysLeft=as.numeric(as.Date("2016-07-25")-Date),
         Candidate=as.character(Candidate))


#sample.avg=poll.shiny%>%group_by(Pollster,Sample.Type)%>%summarise(Sample.Mean=floor(mean(Sample,na.rm = T)))



# poll.shiny=poll.shiny%>%left_join(sample.avg,by=c("Pollster","Sample.Type"))%>%
#   mutate(Results=Results,Sample=ifelse(is.na(Sample),Sample.Mean,Sample),
#          Sample.Error=100*1/sqrt(Sample),
#          DaysLeft=as.numeric(as.Date("2016-07-25")-Date))%>%select(-Sample.Mean)%>%
#   filter(Sample.Type%in%c("LV","RV"))

# poll.shiny$Sample.Type=factor(poll.shiny$Sample.Type,labels=c("Likely Voter","Registered Voter"))

fac_vars=names(poll.shiny)[c(2,6,7,5,1,11,8:10)]

