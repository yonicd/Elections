#Sys.setlocale(locale="English_US")
#Sys.setlocale(category = "LC_TIME",locale="C")
#setwd("C:/Users/yoni/Documents/GitHub/Elections/USA2016/shiny")
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

h2h=function(url){
x=read_html(url)
x1=x%>%html_table(header = T)
names(x1)=seq(1,length(x1))
x2=ldply(x1[seq(2,60,2)],.fun = function(y) y%>%select(-Spread),.id = "id")%>%mutate(id=as.numeric(id))
x3=ldply(x1[seq(1,59,2)],.fun = function(y) data.frame(Date=strptime(names(y[1]),"%A, %B %d")),.id="id")%>%mutate(id=as.numeric(id))
x3=x3%>%left_join(x2,"id")%>%select(-id)
names(x3)[2]="Race"
x4=data.frame(apply(do.call("rbind",lapply(strsplit(x3$Results,","),'[')),2,stringi::stri_trim_both))%>%
  mutate_each(funs(name=gsub('[0-9 ]','',.),result=as.numeric(gsub('[^0-9]','',.))))%>%
  do(.,cbind(x3%>%select(Date,Poll),.))%>%select(-c(X1,X2))%>%mutate(Month=format(Date,"%Y/%m"))

x4$Democratic_name=x4$Democratic_result=NA
x4$Republican_name=x4$Republican_result=NA

x4$Democratic_name=ifelse(x4$X1_name%in%c("Clinton","Sanders"),x4$X1_name,x4$X2_name)
x4$Democratic_result=ifelse(x4$X1_name%in%c("Clinton","Sanders"),x4$X1_result,x4$X2_result)

x4$Republican_name=ifelse(!x4$X1_name%in%c("Clinton","Sanders"),x4$X1_name,x4$X2_name)
x4$Republican_result=ifelse(!x4$X1_name%in%c("Clinton","Sanders"),x4$X1_result,x4$X2_result)

x4=x4%>%select(-contains("X"))

x4$state=unlist(lapply(strsplit(x3$Race,":"),"[",1))
x4$race=ifelse(x4$state=="General Election","National Polls","State Polls")

plot.trend=x4%>%filter(Republican_name%in%c("Trump","Cruz","Kasich"))%>%
  ggplot(aes(x=Republican_name,y=Republican_result-50,fill=Month))+
  geom_boxplot()+geom_hline(yintercept=0,linetype=2)+
  facet_grid(race~Democratic_name)+
  ylab("Republican Spread")+xlab("Republican Candidate")+
  scale_colour_discrete(name="Republican Candidate")+
  theme_bw()+theme(legend.position="top")+
  ggtitle("General Election: Head to Head Polling \n Monthy Spread Distribution")

plot.spread=x4%>%filter(Republican_name%in%c("Trump","Cruz","Kasich")&as.Date(Date)>=Sys.Date()-14)%>%
  group_by(Date,race,Republican_name,Democratic_name)%>%summarise_each(funs(mean),contains("Result"))%>%
  ggplot(aes(x=format(Date,"%m/%d"),y=Republican_result-50,fill=Republican_name))+
  geom_bar(position="dodge",stat="identity")+geom_hline(yintercept=0,linetype=2)+
  facet_grid(race~Democratic_name)+
  xlab("Date")+ylab("Republican Spread")+
  scale_fill_discrete(name="Republican Candidate")+
  theme_bw()+theme(legend.position="top",axis.text.x = element_text(angle = 90))+
  ggtitle("General Election: Head to Head Polling (Last 14 Days) \n Average Daily Republican Spread")

return(list(data=x4,plot.trend=plot.trend,plot.spread=plot.spread))

}

h2h.out=h2h("http://www.realclearpolitics.com/epolls/latest_polls/pres_general/")

#State Polls
url.in=data.frame(Party=c("Republican","Democratic"),
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

load("DelegatesCurrent.Rdata")

delegate=delegates%>%filter(Date!="-")%>%mutate_each(funs(as.character))
delegate$Delegates=as.numeric(gsub('\\([^)]*\\)','',delegate$Delegates))
delegate$State=gsub('^.{0,2}|[0-9]','',delegate$State)

del.Date=function(x.in){
  paste('2016',gsub(x.in[2],'',x.in[1]),x.in[2],sep="-")
}

delegate$Date[delegate$Party=="republican"]=unlist(lapply(strsplit(gsub('[ aA-zZ]','',delegate$Date[delegate$Party=="republican"]),'/'),del.Date))
delegate$Date[delegate$Party=="democratic"]=as.character(as.Date(x = paste(delegate$Date[delegate$Party=="democratic"],"2016"),format = "%B %d %Y"))
delegate$Date=as.Date(delegate$Date)
delegate$Date[is.na(delegate$Date)]=as.Date("2016-03-01")
