#Sys.setlocale(locale="English_US")
library(ggplot2);library(rvest);library(reshape2);library(zoo);library(stringr);library(plyr);library(shinyAce);library(dplyr)

remove_geom <- function(p, geom) {
  layers <- lapply(p$layers, function(x) if(any(grepl(paste0('(?i)',geom),class(x$geom)))) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  p$layers <- layers
  p
}

load("poll_name.rdata")
df.in=data.frame(Poll.Party=c("Democrat","Republican"),
                 Poll.URL=c('http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html',
                            'http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html'),stringsAsFactors = F)

poll=ddply(df.in,.(Poll.Party),.fun = function(df){
  x=read_html(df$Poll.URL)
  x1=x%>%html_table(header=T)
  xt=x1[[4]]
  if(!any(names(xt)%in%"MoE")) xt=xt%>%mutate(MoE=NA)
  xt$start.Date=unlist(lapply(strsplit(xt$Date," - "),'[',1))
  xt$end.Date=unlist(lapply(strsplit(xt$Date," - "),'[',2))
  xt$yr=NA
  xt$m=str_pad(unlist(lapply(strsplit(xt$end.Date,"/"),'[',1)),width = 2,side = "left",pad = "0")
  xt$d=str_pad(unlist(lapply(strsplit(xt$end.Date,"/"),'[',2)),width = 2,side = "left",pad = "0")
  xind=which(lapply(strsplit(xt$end.Date,"/"),'[',1)=='12')
  xind=c(1,xind[1],xind[which(xind-lag(xind)>1)])
  xt$yr[xind]=seq(2016,2012,-1)
  xt$yr=na.locf(xt$yr)
  xt$Poll.Date=as.Date(paste(xt$yr,xt$m,xt$d),format("%Y%m%d"))
  xt=xt%>%select(-c(Date,start.Date,end.Date))
  xt=xt%>%melt(.,c("Poll.Date","Poll","Sample","MoE","yr","m","d"))
  return(xt)
})%>%distinct

poll=poll%>%mutate_each(funs(as.character),-c(Poll.Date))%>%mutate_each(funs(ifelse(.=="--",NA,.)),-c(Poll.Date))
poll=poll%>%mutate_each(funs(as.numeric),value)
poll=poll%>%left_join(poll.name,by=c("Poll"="old"))

whichWeek <- function(aDate) paste0("0",ceiling(as.numeric(format(aDate, "%d"))/7 ))

poll.shiny=poll%>%select(Date=Poll.Date,Year=yr,Month=m,Pollster=V1,Sample,
                         Party=Poll.Party,Candidate=variable,
                         Results=value)%>%
  mutate(Sample.Type=gsub('[^A-Z]','',Sample),Sample=as.numeric(gsub('[^0-9]','',Sample)),
         Weekday=format(Date,"%A"),MonthWeek=whichWeek(Date))%>%
  filter(!grepl("Average|Reason-Rupe",Pollster)&Candidate!="Spread")


sample.avg=poll.shiny%>%group_by(Pollster,Sample.Type)%>%summarise(Sample.Mean=floor(mean(Sample,na.rm = T)))



poll.shiny=poll.shiny%>%left_join(sample.avg,by=c("Pollster","Sample.Type"))%>%
  mutate(Results=Results,Sample=ifelse(is.na(Sample),Sample.Mean,Sample),
         Sample.Error=100*1/sqrt(Sample),
         DaysLeft=as.numeric(as.Date("2016-11-08")-Date))%>%select(-Sample.Mean)

fac_vars=names(poll.shiny)[c(7,6,4,8,13,1,3,11,10,2)]
fac_vars=c(fac_vars,names(poll.shiny)[!names(poll.shiny)%in%fac_vars])
