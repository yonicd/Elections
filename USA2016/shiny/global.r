#Sys.setlocale(locale="English_US")
#Sys.setlocale(category = "LC_TIME",locale="C")
#setwd("C:/Users/yoni/Documents/GitHub/Elections/USA2016/shiny")
library(plotly);library(ggplot2);library(rvest);library(reshape2);library(zoo);library(stringr);library(plyr);library(shinyAce);library(dplyr)


#Function that removes a specific geometery from ggplot
remove_geom <- function(p, geom) {
  layers <- lapply(p$layers, function(x) if(any(grepl(paste0('(?i)',geom),class(x$geom)))) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  p$layers <- layers
  p
}

#Function that returns week of month
whichWeek <- function(aDate) paste0("0",ceiling(as.numeric(format(aDate, "%d"))/7 ))

#Head to Head General Election data
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
  ggtitle("General Election: Head to Head Polling \n Monthly Spread Distribution")

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


#Load Delegate Data (Currently offline)
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

#Calculate remaining states in primaries
remaining.states=delegate%>%filter(value=='')%>%select(State,Date)%>%distinct%>%filter(Date>=Sys.Date())%>%arrange(Date,State)

#Load historical polling data
load("Temp/StatePollsHistory.Rdata")

#Read in latest polling data
source("UpdatePollData.r")

StatePollsCurrent$MoE=as.numeric(gsub('--',NA,StatePollsCurrent$MoE))
StatePollsCurrent$SampleType=gsub('[ 0-9]','',StatePollsCurrent$Sample)
StatePollsCurrent$SampleSize=as.numeric(gsub('[^0-9]','',StatePollsCurrent$Sample))
StatePollsCurrent=StatePollsCurrent%>%mutate(MoE=ifelse(is.na(MoE),100*SampleSize^(-.5),MoE))

StatePollsCurrent$Party=ifelse(StatePollsCurrent$Party=="democratic","Democratic","Republican")

#Clean up any dates that are past current date
StatePollsCurrent=StatePollsCurrent%>%filter(Date<=Sys.Date())

#Create shiny data
poll.shiny=StatePollsCurrent%>%select(-Sample)%>%mutate(
          Date=as.Date(Date),Month=format(Date,"%b"),
          Weekday=format(Date,"%A"),MonthWeek=whichWeek(Date),
         DaysLeft=as.numeric(as.Date("2016-07-25")-Date),
         Candidate=as.character(Candidate))

poll.shiny=poll.shiny[,c(c(1:17)[-4],4)]
names(poll.shiny)[3]="State.Abb"

#factor variables for the plots
fac_vars=head(names(poll.shiny),-1)