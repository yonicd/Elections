library(rvest);library(RSelenium);library(plyr);library(reshape2);library(dplyr)

RSelenium::startServer()
remDr <- remoteDriver()
delegate.list=vector('list',2)
names(delegate.list)=c("republican","democratic")
StateLinks=delegate.list
for(i in 1:2){
remDr$open(silent = T)
remDr$setTimeout(type = "page load", milliseconds = 50000)
remDr$navigate(paste0("http://www.realclearpolitics.com/epolls/2016/president/",names(delegate.list)[i],"_delegate_count.html"))
out=htmlParse(remDr$getPageSource()[[1]])
StateLinks[[i]]=as.character(getNodeSet(out,'//*[contains(concat( " ", @class, " " ), concat( " ", "state_col", " " ))]//a/@href'))
dataNode=getNodeSet(out,'//*[@id="polling-data-rcp"]//table')
delegate.list[[i]]=readHTMLTable(dataNode[[1]],header = T)
remDr$close()
}


StateLinks.df=ldply(StateLinks,function(x){ 
  state=toupper(substr(gsub("http://www.realclearpolitics.com/epolls/2016/president/",'',x),1,2))
  df=data.frame(state.abb=state,url=x,stringsAsFactors = F)},.id = "Party")

StateLinks.df$State=state.name[match(StateLinks.df$state.abb,state.abb)]

StateLinks.df=rbind(data.frame(Party=c("republican","democratic"),state.abb=rep("National",2),url=c("http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html","http://www.realclearpolitics.com/epolls/2016/president/us/2016_democratic_presidential_nomination-3824.html"),State=rep("National",2),stringsAsFactors = F),
                    StateLinks.df
)

delegates=ldply(delegate.list,function(df){
  df=df%>%mutate_each(funs(as.character))
  names(df)[2]="Date"
  if(!any(names(df)%in%"Primary/Caucus")){
    df$`Primary/Caucus`=df$DelegateAllocation
    df$DelegateAllocation="Proportional"}
  df=df%>%mutate_each(funs(gsub('\u2207|\\*|#','',.)),DelegateAllocation,`Open/Closed`)
  df%>%melt(.,id=c("State","Date","Delegates","Primary/Caucus","DelegateAllocation","Open/Closed"))
},.id = "Party")%>%mutate(now=Sys.time())

save(StateLinks.df,delegates,file="Temp/DelegatesCurrent.Rdata")

 
# StatePolls%>%filter(Candidate%in%c("Clinton","Sanders"),Date<=Sys.Date()&!is.na(PollError))%>%
#   filter(Date>="2015-01-01")%>%
#   mutate(m=format(Date,"%Y\n%m"))%>%
#   ggplot()+
#   geom_density(aes(x=PollError,fill=Candidate,y=..scaled..),alpha=.3)+facet_grid(m~.)+geom_vline(aes(xintercept=0))+
#   theme_bw()+
#   theme(legend.position="top")+
#   scale_x_continuous(limits=c(-60,60))+scale_y_continuous(breaks=c(1))+
#   ylab("Density")+xlab("Poll Error = Primary Result (%) - Polling Result (%)")+
#   ggtitle("The Evolution and Ceiling of the Sanders Campain: State Polling Results 2015 to Today \n Negative: Over Estimation, Positive: Under Estimation, Zero: Pollsters have Calibrated to Public Opinion \n Data Source: realclearpolitics.com")
