library(rvest);library(RSelenium);library(plyr);library(dplyr)

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

delegates=ldply(delegate.list,function(df){
  df=df%>%mutate_each(funs(as.character))
  #cols=head(names(df),-2)
  #if(tail(cols,1)=="Primary/Caucus") cols=cols[1:(length(cols)-1)]
  if(!any(names(df)%in%"Primary/Caucus")){
    df$`Primary/Caucus`=df$DelegateAllocation
    df$DelegateAllocation="Proportional"}
  df=df%>%mutate_each(funs(gsub('\u2207|\\*|#','',.)),DelegateAllocation,`Open/Closed`)
  df%>%melt(.,id=c("State","Date","Delegates","Primary/Caucus","DelegateAllocation","Open/Closed"))
},.id = "Party")
  
save(StateLinks.df,delegates,file="DelegatesCurrent.Rdata")

# nopoll=c(15,22)
# 
# RawStatePollsCurrent=rbind(RawStatePolls40,RawStatePolls)
# 
# RawStatePolls=ddply(StateLinks.df%>%slice(c(41:71)),.(Party,state.abb),.fun = function(x){
#   p=read_html(x$url)
#   pt=html_nodes(p,xpath = '//*[@id="polling-data-full"]//table')
#   pname=html_text(html_nodes(pt,xpath='//*[(@id = "polling-data-full")]//a[1]'))
#   pa=html_attr(html_nodes(pt,xpath='//*[(@id = "polling-data-full")]//a'),"href")
#   if(!any(pname%in%"Final Results")){
#     pa=c(NA,pa)
#     pvec=c(matrix(pa,ncol=2,byrow = T)[,2])
#   }else{
#     pvec=c(NA,matrix(pa,ncol=2,byrow = T)[,2])
#   }
#   pa=data.frame(poll=pname,url=pvec,stringsAsFactors = F)
#   pa=pa[complete.cases(pa),]
#   df=tail(html_table(pt,header = T),1)[[1]]
#   if(!any(names(df)%in%"MoE")) df$MoE=NA
#   if(!any(names(df)%in%"Sample")) df$Sample=NA
#   df$Spread=NULL
#   df=df[!grepl("Average",df$Poll),]
#   df$URL=pa$url
#   df$Poll=pa$poll
#   
#   df$start.Date=unlist(lapply(strsplit(df$Date," - "),'[',1))
#   df$end.Date=unlist(lapply(strsplit(df$Date," - "),'[',2))
#   df$yr=NA
#   df$m=str_pad(unlist(lapply(strsplit(df$end.Date,"/"),'[',1)),width = 2,side = "left",pad = "0")
#   df$d=str_pad(unlist(lapply(strsplit(df$end.Date,"/"),'[',2)),width = 2,side = "left",pad = "0")
#   xind=which(lapply(strsplit(df$end.Date,"/"),'[',1)=='12')
#   xind=c(1,xind[1],xind[which(xind-lag(xind)>1)])
#   if(any(is.na(xind))) {
#     df$yr=2016
#   }else{
#     df$yr[xind]=seq(2016,2016-(length(xind)-1),-1)
#     df$yr=na.locf(df$yr)      
#   }
#   df$Date=as.Date(paste(df$yr,df$m,df$d),format("%Y%m%d"))
#   df=df%>%select(-c(m,d,yr,end.Date,start.Date))
#   df=df%>%melt(.,id=c("URL","Poll","Date","Sample","MoE"),variable.name="Candidate",value.name="Result")
#   df=df%>%left_join(df%>%filter(Poll=="Final Results")%>%
#                       select(Candidate,FinalResult=Result),by="Candidate")%>%
#     filter(Result!="--"&Poll!="Final Results")%>%
#     mutate_each(funs(as.numeric),contains("Result"))%>%mutate(PollError=FinalResult-Result)
#   return(df)
# },.progress = "text")
# 
# 
# RawStatePollsCurrent%>%filter(Candidate%in%c("Trump","Rubio","Cruz","Kasich","Clinton","Sanders"),Date<=Sys.Date()&!is.na(PollError))%>%
#   mutate(m=format(Date,"%y.%m"))%>%
#   ggplot(aes(x=Date,y=PollError,colour=Candidate))+geom_point()+facet_wrap(~state.abb)
# 
# RawStatePollsCurrent%>%filter(Candidate%in%c("Trump","Rubio","Cruz","Kasich","Clinton","Sanders"),Date<=Sys.Date()&!is.na(PollError))%>%
#   filter(Date>="2015-01-01")%>%
#   mutate(m=format(Date,"%y.%m"))%>%
#   ggplot(aes(x=Date,y=PollError))+geom_point()+facet_wrap(~Candidate)
# 
# setwd("C:/Users/yoni/Documents/GitHub/Elections/USA2016/shiny")
# save(RawStatePollsCurrent,delegates,StateLinks.df,file="PollHistory.Rdata")
# 
# 
# RawStatePollsCurrent%>%filter(Candidate%in%c("Clinton","Sanders"),Date<=Sys.Date()&!is.na(PollError))%>%
#   filter(Date>="2015-01-01")%>%
#   mutate(m=format(Date,"%Y\n%m"))%>%
#   ggplot()+
#   geom_density(aes(x=PollError,fill=Candidate,y=..scaled..),alpha=.3)+facet_grid(m~.)+geom_vline(aes(xintercept=0))+
#   theme_bw()+
#   theme(legend.position="top")+
#   scale_x_continuous(limits=c(-60,60))+scale_y_continuous(breaks=c(1))+
#   ylab("Density")+xlab("Poll Error = Primary Result (%) - Polling Result (%)")+
#   ggtitle("The Evolution and Ceiling of the Sanders Campain: State Polling Results 2015 to Today \n Negative: Over Estimation, Positive: Under Estimation, Zero: Pollsters have Calibrated to Public Opinion \n Data Source: realclearpolitics.com")
