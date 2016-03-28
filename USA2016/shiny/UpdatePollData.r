
StatePollsOpen=ddply(StateLinks.df%>%filter(State%in%c("National",remaining.states$State)),
                    .(Party,State,state.abb),.fun = function(x){
  p=read_html(x$url)
  pt=html_nodes(p,xpath = '//*[@id="polling-data-full"]//table')
  pname=html_text(html_nodes(pt,xpath='//*[(@id = "polling-data-full")]//a[1]'))
  pa=html_attr(html_nodes(pt,xpath='//*[(@id = "polling-data-full")]//a'),"href")
  if(!any(pname%in%"Final Results")){
    pa=c(NA,pa)
    pvec=c(matrix(pa,ncol=2,byrow = T)[,2])
  }else{
    pvec=c(NA,matrix(pa,ncol=2,byrow = T)[,2])
  }
  pa=data.frame(poll=pname,url=pvec,stringsAsFactors = F)
  pa=pa[complete.cases(pa),]
  df=tail(html_table(pt,header = T),1)[[1]]
  if(!any(names(df)%in%"MoE")) df$MoE=NA
  if(!any(names(df)%in%"Sample")) df$Sample=NA
  df$Spread=NULL
  df=df[!grepl("Average",df$Poll),]
  df$URL=pa$url
  df$Poll=pa$poll
  df=df%>%rename(Pollster=Poll)%>%mutate(Pollster=gsub("[*]",'',Pollster))
  
  df$end.Date=unlist(lapply(strsplit(df$Date," - "),'[',2))
  df$yr=NA
  df$m=str_pad(unlist(lapply(strsplit(df$end.Date,"/"),'[',1)),width = 2,side = "left",pad = "0")
  df$d=str_pad(unlist(lapply(strsplit(df$end.Date,"/"),'[',2)),width = 2,side = "left",pad = "0")
  xind=which(lag(as.numeric(df$m),default = 0)-as.numeric(df$m)<0)
  if(any(is.na(xind))||length(xind)==0) {
    df$yr=2016
  }else{
    if(xind[1]!=1) xind=c(1,xind)
    df$yr[xind]=seq(2016,2016-(length(xind)-1),-1)
    df$yr=na.locf(df$yr)
  }
  df$Date=as.Date(paste(df$yr,df$m,df$d),format("%Y%m%d"))
  df=df%>%select(-c(m,d,yr,end.Date))
  df=df%>%melt(.,id=c("URL","Pollster","Date","Sample","MoE"),
               variable.name="Candidate",value.name="Results")
  
  df=df%>%left_join(df%>%filter(Pollster=="Final Results")%>%
                      select(Candidate,FinalResult=Results),by="Candidate")%>%
    filter(Results!="--"&Pollster!="Final Results")%>%
    mutate_each(funs(as.numeric),contains("Result"))%>%mutate(PollError=FinalResult-Results)
  return(df)
},.progress = "text")

StatePollsCurrent=rbind(StatePollsOpen,StatePollsCurrent%>%filter(!State%in%c("National",remaining.states$State)))

#save(StatePollsCurrent,file="Temp/StatePollsHistory.Rdata")
