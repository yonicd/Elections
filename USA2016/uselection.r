library(rvest);library(reshape2);library(zoo);library(stringr);library(plyr);library(dplyr)
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


# writeClipboard(levels(factor(poll$Poll)))
# new.poll.name=read.table("clipboard",header=F,sep="\t",stringsAsFactors = F)
# old.poll.name=data.frame(old=levels(factor(poll$Poll)),stringsAsFactors = F)
# poll.name=data.frame(old.poll.name,new.poll.name,check.names = F)
# poll$Poll=droplevels(factor(poll$Poll,labels=poll.name$V1))
# save(poll.name,file="C:/Users/yoni/Documents/GitHub/Elections/US 2016 Elections/poll_name.rdata")

load("C:/Users/yoni/Documents/GitHub/Elections/US 2016 Elections/poll_name.rdata")
poll=poll%>%left_join(poll.name,by=c("Poll"="old"))

pdf("C:/Users/yoni/Documents/GitHub/Elections/US 2016 Elections/US elections.pdf",paper="a4r",width=1200)

poll%>%filter(Poll.Date==max(Poll.Date)&!grepl("Average",Poll)&!is.na(value))%>%
  ggplot(aes(x=variable,y=value,fill=V1))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~Poll.Party,scales="free_x")+
  #theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Candidate")+ylab("Percent")+scale_fill_discrete(name="Pollster")+
  theme_bw()+theme(legend.position="bottom")

poll%>%filter(Poll.Party=="Republican",variable%in%c("Cruz","Rubio","Trump","Clinton","Sanders")&!is.na(value)&!grepl("Average",Poll))%>%
  ggplot(aes(x=Poll.Date,y=value,colour=variable))+geom_point()+geom_line()+facet_wrap(~Poll)+
  theme(axis.text.x=element_text(angle=90,hjust=1))

poll%>%filter(Poll.Party=="Democrats",variable%in%c("Cruz","Rubio","Trump","Clinton","Sanders")&!is.na(value)&!grepl("Average",Poll))%>%
  ggplot(aes(x=Poll.Date,y=value,colour=variable))+geom_point()+geom_line()+facet_wrap(~Poll)+
  theme(axis.text.x=element_text(angle=90,hjust=1))


poll%>%filter(Poll.Party=="Democrats",variable%in%c("Cruz","Rubio","Trump","Clinton","Sanders")&!is.na(value))%>%
  ggplot(aes(x=variable,y=value,fill=variable))+geom_boxplot()+facet_wrap(~Poll)


poll%>%filter(Poll.Party=="Democrats",variable%in%c("Cruz","Rubio","Trump","Clinton","Sanders")&!is.na(value))%>%
  ggplot(aes(x=Poll,y=value,fill=Poll))+geom_boxplot()+facet_wrap(~variable)+
  theme(axis.text.x=element_text(angle=90,hjust=1))

poll%>%filter(Poll.Party=="Republican",variable%in%c("Cruz","Rubio","Trump","Clinton","Sanders")&!is.na(value))%>%
  ggplot(aes(x=variable,y=value,fill=variable))+geom_boxplot()+facet_wrap(~Poll)

poll%>%filter(Poll.Party=="Republican",variable%in%c("Cruz","Rubio","Trump","Clinton","Sanders")&!is.na(value))%>%
  ggplot(aes(x=Poll,y=value,fill=Poll))+geom_boxplot()+facet_wrap(~variable)+
  theme(axis.text.x=element_text(angle=90,hjust=1))
dev.off()


save(poll,file="C:/Users/yoni/Documents/GitHub/Elections/US 2016 Elections/polls.rdata")
save(new.poll.name,new.poll.name,file="C:/Users/yoni/Documents/GitHub/Elections/US 2016 Elections/poll_levels.rdata")

