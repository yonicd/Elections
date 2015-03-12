library(shiny)
library(shinyAce)
library(httr)
library(XML)
library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(scales)
library(dplyr)

#library(png)
#library(grid)
#library(gridExtra)

#helper functions

re.encode=function(df){
  v.id=which(sapply(df%>%summarise_each(funs(class))%>%c,'[',1)%in%"character")
  for(i in v.id)  Encoding(df[,i])<-enc2native(df[,i])
  df
}

#http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string (Andrie 15.06.11)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#http://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart (erik shilts 15.12.12)
remove_geom <- function(ggplot2_object, geom_type) {
  layers <- lapply(ggplot2_object$layers, function(x) if(x$geom$objname == geom_type) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  ggplot2_object$layers <- layers
  ggplot2_object
}

#Read old Elections Data and Auxillary Data
loc.run="windows-1255"
#loc.run=""

# x.old=read.csv("election_data.csv",stringsAsFactors=F,encoding = "UTF-8")%>%re.encode%>%mutate(Date=as.Date(Date))
# attribute=read.csv("election_results.csv",stringsAsFactors=F,encoding = "UTF-8")%>%re.encode
# Poll=read.csv("Pollster_En.csv",stringsAsFactors=F,encoding = "UTF-8")%>%re.encode
# ProjectScore=read.csv("ProjectScore.csv",stringsAsFactors=F,encoding = "UTF-8")%>%re.encode
# fac_vars.df=read.csv("fac_vars.csv",stringsAsFactors=F,encoding = "UTF-8")%>%re.encode

x.old=read.csv("election_data.csv",stringsAsFactors=F,fileEncoding=loc.run)%>%mutate(Date=as.Date(Date))
attribute=read.csv("election_results.csv",stringsAsFactors=F,fileEncoding=loc.run)
Poll=read.csv("Pollster_En.csv",stringsAsFactors=F,fileEncoding=loc.run)
ProjectScore=read.csv("ProjectScore.csv",stringsAsFactors=F,fileEncoding=loc.run)
fac_vars.df=read.csv("fac_vars.csv",stringsAsFactors=F,fileEncoding=loc.run)

fac_vars=fac_vars.df[,1]

if (Sys.info()[1] == "Windows"){
x=read.csv("x_static.csv",stringsAsFactors=F,fileEncoding=loc.run)%>%mutate(Date=as.Date(Date))
x=x%>%mutate_each(funs(factor),Publisher,Ideology,Ideology.Group)
}else{
#Fetch 2015 data
url="https://docs.google.com/spreadsheets/d/1-Xr5gb-HfO08RaP3Y5s6YBs9Uvv0JenZGnvPsqsyBNc/pubhtml?gid=0&single=true"
doc <- content(GET(url),as="text",encoding="UTF-8")
htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
tableNodes = getNodeSet(htmlParse(htmlTable,encoding="UTF-8"), "//table") 
ret=readHTMLTable(tableNodes[[1]],header=T,stringsAsFactors=FALSE,
                  as.data.frame=TRUE,row.names = NULL,encoding=loc.run)

#Clean Data
df=as.data.frame(ret[-1,-1])
names(df)=c(as.character(ret[1,2:6]),paste0("V",seq(1:11)))
df$Date=as.Date(df$Date,"%m/%d/%Y")
df=df%>%mutate_each(funs(as.numeric(as.character(.))),-c(Date,Publisher,Pollster,Type))
df=melt(df,id=names(df)[1:5],value.name = "Mandates",variable.name="Party")%>%
  mutate(Party=str_replace_all(as.character(factor(Party,labels=ret[1,7:17])),'\"',""),
         Party=str_replace(Party,
                           "הרשימה המשותפת"
                           ,"רשימה משותפת ערבית"
                           ),
         Publisher=str_replace_all(Publisher,"!",""),
         Election=2015)

#df=re.encode(df)

#Combine Current Data to Old Data
x=rbind(df,x.old)%>%filter(!is.na(Mandates))%>%
  mutate(Mandate.Group=cut(Mandates,breaks = c(0,1,5,10,15,20,30,50),include.lowest = T))%>%
  unique

x$Publisher=sapply(x$Publisher,simpleCap)

x=join(x,attribute,by=c("Election","Party"))
x=left_join(x,Poll%>%select(Publisher,Publisher.En)%>%distinct,by=c("Publisher"))
x=left_join(x,Poll%>%select(Pollster,Pollster.En)%>%distinct,by=c("Pollster"))


x=left_join(x,x%>%select(Election,Pollster,Date)%>%unique%>%dplyr::count(Election,Pollster),by=c("Election","Pollster"))%>%
  dplyr::rename(N=n)%>%mutate(Error=Mandates-Results,Error.abs=abs(Mandates-Results),week=format(Date,"%w"),month=format(Date,"%m"),year=format(Date,"%Y"))%>%
  arrange(desc(Date),Election,Publisher,desc(Mandates))%>%ungroup

x=x%>%mutate(Partyid=as.numeric(factor(str_trim(Party))),
         Pollsterid=as.numeric(factor(str_trim(Pollster))),
         Publisherid=as.numeric(factor(str_trim(Publisher))),
         Election.Date=as.Date(factor(Election,labels=c("2003-01-28","2006-03-28","2009-02-10","2013-01-22","2015-03-17"))),
         DaysLeft=as.numeric(Election.Date-Date),
         Sample.Error=1/sqrt(Sample),
         Mandates.lb=floor(Mandates*(1-Sample.Error)),
         Mandates.ub=floor(Mandates*(1+Sample.Error)))

 x$Ideology=factor(x$Ideology)
 x$Ideology.Group=factor(x$Ideology.Group)
}
#Project61 weighting scheme

party=x%>%filter(Election==2015&!is.na(Mandates))%>%select(Partyid,Party,Party.En)%>%unique
pollster=x%>%filter(Election==2015&!is.na(Mandates))%>%select(Pollsterid,Pollster,Pollster.En)%>%unique 
 
x3=x%>%filter(Election==2015)%>%select(Pollster,Pollsterid,Date)%>%group_by(Date,Pollster)%>%filter(row_number(Pollster)==1)%>%head(.,10)

x2=left_join(x3,x%>%select(Date,Pollster,Party,Pollsterid,Partyid,Mandates,Sample.Error),by=c("Pollster","Pollsterid","Date"))

x2=left_join(x2,x2%>%select(Date,Pollster,Pollsterid)%>%distinct%>%mutate(x=1)%>%group_by(Pollster)%>%mutate(N=cumsum(x))%>%select(-x),by=c("Pollster","Pollsterid","Date"))

x2=left_join(x2,ProjectScore%>%select(-Block,-Partyid,-Pollsterid),by=c("Pollster","Party"))%>%
  mutate(Mandates.adj=Mandates-Score,Weight=sqrt(Pollster.weight-(as.numeric(max(Date)-Date))-abs(Sample.Error*50)/N))

y=sum(unique(x2$Weight))

project61=x2%>%group_by(Party)%>%
  dplyr::summarise(base=sum(Mandates*Weight)/y)%>%ungroup%>%
  dplyr::mutate(base=120*base/sum(base),
         base.floor=floor(base),
         pct=base/(base.floor+1),
         add=0,id=0,
         share=factor(Party,labels=c(1,1,2,3,4,5,6,6,2,7,3)))

for(i in (1:(120-sum(project61$base.floor)))){
y1=(project61%>%group_by(share)%>%summarise(group_pct=sum(base)/(sum(base.floor)+1))%>%filter(group_pct==max(group_pct)))$share
id=project61$pct==max(project61$pct[project61$share==y1])
project61$id[id]=i
project61$base.floor[id]=project61$base.floor[id]+1
project61$add[id]=project61$add[id]+1
project61$pct[id]=project61$base[id]/(project61$base.floor[id]+1)}

project61=left_join(project61%>%select(Party,Mandates=base.floor),attribute%>%filter(Election==2015)%>%select(Party,Ideology),by="Party")%>%mutate(Date=max(x3$Date),Pollster="פרויקט 61")%>%
  arrange(desc(Date),desc(Mandates))%>%ungroup

#Clean Workspace
rm(list=ls(pattern = ("[^x*|fac_vars|fac_vars.df|project61|party|remove_geom]")))
