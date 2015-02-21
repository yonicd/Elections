library(plyr)
library(dplyr)
library(reshape2)
library(XML)
library(stringr)
library(ggplot2)
library(httr)

#load("historical_data.rdata")

loc.run="windows-1255"

x.old=read.csv("election_data.csv",stringsAsFactors=F,fileEncoding=loc.run)%>%mutate(Date=as.Date(Date))
attribute=read.csv("election_results.csv",stringsAsFactors=F,fileEncoding=loc.run)
Poll=read.csv("Pollster_En.csv",stringsAsFactors=F,fileEncoding=loc.run)
fac_vars.df=read.csv("fac_vars.csv",stringsAsFactors=F,fileEncoding=loc.run)

fac_vars=fac_vars.df[,1]

#save(x.old,attribute,Poll,fac_vars.df,file="historical_data.rdata")
# attribute=data.frame( Party=c("הבית היהודי","הליכוד","המחנה הציוני","יהדות התורה","יחד","יש עתיד","ישראל ביתנו","כולנו","מרצ","רשימה משותפת ערבית","שס"),
#                        Party.En=c("Habayit Hayehudi","Likud","Machane Zioni","Yehadut Hatorah","Yachad","Yesh Atid","Israel Beytenu","Kulanu","Meretz","Combined Arab Parties","Shas"),
#                       Attribute=c("combined","old,split","combined","old","new,split","old","old,split","new,split","old","combined","old,split"))%>%mutate(Election=2015)

#Fetch 2015 data
url="https://docs.google.com/spreadsheets/d/1-Xr5gb-HfO08RaP3Y5s6YBs9Uvv0JenZGnvPsqsyBNc/pubhtml?gid=0&single=true"
doc <- content(GET(url),as="text",encoding="UTF-8")
htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
tableNodes = getNodeSet(htmlParse(htmlTable,encoding="UTF-8"), "//table") 
ret=readHTMLTable(tableNodes[[1]],header=T,stringsAsFactors=FALSE,
                  as.data.frame=TRUE,row.names = NULL,encoding="windows-1255")

#Clean Data
df=as.data.frame(ret[-1,-1])
names(df)=c(as.character(ret[1,2:6]),paste0("V",seq(1:11)))
df$Date=as.Date(df$Date,"%m/%d/%Y")
df=df%>%mutate_each(funs(as.numeric(as.character(.))),-c(Date,Publisher,Pollster,Type))
df=melt(df,id=names(df)[1:5],value.name = "Mandates",variable.name="Party")%>%
  mutate(Party=str_replace_all(as.character(factor(Party,labels=ret[1,7:17])),'\"',""),
         Party=str_replace(Party,"הרשימה המשותפת","רשימה משותפת ערבית"),
         Publisher=str_replace_all(Publisher,"!",""),
         Election=2015)

#Combine to Old Data
x=rbind(df,x.old)%>%filter(!is.na(Mandates))%>%
  mutate(Mandate.Group=cut(Mandates,breaks = c(0,1,5,10,15,20,30,50),include.lowest = T))%>%
  distinct

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

x$Publisher=sapply(x$Publisher,simpleCap)

x=join(x,attribute,by=c("Election","Party"))
x=left_join(x,Poll%>%select(Publisher,Publisher.En)%>%distinct,by=c("Publisher"))
x=left_join(x,Poll%>%select(Pollster,Pollster.En)%>%distinct,by=c("Pollster"))


x=left_join(x,attribute%>%group_by(Election,Party)%>%filter(row_number(Party)==1)%>%count(Election)%>%ungroup,
             by="Election")%>%rename(N=n)%>%mutate(Error=Mandates-Results,week=format(Date,"%w"),month=format(Date,"%m"),year=format(Date,"%Y"))%>%
  arrange(desc(Date),Election,Publisher,desc(Mandates))%>%ungroup

x=x%>%mutate(Partyid=as.numeric(factor(str_trim(Party))),
         Pollsterid=as.numeric(factor(str_trim(Pollster))),
         Publisherid=as.numeric(factor(str_trim(Publisher))),
         Election.Date=as.Date(factor(Election,labels=c("2003-01-28","2006-03-28","2009-02-10","2013-01-22","2015-03-17"))),
         DaysLeft=as.numeric(Election.Date-Date),
         Sample.Error=1/sqrt(Sample),
         Mandates.lb=floor(Mandates*(1-Sample.Error)),
         Mandates.ub=floor(Mandates*(1+Sample.Error)))

#Clean Workspace
rm(list=ls(pattern = ("[^x*|fac_vars|fac_vars.df]")))
