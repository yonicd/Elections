 Sys.setlocale("LC_ALL", "Hebrew")

pkg=c("plyr","dplyr","reshape2","ggplot2","XML","stringr","shiny","shinyapps")
sapply(pkg,require,character.only = T,warn.conflicts = F,quietly = T)
rm(pkg)

results=read.csv("election_results.csv",stringsAsFactors=F)%>%select(-Origin)

  #URL of project61 google spreadsheet
    url="https://docs.google.com/spreadsheets/d/13XIAgbVk_c2Zxxa5xsR0EJFb6W9HMQpAjBImtFxZdxo/"

 #Read the spreadsheet into R (~20mb)
  doc <- paste(readLines(url), collapse=" ")
  htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
  tableNodes = getNodeSet(htmlParse(htmlTable,encoding="UTF-8"), "//table")
  ret <- mlply(c(1,seq(15,21,2)),.fun = function(i){
                        readHTMLTable(tableNodes[[i]],
                        header=T,
                        stringsAsFactors=FALSE,
                        as.data.frame=TRUE)})

 #Organize the data 
  x=ldply(ret[c(1)],function(df){
  df=df[!(df[,2]%in%c(NA,"")),-1]
  rownames(df)=NULL
  r=which(str_detect(df[,1],"/"))
  r.head=min(r)-1
  nc=which(str_detect(as.matrix(df[r.head,]),"כמות נסקרים"))+1
  party.names=df[r.head,nc:(min(which(str_detect(as.matrix(df)[r[1],],"%")))-1)]
  df=df[which(str_detect(df[,1],"/")),]
  df=data.frame(df[,1:(min(which(str_detect(as.matrix(df)[1,],"%")))-1)])
  if(sum(str_detect(names(df),"Var"))!=0)  df=df%>%select(-contains("Var"))
  names(df)=c(ret[[1]][which(ret[[1]][,2]=="Date"),2:6],paste0("V",seq(1,ncol(df)-5)))
  df$Date=as.Date(df$Date,"%d/%m/%Y")
  df=df%>%mutate_each(funs(as.numeric(as.character(.))),-c(Date,Publisher,Pollster,Type))%>%filter(Sample>10)
  df=melt(df,id=names(df)[1:5],value.name = "Mandates")
  df$Party=as.character(factor(df$variable,labels=party.names))
  df%>%select(-variable)},.id="Election")%>%mutate(Election=2015,Party=str_replace(Party,"מפלגות","רשימות"))
 
#     mutate(X1=as.numeric(as.character(factor(X1,labels=c("2015","2013","2009","2006","2003")))))%>%
#    rename(Election=X1)

  #join from results.csv
  x=join(x,results,by=c("Election","Party"))%>%arrange(Election,Date,Party)

  x=left_join(x,results%>%group_by(Election,Party)%>%filter(row_number(Party)==1)%>%count(Election)%>%ungroup,
            by="Election")%>%rename(N=n)

  x=x%>%mutate(Error=((Mandates-Results)),Publisher=str_replace_all(tolower(Publisher),"!",""),
               Party=str_replace_all(Party,'\"',""))
 
 x.old=read.csv("C:\\Users\\yoni\\Documents\\GitHub\\election_data.csv",stringsAsFactors=F)
 x=rbind(x%>%filter(Date>max(x.old$Date)),x.old%>%select(-ends_with("id")))%>%
   mutate(Date=as.Date(Date),week=format(Date,"%w"),month=format(Date,"%m"),year=format(Date,"%Y"))
 
 x$Partyid=as.numeric(factor(x$Party))
 x$Pollsterid=as.numeric(factor(x$Pollster))
 x$Publisherid=as.numeric(factor(x$Publisher))


 write.csv(x%>%distinct,file="C://Users//yoni//Documents//GitHub//Elections//election_data.csv",row.names=F)
 
# a few graphs to check the data
 
# ggplot(x%>%filter(election!=2015),aes(x=Date,y=se,colour=party))+
#   geom_step()+facet_wrap(~election,scales="free",ncol=1)
# 
# ggplot(x%>%filter(election!=2015)%>%group_by(election,party)%>%do(tail(., n=20)),aes(y=se,x=attribute,fill=factor(election)))+geom_boxplot()
# 
# ggplot(x%>%mutate_each(funs(factor),party,Publisher,Pollster))+
#   geom_density(aes_string(x="mandates",fill=fill_var,y = "..scaled.."),alpha=.2)+
#   facet_grid(paste(facet_var,"party",sep="~"),drop=T)
