 Sys.setlocale("LC_ALL", "Hebrew")

pkg=c("plyr","dplyr","reshape2","ggplot2","XML","stringr","shiny")
sapply(pkg,require,character.only = T,warn.conflicts = F,quietly = T)
rm(pkg)

results=read.csv("election_results.csv")%>%mutate(party=as.character(party))

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
  x=ldply(ret,function(df){
  df=df[!(df[,2]%in%c(NA,"")),-1]
  rownames(df)=NULL
  r=which(str_detect(df[,1],"/"))
  r.head=min(r)-1
  nc=which(str_detect(as.matrix(df[r.head,]),"כמות נסקרים"))+1
  party.names=df[r.head,nc:(min(which(str_detect(as.matrix(df)[r[1],],"%")))-1)]
  df=df[which(str_detect(df[,1],"/")),]
  df=data.frame(df[,1:(min(which(str_detect(as.matrix(df)[1,],"%")))-1)])
  if(sum(str_detect(names(df),"Var"))!=0)  df=df%>%select(-contains("Var"))
  names(df)=c(ret[[1]][5,2:6],paste0("V",seq(1,ncol(df)-5)))
  df$Date=as.Date(df$Date,"%d/%m/%Y")
  df=df%>%mutate_each(funs(as.numeric(as.character(.))),-c(Date,Publisher,Pollster,Type))%>%filter(Sample>10)
  df=melt(df,id=names(df)[1:5],value.name = "mandates")
  df$party=as.character(factor(df$variable,labels=party.names))
  df%>%select(-variable)
},.id="election")%>%
    mutate(X1=as.numeric(as.character(factor(X1,labels=c("2015","2013","2009","2006","2003")))))%>%
   rename(election=X1)
  
  #unify name of "other parties"
  x$party=str_replace(x$party,"מפלגות","רשימות")

  #join from results.csv
  x=join(x,results,by=c("election","party"))%>%arrange(election,Date,party)

  x=left_join(x,results%>%group_by(election,party)%>%filter(row_number(party)==1)%>%count(election)%>%ungroup,
            by="election")

  x=x%>%mutate(se=((mandates-results)),Publisher=str_replace_all(tolower(Publisher),"!",""),
               party=str_replace_all(party,'\"',""))%>%
   mutate_each(funs(as.numeric(factor(.))),party,Publisher,Pollster)

# a few graphs to check the data
 
# ggplot(x%>%filter(election!=2015),aes(x=Date,y=se,colour=party))+
#   geom_step()+facet_wrap(~election,scales="free",ncol=1)
# 
# ggplot(x%>%filter(election!=2015)%>%group_by(election,party)%>%do(tail(., n=20)),aes(y=se,x=attribute,fill=factor(election)))+geom_boxplot()
# 
# ggplot(x%>%mutate_each(funs(factor),party,Publisher,Pollster))+
#   geom_density(aes_string(x="mandates",fill=fill_var,y = "..scaled.."),alpha=.2)+
#   facet_grid(paste(facet_var,"party",sep="~"),drop=T)
