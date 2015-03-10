shinyServer(function(input, output, session) {  

#Sheet 1
  IntroPrePlot <- reactive({
    mainplot=rbind(x%>%filter(Date==max(Date))%>%select(Date,Pollster,Party,Mandates,Ideology),project61)%>%
      group_by(Pollster,Ideology)%>%mutate(MandatesC=cumsum(Mandates))%>%arrange(Pollster,Ideology,MandatesC)%>%ungroup
    
    mainplot=left_join(mainplot,
                       x%>%filter(Date==max(Date))%>%select(Party,Pollster,Ideology,Party.En,Pollster.En,Ideology.En),
                       by=c("Pollster", "Party", "Ideology"))%>%mutate(Pollster.En=ifelse(Pollster=="פרויקט 61","Project 61",Pollster.En))
    
    party.temp=x%>%filter(Date==max(Date))%>%select(Party,Party.En,Ideology.En)%>%unique
    
    party.temp=party.temp[match(mainplot$Party[which(mainplot$Pollster.En=="Project 61")],party.temp$Party),-1]
    mainplot$Party.En[which(mainplot$Pollster.En=="Project 61")]=party.temp$Party.En
    mainplot$Ideology.En[which(mainplot$Pollster.En=="Project 61")]=party.temp$Ideology.En
    
    mainplot$Ideology=droplevels(mainplot$Ideology)
    mainplot$Ideology.En=factor(mainplot$Ideology.En,levels=c("Right","Center-Right","Center-Left","Left"))
    
    
    str_x=paste0("as.numeric(",paste0("Ideology",input$lang.main),")")
    str_fill=paste0("Party",input$lang.main)
    str_title=ifelse(input$lang.main=="",
                     paste("תוצאות נכון ל:",max(x$Date)),
                     paste("Polling Results:",max(x$Date)))
    if(input$lang.main==""){
      str_lvl=levels(mainplot$Ideology)[4:1]
    }else{
      str_lvl=levels(mainplot$Ideology.En)[4:1]
    }
    
    lang.id=ifelse(input$lang.main=="",2,1)
    
    top.bar=mainplot%>%mutate_each(funs(as.numeric),Ideology.En,Ideology)%>%group_by(Pollster,Pollster.En,Ideology,Ideology.En)%>%
      filter(MandatesC==max(MandatesC))
    
    p=ggplot(mainplot,aes_string(x=str_x,y="Mandates"))+theme_bw()
    p=p+geom_bar(stat="identity",position="stack",aes_string(fill=str_fill))+scale_fill_discrete(name=fac_vars.df[which(fac_vars=="Party"),lang.id])
    p=p+geom_hline(yintercept=61,linetype=2)+facet_wrap(as.formula(paste0("~",paste0("Pollster",input$lang.main))))+
      geom_text(aes_string(x=str_x,y="MandatesC",label="Mandates"),vjust=1,size=4)
    p=p+xlab("\n\n\n https:\\\\yonicd.shinyapps.io\\Elections \n Project61: http:\\\\infomeyda.com")+ylab(fac_vars.df[which(fac_vars=="Mandates"),lang.id])+ggtitle(str_title)
    p=p+scale_x_reverse(breaks=seq(4,1),labels=str_lvl)
    p+geom_text(aes_string(x=str_x,y="MandatesC",label="MandatesC"),vjust=-.5,data=top.bar)+ylim(0,70)
  })
  
  #Plot Object
  output$IntroPlot=renderPlot({
    print(IntroPrePlot())
  })

  #Download
  output$main.down = downloadHandler(filename = "LastDayPlot.png",
                                     content = function(file){
                                       p=IntroPrePlot()+theme(text=element_text(size=25),axis.text.x = element_text(angle = 90))
                                       ggsave(file, plot = p,width=20,height=10)})
  
#Sheet 2  
  #Filters
    output$Election<-renderUI({
      selectInput("Election",label = "Election",
                  choices = c(2015,2013,2009,2006,2003),
                  selected = 2015,multiple=T)
    })
    output$Party <- renderUI({
      Party=x%>%filter(Election%in%input$Election)%>%select(Partyid,Party,Party.En)%>%unique
      selectInput("Party","Party Filter",choices = split(Party[,1],paste(Party[,3],Party[,2],sep=":")),multiple=T)
    })
    output$Publisher <- renderUI({
      Publisher=x%>%filter(Election%in%input$Election)%>%select(Publisherid,Publisher,Publisher.En)%>%unique
      selectInput("Publisher","Publisher Filter",choices = split(Publisher[,1],paste(Publisher[,3],Publisher[,2],sep=":")),multiple=T)
    })
    output$Pollster <- renderUI({
      Pollster=x%>%filter(Election%in%input$Election)%>%select(Pollsterid,Pollster,Pollster.En)%>%unique
      selectInput("Pollster","Pollster Filter",choices = split(Pollster[,1],paste(Pollster[,3],Pollster[,2],sep=":")),multiple=T)
    })
  #Slider
    output$DaysLeft<-renderUI({
    xmin=min(x[x$Election%in%input$Election,'DaysLeft'],na.rm = T)
    xmax=max(x[x$Election%in%input$Election,'DaysLeft'],na.rm = T)
    
    sliderInput(inputId = "DaysLeft",label = "Days Left to Election",
                min = xmin,max = xmax,step=1,value=c(xmin,30))
  })
  #Data
    selectedData <- reactive({
    a=x%>%filter(Election%in%input$Election&
                   !is.na(Mandates)&
                   DaysLeft>=input$DaysLeft[1]&DaysLeft<=input$DaysLeft[2])
    if(length(input$Party)>0)a=a%>%filter(Partyid%in%input$Party)
    if(length(input$Publisher)>0)a=a%>%filter(Publisherid%in%input$Publisher)
    if(length(input$Pollster)>0)a=a%>%filter(Pollsterid%in%input$Pollster)

    
    en.list=c(1,2,3,10,11,16)
    
    x_str=ifelse(input$varx%in%(fac_vars[en.list]),paste0(input$varx,input$lang),input$varx)
    y_str=ifelse(input$vary%in%(fac_vars[en.list]),paste0(input$vary,input$lang),input$vary)
    if(input$fill_var=="."){
      str_fill=NULL
    }else{
      str_fill=ifelse(input$fill_var%in%(fac_vars[en.list]),paste0(input$fill_var,input$lang),input$fill_var)
    }
    fr_str=ifelse(input$facet_row%in%(fac_vars[en.list]),paste0(input$facet_row,input$lang),input$facet_row)
    fc_str=ifelse(input$facet_col%in%(fac_vars[en.list]),paste0(input$facet_col,input$lang),input$facet_col)
    
    if("Discrete"%in%input$axis.attr) x_str=paste0("factor(",x_str,")")

    a$Election=factor(a$Election)
    
    p=ggplot(a)+geom_blank()+theme_bw()+theme(axis.text.x = element_text(angle = 90*as.numeric("Rotate Label"%in%input$axis.attr)))

    #yerr=aes_string(x=paste0("factor(",input$varx,")"),y=input$vary,
    #ymin="Mandates.lb",ymax="Mandates.ub",
    #group=paste0("factor(",input$fill_var,")"))
    
    #barplot+geom_errorbar(mapping=yerr,position="dodge")    
    #point_plot+geom_errorbar(mapping=yerr)
    
    #if(input$ptype%in%c("point","line","bar","step"))    p=p+stat_summary(fun.y=mean,aes_string(x=x_str,y=y_str,colour=str_fill,fill=str_fill),geom=input$ptype,position="dodge")
                                                              #stat_summary(fun.y=mean,aes_string(ymin="Mandates.lb",ymax="Mandates.ub",y=y_str,x=x_str,group=str_fill),geom="errorbar",position="dodge")

    if(input$ptype=="point")     p=p+geom_point(aes_string(x=x_str,y=y_str,colour=str_fill)) 
    if(input$ptype=="line")     p=p+geom_line(aes_string(x=x_str,y=y_str,colour=str_fill))
     if(input$ptype=="step")     p=p+geom_step(aes_string(x=x_str,y=y_str,colour=str_fill))
     if(input$ptype=="bar")      p=p+geom_bar(aes_string(x=x_str,y=y_str,fill=str_fill),stat="identity",position="dodge")
    if(input$ptype=="boxplot")  p=p+geom_boxplot(aes_string(x=x_str,y=y_str,fill=str_fill))
    if(input$ptype=="density")  p=p+geom_density(aes_string(x=x_str,fill=str_fill,y="..scaled.."),alpha=.25)
    
     nm=ifelse(input$lang=="",fac_vars.df[which(fac_vars.df[,1]%in%input$fill_var),2],input$fill_var)
    
     p=p+scale_colour_discrete(name=nm)+scale_fill_discrete(name=nm)  

     if(input$trend=="No Color") p=p+geom_smooth(aes_string(x=x_str,y=y_str))
    if(input$trend=="Color") p=p+geom_smooth(aes_string(x=x_str,y=y_str,fill=str_fill))
    
    if(input$chkbox&input$vary=="Mandates"&input$ptype!="density") p=p+geom_hline(aes(yintercept=4),linetype=2)
    
#    p=p+geom_errorbar(aes(ymin=lb,ymax=ub))
    
if(input$facet.shp=="Wrap"){
  if(input$facet_row!="."&input$facet_col=="."){
    p=p+facet_wrap(as.formula(paste0("~",fr_str)),scales=input$scales)
  }
  
  if(input$facet_row=="."&input$facet_col!="."){
    p=p+facet_wrap(as.formula(paste0("~",fc_str)),scales=input$scales)
  }
  
  if(input$facet_row!="."&input$facet_col!="."){
    p=p+facet_wrap(as.formula(paste(fr_str,fc_str,sep="~")),scales=input$scales)
  }  
}else{
  if(input$facet_col!="."|input$facet_row!="."){
    p=p+facet_grid(paste(fr_str,fc_str,sep="~"),scales=input$scales)
  }
}
      xl=ifelse(input$lang=="",fac_vars.df[which(fac_vars.df[,1]%in%input$varx),2],input$varx)
      xl=paste0(xl,"\n \n https:\\\\yonicd.shinyapps.io\\Elections")
      yl=ifelse(input$lang=="",fac_vars.df[which(fac_vars.df[,1]%in%input$vary),2],input$vary)
      p=p+xlab(xl)
      if(input$ptype!="density") p=p+ylab(yl)
      p
  })
  #Plot
    output$plot1 <- renderPlot({
    p=selectedData()
    input$send
    isolate({
      print(eval(parse(text=input$code)))
    })
  })
  #Download Main Plot
    output$foo = downloadHandler(filename = "ElectionPlot.png",
                               content = function(file){
                                 p=selectedData()+theme(text=element_text(size=18))
                                ggsave(file, plot = eval(parse(text=input$code)),width=20,height=10)})
#Sheet 3  
  #Filters
  output$Coalition <- renderUI({
    selectInput("coalition","Coalition",choices = split(party[,1],paste(party[,3],party[,2],sep=":")),multiple=T)
  })
  output$Opposition <- renderUI({
    selectInput("opposition","Opposition",choices = split(party[,1],paste(party[,3],party[,2],sep=":")),multiple=T)
  })
  #Data
    CoalitionData <- reactive({
    a=x%>%mutate(Dateid=factor(Date),DatePoll=factor(paste(Dateid,Pollsterid,sep=".")))
    nt=tail(levels(a$DatePoll),input$poll[2])
    nt=tail(nt,input$poll[2]-input$poll[1]+1)
    a=a%>%filter(DatePoll%in%nt)
    maxdate=max(a$Date)
    a$nr=1:nrow(a)
    out=ddply(a%>%select(Mandates.lb,Mandates.ub,nr),.(nr),
              .fun = function(df) {
                B=input$Boot
                mout=cbind(as.matrix(sample(as.numeric(df[1]):as.numeric(df[2]),B,replace=T),ncol=1),1:B)
              })
    names(out)[-1]=c("rMandates","bs.id")
    a=left_join(out,a,by=c("nr"))

    a=a%>%group_by(bs.id,Party)%>%summarise_each(funs(mean(.,na.rm = T)),Mandates,rMandates)%>%
      mutate(rMandates=ifelse(floor(rMandates)<4,0,floor(rMandates)),
             Mandates=ifelse(floor(Mandates)<4,0,floor(Mandates)))%>%arrange(Party)
    
    df=melt(a,id=c("bs.id","Party"),value.name = "base")
    df$base=df$base+runif(nrow(df))*1e-3
    

      df=df%>%group_by(bs.id,variable)%>%
        mutate(base=120*base/sum(base),
               base.floor=floor(base),
               pct=base/(base.floor+1),
               add=0,id=0,
               share=factor(Party,labels=c(1,1,2,3,4,5,6,6,2,7,3)))  

    
    
    #hagenbach-bischoff system 
    df=ddply(df,.(bs.id,variable),.fun = function(df){
      for(i in (1:(120-sum(df$base.floor)))){
        y1=(df%>%group_by(share)%>%summarise(group_pct=sum(base)/(sum(base.floor)+1))%>%filter(group_pct==max(group_pct)))$share
        id=df$pct%in%max(df$pct[df$share==y1])
        df$id[id]=i
        df$base.floor[id]=df$base.floor[id]+1
        df$add[id]=df$add[id]+1
        df$pct[id]=df$base[id]/(df$base.floor[id]+1)}
      
      return(df)
    })

    Party.En=x%>%filter(Election==2015)%>%select(Party,Party.En)%>%unique
      
    df=left_join(df,Party.En,by="Party")  
    
    fill_var="Party"
    pos="dodge"
    yint=4
    
    b.id=sample(1:input$Boot,1)
    
    lout=list(df=df%>%filter(bs.id==b.id),df.full=df,fill_var=fill_var,pos=pos,yint=yint,maxdate=maxdate)
    return(lout)
  })
  
  #Simulation Data
    SimPrePlot<-reactive({
    lin=CoalitionData()
    df=lin$df.full
    
    base.lvl=df%>%filter(variable=="rMandates")%>%count(Party.En,Party,base.floor)
    txt.lvl=df%>%filter(variable=="Mandates")%>%group_by(Party.En,Party)%>%summarise(base.floor=floor(mean(base.floor)))
    
    txt.lvl=left_join(txt.lvl,base.lvl,by=c("Party.En","Party","base.floor"))


    str_fill=paste0("Party",input$lang.sim)
    if(input$lang.sim==""){
      xl="מנדטים"
      xl=paste0(xl,"\n \n https:\\\\yonicd.shinyapps.io\\Elections")
      yl=""
      ttl=paste("התפלגות סימולציה לחלוקה סופית של מנדטים לאחר אחוז החסימה וסיווג עודפים לפי מפלגה",
                "משולש מהווה חציון המנדטים למפלגה על פי הסקרים בפועל",
                lin$maxdate,sep="\n")
      nm="מפלגה"
    }else{
      xl="Mandates"
      xl=paste0(xl,"\n \n https:\\\\yonicd.shinyapps.io\\Elections")
      yl=""
      ttl=paste("Distribution of Simulated of Mandate Results Conditioned on Mandate Threshold and Surplus Agreements \n Triangle shows the Median Published Result",lin$maxdate,sep="\n")
      nm="Party"
    }
    
    base.lvl$n=base.lvl$n/as.numeric(input$Boot)
    txt.lvl$n=txt.lvl$n/as.numeric(input$Boot)
    
    p=ggplot(base.lvl)+
      geom_bar(aes_string(x="factor(base.floor)",y="n",fill=str_fill),stat="identity",position = "dodge")+
      geom_point(aes(x=factor(base.floor),y=n+.02),size=3,shape=17,data=txt.lvl)+
      facet_wrap(as.formula(paste0("~",str_fill)),scales="free_x")+theme_bw()+
      ggtitle(ttl)+ylab(yl)+xlab(xl)+scale_fill_discrete(name=nm)+scale_y_continuous(label=percent)
    p
  })
  #Simulaton Plot
    output$SimPlot <- renderPlot({
      print(SimPrePlot())
    })
    
  #Coalition Data                                                                                              
    CoalitionPrePlot <- reactive({
      lin=CoalitionData()
      df=lin$df
      a1=df%>%select(variable,Party.En,Party,Mandates=base.floor)%>%group_by(variable)%>%arrange(desc(Mandates))
      a1$Party=factor(a1$Party,levels=a1$Party[1:11])
      if(length(c(input$coalition,input$opposition))!=0){
        c1=data.frame(Partyid=as.numeric(input$coalition))%>%mutate(Block="קואליציה")
        c2=data.frame(Partyid=as.numeric(input$opposition))%>%mutate(Block="אופוזיציה")
        b=rbind(c1,c2)
        
        b0=left_join(df%>%select(variable,Party.En,Party,base.floor),party,by=c("Party","Party.En"))
        b0$Partyid=as.numeric(as.character(b0$Partyid))
        
        c1=left_join(b0,b,by="Partyid")%>%mutate(Block=ifelse(is.na(Block),"לא סווג",Block))%>%
          group_by(variable,Block,Party,Party.En)%>%summarise_each(funs(floor(mean(.,na.rm = T))),base.floor)
        c1=left_join(c1,data.frame(Block.En=c("Coalition","Opposition","Unassigned"),
                   Block=c("קואליציה","אופוזיציה","לא סווג")),by="Block")
        c1$Block=factor(c1$Block,levels = c("קואליציה","אופוזיציה","לא סווג"))
        c1$Block.En=factor(c1$Block.En,levels = c("Coalition","Opposition","Unassigned"))
        lin$fill_var="Block"
        lin$pos="stack"
        lin$yint=60
        lin$df=c1
      }
      
      b=lin$df%>%filter(variable==input$var_y)%>%
        group_by_(.dots=c(as.symbol(as.character(lin$fill_var)),as.symbol(paste0(as.character(lin$fill_var),".En"))))%>%
        summarise(Mandates=sum(base.floor))
      
      str_fill=paste0("Party",input$lang.sim)
      str_x=paste0(lin$fill_var,input$lang.sim)
      
      if(input$lang.sim==""){
        yl="מנדטים ממוצע"
        ttl=paste("סהכ מנדטים:",floor(sum(b$Mandates)))
        nm="מפלגה"
      }else{
        yl="Average Mandates"
        ttl=paste("Total Mandates:",floor(sum(b$Mandates)))
        nm="Party"
      }
      
      p=ggplot(lin$df%>%filter(variable==input$var_y)%>%rename(Mandates=base.floor),
                aes_string(x=str_x,y="Mandates"))+theme_bw()
      p=p+geom_bar(aes_string(fill=str_fill),stat="identity",position=lin$pos)+
        scale_fill_discrete(name=nm)
      p=p+geom_text(aes_string(x=str_x,y="Mandates",label="Mandates"),vjust=-.1,data=b)
      p=p+geom_hline(yintercept=lin$yint,linetype=2)
      p=p+xlab("\n\n\n https:\\\\yonicd.shinyapps.io\\Elections")+ylab(yl)+ggtitle(ttl)
      p
    })

  #Coalition Plot
    output$CoalitionPlot <- renderPlot({
        print(CoalitionPrePlot())
    })
  
    output$sim.down = downloadHandler(filename = "ElectionSimPlot.png",
                                      content = function(file){
                                        if(input$sim=="sim"){
                                          p=SimPrePlot()
                                          #m=readPNG("PADqr.png")
                                          #p1=ggplotGrob(p)
                                          #p2=ggplotGrob(
                                          #qplot(1,1,geom="blank")+theme_bw()+theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                                          #text=element_blank(),axis.ticks=element_blank())+
                                          
                                          #p+annotation_custom(g=rasterGrob(m,x = 4.6,y =.85),xmax=1,xmin=-Inf,ymax=Inf,ymin=-Inf)
                                          #p0=arrangeGrob(p1,p2,)
                                          #grid.arrange(p0)
                                          #grid.newpage()
                                          #p=grid.draw(p3)
                                        }else{
                                          p=CoalitionPrePlot()
                                        }
                                        ggsave(file, plot = p+theme(text=element_text(size=22)),width=20,height=10)})
#Sheet 4  
  output$table <- renderDataTable(x%>%select(-c(N,contains("id"))))
})