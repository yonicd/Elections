shinyServer(function(input, output, session) {  
  
  output$Election<-renderUI({
    selectInput("Election",label = "Election",
                choices = c(2015,2013,2009,2006,2003),
                selected = 2015,multiple=T)
  })
  
  output$Party <- renderUI({
    Party=x%>%filter(Election%in%input$Election)%>%select(Partyid,Party)%>%unique
    selectInput("Party","Party Filter",choices = split(Party[,1],Party[,2]),multiple=T)
  })
  
  output$Publisher <- renderUI({
    Publisher=x%>%filter(Election%in%input$Election)%>%select(Publisherid,Publisher)%>%unique
    selectInput("Publisher","Publisher Filter",choices = split(Publisher[,1],Publisher[,2]),multiple=T)
  })
  
  output$Pollster <- renderUI({
    Pollster=x%>%filter(Election%in%input$Election)%>%select(Pollsterid,Pollster)%>%unique
    selectInput("Pollster","Pollster Filter",choices = split(Pollster[,1],Pollster[,2]),multiple=T)
  })
  
  output$daterange <- renderUI({
    a=x%>%filter(Election%in%input$Election)%>%do(.,data.frame(range=as.Date(range(.$Date))))
    
    dateRangeInput("daterange", "Date Range:",
                   #start  = as.Date(paste0(format(a$range[2],"%Y-%m"),"-01"))-1,
                   start= a$range[1]-1,end=a$range[2]+1,
                   min=as.Date(range(x$Date)[1]),max=as.Date(range(x$Date)[2]))
  })
  
  output$DaysLeft<-renderUI({
    xmin=min(x[x$Election%in%input$Election,'DaysLeft'],na.rm = T)
    xmax=max(x[x$Election%in%input$Election,'DaysLeft'],na.rm = T)
    
    sliderInput(inputId = "DaysLeft",label = "Days Left",
                min = xmin,max = xmax,step=1,value=c(xmin,50))
  })
  
  selectedData <- reactive({
    a=x%>%filter(Election%in%input$Election&Date>=input$daterange[1]&
                   Date<=input$daterange[2]&!is.na(Mandates)&
                   DaysLeft>=input$DaysLeft[1]&DaysLeft<=input$DaysLeft[2])
    if(length(input$Party)>0)a=a%>%filter(Partyid%in%input$Party)
    if(length(input$Publisher)>0)a=a%>%filter(Publisherid%in%input$Publisher)
    if(length(input$Pollster)>0)a=a%>%filter(Pollsterid%in%input$Pollster)

    
#     b=a%>%filter(Date==max(Date))%>%arrange(desc(Mandates))
#     a$Party=factor(a$Party,levels=b$Party)
#     a$Party.En=factor(a$Party.En,levels=b$Party.En)

    x_str=ifelse(input$varx%in%(fac_vars[c(2,3,4)]),paste0(input$varx,input$lang),input$varx)
    y_str=ifelse(input$vary%in%(fac_vars[c(2,3,4)]),paste0(input$vary,input$lang),input$vary)
    str_fill=ifelse(input$fill_var%in%(fac_vars[c(2,3,4)]),paste0(input$fill_var,input$lang),input$fill_var)
    fr_str=ifelse(input$facet_row%in%(fac_vars[c(2,3,4)]),paste0(input$facet_row,input$lang),input$facet_row)
    fc_str=ifelse(input$facet_col%in%(fac_vars[c(2,3,4)]),paste0(input$facet_col,input$lang),input$facet_col)

    if("Discrete"%in%input$axis.attr) x_str=paste0("factor(",x_str,")")
    str_fill=paste0("factor(",str_fill,")")
    
#     if(input$ptype%in%c("step")){ 
#     a=x%>%count(Election,input$varx,input$vary,input$fill_var)%>%filter(n>1)}

    p=ggplot(a)+geom_blank()+theme_bw()+theme(axis.text.x = element_text(angle = 90*as.numeric("Rotate Label"%in%input$axis.attr)))+xlab(input$varx)

    #yerr=aes_string(x=paste0("factor(",input$varx,")"),y=input$vary,ymin="Mandates.lb",ymax="Mandates.lb",group=paste0("factor(",input$fill_var,")"))
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
    
    if(input$chkbox&input$vary=="Mandates"&input$ptype!="density") p=p+geom_hline(aes(yintercept=3.25),linetype=2)
    
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
      yl=ifelse(input$lang=="",fac_vars.df[which(fac_vars.df[,1]%in%input$vary),2],input$vary)
      p=p+xlab(xl)
      if(input$ptype!="density") p=p+ylab(yl)
      p
  })
  
  output$plot1 <- renderPlot({
    p=selectedData()
    input$send
    isolate({
      print(eval(parse(text=input$code)))
    })
  })
  
  output$foo = downloadHandler(filename = "ElectionPlot.png",
                               content = function(file){
                                ggsave(file, plot = selectedData(),width=20,height=10)})
    
  
})