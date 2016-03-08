shinyServer(function(input, output, session) {  

#Sheet 1
  IntroPrePlot <- reactive({
    mainplot=poll.shiny%>%filter(Date==max(Date)&!is.na(Results))%>%select(Date,Pollster,Party,Candidate,Results)%>%
      group_by(Pollster,Party)%>%mutate(ResultsC=cumsum(Results))%>%arrange(Pollster,Party,ResultsC)%>%ungroup
    
    mainplot=left_join(mainplot,
                       poll.shiny%>%filter(Date==max(Date))%>%select(Candidate,Party,Pollster),
                       by=c("Pollster", "Party", "Candidate"))

    mainplot$Party=factor(mainplot$Party)
    str_x=paste0("as.numeric(Party)")
    str_fill="Candidate"
    str_lvl=levels(mainplot$Party)[2:1]

#     top.bar=mainplot%>%mutate_each(funs(as.numeric),Party)%>%group_by(Pollster,Party)%>%
#       filter(ResultsC==max(ResultsC))
    
    p=mainplot%>%ggplot(aes(x=as.numeric(Party),y=Results))+theme_bw()
    p=p+geom_bar(stat="identity",position="stack",aes(fill=Candidate))
    p=p+facet_wrap(~Pollster)+
      geom_text(aes(x=as.numeric(Party),y=ResultsC,label=Results),vjust=1,size=4)
    p=p+xlab("")+ylab("Results (%)")+ggtitle(paste("Polling Results:",max(poll.shiny$Date)))
    p=p+scale_x_reverse(breaks=seq(2,1),labels=str_lvl)
    p
    #p+geom_text(aes(x=as.numeric(Party),y=ResultsC,label=ResultsC),vjust=-.5,data=top.bar)
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
    output$Party <- renderUI({
      Party=unique(poll.shiny$Party)
      selectInput("Party","Party Filter",choices = Party,multiple=T)
    })
    output$Candidate <- renderUI({
      Candidate=poll.shiny%>%filter(Party%in%input$Party)%>%select(Candidate)%>%unique
      selectInput("Candidate","Candidate Filter",choices = Candidate,multiple=T)
    })
    output$Pollster <- renderUI({
      Pollster=unique(poll.shiny$Pollster)
      selectInput("Pollster","Pollster Filter",choices = Pollster,multiple=T)
    })
  #Slider
    output$DaysLeft<-renderUI({
    xmin=min(poll.shiny$DaysLeft,na.rm = T)
    xmax=max(poll.shiny$DaysLeft,na.rm = T)
    
    sliderInput(inputId = "DaysLeft",label = "Days Left to Election",
                min = xmin,max = xmax,step=1,value=c(xmin,60))
  })
  #Data
    selectedData <- reactive({
    a=poll.shiny%>%filter(!is.na(Results)&
                            DaysLeft>=input$DaysLeft[1]&DaysLeft<=input$DaysLeft[2])
    if(length(input$Party)>0)a=a%>%filter(Party%in%input$Party)
    if(length(input$Candidate)>0)a=a%>%filter(Candidate%in%input$Publisher)
    if(length(input$Pollster)>0)a=a%>%filter(Pollster%in%input$Pollster)

    x_str=input$varx
    y_str=input$vary
    str_fill=input$fill_var
    fr_str=input$facet_row
    fc_str=input$facet_col
    
    if("Discrete"%in%input$axis.attr) x_str=paste0("factor(",x_str,")")

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
    
     nm=input$fill_var
    
     p=p+scale_colour_discrete(name=nm)+scale_fill_discrete(name=nm)  

    if(input$trend=="No Color") p=p+geom_smooth(aes_string(x=x_str,y=y_str),method="loess")
    if(input$trend=="Color") p=p+geom_smooth(aes_string(x=x_str,y=y_str,fill=str_fill,colour=str_fill),method="loess")
    
    
    
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
      xl=input$varx
      xl=paste0(xl,"\n \n https:\\\\yonicd.shinyapps.io\\Elections")
      yl=input$vary
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
  output$table <- renderDataTable(poll.shiny)
})