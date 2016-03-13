shinyServer(function(input, output, session) {  
#update
#Sheet 1
  IntroPrePlot <- reactive({
    State.roll=State.Polls%>%filter(!Candidate%in%c("Christie","Fiorina","Carson","Bush"))%>%arrange(Party,Candidate,Date)%>%
      group_by(Date,Party,Candidate)%>%summarise_each(funs(mean),Results)%>%group_by(Party,Candidate)%>%
      do(.,data.frame(roll.sd=zoo:::rollapplyr(.$Results,width=7,FUN=sd,fill=NA),roll.mean=zoo:::rollapplyr(.$Results,width=7,FUN=mean,fill=NA)))
    
    State.roll$Date=(State.Polls%>%filter(!Candidate%in%c("Christie","Fiorina","Carson","Bush"))%>%select(Candidate,Date)%>%arrange(Candidate,Date)%>%distinct)$Date
    
    State.roll%>%
      ggplot(aes(x=Date))+
      geom_line(aes(y=roll.mean,group=Candidate))+
      geom_ribbon(aes(ymin=roll.mean-roll.sd,ymax=roll.mean+roll.sd,fill=Candidate),alpha=.5)+
      facet_wrap(Party~Candidate,ncol=2)+theme_bw()+
      geom_text(aes(y=Results,colour=State.Abb,label=State.Abb),
                data=State.Polls%>%group_by(Candidate)%>%
                  filter(!Candidate%in%c("Christie","Fiorina","Carson","Bush")&Date==max(Date)))+
      ggtitle("Rolling Mean and Standard Deviation (7 Day Window) \n Ribbon:Mean +/- 1 Sd, Points are last day polls published")+
      ylab("Percent")+scale_colour_discrete(name="Race")
    #+theme(legend.position="top")
  })
  
  #Plot Object
  output$IntroPlot=renderPlotly({
    IntroPrePlot()
  })

#Download
#   output$main.down = downloadHandler(filename = "LastDayPlot.png",
#                                      content = function(file){
#                                        p=IntroPrePlot()+theme(text=element_text(size=25),axis.text.x = element_text(angle = 90))
#                                        ggsave(file, plot = p,width=20,height=10)})
  
#Sheet 2  
  #Filters
  output$State <- renderUI({
    State=unique(poll.shiny$State)
    selectInput("State","State Filter",choices = State,multiple=T)
  })
  
    output$Party <- renderUI({
      Party=unique(poll.shiny$Party)
      selectInput("Party","Party Filter",choices = Party,multiple=T)
    })
    output$Candidate <- renderUI({
      Candidate=unique(poll.shiny$Candidate)
      selectInput("Candidate","Candidate Filter",choices = Candidate,multiple=T)
    })
    output$Pollster <- renderUI({
      Pollster=unique(poll.shiny$Pollster)
      #if(length(input$State)>0) Pollster=poll.shiny%>%filter(State%in%input$State)%>%select(Pollster)%>%unique
      selectInput("Pollster","Pollster Filter",choices = Pollster,multiple=T)
    })
  #Slider
    output$DaysLeft<-renderUI({
    xmin=min(poll.shiny$DaysLeft,na.rm = T)
    xmax=max(poll.shiny$DaysLeft,na.rm = T)
    
    sliderInput(inputId = "DaysLeft",label = "Days Left to Conventions",
                min = xmin,max = xmax,step=1,value=c(xmin,60))
  })
  #Data
    selectedData <- reactive({
    a=poll.shiny%>%filter(!is.na(Results)&!Candidate%in%c("Christie","Fiorina","Carson","Bush"))
    if(!is.null(input$DaysLeft)) a=a%>%filter(DaysLeft>=input$DaysLeft[1]&DaysLeft<=input$DaysLeft[2])
    if(length(input$State)>0)a=a%>%filter(State%in%input$State)
    if(length(input$Party)>0)a=a%>%filter(Party%in%input$Party)
    if(length(input$Candidate)>0)a=a%>%filter(Candidate%in%input$Candidate)
    if(length(input$Pollster)>0)a=a%>%filter(Pollster%in%input$Pollster)

    x_str=input$varx
    y_str=input$vary
    str_fill=input$fill_var
    fr_str=input$facet_row
    fc_str=input$facet_col
    
    if("Discrete"%in%input$axis.attr) x_str=paste0("factor(",x_str,")")

    p=a%>%ggplot()+theme_bw()+theme(axis.text.x = element_text(angle = 90*as.numeric("Rotate Label"%in%input$axis.attr)))

    #yerr=aes_string(x=paste0("factor(",input$varx,")"),y=input$vary,
    #ymin="Mandates.lb",ymax="Mandates.ub",
    #group=paste0("factor(",input$fill_var,")"))
    
    #barplot+geom_errorbar(mapping=yerr,position="dodge")    
    #point_plot+geom_errorbar(mapping=yerr)
    
    #if(input$ptype%in%c("point","line","bar","step"))    p=p+stat_summary(fun.y=mean,aes_string(x=x_str,y=y_str,colour=str_fill,fill=str_fill),geom=input$ptype,position="dodge")
                                                              #stat_summary(fun.y=mean,aes_string(ymin="Mandates.lb",ymax="Mandates.ub",y=y_str,x=x_str,group=str_fill),geom="errorbar",position="dodge")

    xl=input$varx
    yl=input$vary
    
    if(input$ptype=="point")     p=p+geom_point(aes_string(x=x_str,y=y_str)) 
    if(input$ptype=="line")     p=p+geom_line(aes_string(x=x_str,y=y_str))
     if(input$ptype=="step")     p=p+geom_step(aes_string(x=x_str,y=y_str))
     if(input$ptype=="bar")      p=p+geom_bar(aes_string(x=x_str,y=y_str),stat="identity",position="dodge")
    if(input$ptype=="boxplot")  p=p+geom_boxplot(aes_string(x=x_str,y=y_str))
    if(input$ptype=="density"){
      x_str1=y_str
      xl=input$vary
      p=p+geom_density(aes_string(x=x_str1,y="..scaled.."),alpha=.25)}

    if (input$fill_var != '.'){
      filltxt=ifelse(input$factor,paste0("factor(",input$fill_var,")"),input$fill_var)
      
      if(input$ptype%in%c("line","point","step")){
        p = p + aes_string(color=filltxt)
        if(input$factor) p=p+scale_color_discrete(name=input$fill_var)
      }
      else if(input$ptype%in%c("boxplot","density")){
        p = p + aes_string(fill=filltxt)
        if(input$factor) p+scale_fill_discrete(name=input$fill_var)
      }
      
      }


      if(input$trend=="No Color") p=p+geom_smooth(aes_string(x=x_str,y=y_str),method="loess")
      if(input$trend=="Color") p=p+geom_smooth(aes_string(x=x_str,y=y_str,color=filltxt,fill=filltxt),method="loess")
      if(input$trend!="None" & input$factor) p+scale_colour_discrete(name=input$fill_var)+scale_fill_discrete(name=input$fill_var)
    
    
    
    
#      nm=input$fill_var
#     
#      p=p+scale_colour_discrete(name=nm)+scale_fill_discrete(name=nm)  

    
    
    
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

      p=p+xlab(xl)
      if(input$ptype!="density") p=p+ylab(yl)
      #p=p+geom_blank()
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
    
    output$plot1ly <- renderPlotly({
      pdf(NULL)
      p=selectedData()
      input$send
      isolate({
        eval(parse(text=input$code))
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