library(dplyr);library(ggplot2);library(stringr)
Sys.setlocale("LC_ALL", "Hebrew")
x=read.csv("election_data.csv",stringsAsFactors=F,fileEncoding="windows-1255")
x=x%>%mutate(Date=as.Date(Date))

shinyServer(function(input, output, session) {  
  
  output$Election<-renderUI({
    selectInput("Election",label = "Election",
                choices = c(2015,2013,2009,2006,2003),
                selected = 2015,multiple=T)
  })
  
  output$Party <- renderUI({
    Party=x%>%filter(Election%in%input$Election)%>%select(Partyid,Party)%>%distinct
    selectInput("Party","Party Names",choices = split(Party[,1],Party[,2]),multiple=T)
  })
  
  output$Publisher <- renderUI({
    Publisher=x%>%filter(Election%in%input$Election)%>%select(Publisherid,Publisher)%>%distinct
    selectInput("Publisher","Publisher Names",choices = split(Publisher[,1],Publisher[,2]),multiple=T)
  })
  
  output$Pollster <- renderUI({
    Pollster=x%>%filter(Election%in%input$Election)%>%select(Pollsterid,Pollster)%>%distinct
    selectInput("Pollster","Pollster Names",choices = split(Pollster[,1],Pollster[,2]),multiple=T)
  })
  
  output$daterange <- renderUI({
    a=x%>%filter(Election%in%input$Election)%>%do(.,data.frame(range=as.Date(range(.$Date))))
    
    dateRangeInput("daterange", "Date range:",
                   start  = as.Date(paste0(format(a$range[2],"%Y-%m"),"-01"))-1,
                   end    = a$range[2]+1,
                   min=as.Date(range(x$Date)[1]),
                   max=as.Date(range(x$Date)[2]))
  })
  
  selectedData <- reactive({
    a=x%>%filter(Election%in%input$Election&Date>=input$daterange[1]&Date<=input$daterange[2])
    if(length(input$Party)>0)a=a%>%filter(Partyid%in%input$Party)
    if(length(input$Publisher)>0)a=a%>%filter(Publisherid%in%input$Publisher)
    if(length(input$Pollster)>0)a=a%>%filter(Pollsterid%in%input$Pollster)
    a
  })
  
  output$plot1 <- renderPlot({
    
    p=ggplot(selectedData())+theme_bw()+theme(axis.text.x = element_text(angle = 90))

    
    if(input$ptype=="point")  p=p+geom_point(aes_string(x=input$varx,y=input$vary,colour=paste0("factor(",input$fill_var,")")))+scale_colour_discrete(name=input$fill_var)
    if(input$ptype=="line")  p=p+geom_line(aes_string(x=input$varx,y=input$vary,colour=paste0("factor(",input$fill_var,")")))+scale_colour_discrete(name=input$fill_var)
    if(input$ptype=="bar")  p=p+geom_bar(aes_string(x=paste0("factor(",input$varx,")"),y=input$vary,fill=paste0("factor(",input$fill_var,")")),stat="identity",position="dodge")+scale_fill_discrete(name=input$fill_var)
    if(input$ptype=="boxplot")  p=p+geom_boxplot(aes_string(x=paste0("factor(",input$varx,")"),y=input$vary,fill=paste0("factor(",input$fill_var,")")))+scale_fill_discrete(name=input$fill_var)
    if(input$ptype=="density")  p=p+geom_density(aes_string(x=input$varx,fill=paste0("factor(",input$fill_var,")"),y="..scaled.."),alpha=.25)+scale_fill_discrete(name=input$fill_var)
    
    if("Elections Threshold"%in%input$chkbox&input$vary=="Mandates") p=p+geom_hline(aes(yintercept=3),linetype=2)
    
    if("Wrap Graph"%in%input$chkbox&input$facet_row!="."&input$facet_col=="."){
      p=p+facet_wrap(as.formula(paste0("~",input$facet_row)),scales=input$scales)
    }
    
    if("Wrap Graph"%in%input$chkbox&input$facet_col!="."&input$facet_row=="."){
      p=p+facet_wrap(as.formula(paste0("~",input$facet_col)),scales=input$scales)
    }
    
    if(!"Wrap Graph"%in%input$chkbox&(input$facet_col!="."|input$facet_row!=".")){
      p=p+facet_grid(paste(input$facet_row,input$facet_col,sep="~"),scales=input$scales)
    }
    
    p=p+xlab(input$varx)
    
    suppressWarnings(print(p))
  })
  
})