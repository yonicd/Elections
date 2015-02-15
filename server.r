Sys.setlocale("LC_ALL", "Hebrew")
library(dplyr);library(ggplot2);library(stringr)

x=read.csv(url("https://github.com/yonicd/Elections/raw/master/election_data.csv"))

shinyServer(function(input, output, session) {  

  output$election<-renderUI({
    selectInput("election",label = "Election",
                choices = unique(x$election),
                selected = unique(x$election)[5],multiple=T)
  })
  
  output$party <- renderUI({
    party=x%>%filter(election%in%input$election)%>%select(partyid,party)%>%distinct
    selectInput("party","Party Names",choices = split(party[,1],party[,2]),multiple=T)
  })
  
  output$Publisher <- renderUI({
    Publisher=x%>%filter(election%in%input$election)%>%select(Publisherid,Publisher)%>%distinct
    selectInput("Publisher","Publisher Names",choices = split(Publisher[,1],Publisher[,2]),multiple=T)
  })
  
  output$Pollster <- renderUI({
    Pollster=x%>%filter(election%in%input$election)%>%select(Pollsterid,Pollster)%>%distinct
    selectInput("Pollster","Pollster Names",choices = split(Pollster[,1],Pollster[,2]),multiple=T)
  })
  
  output$daterange <- renderUI({
  dateRangeInput("daterange", "Date range:",
                 start  = range((x%>%filter(election%in%input$election))$Date)[1],
                 end    = range((x%>%filter(election%in%input$election))$Date)[2])
  })
  
  selectedData <- reactive({
    a=x%>%filter(election%in%input$election&Date>=input$daterange[1]&Date<=input$daterange[2])
    if(length(input$party)>0)a=a%>%filter(partyid%in%input$party)
    if(length(input$Publisher)>0)a=a%>%filter(Publisherid%in%input$Publisher)
    if(length(input$Pollster)>0)a=a%>%filter(Pollsterid%in%input$Pollster)
    a
  })
  

  
    output$plot1 <- renderPlot({

    p=ggplot(selectedData())+theme_bw()
    
    if(input$ptype=="point")  p=p+geom_point(aes_string(x=input$varx,y=input$vary,colour=paste0("factor(",input$fill_var,")")))
    if(input$ptype=="line")  p=p+geom_line(aes_string(x=input$varx,y=input$vary,colour=paste0("factor(",input$fill_var,")")))
    if(input$ptype=="step")  p=p+geom_step(aes_string(x=input$varx,y=input$vary,colour=paste0("factor(",input$fill_var,")")))
    if(input$ptype=="density")  p=p+geom_density(aes_string(x=input$varx,fill=paste0("factor(",input$fill_var,")"),y="..scaled.."),alpha=.25)
    if(input$ptype=="boxplot")  p=p+geom_boxplot(aes_string(x=paste0("factor(",input$varx,")"),y=input$vary,fill=paste0("factor(",input$fill_var,")")))
    
    if(input$wrap==T&input$facet_row!="."&input$facet_col=="."){
      p=p+facet_wrap(as.formula(paste0("~",input$facet_row)),scales=input$scales)
    }
    
    if(input$wrap==T&input$facet_col!="."&input$facet_row=="."){
      p=p+facet_wrap(as.formula(paste0("~",input$facet_col)),scales=input$scales)
    }
    
    if(input$wrap==F&input$facet_col!="."&input$facet_row!="."){
      p=p+facet_grid(paste(input$facet_row,input$facet_col,sep="~"),scales=input$scales)
    }
    
    suppressWarnings(print(p))
  })
  
})