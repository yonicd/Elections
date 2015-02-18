library(dplyr);library(ggplot2);library(stringr)

x=read.csv("election_data.csv",stringsAsFactors=F,fileEncoding="windows-1255")
x=x%>%mutate(Date=as.Date(Date))

fac_vars=names(x)[c(1,2,7,8,3,4,10,12,16:18)]

shinyUI(
  
  fluidPage(
    img(src='CDlogo.png', align = "right", height='75px'),
    titlePanel("Israel Election Polls Analysis Depot"),
    h6(a("Created and maintained by Jonathan Sidi",href="https://github.com/yonicd/Elections")),
    h5("Analyze polling results from Israeli Elections from 2003, 2006, 2009, 2013 and the current election, and slice the data on any variable as you please. 
       You can also compare pollster outcomes across Elections, Publishers (new outlets) and Pollsters (Midgam, Dialogue,...), and Parties.
       You can also filter any combination of values of the variables, e.g. Polls of Election 2015 Parties Likud and HaMachane HaZioni Published in Haaretz and Israel Hayom. 
       If you are proud of the plot you made save a high resolution version of it on your computer and share it with you friends."),
    hr(),
    fluidRow(
      column(3,uiOutput("Election")),
      column(3,uiOutput("Party")),
      column(3,uiOutput("Pollster")),
      column(3,uiOutput("Publisher"))
    ),
    
    hr(),
    
    fluidRow(
      column(3,uiOutput("daterange"),
             checkboxGroupInput("chkbox",label=NULL,inline=T,
                                choices = c("Wrap Graph","Elections Threshold"),
                                selected=c("Wrap Graph")),
             checkboxGroupInput("scales",label = "Facet Scales (Choose one)",inline = T,
                                choices = c("fixed","free","free_x","free_y"),
                                selected="fixed")),
      
      column(3,
             selectInput(inputId = "ptype",label = "Graph Type",
                         choices = c("point","bar","line","boxplot","density"),
                         selected="bar"),
             
             selectInput("fill_var","Colour Groups",
                         choices=c(None=".",fac_vars),selected="Pollster")),
      column(3,
             selectInput("varx","X axis Variable",selected = "Date",choices=fac_vars),
             
             selectInput("vary","Y axis Variable",selected = "Mandates",choices=fac_vars)
      ),
      column(3,
             selectInput("facet_row","Row Facet",
                         choices=c(None=".",fac_vars[-c(2,3,8)]),selected="Party"),
             
             selectInput("facet_col","Column Facet",
                         choices=c(None=".",fac_vars[-c(2,3,8)])))
    ),
    hr(),
    plotOutput('plot1'),
    hr(),
           downloadButton('foo', 'Download Plot'),
helpText(a("Data Source: Project 61 of Infomeyda",href="http://infomeyda.com/"))
  ))