Sys.setlocale("LC_ALL", "Hebrew")
library(dplyr);library(ggplot2);library(stringr)


x=read.csv(url("https://github.com/yonicd/Elections/raw/master/election_data.csv"))


shinyUI(
  fluidPage(
    
    fluidRow(
      column(3,uiOutput("election")),
      column(3,uiOutput("party")),
      column(3,uiOutput("Pollster")),
      column(3,uiOutput("Publisher"))
    ),
    
    hr(),
    
    fluidRow(
      column(3,uiOutput("daterange"),
             checkboxInput("wrap",label = "Wrap Graph",value = T),
             checkboxGroupInput("scales",label = "Fixed Scales",inline = T,
                                choices = c("fixed","free","free_x","free_y"),
                                selected="fixed")),
      
      column(3,
             selectInput(inputId = "ptype",label = "Graph Type",
                         choices = c("point","step","line","density","boxplot"),
                         selected="point"),
             
             selectInput("fill_var","Colour Groups",
                         choices=c(None=".",names(x)[c(1,8,3,4,10)]),selected="party")),
      column(3,
             selectInput("varx","X axis Variable",selected = "Date",
                         choices=names(x)[c(1,2,3,4,7,8,10,1)]),
             
             selectInput("vary","Y axis Variable",selected = "mandates",
                         choices=names(x)[c(1,2,3,4,7,8,10,12)])
      ),
      column(3,
             selectInput("facet_row","Row Facet",
                         choices=c(None=".",names(x)[c(1,8,3,4,10)])),
             
             selectInput("facet_col","Column Facet",
                         choices=c(None=".",names(x)[c(1,8,3,4,10)])))
    ),
    hr(),
    plotOutput('plot1')
    
    
    
  ))