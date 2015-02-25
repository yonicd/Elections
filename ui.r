shinyUI(
  
  fluidPage(
    img(src='CDlogo.png', align = "right", height='75px'),
    titlePanel("Israel Election Polls Analysis Depot"),
    h6(a("Created and maintained by Jonathan Sidi",href="https://github.com/yonicd/Elections")),
    h5("Analyze polling results from Israeli Elections from 2003, 2006, 2009, 2013 and the current election, and slice the data on any variable as you please. 
       You can also compare pollster outcomes across Elections, Publishers (news outlets) and Pollsters (Midgam, Dialogue,...), and Parties.
       You can also filter any combination of values of the variables, e.g. Polls of Election 2015 Parties Likud and HaMachane HaZioni Published in Haaretz and Israel Hayom. 
       If you are proud of the plot you made save a high resolution version of it on your computer and share it with you friends."),
    hr(),
    fluidRow(
      column(3,uiOutput("Election")),
      column(2,uiOutput("Party")),
      column(2,uiOutput("Pollster")),
      column(2,uiOutput("Publisher")),
      column(2, radioButtons(inputId = "lang",label="Plot Language",inline=T,
                          choices = split(c("",".En"),c("Hebrew","English")),
                          selected=""))
    ),
    
    hr(),
    
    fluidRow(
      column(3,uiOutput("daterange"),
             uiOutput("DaysLeft"),
             checkboxInput(inputId = "chkbox",label="Mandate Threshold",value = T)
             ),
      
      column(2,
             selectInput(inputId = "ptype",label = "Graph Type",
                         choices = c("point","bar","line","step","boxplot","density"),
                         selected="bar"),
             
             selectInput("fill_var","Colour Groups",
                         choices=fac_vars,selected="Party")),
      column(2,
             selectInput("varx","X axis Variable",selected = "Date",choices=fac_vars),
             
             selectInput("vary","Y axis Variable",selected = "Mandates",choices=fac_vars)
      ),
      column(2,
             selectInput("facet_row","Row Facet",
                         choices=c(None=".",fac_vars),selected="Pollster"),
             
             selectInput("facet_col","Column Facet",
                         choices=c(None=".",fac_vars))),
      column(1,
              radioButtons(inputId = "scales",label = "Facet Scales",
                           choices = c("fixed","free","free_x","free_y"),
                           selected="free_x")),
      column(2,
             checkboxGroupInput(inputId = "axis.attr",label = "X-axis Attrib",
                          choices = c("Rotate Label","Discrete"),
                          selected=c("Discrete")),
             radioButtons(inputId = "facet.shp",label = "Facet Layout",inline=T,
                                choices = c("Wrap","Grid"),
                                selected=c("Wrap")))
    ),
    hr(),
    plotOutput('plot1'),
    hr(),
    helpText(a("Add layers with R ggplot2 open code (base layer p=ggplot(x))",href="http://docs.ggplot2.org/current/")),
    fluidRow(
      column(8,
             actionButton("send", "Add"),
             aceEditor(outputId = "code",
                       value="p",
                       mode = "r", theme = "chrome", height = "50px", fontSize = 12))),
    hr(),
           downloadButton('foo', 'Download Plot'),
helpText(a("Data Source: Project 61 of Infomeyda",href="http://infomeyda.com/"))
  ))