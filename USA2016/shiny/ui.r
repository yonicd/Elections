shinyUI(
  navbarPage("Election Polls Analysis Depot",
             
             tabPanel("Current Polling",
                      img(src='https://avatars2.githubusercontent.com/u/1755487?v=3&s=460', align = "right", height='75px'),
                      img(src='http://www.realclearpolitics.com/asset/img/rcp-logo-ss-red-250.gif', align = "right", height='75px'),
                      h3("The Election Polls Analysis Depot is an interactive web application for analysing the USA elections powered by the",
                         a("Shiny library of RStudio",href="http://shiny.rstudio.com/"),"and realtime published polling data from the",
                         a("Realclearpolitics.com",href="http://www.realclearpolitics.com"),"database."),
                      h4("Application Layout (Navigate on top ribbon of page):"),
                      strong("Election Analyis:"),p("An interactive polling analysis layout where the user can filter elections, parties,candidates, pollster, dates
                              and create different types of plots using any variable as the x and y axis.\n 
                              If you are an R user and know ggplot there is an additional editor console where you can create advanced plots freehand"),
                      strong("Polling Database:"),p("All data used in the site can be viewed and filtered."),
                      p("Keeping in an open source state of mind all code and data used can be accessed on the project homepage on" , a("Github",href="https://github.com/yonicd/Elections")),
                      br(),
                      fluidRow(column(2,downloadButton('main.down', 'Download Plot'))),
                      h4("Polling Results",align="center"),
                      plotOutput("IntroPlot",height="500px"),
                      
                      h6(a("Created and maintained by Jonathan Sidi",href="https://github.com/yonicd/Elections")),
                      h6(a("Data Source: Realclearpolitics.com",href="http://www.realclearpolitics.com")),
                      h6(a("@yoniceedee",href="https://twitter.com/yoniceedee"))),
             
             tabPanel("Election Analysis",
                      h5("Analyze polling results from the Current USA Presidential Elections by slicing the data on any variable as you please. \n
                          You can also compare poll results across Party, Candidate, Pollster, Date,.... \n
                          You can also filter any combination of values of the variables, e.g. Polls of during January across Parties. \n
                          If you are proud of the plot you made save a high resolution version of it on your computer and share it with you friends."),
                      fluidRow(
                        column(2,uiOutput("Party")),
                        column(2,uiOutput("Candidate")),
                        column(2,uiOutput("Pollster")),
                        column(2,downloadButton('foo', 'Download Plot'))
                      ),
                      
                      hr(),
                      
                      fluidRow(
                        column(2,uiOutput("DaysLeft")),
                        column(1,
                               selectInput(inputId = "ptype",label = "Graph Type",choices = c("point","bar","line","step","boxplot","density"),selected="point"),
                               selectInput("fill_var","Colour",choices=c(None=".",fac_vars),selected="Candidate")),
                        column(2,
                               selectInput("varx","X axis",selected = "Date",choices=fac_vars),
                               
                               selectInput("vary","Y axis",selected = "Results",choices=fac_vars)
                        ),
                        column(2,
                               selectInput("facet_row","Row Facet",choices=c(None=".",fac_vars),selected="Party"),
                               
                               selectInput("facet_col","Column Facet",choices=c(None=".",fac_vars))),
                        column(1,
                               radioButtons(inputId = "scales",label = "Facet Scales",choices = c("fixed","free","free_x","free_y"),selected="free_x")),
                        column(2,
                               checkboxGroupInput(inputId = "axis.attr",label = "X-axis Attrib",inline=T,choices = c("Rotate Label","Discrete")),
                               radioButtons(inputId = "facet.shp",label = "Facet Layout",inline=T,choices = c("Wrap","Grid"),selected=c("Wrap"))),
                        column(2,radioButtons(inputId = "trend",label = "Trend Line",choices = c("None","No Color","Color"),selected="Color"))
                      ),
                      hr(),
                      plotOutput('plot1',height="500px"),
                      hr(),
                      helpText(a("Add layers with R ggplot2 open code (base layer p=ggplot(x), use remove_geom(p,geom='bar') to drop layers)",href="http://docs.ggplot2.org/current/")),
                      fluidRow(
                        column(8,
                               actionButton("send", "Update Plot"),
                               aceEditor(outputId = "code",value="p",mode = "r", theme = "chrome", height = "100px", fontSize = 12))),
                      hr(),
                      helpText(a("Data Source: Realclearpolitics.com",href="http://www.realclearpolitics.com"))
             ),
             
             tabPanel("Polling Datatable",tags$head(tags$style("tfoot {display:table-header-group;}")),
                      fluidPage(
                        fluidRow(
                          column(12,
                                 dataTableOutput('table')
                          )
                        )
                      ),  
                      hr(),
                      helpText(a("Data Source: Realclearpolitics.com",href="http://www.realclearpolitics.com"))
             )
  )
)