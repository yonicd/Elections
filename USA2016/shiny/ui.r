shinyUI(
  fluidPage(
    list(tags$head(HTML('<link rel="icon", href="MyIcon.png", type="image/png" />'))),

    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="",windowTitle="Election PAD"
        )
    ),
    
#     p("The Election Polls Analysis Depot is an interactive web application for analysing the USA elections powered by the",
#       a("Shiny library of RStudio",href="http://shiny.rstudio.com/"),"and realtime published polling data from the",
#       a("Realclearpolitics.com",href="http://www.realclearpolitics.com"),"database.")
    
  navbarPage(title=div("Election Polls Analysis Depot",
                       a(href="http://github.com/yonicd/Elections",img(src='https://raw.githubusercontent.com/yonicd/Elections/master/USA2016/shiny/www/CDlogo_grey.png', height='35px')),
                       #a(href="https://twitter.com/yoniceedee",img(src='http://vignette1.wikia.nocookie.net/sims/images/7/7d/Twitter_icon_logo.png/revision/latest?cb=20100709233110', height='35px')),
                       a(href="http://www.realclearpolitics.com",img(src='http://www.realclearpolitics.com/asset/img/rcp-logo-ss-red-250.gif', height='35px'))
                       ),
             
             tabPanel("Current Polling",
                      h4("Application Layout (Navigate on top ribbon of page):"),
                      strong("Current Polling:"),p("A quick snapshot showing how each candidate is trending in the polls. At the end of each trend are the current published poll results for the candidate by state. If they are outside of the ribbon it means that the polling shows that the candidate is one standard deviation outside of their overall 7 day trend."),
                      strong("Election Analysis:"),p("An interactive polling analysis layout where the user can filter elections, parties,candidates, pollster, dates
                              and create different types of plots using any variable as the x and y axis.\n 
                              If you are an R user and know ggplot there is an additional editor console where you can create advanced plots freehand"),
                      strong("Polling Database:"),p("All data used in the site can be viewed and filtered."),
                      p("Keeping in an open source state of mind all code and data used can be accessed on the project homepage on" , a("Github",href="https://github.com/yonicd/Elections")),
                      p("Created and maintained by",a("Jonathan Sidi",href="https://github.com/yonicd/Elections"),
                        ", all data is from",a("Realclearpolitics.com",href="http://www.realclearpolitics.com")),
                      br(),
                      fluidRow(column(2,downloadButton('main.down', 'Download Plot'))),
                      #h4("Polling Results",align="center"),
                      plotlyOutput("IntroPlot",height="500px")
                      ),
             
             tabPanel("Election Analysis",
                      h5("Analyze polling results from the Current USA Presidential Elections by slicing the data on any variable as you please. \n
                          You can also compare poll results across Party, Candidate, Pollster, Date,.... \n
                          You can also filter any combination of values of the variables, e.g. Polls of during January across Parties. \n
                          If you are proud of the plot you made save a high resolution version of it on your computer and share it with you friends."),
                      fluidRow(
                        column(2,uiOutput("State")),
                        column(2,uiOutput("Party")),
                        column(2,uiOutput("Candidate")),
                        column(2,uiOutput("Pollster"))
                        
                      ),
                      
                      hr(),
                      
                      fluidRow(
                        column(2,uiOutput("DaysLeft"),
                        radioButtons(inputId="plotmode",label = "Plot Mode",choices=split(c("ggplot","plotly"),c("Static","Interactive")),selected = "ggplot")),
                        column(2,
                               selectInput(inputId = "ptype",label = "Graph Type",choices = c("point","bar","line","step","boxplot","density"),selected="point"),
                               selectInput("fill_var","Colour",choices=c(None=".",fac_vars),selected="Candidate")),
                        column(2,
                               selectInput("varx","X axis",selected = "Date",choices=c("Date",fac_vars)),
                               
                               selectInput("vary","Y axis",selected = "Results",choices=c("Results",fac_vars))
                        ),
                        column(2,
                               selectInput("facet_row","Row Facet",choices=c(None=".",fac_vars),selected="Party"),
                               
                               selectInput("facet_col","Column Facet",choices=c(None=".",fac_vars))),
                        column(1,
                               radioButtons(inputId = "scales",label = "Facet Scales",choices = c("fixed","free","free_x","free_y"),selected="free_x")),
                        column(2,
                               checkboxGroupInput(inputId = "axis.attr",label = "X-axis Attrib",inline=T,choices = c("Rotate Label","Discrete")),
                               radioButtons(inputId = "facet.shp",label = "Facet Layout",inline=T,choices = c("Wrap","Grid"),selected=c("Wrap"))),
                        column(3,radioButtons(inputId = "trend",label = "Trend Line",choices = c("None","No Color","Color"),selected="Color",inline=T),
                               checkboxInput("factor",label = "Group Factor",value = F))
                      ),
                      hr(),
                      fluidRow(
                       conditionalPanel("input.plotmode==='ggplot'",
                                         column(width=2,downloadButton('foo', 'Download Plot'))),
                        conditionalPanel('input.plotmode=="ggplot"',
                                         column(width=12,plotOutput(outputId = "plot1",height = "500px"))),
                        conditionalPanel('input.plotmode==="plotly"',
                                         column(width=12,plotly:::plotlyOutput(outputId = "plot1ly",width = "auto",height = "auto")))
                      ),
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
              ),
             tags$script(HTML("var header = $('.navbar > .container');
                       header.append('<div style=\"float:right\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" data-via=\"yoniceedee\" data-hashtags=\"election2016\" aling=\"middle\" data-url=\"github.com/yonicd/Elections\" data-text=\"TryR Election tRends http://bit.ly/1XkeUNb\">Tweet</a></div>');
                              console.log(header)")),
             tags$script(HTML("!function(d,s,id){
                              var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                              if(!d.getElementById(id)){
                              js=d.createElement(s);
                              js.id=id;
                              js.src=p+'://platform.twitter.com/widgets.js';
                              fjs.parentNode.insertBefore(js,fjs);
                              }
                              }(document, 'script', 'twitter-wjs');"))
  )
))