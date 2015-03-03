shinyUI(
  navbarPage("Israel Election Polls Analysis Depot",
  tabPanel("Election PAD",
   img(src='CDlogo.png', align = "right", height='75px'),
   img(src='Project61logo.png', align = "right", height='75px'),
    h3("The Israel Election Polls Analysis Depot (",a("#IsraelElectionPAD",href="https://twitter.com/hashtag/IsraelElectionPAD?src=hash"),") is an interactive web application for analysing the elections in Israel powered by",
       a("Shiny of Rstudio",href="http://shiny.rstudio.com/"),"and realtime published polling data from the",
        a("Project 61",href="http://infomeyda.com/"),"database."),
   h4("Application Layout:"),
      strong("Election Analyis:"),p("An interactive polling analysis layout where the user can filter elections, parties, publishers and pollster, dates
          and create different types of plots using any variable as the x and y axis.\n 
          If you are an R user and know ggplot there is an additional editor console where you can create advanced plots freehand"),
   strong("Coalition Whiteboard:"),p("A bootstrap simulation is run on Polling results from up to 10 of the latest polls using the sampling error. 
         Taking into account mandate surplus agreements and the mandate threshold, giving the final tally of mandates. 
         The distributions are plotted per party and the location of the median published results in the media. 
         Once the simulator is complete you can create coalitions based on either the simulated distribution or 
         actual published polls and see who can pass 60 mandates."),
   strong("Polling Database:"),p("Keeping in an open source state of mind all data used in the site can be viewed and filtered and can be accessed in" , a("github",href="https://github.com/yonicd/Elections"),"."),
  br(),
  radioButtons(inputId = "lang.main",label="Plot Language",inline=T,
               choices = split(c("",".En"),c("Hebrew","English")),
               selected=".En"),
   h4("The latest Published Polling Results compared to Project 61* weighted mandates",align="center"),
    plotOutput("IntroPlot"),
   
   h6("*Project 61 results are based on last 10 Published Polls"),
   h6(a("Created and maintained by Jonathan Sidi",href="https://github.com/yonicd/Elections")),
   h6(a("Data Source: Project 61 of Infomeyda",href="http://infomeyda.com/"))
   
   ),
  tabPanel("Election Analysis",
     h5("Analyze polling results from Israeli Elections from 2003, 2006, 2009, 2013 and the current election, and slice the data on any variable as you please. 
      You can also compare pollster outcomes across Elections, Publishers (news outlets) and Pollsters (Midgam, Dialogue,...), and Parties.
      You can also filter any combination of values of the variables, e.g. Polls of Election 2015 Parties Likud and HaMachane HaZioni Published in Haaretz and Israel Hayom. 
      If you are proud of the plot you made save a high resolution version of it on your computer and share it with you friends."),
    fluidRow(
      column(3,uiOutput("Election")),
      column(2,uiOutput("Party")),
      column(2,uiOutput("Pollster")),
      column(2,uiOutput("Publisher")),
      column(2, radioButtons(inputId = "lang",label="Plot Language",inline=T,
                          choices = split(c("",".En"),c("Hebrew","English")),
                          selected=".En"))
    ),
    
    hr(),
    
    fluidRow(
      column(3,
             # uiOutput("daterange"),
             uiOutput("DaysLeft"),
             checkboxInput(inputId = "chkbox",label="Mandate Threshold",value = T)
             ),
      column(2,
             selectInput(inputId = "ptype",label = "Graph Type",choices = c("point","bar","line","step","boxplot","density"),selected="bar"),
             selectInput("fill_var","Colour Groups",choices=fac_vars,selected="Party")),
      column(2,
             selectInput("varx","X axis Variable",selected = "Date",choices=fac_vars),
             
             selectInput("vary","Y axis Variable",selected = "Mandates",choices=fac_vars)
      ),
      column(2,
             selectInput("facet_row","Row Facet",choices=c(None=".",fac_vars),selected="Pollster"),
             
             selectInput("facet_col","Column Facet",choices=c(None=".",fac_vars))),
      column(1,
              radioButtons(inputId = "scales",label = "Facet Scales",choices = c("fixed","free","free_x","free_y"),selected="free_x")),
      column(2,
             checkboxGroupInput(inputId = "axis.attr",label = "X-axis Attrib",choices = c("Rotate Label","Discrete"),selected=c("Discrete")),
             radioButtons(inputId = "facet.shp",label = "Facet Layout",inline=T,choices = c("Wrap","Grid"),selected=c("Wrap")))
    ),
    hr(),
    plotOutput('plot1',height="500px"),
    downloadButton('foo', 'Download Plot'),
    hr(),
    helpText(a("Add layers with R ggplot2 open code (base layer p=ggplot(x))",href="http://docs.ggplot2.org/current/")),
    fluidRow(
      column(8,
             actionButton("send", "Add"),
             aceEditor(outputId = "code",value="p",mode = "r", theme = "chrome", height = "100px", fontSize = 12))),
      hr(),
      helpText(a("Data Source: Project 61 of Infomeyda",href="http://infomeyda.com/"))
),

  tabPanel("Coalition Whiteboard",
           h5("Polling results are usually shown to the public as the mean result and the sample error is reported as a sidenote. 
              To better illustrate the sample error affect on the final outcome a simulation has been run which draws from the statistically possible
              number of mandates which fall within the sample error for each poll within a chosen number of latest polls (you can choose any of the last ten polls)."),
           h5("You can view the results of the simulation by ticking the View Simulation Results checkbox. The results shown are histograms for each party and their final outcome
              number of mandates after taking into account the mandate threshold, this election it is 4, and surplus voting agreements, using the",a("Hagenbach-Bischoff method",href="http://en.wikipedia.org/wiki/Hagenbach-Bischoff_quota"),"."),
            h5("After specifying the simulation, one is run for you by default, you can build your own coalition, either from the actual polling results or from a random result from the simulation,
                 and see what combination passes the threshold of 60 mandates. Choose in the dropdown boxes what parties build the coalition and which ones are in the opposotion."),
           
             sliderInput("poll","Number of Last Polls to Use",min = 1,max=10,value = c(1,5),step = 1),
             checkboxInput("sim","View Simulation Results",value = F),
           hr(),
           radioButtons(inputId = "lang.sim",label="Plot Language",inline=T,
                        choices = split(c("",".En"),c("Hebrew","English")),
                        selected=".En"),
           conditionalPanel("input.sim",plotOutput('SimPlot',height="500px")),
           conditionalPanel(condition = "!input.sim",
                            fluidRow(
                              h4("Create Coalition"),
                              column(4,radioButtons("var_y",NULL,split(c("Mandates","rMandates"),c("Average Polling Results","Polling Results with Sample Error")),selected="Mandates",inline=T)),
                              column(3,uiOutput("Coalition")),
                              column(3,uiOutput("Opposition"))
                            ),
                            hr(),
                            plotOutput('CoalitionPlot',height="400px")),
           hr(),
           helpText(a("Data Source: Project 61 of Infomeyda",href="http://infomeyda.com/"))
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
         helpText(a("Data Source: Project 61 of Infomeyda",href="http://infomeyda.com/"))
         )
)
)