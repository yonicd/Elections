####The Israel Election Polls Analysis Depot is an interactive web application for analysing the elections in Israel powered by the [Shiny library of RStudio](http://shiny.rstudio.com/) and realtime published polling data from the [Project 61](http://infomeyda.com/) database.

####The App can be found on the [shiny servers](https://yonicd.shinyapps.io/Elections) or can be run through github

```r
runGitHub("Elections","yonicd")
```
####Application Layout:
1. Election PAD
  * The latest polling day results published in the media and the prediction made using the Project 61 weighting schemes.
  
2. Election Analyis
  * An interactive polling analysis layout where the user can filter elections, parties, publishers and pollster, dates and create different types of plots using any variable as the x and y axis.
  * If you are an R user and know ggplot there is an additional editor console where you can create advanced plots freehand
3. Coalition Whiteboard
  * A bootstrap simulation is run on Polling results from up to 10 of the latest polls using the sampling error. Taking into account mandate surplus agreements and the mandate threshold, giving the final tally of mandates. The distributions are plotted per party and the location of the median published results in the media.
  * Once the simulator is complete you can create coalitions based on either the simulated distribution or actual published polls and see who can pass 60 mandates.

4. Polling Database
  * All data used in the site can be viewed and filtered in a datatable.
  
