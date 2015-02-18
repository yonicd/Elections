require(ggplot2);require(proto)
stairstepn <- function( data, direction="hv", yvars="y" ) {
  direction <- match.arg( direction, c( "hv", "vh" ) )
  data <- as.data.frame( data )[ order( data$x ), ]
  n <- nrow( data )
  
  if ( direction == "vh" ) {
    xs <- rep( 1:n, each = 2 )[ -2 * n ]
    ys <- c( 1, rep( 2:n, each = 2 ) )
  } else {
    ys <- rep( 1:n, each = 2 )[ -2 * n ]
    xs <- c( 1, rep( 2:n, each = 2))
  }
  
  data.frame(
    x = data$x[ xs ]
    , data[ ys, yvars, drop=FALSE ]
    , data[ xs, setdiff( names( data ), c( "x", yvars ) ), drop=FALSE ]
  ) 
}

stat_stepribbon <- function( mapping=NULL, data=NULL, geom="ribbon", position="identity" ) {
  StatStepribbon$new( mapping=mapping, data=data, geom=geom, position=position )
}

StatStepribbon <- proto(ggplot2:::Stat, {
  objname <- "stepribbon"
  desc <- "Stepwise area plot"
  desc_outputs <- list(
    x = "stepped independent variable",
    ymin = "stepped minimum dependent variable",
    ymax = "stepped maximum dependent variable"
  )
  required_aes <- c( "x", "ymin", "ymax" )
  
  default_geom <- function(.) GeomRibbon
  default_aes <- function(.) aes( x=..x.., ymin = ..y.., ymax=Inf )
  
  calculate <- function( ., data, scales, direction = "hv", yvars = c( "ymin", "ymax" ), ...) {
    stairstepn( data = data, direction = direction, yvars = yvars )
  }
  
  examples <- function(.) {
    DF <- data.frame( x = 1:3, ymin = runif( 3 ), ymax=rep( Inf, 3 ) )
    ggplot( DF, aes( x=x, ymin=ymin, ymax=ymax ) ) + stat_stepribbon()
    
  }
  
})

# dta <- read.table( text=
#                      "Indep   Dep   DepLim
#      0        0.5   2
#      1        1.5   Inf
#      2        2.5   Inf
# ", header=TRUE, as.is=TRUE )
# limmaxy <- 3
# 
# ggplot( dta, aes( x=Indep, y=Dep ) ) +
#   geom_step( size=2, direction="hv" ) +
#   geom_ribbon( aes( x=Indep, ymin=Dep, ymax=DepLim ), stat="stepribbon", fill="blue", alpha=0.3, direction="hv" ) +
#   coord_cartesian( ylim=c( 0, limmaxy ) )