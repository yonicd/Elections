require(proto)

StatRoll <- ggproto("StatRoll", ggplot2:::Stat,
                     compute_group = function(data, scales, width, FUN, fill=NA, ...) {
                       filtered <- zoo:::rollapplyr(data=data$y,width,FUN,fill=fill)
                       result <- data.frame(x=data$x, y=filtered)
                       return(result)
                     },
                     required_aes = c("x", "y")
)

stat_roll <- function(mapping = NULL, data = NULL, geom = "line",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatRoll, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# ggplot(mpg, aes(displ, hwy)) + 
#   geom_point() + 
#   stat_roll(width=9, FUN=mean,geom="point")

