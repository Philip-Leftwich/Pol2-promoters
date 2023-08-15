annotation_custom2 <- 
  function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ layer(data = data, stat = StatIdentity, position = PositionIdentity, 
                                                                                 geom = ggplot2:::GeomCustomAnn,
                                                                                 inherit.aes = TRUE, params = list(grob = grob, 
                                                                                                                   xmin = xmin, xmax = xmax, 
                                                                                                                   ymin = ymin, ymax = ymax))}
### adding custom images to plots

aegypti <- readPNG("img/aegypti2.png")
albopictus <- readPNG("img/albopictus.png")
frugiperda <- readPNG("img/frugiperda.png")
culex <- readPNG("img/culex.png")