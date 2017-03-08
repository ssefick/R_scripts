gg.candisc.plot <- function(candisc.object, enlarge){
  
  
  # Plot with ellipses using ggplot2 ####
  require(grid)
  require(ggplot2)
  require(devtools)
  require(digest)
  #For info : http://stackoverflow.com/questions/2397097/how-can-a-data-ellipse-be-superimposed-on-a-ggplot2-scatterplot
  #source_url("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R") 
  
  #Nicer theme
  theme_set(theme_bw())
  
  #Basic score plot
  labels <- names(candisc.object$scores)[1]
  can.plot <- qplot(data=candisc.object$scores,
                    x=Can1,
                    y=Can2,
                    colour=get(labels))+
    stat_ellipse()+  #Add ellipses
    coord_equal()
  
  
  #Add arrows
  
  #Create arrow coordinates
  arrow.coord <- data.frame(x=0,y=0,
                            xend=candisc.object$structure[,1],yend=candisc.object$structure[,2],
                            variable=row.names(candisc.object$structure))
  
  #Enlarge arrows
  enlarge <- enlarge
  arrow.coord$xend <- enlarge*arrow.coord$xend
  arrow.coord$yend <- enlarge*arrow.coord$yend
  
  
  #Plot arrows
  can.plot <- can.plot+geom_segment(data=arrow.coord,
                                    colour=I("black"),
                                    aes(x=x,
                                        y=y,
                                        xend=xend,
                                        yend=yend),
                                    arrow = arrow(length = unit(0.5,"cm")))
  
  #Add arrow labels
  can.plot <- can.plot+geom_text(data=arrow.coord,
                                 colour=I("black"),
                                 aes(x=xend,
                                     y=yend,
                                     label=variable,
                                     hjust=0.5,
                                     vjust=0.5))
  
  
  
  print(can.plot)
}

require(proto)

StatEllipse <- proto(ggplot2:::Stat,
{
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPath
  objname <- "ellipse"
  
  calculate_groups <- function(., data, scales, ...){
    .super$calculate_groups(., data, scales,...)
  }
  calculate <- function(., data, scales, level = 0.75, segments = 51,...){
    dfn <- 2
    dfd <- length(data$x) - 1
    if (dfd < 3){
      ellipse <- rbind(c(NA,NA))	
    } else {
      require(MASS)
      v <- cov.trob(cbind(data$x, data$y))
      shape <- v$cov
      center <- v$center
      radius <- sqrt(dfn * qf(level, dfn, dfd))
      angles <- (0:segments) * 2 * pi/segments
      unit.circle <- cbind(cos(angles), sin(angles))
      ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
    }
    
    ellipse <- as.data.frame(ellipse)
    colnames(ellipse) <- c("x","y")
    return(ellipse)
  }
}
)

stat_ellipse <- function(mapping=NULL, data=NULL, geom="path", position="identity", ...) {
  StatEllipse$new(mapping=mapping, data=data, geom=geom, position=position, ...)
}