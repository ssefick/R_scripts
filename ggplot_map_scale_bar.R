hscale_segment = function(breaks, ...)
{
    y = unique(breaks$y)
    stopifnot(length(y) == 1)
    dx = max(breaks$x) - min(breaks$x)
    dy = 1/30 * dx
    hscale = data.frame(ix=min(breaks$x), iy=y, jx=max(breaks$x),
jy=y)
    vticks = data.frame(ix=breaks$x, iy=(y - dy), jx=breaks$x, jy=(y +
dy))
    df = rbind(hscale, vticks)
    return(geom_segment(data=df,
                        aes(x=ix, xend=jx, y=iy, yend=jy),
                        ...))

}

hscale_text = function(breaks, ...)
{
    dx = max(breaks$x) - min(breaks$x)
    dy = 2/30 * dx
    breaks$y = breaks$y + dy
    return(geom_text(data=breaks,
                     aes(x=x, y=y, label=label),
                     hjust=0.5,
                     vjust=0,
                     ...))

}


#example
#scalebreaks = data.frame(x=seq(xmax - 11000, xmax - 1000, 5000),
#                         y=(ymin + 1000),
#                         label=c("0", "5", "10 km"))

#plot = (ggplot(...)
#        + ...
#        + hscale_segment(scalebreaks, size=0.1)
#        + hscale_text(scalebreaks, size=2)
#        + ...
#        )

