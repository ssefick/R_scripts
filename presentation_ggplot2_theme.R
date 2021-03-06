presentation <- function(base_size = 12) { 
	require(grid)        
	theme( 
                                        axis.line =         element_line(), 
                                        axis.text.x =       element_text(colour = "black", size = base_size * 3, vjust = 1, lineheight = 0.9), 
					axis.text.y =       element_text(colour = "black", size = base_size * 3, hjust = 1, lineheight = 0.9), 
                                        axis.ticks =        element_line(colour = "black"), 
                                        axis.title.x =      element_text(size = base_size*3, vjust = 0.25, face="bold"), 
                                        axis.title.y =      element_text(size = base_size*3, vjust = 0.25, angle = 90, face="bold"), 
                                        axis.ticks.length = unit(0.15, "cm"), 
                                        
                                        #depricated 2016-03-27
                                        #axis.ticks.margin = unit(0.1, "cm"), 
                                        
                                        legend.background = element_rect(colour="white"), 
                                        legend.key =        element_blank(), 
                                        legend.key.size =   unit(1.2, "lines"), 
                                        legend.text =       element_text(size = base_size * 0.8), 
                                        legend.title =      element_text(face = "bold", size = base_size * 0.8, hjust = 0), 
                                        legend.position =   "right", 
                                        
                                        panel.background =  element_blank(), 
                                        panel.border =      element_rect(colour="black", fill="transparent"), 
                                        panel.grid.major =  element_blank(), 
                                        panel.grid.minor =  element_blank(), 
                                        panel.margin =      unit(0.0, "lines"), 
                                        
                                        strip.background =  element_rect(fill = "grey80", colour = NA), 
                                        #strip.label =       function(variable, value) value, 
                                        strip.text.x =      element_text(colour = "black", size = base_size * 2, vjust = 1, lineheight = 0.9, face="bold"), 
                                        strip.text.y =      element_text(size = base_size * 0.8, angle = -90), 
                                        
                                        plot.background =   element_rect(fill = "white", colour = NA), 
                                        plot.title =        element_text(size = base_size * 4, face="bold"), 
                                        plot.margin =       unit(c(1, 1, 1, 1), "lines") 
                        )
}
