poster <- function(base_size = 15) { 
        structure(list( 
                                        axis.line =         theme_segment(), 
                                        axis.text.x =       theme_text(colour = "black", size = base_size * 0.8, vjust = 1, lineheight = 0.9), 
                                        axis.text.y =       theme_text(colour = "black", size = base_size * 0.8, hjust = 1, lineheight = 0.9), 
                                        axis.ticks =        theme_segment(colour = "black"), 
                                        axis.title.x =      theme_text(size = base_size, vjust = 0.5), 
                                        axis.title.y =      theme_text(size = base_size, vjust = 0.5, angle = 90), 
                                        axis.ticks.length = unit(0.15, "cm"), 
                                        axis.ticks.margin = unit(0.1, "cm"), 
                                        
                                        legend.background = theme_rect(colour="white"), 
                                        legend.key =        theme_blank(), 
                                        legend.key.size =   unit(1.2, "lines"), 
                                        legend.text =       theme_text(size = base_size * 0.8), 
                                        legend.title =      theme_text(face = "bold", size = base_size * 0.8, hjust = 0), 
                                        legend.position =   "right", 
                                        
                                        panel.background =  theme_blank(), 
                                        panel.border =      theme_blank(), 
                                        panel.grid.major =  theme_blank(), 
                                        panel.grid.minor =  theme_blank(), 
                                        panel.margin =      unit(0.25, "lines"), 
                                        
                                        strip.background =  theme_rect(fill = "grey80", colour = NA), 
                                        strip.label =       function(variable, value) value, 
                                        strip.text.x =      theme_text(size = base_size * 0.8), 
                                        strip.text.y =      theme_text(size = base_size * 0.8, angle = -90), 
                                        
                                        plot.background =   theme_rect(fill = "white", colour = NA), 
                                        plot.title =        theme_text(size = base_size * 1.2), 
                                        plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines") 
                        ), class = "options") 
}
