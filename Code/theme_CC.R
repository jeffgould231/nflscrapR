theme_CC = function(base_size = 11, base_family = "") {
  
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      title = element_text(color = "black", family = "Impact", face = "bold"),
      # Specify axis options
      axis.line = element_blank(), 
      axis.text.x = element_text(size = base_size * 1.2, color = "black", lineheight = 0.9), 
      axis.text.y = element_text(size = base_size * 1.2, color = "black", lineheight = 0.9), 
      axis.ticks = element_line(color = "black", size  =  0.2), 
      axis.title.x = element_text(size = base_size * 1.5, color = "black", margin = margin(0, 10, 0, 0)), 
      axis.title.y = element_text(size = base_size * 1.5, color = "black", angle = 90, margin = margin(0, 10, 0, 0)), 
      axis.ticks.length = unit(0.3, "lines"),  
      # Specify legend options
      #legend.background = element_rect(color = "black", fill = "#00ba27"), 
      #legend.key = element_rect(color = "black",  fill = "#00ba27"), 
      #legend.key.size = unit(1.2, "lines"), 
      legend.key.height = NULL, 
      legend.key.width = NULL,     
      legend.text = element_text(size = base_size * 1, color = "black"), 
      legend.title = element_text(size = base_size * 1, face = "bold", hjust = 0, color = "black"), 
      legend.position = c(0.9,0.8), 
      legend.text.align = NULL, 
      legend.title.align = NULL, 
      legend.direction = "vertical", 
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = "white", color  =  NA), 
      panel.border = element_rect(fill = NA, color = "black"), 
      panel.grid.major = element_line(color = "#00ba27", size = 0.1), 
      panel.grid.minor = element_line(color = "#00ba27", size = 0.05), 
      #panel.spacing= unit(0.5, "lines"),  
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"), 
      strip.text.x = element_text(size = base_size * 1.5, color = "white"), 
      strip.text.y = element_text(size = base_size * 1.5, color = "white", angle = -90), 
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "#00ba27"), 
      plot.title = element_text(size = base_size * 1.5, color = "black", face = "bold", hjust = 0, family = "Impact")
      #plot.subtitle = element_text(size = base_size * 1.1, color = "black"),
      #plot.caption = element_text(size = base_size * 1.1, color = "black", hjust = 1),
      #plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}
