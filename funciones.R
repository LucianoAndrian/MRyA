# funciones

#### poco util, rosa de los vientos ####
r.vientos =  function(vel, dir, step, p.card = 4, colorbar = "Spectral", titulo = "", size.titulo = 20, h.just = 0.5, colorbar.name = "", 
                      colorbar.rev = F, col_bar = F, salida = getwd(), nombre = "rs", width = 15, height = 15, save = F, text.size = 15,
                      cb.size = 5, letter.size = 10){
  require(ggplot2)
  require(RColorBrewer)
  require(scales)
  
  data = data.frame(vel = vel, dir = dir)
  
  
  while(p.card != 4 & p.card != 8 & p.card != 16){
    p.card = as.numeric(readline("Puntos cardinales pueden ser 4, 8 o 16. p.card = "))
  }
  
  
  dir.step = 360/p.card
  
  
  i = 0
  max.esc = seq(0, 500, by = 5)
  
  x = ceiling(max(data$vel))
  while(length(unique(x == max.esc))== 1){
    x = x + 1
    
  }
  print(paste("Max", x))
  
  step = 1
  y = 11
  while(y>10 | y%%2 != 0 & y%%2 !=1){
    y = x/step
    step = step + 1
    print(step)
  }
  
  vel.breaks = seq(0,x, by = step) #seq(floor(min(data$vel)),ceiling(max(data$vel)), by = step) 
  
  
  vel.labels = paste(c(vel.breaks[1:(length(vel.breaks-1))]),
                     '-',
                     c(vel.breaks[2:(length(vel.breaks))]))
  
  data$vel.c = cut(x = data$vel,
                   breaks = vel.breaks,
                   labels = vel.labels[1:(length(vel.breaks)-1)],
                   ordered_result = TRUE)
  
  
  dir.breaks = c(-(dir.step)/2, seq((dir.step/2),360-(dir.step/2), by = dir.step),360+dir.step/2)
  
  dir.columnas = cut(data$dir, breaks = dir.breaks, ordered_result = TRUE)
  
  dir.labels = c(paste(360-dir.step/2,"-",dir.step/2),
                 paste(seq(dir.step/2, 360-3*dir.step/2, by = dir.step), "-",  
                       seq(3*dir.step/2, 360-dir.step/2, by = dir.step)), paste(360-dir.step/2,"-",dir.step/2))
  
  
  levels(dir.columnas) = dir.labels
  data$dir.c = dir.columnas
  
  data$vel.c = with(data, factor(vel.c, levels = rev(levels(vel.c))))
  
  if(colorbar.rev){
    colorbar = rev(colorRampPalette(brewer.pal(name = colorbar, n = length(vel.labels)))(length(vel.labels)))
  } else {
    colorbar = colorRampPalette(brewer.pal(name = colorbar, n = length(vel.labels)))(length(vel.labels))
  }
  
  
  if(p.card == 16){
    p.card = c("N","NNE","NE","ENE", "E", "ESE", "SE","SSE", 
               "S","SSW", "SW","WSW", "W", "WNW","NW","NNW")
  } else if(p.card == 8){
    
    p.card = c("N","NE", "E", "SE", 
               "S", "SW", "W","NW")
  } else {
    
    p.card = c("N", "E", "S", "W")
  }
  
  if(col_bar){
    g = ggplot(data = data, aes(x = dir.c, y = vel)) +
      geom_col(aes( fill = vel.c)) + scale_fill_manual(name = colorbar.name, values = colorbar, drop = FALSE) 
  } else {
    g = ggplot(data = data, aes(x = dir.c, fill = vel.c)) +
      geom_bar() + scale_fill_manual(name = colorbar.name, values = colorbar, drop = FALSE)
  }
  
  g = g +  scale_x_discrete(drop = F, labels = p.card) +
    coord_polar(start = -((dir.step/2)/360) * 2*pi) + theme_minimal()  +
    ggtitle(titulo) + ylab("") +
    
    theme(axis.text.y   = element_blank(),
          axis.text.x = element_text(size = text.size),
          axis.title.x  = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1.5),
          panel.ontop = TRUE, legend.key.height = unit(cb.size, "cm"),
          legend.key.width = unit(1, "cm"), legend.text = element_text(size = letter.size),
          plot.title = element_text(hjust = h.just, size = size.titulo))
  
  if(save){
    ggsave(paste(salida, nombre, ".jpg", sep = ""), plot = g, width = width, height = height)
  }
  
  return(g)
}


