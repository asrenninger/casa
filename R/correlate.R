correlating <- function(correlations) {

  mat <- round(cor(correlations), 2)
  
  ##
  
  get_lower_tri <- function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  ##
  
  upper_tri <- get_upper_tri(mat)
  melted_mat <- reshape2::melt(upper_tri, na.rm = TRUE)
  
  ##
  
  ggheatmap <- 
    ggplot(data = na.omit(melted_mat), aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_colour_scico(palette = 'cork', direction = -1,
                       guide = 'none') +
    scale_fill_scico(palette = 'cork',
                     limit = c(-1,1), 
                     name = "pearson\ncorrelation",
                     guide = guide_colorbar(direction = "vertical",
                                            barheight = unit(50, units = "mm"),
                                            barwidth = unit(2, units = "mm"),
                                            draw.ulim = FALSE,
                                            title.position = 'left',
                                            label.position = 'right',
                                            title.hjust = 0.5,
                                            label.hjust = 0.5)) +
    theme_minimal() +
    theme(legend.text = element_text(angle = 90),
          legend.title = element_text(angle = 90),
          axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 8, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 1,
                                     size = 8, hjust = 1),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          panel.grid.major = element_line(size = 0.25), 
          panel.grid.minor = element_line(size = 0.25), 
          legend.position = c(0.25, 0.75),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) +
    coord_fixed()
  
  ggmatrix <- 
    ggheatmap +
    geom_text(aes(Var2, Var1, label = value, colour = value), size = 3) 
  
  ggsave(ggmatrix, filename = "corplot.png", height = 10, width = 10, dpi = 300)
  
}
  
  