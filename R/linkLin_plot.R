linkLin_plot <- function(zeta_eta, smooth = T, xlab, ylab, title, points.size, points.col) {
  
  if(missing(xlab)) xlab <- expression(eta)
  if(missing(ylab)) ylab <- 'Z'
  if(missing(title)) title <- NULL
  if(missing(points.size)) points.size <- 1
  if(missing(points.col)) points.col <- 'black'
  
  eta <- zeta_eta$eta
  zeta <- zeta_eta$zeta
  
  p <- ggplot(zeta_eta, aes(x = eta, y = zeta)) +
    geom_point(size = points.size, colour = points.col) +
    xlab(xlab) + 
    ylab(ylab) +
    theme_light(base_size = 15) +
    theme( 
      plot.margin = margin(0.7,0.7,0.7,0.7, "cm")) +
    ggtitle(title) +
    if(smooth) geom_smooth(formula = y ~ x, col = "red", method = 'loess') 
    
  p

}
