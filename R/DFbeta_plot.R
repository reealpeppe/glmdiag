DFbeta_plot <- function(values, var.lab, label.id, n.label.id, xlab, ylab, vjust, hjust, points.size, points.col) { 
  
  n.obs <- length(values)
  index <- seq_len(n.obs)
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- 'Index' 
  if(missing(ylab)) ylab <- "DFbeta" 
  if(missing(vjust)) vjust <- 0
  if(missing(hjust)) hjust <- 1.3
  if(missing(points.size)) points.size <- 1.8
  if(missing(points.col)) points.col <- 'black'
  
  xlab <- paste0(xlab, '\n')
  ylab <- paste0(ylab, '\n')
  
  
  DF <- data.frame(values = values, index = index)
  
  points.lab <- getMaxIndex(values, label.id, k = n.label.id)
  p <- ggplot(DF, aes(x = index, y = values)) +
    geom_point(size = points.size, colour = points.col) +
    geom_hline(yintercept = 0, col = 2, lty = 2) +
    geom_text(aes(label = points.lab, hjust = hjust, vjust = vjust)) +
    ylab(ylab) +
    xlab(xlab) + 
    ggtitle(var.lab)
  
  suppressWarnings(print(p))
}
