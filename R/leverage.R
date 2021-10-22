leverage <- function(object, label.id, n.label.id, xlab, ylab, title, vjust, hjust, points.size, points.col) {
  
  if(!inherits(object, 'influence')) stop('cookDist can only be used with object of class', dQuote('influence'), ', see ?influenceDiag')
  
  hii <- object$leverage
  n <- length(hii)
  index <- 1:n
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- 'Index' 
  if(missing(ylab)) ylab <- "Leverage" 
  if(missing(title)) title <- NULL
  if(missing(vjust)) vjust <- 0
  if(missing(hjust)) hjust <- 1.5
  if(missing(points.size)) points.size <- 1.8
  if(missing(points.col)) points.col <- 'black'
  
  xlab <- paste0(xlab, '\n')
  ylab <- paste0(ylab, '\n')
  
  DF <- data.frame(index = index, hii = hii)
  
  point.labs <- getMaxIndex(hii, label.id, k = n.label.id)
  p <- ggplot(DF, mapping = aes(x = index, y = hii)) +
        geom_point(size = points.size, colour = points.col) +
        geom_text(aes(label = point.labs, hjust = hjust, vjust = vjust)) +
          ylab(ylab) +
          xlab(xlab) +
          ggtitle(title)
  
  suppressWarnings(print(p))
}


