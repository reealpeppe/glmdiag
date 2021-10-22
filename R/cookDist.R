cookDist <- function(object, label.id, n.label.id, xlab, ylab, title, vjust, hjust) {
  
  if(!inherits(object, 'influence')) stop('ookDist can only be used with object of class', dQuote('influence'), ', see ?influenceDiag')
  
  cookd <- object$cookDist
  n.obs <- length(cookd)
  index <- seq_len(n.obs)
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- 'Index' 
  if(missing(ylab)) ylab <- "Cook's distance" 
  if(missing(title)) title <- NULL
  if(missing(vjust)) vjust <- -.3
  if(missing(hjust)) hjust <- 0
  
  xlab <- paste0(xlab, '\n')
  ylab <- paste0(ylab, '\n')
  
  DF <- data.frame(cookd = cookd, index = index)
  points.lab <- getMaxIndex(cookd, label.id, k = n.label.id)

  p <- ggplot(DF, mapping = aes(x = index, y = cookd)) +
    geom_segment(aes(x = index, xend = index, y = 0, yend = cookd))+
    geom_text(aes(label = points.lab, vjust = vjust, hjust = hjust)) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)
    
  suppressWarnings(print(p))
  
}

