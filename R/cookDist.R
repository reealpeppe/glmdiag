cookDist <- function(object, label.id, n.label.id, xlab, ylab, pos, ...) {
  
  if(!inherits(object, 'influence')) stop('cookDist can only be used with object of class', dQuote('influence'), ', see ?influenceDiag')
  
  cookd <- object$cookDist
  n.obs <- length(cookd)
  index <- seq_len(n.obs)
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- 'Index' 
  if(missing(ylab)) ylab <- "Cook's distance" 
  if(missing(pos)) pos <- 3

  
  points.lab <- getMaxIndex(cookd, label.id, k = n.label.id)
  plot(cookd, xlab = xlab, ylab = ylab, type = "h", ...)
  text(x = index, y = cookd, label = points.lab, pos = pos)
  
}

