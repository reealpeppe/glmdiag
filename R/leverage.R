leverage <- function(object, label.id, n.label.id, xlab, ylab, pos, hline, lcol, lwd, lty, ...) {
  
  if(!inherits(object, 'influence')) stop('cookDist can only be used with object of class', dQuote('influence'), ', see ?influenceDiag')
  
  hii <- object$leverage
  n <- length(hii)
  p <- length(object$full.beta)
  index <- seq_len(n)
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- 'Index' 
  if(missing(ylab)) ylab <- "Leverage" 
  if(missing(pos)) pos <- 3
  if(missing(hline)) hline <- 2 * p/n
  if(missing(lcol)) lcol <- 2
  if(missing(lwd)) lwd <- 1.5
  if(missing(lty)) lty <- 2
  
  

  DF <- data.frame(index = index, hii = hii)
  
  points.lab <- getMaxIndex(hii, label.id, k = n.label.id)
  plot(x = index, y = hii, xlab = xlab, ylab = ylab, ...)
  text(x = index, y = hii, label = points.lab, pos = pos)
  abline(h = hline, col = lcol, lwd = lwd, lty = lty)
  
}


