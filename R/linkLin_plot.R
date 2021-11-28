linkLin_plot <- function(zeta, eta, smooth, xlab, ylab, main, pch, lcol, lwd, link.name, ...) {
  
  if(missing(xlab)) xlab <- expression(eta)
  if(missing(ylab)) ylab <- 'Z'
  if(missing(main)) main <- paste('Link =', link.name)
  if(missing(pch)) pch <- 16
  if(missing(lcol)) lcol <- 2
  if(missing(lwd)) lwd <- 3
  
  plot(eta, zeta, xlab = xlab, ylab = ylab, main = main,
       pch = pch, ...)
  if(smooth) lines(loess.smooth(eta, zeta), col = lcol, lwd = lwd)

}
