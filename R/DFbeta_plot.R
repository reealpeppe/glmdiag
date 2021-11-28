DFbeta_plot <- function(values, centered, var.lab, beta.i, label.id, n.label.id, xlab, 
                        ylab, main, pos, pch, cex, lcol, lwd, lty, ...) { 
  
  n.obs <- length(values)
  index <- seq_len(n.obs)
  
  if(missing(label.id)) label.id <- index
  if(missing(n.label.id)) n.label.id <- 2
  if(missing(xlab)) xlab <- 'Index' 
  if(missing(ylab)) {if(centered) ylab <- paste('Beta (-i) -', var.lab)
                    else ylab <- paste('DFbeta -', var.lab)}
  if(missing(pos)) pos <- 1
  if(missing(pch)) pch <- 16
  if(missing(cex)) cex <- 1
  if(missing(lcol)) lcol <- 2
  if(missing(lwd)) lwd <- 2
  if(missing(lty)) lty <- 2
  if(missing(main)) main <- var.lab
  
  lincent <- if(centered) beta.i 
              else 0
  points.lab <- getMaxIndex((values - lincent), label.id, k = n.label.id)
  plot(index, values, xlab = xlab, ylab = ylab, pch = pch, cex = cex, 
       main = main, ...)
  text(index, values, label = points.lab, pos = pos)
  abline(h = lincent, col = lcol, lwd = lwd, lty = lty)
}
