DFbeta <- function(object, variables, centered = T, label.id, n.label.id, xlab, ylab, 
                   main, pos, pch, cex, lcol, lwd, lty, ...) {
  
  if(!inherits(object, 'influence')) stop('DFbeta can only be used with object of class', dQuote('influence'), ', see ?influenceDiag')
  
  dfb <- object$DFbeta
  var.names <- colnames(dfb)
  b.full <- object$full.beta
  names(b.full) <- var.names
  
  if(missing(variables)) variables <- var.names
  else {if(any(!(variables %in% var.names))) stop('Input variables not correct')}
  dfb <- dfb[, variables, drop = F]
  b.full <- b.full[variables]
  p <- ncol(dfb)
  
  
  if(centered) {
    dfb <- if(p == 1) dfb + b.full
    else t(apply(dfb, MARGIN = 1, FUN = function(x) x + b.full))
  }
  
  
  DFbeta_plot(dfb[ ,1], centered = centered, var.lab = variables[1], 
              beta.i = b.full[1], label.id, n.label.id, xlab, ylab,
              pos, pch, cex, lcol, lwd, lty, ...)
  
  if(p > 1) {
  
  for(i in 2:p) {
    
    readline(prompt = "Type <Enter> to go to the next plot: ")
    DFbeta_plot(dfb[ ,i], centered = centered, var.lab = variables[i],
                beta.i = b.full[i], label.id, n.label.id, xlab, ylab,
                pos, pch, cex, lcol, lwd, lty, ...)
  }
  }
}
