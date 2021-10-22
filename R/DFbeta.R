DFbeta <- function(object, label.id, n.label.id, xlab, ylab, vjust, hjust, points.size, points.col) {
  
  if(!inherits(object, 'influence')) stop('DFbeta can only be used with object of class', dQuote('influence'), ', see ?influenceDiag')
  
  dfb <- object$DFbeta
  var.names <- colnames(dfb)
  p <- ncol(dfb)
  
  DFbeta_plot(dfb[ ,1], var.lab = var.names[1], label.id, n.label.id, xlab, ylab, vjust, hjust, points.size, points.col)
  
  for(i in 2:p) {
    
    readline(prompt = "Type <Enter> to go to the next plot: ")
    DFbeta_plot(dfb[ ,i], var.lab = var.names[i], label.id, n.label.id, xlab, ylab, vjust, hjust, points.size, points.col)
  }
}
