Qresiduals.negbin <- function(model, plot.it = T) {
  qres <- qresid_nbinom(model)
  
  if(plot.it) qres_plot(qres)
  
  return(invisible(qres))
}