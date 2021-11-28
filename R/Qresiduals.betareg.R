Qresiduals.betareg <- function(model, plot.it = T) {
  qres <- qresid_beta(model)
  
  if(plot.it) qres_plot(qres)
  
  return(invisible(qres))
}
