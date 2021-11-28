Qresiduals.vglm <- function(model, plot.it = T) {
  fam <- model@family@vfamily
  if(fam != 'betabinomial') stop('Only betabinomial models are admitted within VGAM package')
  qres <- qresid_betabin(model)
  
  if(plot.it) qres_plot(qres)
  
  return(invisible(qres))
}