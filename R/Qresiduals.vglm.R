Qresiduals.vglm <- function(model, plot.it = T, global.title, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                            dns.xlab, dns.ylab, dns.title, dns.lines.col) {
  fam <- model@family@vfamily
  if(fam != 'betabinomial') stop('Only betabinomial models are admitted within VGAM package')
  qres <- qresid_betabin(model)
  
  if(plot.it) {
    if(missing(global.title)) global.title <- 'Quantile residuals'
    plots <- qres_plot(qres, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                       dns.xlab, dns.ylab, dns.title, dns.lines.col)
    p <- grid.arrange(plots$density.plot, plots$QQplot, ncol = 2, top = global.title)
    p
  }
  return(invisible(qres))
}