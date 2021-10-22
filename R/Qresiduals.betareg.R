Qresiduals.betareg <- function(model, plot.it = T, global.title, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                               dns.xlab, dns.ylab, dns.title, dns.lines.col) {
  qres <- qresid_beta(model)
  
  if(plot.it) {
    if(missing(global.title)) global.title <- 'Quantile residuals'
    plots <- qres_plot(qres, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                          dns.xlab, dns.ylab, dns.title, dns.lines.col)
    p <- grid.arrange(plots$density.plot, plots$QQplot, ncol = 2, top = global.title)
    p
  }
  return(invisible(qres))
}
