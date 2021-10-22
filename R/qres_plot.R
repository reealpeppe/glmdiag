qres_plot <- function(qres, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                      dns.xlab, dns.ylab, dns.title, dns.lines.col ) {

  if(missing(qq.xlab)) qq.xlab <- "Theoretical quantiles" 
  if(missing(qq.ylab)) qq.ylab <- "Sample quantiles"
  if(missing(qq.title)) qq.title <- 'Q-Q Plot'
  if(missing(qqline.col)) qqline.col <- 'red'
  if(missing(qq.points.size)) qq.points.size <- 2 
  if(missing(dns.xlab)) dns.xlab <- 'Quantile'
  if(missing(dns.ylab)) dns.ylab <- 'Density'
  if(missing(dns.title)) dns.title <- 'Density plot'
  if(missing(dns.lines.col)) dns.lines.col <- c(1, 2)
    
  qqnrm <- qqnorm(qres, plot.it = F)
  norm.quantiles <- qqnrm$x
  DF <- data.frame(norm.quantiles = norm.quantiles, qres = qres)
  
  x1 <- qnorm(.25) ; x2 <- qnorm(.75)
  y1 <- quantile(qres, .25) ; y2 <- quantile(qres, .75)
  
  qqline.a <- (x1*y2 - x2*y1)/(x1 - x2)
  qqline.b <- (y1 - y2)/(x1 - x2)
  
  QQplot <- ggplot(DF, aes(x = norm.quantiles, y = qres)) + 
    geom_point(shape = 1, size = qq.points.size) +
    geom_abline(intercept = qqline.a, slope = qqline.b, col = qqline.col, size = 0.8) +
    theme_light() +
    xlab(qq.xlab) + 
    ylab(qq.ylab) + 
    ggtitle(qq.title)
  
  d <- density(DF$qres)
  ld <- length(d$x)
  type = c(rep("Normal std.", ld), rep("Empirical", ld))
  
  df.dens <- data.frame(q = rep(d$x, 2), d = c(dnorm(d$x), d$y), 
                        type)
  
  density.plot <- ggplot(df.dens, aes(x = q, y = d)) + 
    geom_line(aes(col = type), size = .9) +
    scale_colour_manual(values = dns.lines.col) +
    xlab(dns.xlab) + ylab(dns.ylab) +
    ggtitle(dns.title) +
    theme(legend.position = c(.85, .95),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent"))
  
  out <- list(QQplot = QQplot, density.plot = density.plot)
  return(invisible(out))
  
}
