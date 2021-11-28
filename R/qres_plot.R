qres_plot <- function(qres) {
  
  par(mfrow = c(1, 2))
  
  res.dns <- density(qres)
  d.norm <- dnorm(res.dns$x)
  ymax <- max(c(res.dns$y, d.norm))
  ylim <- c(0, ymax * 1.15)
  plot(x = res.dns$x, y = res.dns$y, xlab = 'Quantile', ylab = 'Density', 
       main = 'Density plot', col = 1, type = "l", lwd = 2, ylim = ylim)
  lines(x = res.dns$x, y = d.norm, col = 2, lwd = 2)
  legend("topright", lty = c(1, 1), col = c(1, 2), legend = c('Empirical', 'Normal std.'),
         bty = "n", lwd = c(1.9, 1.9))
  
  qqnorm(qres, pch = 16)
  qqline(qres, col = 2, lwd = 1.9)
  
  par(mfrow = c(1, 1))
}
