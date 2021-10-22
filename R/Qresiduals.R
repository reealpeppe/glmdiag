Qresiduals <- function(model, plot.it = T, global.title, qq.xlab, qq.ylab, qq.title, qqline.col, qq.points.size, 
                       dns.xlab, dns.ylab, dns.title, dns.lines.col) {
  UseMethod('Qresiduals')
}

