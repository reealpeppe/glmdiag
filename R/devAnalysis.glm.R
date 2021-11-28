devAnalysis.glm <- function(model, cex.pl = .8, cex.vars = 1, cex.vars2 = 1, 
                            layout.heights = c(1.2,0.8,4), pl.scale = 1,
                            xlab, ylab, title) {
  
  if(missing(xlab)) xlab <- 'Model comparison No.'
  if(missing(ylab)) ylab <- 'Deviance'
  if(missing(title)) title <- "Graphical analysis of Deviance"
  backup.par <- par(no.readonly = T)
  vars <- attr(terms(model), which = 'term.labels')
  if(length(vars) < 2) stop('You should use a model with more than one variable')
  p <- length(vars)
  mat <- createMat(vars)
  n.mods <- nrow(mat)
  dev <- VectDev(model, mat)
  conf.v <- 1 # numero di confronti per variabile
  for(i in 1:(p - 1)) conf.v <- conf.v + choose(p-1, i)
  n.conf <- conf.v * p # numero totale di confronti
  
  xlim <- c(0, (n.conf + conf.v*pl.scale + .5))
  ylim <- c(min(dev) - .2*sd(dev), max(dev) + .2*sd(dev))
  
  # segmenti verticali separatori
  
  separatori <- seq(from = 0, to = n.conf, by = conf.v)[-1] + .5
  
  
  # segmenti di devianza tra i modelli
  
  diff.dev <- rep(NA, n.conf)
  segm.verticali <- matrix(NA, nrow = n.conf, ncol = (p + 2)) # le prime due colonne sono y0 e y1, le altre l'iesima
  #riga della matrice mat per poi fare i punti sul secondo plot
  count <- 0
  for(i in 1:p){
    mat_i <- mat[which(mat[ ,i] == 1), ]
    m <- nrow(mat_i)
    index.row <- as.numeric(rownames(mat_i))
    for(j in 1:m) {
      count <- count + 1
      k <- apply(mat[ ,-i], MARGIN = 1, FUN = function(x) all(x == mat_i[j, -i]))
      vec <- mat[k,i, drop = F] 
      vec.names <- as.numeric(rownames(vec))
      k <- vec.names[which(vec == 0)]
      
      y0_count <- dev[k]
      y1_count <- dev[index.row[j]]
      diff.dev[count] <- abs(y0_count - y1_count)
      segm.verticali[count, ] <- c(y0_count, y1_count, unlist(mat[k, ]))
      
    }
  }
  
  
  nf <- layout(matrix(c(1,2,3), nrow = 3), heights = layout.heights)
  
  # plot 1
  
  par(mar = c(.5,4,3,2))
  plot(1, type = 'n', axes = F, frame.plot = T, xlab = '', ylab = '', 
       ylim = c(0, 1.1), xlim = xlim, yaxs = 'i', xaxs = 'i')
  
  abline(v = separatori) # separatori variabili
  points(diff.dev/max(diff.dev), type = 'h')
  
  # plot 2
  
  par(mar = c(1,4,.5,2))
  plot(1, type = 'n', axes = F, frame.plot = T, xlab = '', ylab = '', 
       ylim = c(0, 1), xlim = xlim, yaxs = 'i', xaxs = 'i')
  abline(v = separatori) # separatori variabili
  
  separatori2 <- seq(from = 0, to = 1, length.out = p + 1)[1:p] # separatori orizzontali per il secondo plot
  abline(h = separatori2)
  central.pos.x <- rep((xlim[2] + n.conf)/2, p) # posizione centrale dell'ultimo blocco a destra (dove inserire i nomi delle variabili)
  central.pos.y <- separatori2 + .125
  text(x = central.pos.x, y = central.pos.y, label = vars, cex = cex.vars2) # nomi delle variabili 
  
  # punti dei predittori lineari
  
  mat_ordered <- segm.verticali[, -c(1:2)]
  for(i in 1:n.conf) {
    pch.i <- mat_ordered[i,] 
    pch.i <- ifelse(pch.i == 1, 3, NA_integer_)
    points(x = rep(i, p), y = central.pos.y, pch = pch.i, cex = cex.pl)
    
  }
  
  
  # plot 3
  
  par(mar = c(4,4,1,2))
  plot(1, type = 'n', xlim = xlim, ylim = ylim,
       xaxs = 'i', xlab = xlab,
       ylab = ylab)
  
  
  abline(v = separatori) # separatori variabili
  
  # linee orizzontali delle deviance dei modelli
  
  n.sep <- length(separatori)
  sep <- c(0, separatori[-n.sep] + .1)
  text(x = sep, y = ylim[2], label = vars, pos = 4, cex = cex.vars)
  segments(x0 = -1000, x1 = separatori[n.sep], y0 = dev) 
  
  # separatori predittori lineari
  pl.v <- seq(from = separatori[n.sep], xlim[2], length.out = p + 1)[1:p]
  abline(v = pl.v)
  text(x = pl.v, y = ylim[2], label = vars, pos = 4, cex = cex.vars)
  central.pl <- pl.v + (xlim[2] - pl.v[p])/2
  
  # punti dei predittori lineari
  
  for(i in 1:n.mods) {
    pch.i <- mat[i,] 
    pch.i <- ifelse(pch.i == 1, 3, NA_integer_)
    points(x = jitter(central.pl, factor = .5), y = rep(dev[i], p), pch = pch.i, cex = cex.pl)
    
  }
  
  # segmenti verticali di devianza
  
  segments(x0 = seq_len(n.conf), y0 = segm.verticali[, 1], y1 = segm.verticali[, 2])
  
  title(title, line = -2, outer = TRUE, cex.main = 1.5)
  
  par(backup.par)
}