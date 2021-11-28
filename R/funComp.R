funComp <- function(as.dataframe = T) {
  funcs <- c('avplot', 'cookDist', 'devAnalysis', 'DFbeta', 
             'influenceDiag', 'leverage', 'linkLin', 'Qresiduals', 
             'varCheck', 'variableOut')


    mods <- c('  glm  ', 'neg.bin', 'betabin', ' beta  ')
    tbl <- c(rep('X', 2), rep(' ', 2), # avplot
             rep('X', 4), # cookDist
             rep('X', 2), rep(' ', 2), # devAnalysis
             rep('X', 4), # DFbeta
             rep('X', 4), # influenceDiag
             rep('X', 4), # leverage
             rep('X', 4), # linkLin
             rep('X', 4), # Qresiduals
             rep('X', 4), # varCheck
             rep('X', 4)) # variableOut
    
    mtx <- matrix(tbl, nrow = length(funcs), ncol = length(mods), byrow = T)
    df.out <- data.frame(mtx)
    colnames(df.out) <- mods
    rownames(df.out) <- funcs
    
    if(as.dataframe) {
    name.width <- max(sapply(names(df.out), nchar))
    format(df.out, width = name.width, justify = "centre") }
  
  
  else {
    cat(rep(' ',8 ), ' glm   neg.bin   betabin   beta', end = '\n')
    cat(rep(' ',16), rep('-', 32), end = '\n', sep = '')
    
    for(i in 1:length(funcs)) {
    
    space.out <- 18 - nchar(funcs[i])
    
    cat(funcs[i], rep(' ',space.out), df.out[i, 1], rep(' ', 7), df.out[i, 2], rep(' ', 9), df.out[i, 3], 
        rep(' ', 8), df.out[i, 4], sep = '', end = '\n')
    if(i != length(funcs)) cat(rep('- ', 25), sep = '', end = '\n')
  }
 }
}
