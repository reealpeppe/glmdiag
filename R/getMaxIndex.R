getMaxIndex <- function(values, Names, k = 2) {
  
  val <- abs(values)
  n <- length(val)
  
  
  if(missing(Names)) Names <- seq_len(n)
  if(k < 1) return(rep('', n))
  if(k > n) return(Names)
  
  out <- rep('', n)
  
  for(i in 1:k) {
    id <- which.max(val)
    out[id] <- Names[id]
    val[id] <- NA
  }
  out 
}
