sharededge <- function(X) {
  verifyclass(X, "ppp")
  Y <- X[as.rectangle(X)]
  dX <- deldir(Y)
  DS <- dX$dirsgs
  xyxy <- DS[,1:4]
  names(xyxy) <- c("x0","y0","x1","y1")
  sX <- as.psp(xyxy,window=dX$rw)
  marks(sX) <- 1:nobjects(sX)
  sX <- sX[as.owin(X)]
  tX <- tapply(lengths_psp(sX), marks(sX), sum)
  jj <- as.integer(names(tX))
  ans <- data.frame(ind1=DS[jj,5], 
                    ind2=DS[jj,6], 
                    leng=as.numeric(tX))
  return(ans)
}