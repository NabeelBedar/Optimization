#'Finds the minimum point of a function
#'@export
#'@param f function
#'@param a Initial starting point
#'@param tolerance accuracy of the answer
#'@param n Number of iterations

newton_raphson <- function(f, a, tolerance, n = 1000) {
  require(numDeriv)
  require(Deriv)
  h <- Deriv(f)
  x0 <- a
  k <- n

  ha <- h(a)
  if (ha == 0.0) {
    return(a)
  }

  for (i in 1:n) {
    hh <- genD(func = h, x = x0)$D[1]
    x1 <- x0 - (h(x0) / hh)
    k[i] <- x1

    if (abs(x1 - x0) < tolerance) {
      class(f) <- plot.function(f, y = x0 - tolerance, x1 + tolerance)
      print.newtongraph <- plot.function(f, y = x0 - tolerance, x1 + tolerance)
      root_approximation <- tail(k, n=1)
      Final <- list('root approximation' = root_approximation, 'iterations' = k)
      return(Final)
    }

    x0 <- x1

  }
  print('Choose a new value for a')
}
