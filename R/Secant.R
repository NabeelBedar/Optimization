#' Finds the minimum point of a function
#' @export
#' @param f function
#' @param x0 First initial point where x0 < x1
#' @param x1 Second initial point where x0 < x1

Secant <- function(f, x0, x1, tolerance, n = 1000) {
  require(Deriv)
  h <- Deriv(f)
  k <- n

  for (i in 1:n) {
    x2 <- x1 - h(x1) / ((h(x1) - h(x0)) / (x1 - x0))
    k[i] <- x2
    if (abs(x2 - x1) < tolerance) {
      class(f) <- plot.function(f, y = x1 - tolerance, x2 + tolerance)
      print.secantgraph <- plot.function(f, y = x1 - tolerance, x2 + tolerance)
      root_approximation <- tail(k, n=1)
      Final <- list('root approximation' = root_approximation, 'iterations' = k)
      return(Final)
    }

    x0 <- x1
    x1 <- x2
  }
}
