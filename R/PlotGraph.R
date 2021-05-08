#'Finds the minimum point of a function
#'@export
#'@param f Function
#'@param x Variable of the function
#'@param a Minimum point of the graph
#'@param b Maximum point of the graph

plotgraph <- function(f, a, b){
  require(ggplot2)
  ggplot() + xlim(a, b) + geom_function(fun = f)
}
