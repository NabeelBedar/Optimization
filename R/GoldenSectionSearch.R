#'Finds the minimum point of a function
#'@export
#'@param f function
#'@param ub Upper Bound
#'@param lb Lower Bound
#'@param Tolerance Accuracy of answer

Golden_Section_Search = function(f, ub, lb, Tolerance){
  require(ggplot2)
  Golden_Ratio = 2/(sqrt(5)+1)
  x1 = ub - Golden_Ratio * (ub - lb)
  x2= lb + Golden_Ratio * (ub - lb)

  f1 = f(x1)
  f2 = f(x2)

  iteration = 0

  while (abs(ub - lb) > Tolerance){
    iteration = iteration + 1
    cat('', '\n')
    cat('Iteration #', iteration, '\n')
    cat('f1 =', f1, '\n')
    cat('f2 =', f2, '\n')

    if (f2 > f1){
      cat('f2 > f1', '\n')
      ub = x2
      cat('New Upper Bound =', ub, '\n')
      x2 = x1
      f2 = f1
      x1 = ub - Golden_Ratio * (ub - lb)
      cat('New Lower Test Point =', x1, '\n')
      f1 = f(x1)
    }
    else{
      cat('f2 < f1','\n')
      lb = x1
      cat('New Upper Bound =', ub, '\n')
      cat('New Lower Bound =', lb, '\n')
      x1 = x2
      cat('New Lower Bound =', x1, '/n')
      f1 = f2
      x2 = lb + Golden_Ratio * (ub -lb)
      cat('New Upper Test Point =', x2, '\n')
      f2 = f(x2)
    }
  }
  cat('','\n')
  cat('Final Lower Bound =', lb, '\n')
  cat('Final Upper Bound =', ub, '\n')
  estimated_minimizer = (lb + ub)/2
  cat('Estimated Minimizer =', estimated_minimizer, '\n')
  class(f) = ggplot() + xlim(lb, ub) + geom_function(fun = f)
  print.graph = ggplot() + xlim(lb, ub) + geom_function(fun = f)
  print.graph
}
