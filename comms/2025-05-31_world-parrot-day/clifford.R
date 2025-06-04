# This function creates a dataframe of x and y values based on the equations for
# the Clifford attractor. Default starting values are set at 0.1 and a, b, c,
# and d are constants of choice.

clifford <- function(n, a, b, c, d, x_1 = 0.1, y_1 = 0.1) {
  
  x <- vector(mode = "double", length = n)
  y <- vector(mode = "double", length = n)
  
  x[1] <- x_1
  y[1] <- y_1
  
  for (i in 2:n) {
    
    x[i] <- sin(a*y[i - 1]) + c*cos(a*(x[i - 1]))
    y[i] <- sin(b*(x[i - 1])) + d*cos(b*(y[i - 1])) 
    
  }
  
  data.frame(x = x, y = y)
}