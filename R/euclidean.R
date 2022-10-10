#' find the greatest common divisor of two numbers
#'
#' @param a A number
#' @param b A number
#' @return the greatest common divisor of two numbers
#' @export

euclidean <- function(a,b){
  stopifnot(is.numeric(a) & is.numeric(b))
  a <- abs(a)
  b <- abs(b)
  while(a != b){
    if(a > b){
      c <- a - b
      if(b > c){
        a <- b
        b <- c
      }else{
        a <- c
      }
    }else{
      c <- b - a
      if(a > c){
        b <- c
      }else{
        b <- a
        a <- c
      }
    }
  }
  return(a)
}
##########################################################################

euclidean(100, 1000)
euclidean(-100, 1000)
