#' find the greatest common divisor of two numbers
#'
#' @param a A number
#' @param b A number
#' @return the greatest common divisor of two numbers
#' @export

euclidean <- function(a,b){
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


