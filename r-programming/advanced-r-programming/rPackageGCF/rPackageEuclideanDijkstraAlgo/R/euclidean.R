#' Calculate Greatest Common Divisor(GCD) using Euclidean algorithm. 
#' @param x A number
#' @param y A number
#' @return GCD of \code{x} and \code{y}
#' @examples 
#' \dontrun{
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' }
#' @details 
#' Purpose of this function is to find Greatest Common Divisor(GCD) among entered values using Euclidean algorithm.
#' 
#' In mathematics, the Euclidean algorithm is an efficient method for computing the greatest common divisor (GCD) of two numbers, the largest number that divides both of them without leaving a remainder.
#'
#' Please refer below URL for more information on Algorithm 
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export

 
euclidean = function(x,y){
  # Euclidian algorithm 
  if(x>y){
    max = x ; min = y;
  }else{
    max = y; min = x;
  }
  repeat{
    r = max%%min
    
    if(r != 0)
    {
      max = min
      min = r
    }
    else
    {
      break;
    }
  }#repeat{
    
  return(abs(min))
}#euclidean = function(x,y){
