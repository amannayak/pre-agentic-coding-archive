#' Greedy Knapsack Solution for Kanpsack Problem 
#' @param x is a dataframe constising 
#' @param W is capacity of kanpsack 
#' @return \code{list} List of object containing \code{value} giving maximum value of Knapsack out of dataframe and \code{elements} giving postion of 
#' selected values from dataframe x 
#' @examples
#' \dontrun{
#' greedy_knapsack(data.frame("w" = c(10,20,30,40) , "v" = c(2,4,3,5)), 3500)
#'  }
#' @description 
#' Approach is to use the a heuristic or approximation for the problem. This algorithm will not
#' give an exact result :but it can be shown that it will return at least 50percent of the true maximum value:,
#' but it will reduce the computational complexity considerably :actually to Oh n log n: due to the sorting
#' part of the algorithm.
#' 
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#' @export greedy_knapsack


greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x) , is.numeric(W))
  if(names(x[1])=='w'&& names(x[2])=='v'&& W>0)
    
    x$den = x$v/x$w
  x<-x[order(-x$den),]
  names_row = rownames(x)
  bag = 0
  val = 0
  i = 1
  j = 1
  W_elements = vector()
  while(bag<=W)
  {
    if((W-bag)>x$w[i])
    {
      bag<-bag+x$w[i]
      val<-val+x$v[i]
      W_elements[j]<-as.numeric(names_row[i])
      i<-i+1
      j<-j+1
    }
    else
      break
    
  }
  
  return(list(value = val, elements = W_elements))
}