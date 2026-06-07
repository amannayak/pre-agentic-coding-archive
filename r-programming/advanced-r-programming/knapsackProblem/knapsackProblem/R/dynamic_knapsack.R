#' Dynamic programming Solution for Kanpsack Problem 
#' @param x is a dataframe constising 
#' @param W is capacity of kanpsack 
#' @return \code{list} List of object containing \code{value} giving maximum value of Knapsack out of dataframe and \code{elements} giving postion of 
#' selected values from dataframe x 
#' @examples
#' \dontrun{
#' greedy_knapsack(data.frame("w" = c(10,20,30,40) , "v" = c(2,4,3,5)), 3500)
#'  }
#' @description 
#' If the weights are actually discrete values we can use this to create an algorithm that can solve the knapsack problem exact by iterating
#' over all possible values of w.
#' This function should return the same results as the brute force algorithm, but unlike the brute force it should scale much better since the algorithm will run in O<Wn>.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#' @export dynamic_knapsack


dynamic_knapsack <- function(x,W){
  stopifnot(is.data.frame(x), is.numeric(W))
  
  values = matrix(0, nrow = length(x[[1]]), ncol = (W+1))
  #colnames(values) = 0:W
  for(i in 1:ncol(values)){
    if(x$w[1] <= i-1){
      values[1,i] = x$v[1]
    } else{
      values[1,i] = 0
    }
  }
  
  for(i in 2:nrow(values)){
    for(j in 2:ncol(values)){
      if(x$w[i]>j){
        values[i,j] = values[i-1,j]
      } else{
        values[i,j] = max(values[i-1,j], x$v[i] + values[i-1, j-x$w[i]])
      }
    }
  }
  # max_value = max(values[length(x[[1]]),])
  # temp = max_value
  # # print(max_value)
  #  W_elements = vector()
  #   k = length(x[[1]])
  #   h = 1
  #   while(k>=1)
  #   {
  #     if(any(values[k,]==max_value)){
  #       k = k-1
  #     } else
  #     {
  #       W_elements[h] = k
  #       h = h+1
  #       max_value = max_value - x$v[k]
  #     }
  #   }
  #   
  
  # for(i in nrow(values):1)
  #  {
  #    if(values[i,(W+1)] < max_value)
  #      {
  #        max_value = max_value - x$v[i+1]
  #        W_elements = append(W_elements, which(any(values[i,]==max_value)))
  #      } 
  #  }
  
  i = length(x[[1]])
  j = W+1
  k = 1
  W_elements = c()
  total_weight = 0
  final_value = 0
  
  while (i>1 & j>1) 
  {
    if(values[i,j] == values[i-1,j])
    {
      i = i-1
    }
    else
    {
      W_elements[k] = i
      k<-k+1
      final_value = final_value + x[i,2]
      j = j-x[i,1]
      i = i-1
    }
  }
  
  return(list( 'value' = round(values[length(x[[1]]), (W+1)]), 'elements' = sort(W_elements)))
  
}











