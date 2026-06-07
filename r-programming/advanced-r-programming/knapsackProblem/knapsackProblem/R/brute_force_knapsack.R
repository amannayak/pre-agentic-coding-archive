#' Brute Force Solution for Kanpsack Problem 
#' @param x is a dataframe constising 
#' @param W is capacity of kanpsack 
#' @param parallel default is false, if it it TRUE than parallel operation will be performed. Only set to True when Data set is large 
#' @return \code{list} List of object containing \code{value} giving maximum value of Knapsack out of dataframe and \code{elements} giving postion of 
#' selected values from dataframe x 
#' @examples
#' \dontrun{
#' brute_force_knapsack(data.frame("w" = c(10,20,30,40) , "v" = c(2,4,3,5)), 3500)
#' brute_force_knapsack(data.frame("w" = c(10,20,30,40) , "v" = c(2,4,3,5)), 3500,parallel = TRUE) 
#' }
#' @description 
#' Given a set of items, each with a weight and a value, determine the number of each item to include in a collection so that the total weight is less than or equal to a given limit and the total value is as large as possible.
#' The only solution that is guaranteed to give a correct answer in all situations for the knapsack problem is using brute-force search, i.e. going through all possible alternatives and return the maximum value found.
#' This approach is of complexity BigOh 2n since all possible combinations 2n needs to be evaluated.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export brute_force_knapsack
#' @import parallel
#' @import utils




brute_force_knapsack = function(x, W, parallel = FALSE){
  #check inputs for positive numbers   
  
  stopifnot(is.data.frame(x) , is.numeric(W))
  
  if((length((x[x[,1] < 0 ,])[,1]) > 0) || length((x[x[,2] < 0 ,])[,1]) || (as.integer(W) < 0))  
    stop("All values of inserted data i.e. of dataframe and entered weight should be positive")
  
  if(parallel == FALSE)
  {
    bestValue = 0
    bestPositions = NULL
    permutation = 2^(length(x$v))
    for(i in 1:(permutation-1))
    {
      total = 0
      weight = 0
      vPositions = NULL
      binary = intToBits(i)
      #bit wise comparision 
      for(j in 1:length(binary)){
        
        if(binary[j] == TRUE)
        {
          total = total + x$v[j]
          weight = weight + x$w[j]
          vPositions = c(vPositions,j)
        }#if(binary[j] == TRUE)
      }#for(j in 1:size){
      
      if(weight <= W && total > bestValue)
      {
        bestPositions = vPositions
        bestValue = total
      }#if(weight <= W && total > bestValue)
    }#for(i in 1:permutation)
    
    
    return(list("value" = round(bestValue) , "elements" = bestPositions))
  }#if(paralle == FALSE)
  else
  {
    require(parallel)
    require(utils)
    #enabling parallel computation
    #added parallel:: after package test error
    core = parallel::detectCores() #count all threads 
    cluster = parallel::makeCluster(core)
    parallel::clusterExport(cluster , c("x") , envir = environment()) # setting envir to local env as by default clusterExport looks in Global Env
    parallel::clusterEvalQ(cluster , {
      require(parallel)
    })
    #using combn function to generate all posible combination unlike previous intobits, here we are directly performing operation on df using wrapper parlapply
    #postion posible combination
    position = unlist(parLapplyLB(cluster , 1:length(x$w), fun = function(y){
      utils::combn(rownames(x) , y , paste, collapse = "")
    }))
    
    #weight possible combination
    weight = unlist(parLapplyLB(cluster , 1:length(x$w), fun = function(y){
      utils::combn(x$w , y , sum)
    }))
    
    #value possible combination
    value = unlist(parLapplyLB(cluster , 1:length(x$w), fun = function(y){
      utils::combn(x$v , y , sum)
    }))
    
    stopCluster(cluster)#stoping cluster now 
    
    bestWeights = which(weight < W)
    allowedValues = round(value[bestWeights])
    bestValue  = round(max(allowedValues))
    bestPostion = which(round(value) == bestValue)
    #spliting charcter vector of position for white space in order to get elements related to best position
    elements = unique(as.numeric(unlist(strsplit(position[bestPostion] , ""))))
    
    return(list("value" = as.integer(bestValue) , "elements" = elements))
  }#else
}#brute_force_knapsack = function(x, W)

#' Checking Time taken to Execute for n = 16
#system.time(brute_force_knapsack(knapsack_objects[1:16,] , 3500))
#'Output
# user  system elapsed 
# 5.69    0.07    5.90 
