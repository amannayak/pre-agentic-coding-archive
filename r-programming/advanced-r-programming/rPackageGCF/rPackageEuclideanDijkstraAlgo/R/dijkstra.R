#' Find the shortest path using Dijkstra's algorithm.
#' @param graph Data frame consisting vertices and distance between them
#' @param init_node initial vertex from which the distance is to be calculated 
#' @return shortest distance from init_node to all other vertices
#' 
#' @details 
#' This function finds the shortest distance from the input vertex to all other vertices.
#' sample data set we have is as below 
#'    
#' For additional information click on the URL below
#' \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#' 
#' @examples
#' \dontrun{ 
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), 
#'                          v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'                          w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph,1)}
#' @export



dijkstra <- function(graph, init_node){
  
  if( !is.data.frame(graph) ||
      !is.numeric.scalar(init_node) ||
      !(which(graph$v2==init_node) || which(graph$v2==init_node)) ){
    stop()
  }
  vertex = unique(graph$v1)
  
  dist = numeric(length(vertex))
  prev = numeric(length(vertex))
  for (i in vertex) {
    dist[i] = Inf
    prev[i] = NA
  }
  dist[ which(vertex==init_node) ] = 0
  
  j <- 1
  while(length(vertex) != 0){
    rem_v <- which(dist == min( dist[vertex] ))
    
    vertex = vertex[ -which(vertex==rem_v) ]
    
    for (v in vertex) {
      len = graph[ which( graph$v1==rem_v ), c("v2", "w")]
      
      if( any(len$v2 == v) ){
        alt = dist[rem_v] + len[ which( len$v2==v ), "w" ]
        
        if(alt < dist[v]){
          dist[v] = alt
          prev[v] = rem_v
        }
      }
    }
    j = j+1
  }
  return(dist)
}

is.numeric.scalar <- function(x){
  return( is.numeric(x) && length(x)==1 )
}
