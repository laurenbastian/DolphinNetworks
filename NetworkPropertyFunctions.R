library(igraph)

##This is a function that will find the shortest path
##between a start node and any other two nodes on a graph
##adj.mat: an adjacency matrix representing the graph
##start: the initial point on the graph
##return a list of lengths from the start node to all other nodes
##if two nodes are not connected by any path, return NULL
single.source = function(adj.mat, start)
{
  ##Breadth First Search Algorithm
  ##output.df: the output data frame
  ##queue.df: the queue data fram
  ##    From = previous node in path, 
  ##    To = end node in path, 
  ##    Cost = distance from start node
  ##visited: a logical vector that track whether a node has been visited
  output.df = data.frame("Source" = c(), "To" = c(), "Prev" = c(), "Cost" = c())
  queue.df = data.frame("Source" = c(), "To" = c(), "Prev" = c(), "Cost" = c())
  visited = logical(length = ncol(adj.mat))
  visited[start] = TRUE
  
  ##add all direct neighbors of start node to the queue
  for (i in 1:ncol(adj.mat))
  {
    if (adj.mat[start, i] == 1)
    {
      queue.df = rbind(queue.df, data.frame("Source" = start, "To" = i, "Prev" = start, "Cost" = 1))
    }
  }

  ##while the queue is not empty and there are still unvisited nodes, continue
  ##searching the graph
  while (nrow(queue.df) != 0 && (length(visited) - sum(visited) != 0))
  {
    ## select the first node in the queue
    cur.df = queue.df[1,]
    
    ##if current node is unvisited, add if to the output data frame and mark visited
    if (!visited[cur.df$To])
    {
      output.df = rbind(output.df, cur.df)
      visited[cur.df$To] = TRUE
    }
    
    ##add unvisited neighbors of the vertex to the queue
    for (i in 1:ncol(adj.mat))
    {
      if((adj.mat[cur.df$To, i] == 1) && (!visited[i]))
      {
        queue.df = rbind(queue.df, data.frame("Source" = start, "To" = i, "Prev" = cur.df$To, "Cost" = cur.df$Cost + 1))
      }
    }
    
    ##remove the first node from the queue
    queue.df = queue.df[-1,]
  }
  
  ##add infinite distances for unconnected nodes
  ##if there is a node that is not reached by the algorithm, 
  ##its distance is infinite
  for (i in 1:ncol(adj.mat))
  {
    if (i == start)
    {
      next
    }
    
    if (!(i %in% output.df$To))
    {
      output.df = rbind(output.df, data.frame("Source" = start, "To" = i, "Prev" = NA, "Cost" = Inf))
    }
  }
  
  ##order the output dataframe by the end node "To"
  output.df = output.df[order(output.df$To), ]

  return (output.df)
}


##A function that returns the diameter of a network 
##ie. the maximum distance between any two nodes on a graph
diameter = function(adj.mat)
{
  all.distances = as.numeric(distances(graph_from_adjacency_matrix(adj.mat),
                                       algorithm = "unweighted"))
  all.distances = all.distances[all.distances != 0]
  return (max(all.distances))
}


##A function that calculates the average distance between all nodes in a network
avg.distance = function(adj.mat)
{
  all.distances = as.numeric(distances(graph_from_adjacency_matrix(adj.mat),
                                       algorithm = "unweighted"))
  all.distances = all.distances[all.distances != 0]
  
  return (sum(all.distances) / length(all.distances))
}


##A function that calculates the average efficiency of a network 
avg.efficiency = function(adj.mat)
{
  all.distances = as.numeric(distances(graph_from_adjacency_matrix(adj.mat),
                                       algorithm = "unweighted"))
  all.distances = all.distances[all.distances != 0]

  return ((1 / ((ncol(adj.mat)) * (ncol(adj.mat) -1)) * ((sum(all.distances ^ -1)) / 2)))
}


##this is a function that calculates the average efficiency
##of the initial matrix, created a reduced matrix missing the top
##3 most connected nodes, and calculates the efficiency of the 
##reduced matrix, a the % reduction from the origianl matrix
##it will return calculated values in a matrix
reduced.eff = function(adj.mat)
{
  ##calculate the average efficiencies for the network
  init.eff = avg.efficiency(adj.mat)
  
  ##generate networks by removing the top 3 most connected nodes
  ##calculate the average efficiencies for the reduced networks
  reduced.eff = -1
  
  ##find and remove the top 3 most connected nodes for the matrix
  reduced.net = adj.mat
  degree.seq = colSums(reduced.net)
  for(j in 1:3)
  {
    top = sort(degree.seq, decreasing = TRUE)[1]
    for(k in 1:length(degree.seq))
    {
      #Remove the top valued node from the matrix
      if(degree.seq[k] == top)
      {
        degree.seq = degree.seq[-k]
        reduced.net = reduced.net[-k,-k]
        break
      }
    }
  }
  reduced.eff = avg.efficiency(reduced.net)
  
  ##calculate the percent reduction
  perc.reduct = (init.eff - reduced.eff) / init.eff
  
  output.mat = matrix(c(init.eff, reduced.eff, perc.reduct), nrow = 1, ncol = 3)
  colnames(output.mat) = c("init.eff", "reduced.eff", "%_reduct")
  
  return (output.mat)
}





