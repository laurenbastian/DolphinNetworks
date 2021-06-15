##This is a function that will generate a valid random networks
##using Markov Chain Monte Carlo Simulation
##adj.mat: a realization from a degree sequence
##reps: integer number of random networks to be generated
##burn: and integer, burn period, these replications will be removed from the output

config.valid.networks.MCMC = function(adj.mat, reps, burn = 0)
{
  #make a vector of the edges
  edges = matrix(0, nrow = 0, ncol = 2)
  for(i in (1):(ncol(adj.mat) - 1))
  {
    for(j in (i+1):(ncol(adj.mat)))
    {
      if(adj.mat[i,j] == 1)
      {
        edges = rbind(edges, c(i,j))
      }
    }
  }
  
  #start network configuration with the random matrix
  #set to the original matrix
  rand.nets = list()
  rand.mat = adj.mat
  i = 1
  
  while(i <= (reps + burn))
  {
    ##select distinct edges
    valid.edges = FALSE
    while(!valid.edges)
    {
      sample = sample(1:nrow(edges), 2)
      edge1 = edges[sample[1],]
      edge2 = edges[sample[2],]
      x = edge1[1]
      y = edge1[2]
      u = edge2[1]
      v = edge2[2]
      
      ##check all nodes are distinct
      if(!((x == y) || (x == u) || (x == v) || (y == u) || (y == v) || (v == u)))
      {
          valid.edges = TRUE
      }
    }
    
    ##check whether the edges {x,u} and {y,v} exists in the adj matrix
    if(rand.mat[x,u] == 0 && rand.mat[y,v] == 0)
    {
      ##if edges are swappable, reconfigure the edges
      rand.mat[x,y] = 0
      rand.mat[y,x] = 0
      rand.mat[u,v] = 0
      rand.mat[v,u] = 0
      rand.mat[x,u] = 1
      rand.mat[u,x] = 1
      rand.mat[y,v] = 1
      rand.mat[v,y] = 1
      
      edges[sample[1],] = c(x,u)
      edges[sample[2],] = c(y,v)
    }
    
    ##save random matrix to the list of random networks
    rand.nets[[i]] = rand.mat
    i = i+1
  }

  ##discard burn period
  if(burn > 0) 
  {
    return (rand.nets[-(1:burn)])
  }
  
  #if no burn period given, return the list
  return (rand.nets)
}


#testing
start = Sys.time() ##approx 2 sec for 10,000 reps
mytest.nets = config.valid.networks.MCMC(dolphin.observed.mat, reps = 10000, burn = 10000)
end = Sys.time()
length(unique(mytest.nets))

start = Sys.time()
single.source(mytest.nets[[1]], 1) ##aprox 0.7 sec per run
end = Sys.time()

start = Sys.time()
avg.efficiency(mytest.nets[[1]]) ##approx. 20 sec per run
end = Sys.time()


##A function that utilizes MCMC to generate random networks, removes the 
##top 3 most highly connected nodes, and calculates the percent reduction
##in efficiency

MCMC.eff = function(adj.mat, reps, burn = 0)
{
  #make a vector of the edges
  edges = matrix(0, nrow = 0, ncol = 2)
  for(i in (1):(ncol(adj.mat) - 1))
  {
    for(j in (i+1):(ncol(adj.mat)))
    {
      if(adj.mat[i,j] == 1)
      {
        edges = rbind(edges, c(i,j))
      }
    }
  }
  
  #start network configuration with the random matrix
  #set to the original matrix
  rand.nets = list()
  rand.mat = adj.mat
  i = 1
  
  ##burn-in period
  while(i <= burn)
  {
    ##select distinct edges
    valid.edges = FALSE
    while(!valid.edges)
    {
      sample = sample(1:nrow(edges), 2)
      edge1 = edges[sample[1],]
      edge2 = edges[sample[2],]
      x = edge1[1]
      y = edge1[2]
      u = edge2[1]
      v = edge2[2]
      
      ##check all nodes are distinct
      if(!((x == y) || (x == u) || (x == v) || (y == u) || (y == v) || (v == u)))
      {
        valid.edges = TRUE
      }
    }
    
    ##check whether the edges {x,u} and {y,v} exists in the adj matrix
    if(rand.mat[x,u] == 0 && rand.mat[y,v] == 0)
    {
      ##if edges are swappable, reconfigure the edges
      rand.mat[x,y] = 0
      rand.mat[y,x] = 0
      rand.mat[u,v] = 0
      rand.mat[v,u] = 0
      rand.mat[x,u] = 1
      rand.mat[u,x] = 1
      rand.mat[y,v] = 1
      rand.mat[v,y] = 1
      
      edges[sample[1],] = c(x,u)
      edges[sample[2],] = c(y,v)
    }
    i = i + 1
  }
  
  i = 1
  ##random network generation after burn-in period
  while(i <= reps)
  {
    ##select distinct edges
    valid.edges = FALSE
    while(!valid.edges)
    {
      sample = sample(1:nrow(edges), 2)
      edge1 = edges[sample[1],]
      edge2 = edges[sample[2],]
      x = edge1[1]
      y = edge1[2]
      u = edge2[1]
      v = edge2[2]
      
      ##check all nodes are distinct
      if(!((x == y) || (x == u) || (x == v) || (y == u) || (y == v) || (v == u)))
      {
        valid.edges = TRUE
      }
    }
    
    ##check whether the edges {x,u} and {y,v} exists in the adj matrix
    if(rand.mat[x,u] == 0 && rand.mat[y,v] == 0)
    {
      ##if edges are swappable, reconfigure the edges
      rand.mat[x,y] = 0
      rand.mat[y,x] = 0
      rand.mat[u,v] = 0
      rand.mat[v,u] = 0
      rand.mat[x,u] = 1
      rand.mat[u,x] = 1
      rand.mat[y,v] = 1
      rand.mat[v,y] = 1
      
      edges[sample[1],] = c(x,u)
      edges[sample[2],] = c(y,v)
    }
    
    ##save random matrix to the list of random networks
    rand.nets[[i]] = rand.mat
    i = i+1
  }
  
  ##calculate the average efficiencies for the random networks
  init.eff = numeric(length(rand.nets))
  for(i in 1:length(rand.nets))
  {
    init.eff[i] = avg.efficiency(rand.nets[[i]])
  }
  
  ##generate networks by removing the top 3 most connected nodes
  ##calculate the average efficiencies for the reduced networks
  reduced.eff = numeric(length(rand.nets))
  for(i in 1:length(rand.nets))
  {
    ##find and remove the top 3 most connected nodes for the matrix
    reduced.net = rand.nets[[i]]
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
    reduced.eff[i] = avg.efficiency(reduced.net) 
  }
  
  ##calculate the percent reduction
  perc.reduct = (init.eff - reduced.eff) / init.eff

  output.mat = cbind(init.eff, reduced.eff, perc.reduct)
  colnames(output.mat) = c("init.eff", "reduced.eff", "%_reduct")
  
  return (output.mat)
}

test.mat = matrix(c(0,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,0,0), nrow = 5, ncol = 5)
start = Sys.time()
MCMC.eff(test.mat, 10, 10)
end = Sys.time()











