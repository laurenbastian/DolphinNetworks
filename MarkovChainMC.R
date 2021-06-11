##This is a function that will generate a valid random networks
##using Markov Chain Monte Carlo Simulation
##adj.mat: a realization from a degree sequence
##reps: integer number of random networks to be generated
##burn: and integer, burn period, these replications will be removed from the output

config.valid.networks.MCMC = function(adj.mat, reps, burn = 0)
{
  #start network configuration with the random matrix
  #set to the original matrix
  rand.nets = list()
  rand.mat = adj.mat
  i = 1
  while(i <= (reps + burn))
  {
    ##select distinct nodes
    valid.nodes = FALSE
    while(!valid.nodes)
    {
      nodes = sample(c(1:ncol(adj.mat)), 4)
      x = nodes[1]
      y = nodes[2]
      u = nodes[3]
      v = nodes[4]
      
      ##check all nodes are distinct
      if(!(x == y || x == u || x == v || y == u || y == v || v == u))
      {
          valid.nodes = TRUE
      }
    }
    
    ##check whether the edges {x,y} and {u,v} exists in the adj matrix and
    ##{x,u} and {y,v} do not
    if(rand.mat[x, y] == 1 && rand.mat[u, v] == 1 && rand.mat[x,u] == 0
       && rand.mat[y,v] == 0)
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

burn.period = seq(0, 50000, by = 5000)
unique.nets = numeric(length(burn.period))
for (i in 1:length(burn.period))
{
  random.dolphin.networks = config.valid.networks.MCMC(dolphin.observed.mat, reps = 50000, burn = burn.period[i])
  unique.nets[i] = length(unique(random.dolphin.networks))
}

plot(burn.period, unique.nets)


start = Sys.time()
mytest.nets = config.valid.networks.MCMC(mytest, reps = 100, burn = 0)
end = Sys.time()