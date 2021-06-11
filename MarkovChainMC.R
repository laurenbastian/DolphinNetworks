##This is a function that will generate a valid random networks
##using Markov Chain Monte Carlo Simulation
##adj.mat: a realization from a degree sequence
##reps: integer number of random networks to be generated
##burn: and integer, burn period, these replications will be removed from the output

config.valid.networks.MCMC = function(adj.mat, reps, burn)
{
  rand.nets = list()
  i = 1
  while(i <= (reps + burn))
  {
    ##select valid edges to swap
    valid.edges = FALSE
    while(!valid.edges)
    {
      nodes = sample(c(1:ncol(adj.mat)), 4)
      x = nodes[1]
      y = nodes[2]
      u = nodes[3]
      v = nodes[4]
      
      ##check that none of the values are equal
      if(!(x == y || x == u || x == v || y == u || y == v || v == u))
      {
        ##check that edges {x,y} and {u,v} exists in the adj.mat and
        ##{x,u} and {y,v} do not
        if(adj.mat[x, y] == 1 && adj.mat[u, v] == 1 && adj.mat[x,u] == 0
           && adj.mat[y,v] == 0)
        {
          valid.edges = TRUE
        }
      }
    }
    
    ##after selecting valid edges, reconfigure the edges
    rand.mat = adj.mat
    rand.mat[x,y] = 0
    rand.mat[y,x] = 0
    rand.mat[u,v] = 0
    rand.mat[v,u] = 0
    rand.mat[x,u] = 1
    rand.mat[u,x] = 1
    rand.mat[y,v] = 1
    rand.mat[v,y] = 1
    
    ##save random matrix to the list of random networks
    rand.nets[[i]] = rand.mat
    i = i+1
  }
  
  ##discard burn period
  if(burn > 0) 
  {
    rand.nets = rand.nets[-(1:burn)]
  }
  
  return (rand.nets)
}

start = Sys.time()
random.dolphin.networks = config.valid.networks.MCMC(dolphin.test, reps = 1000, burn = 500)
end = Sys.time()

start = Sys.time()
mytest.nets = config.valid.networks.MCMC(mytest, reps = 100, burn = 0)
end = Sys.time()