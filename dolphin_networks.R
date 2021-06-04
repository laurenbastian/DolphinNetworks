## set working directory
setwd("~/Blackwell Scholars Workshop/dolphins/DolphinNetworks")

## load the multiplex package and read file
library("multiplex")
pairs = read.gml("dolphins.gml")
pairs

## object used for finding degree of each node
pairs.list = c(pairs$S, pairs$R)

## get the names of each dolphin
names = unique(pairs.list)

## construct a degree sequence for the dolphin data
dolphin.deg.seq = numeric(length = length(names))

for (i in 1:length(names))
{
  ##calculate the number of times each name appears in the pairs list
  for (j in 1:length(pairs.list))
  {
    if (names[i] == pairs.list[j])
    {
      dolphin.deg.seq[i] = dolphin.deg.seq[i] + 1
    }
  }
}

## A function to generate random networks using Configuration Model
## given a degree sequence
## randomly select two stubs from stubs.vec and connect them
## remove selected stubs from stubs.vec and take num.stubs - 2
## continue until no stubs remain.
## this algorithm will return an adjacency matrix 
## NOTE: THIS ALGORITHM MAY PRODUCE MATRICES WITH SELF-LOOPS
## AND MULTI-EDGES!

config.model = function(deg.seq)
{
  ## invalid if the sum of the degree sequence is odd
  if(sum(deg.seq) %% 2 == 1)
  {
    return (NULL)
  }

  ##generate the vector of stubs
  stubs.vec = numeric(0)
  for (i in 1:length(deg.seq))
  {
    stubs.vec = c(stubs.vec, rep(i, deg.seq[i]))
  }
  
  ##set number of stubs
  num.stubs = length(stubs.vec)
  
  ## create adjacency matrix
  adj.mat = matrix(0, nrow = length(deg.seq), ncol = length(deg.seq))
  
  while(num.stubs > 0)
  {
    print("START")
    print(stubs.vec)
    
    ## randomly select two stubs
    stubs = sample(stubs.vec, 2)
    
    print(stubs)
    
    ## connect the two stubs to form an edge
    adj.mat[stubs[1], stubs[2]] = adj.mat[stubs[1], stubs[2]] + 1
    adj.mat[stubs[2], stubs[1]] = adj.mat[stubs[2], stubs[1]] + 1
  
    ## remove both stubs from stubs.vec
    ## find the index of stub 1 and remove it
    stub.idx = 0
    for (i in 1:num.stubs)
    {
      if(stubs[1] == stubs.vec[i])
      {
        stub.idx = i
        break
      }
    }
    stubs.vec = stubs.vec[-stub.idx]
    
    ## find the index of stub 2 and remove it
    stub.idx = 0
    for (i in 1:num.stubs)
    {
      if(stubs[2] == stubs.vec[i])
      {
        stub.idx = i
        break
      }
    }
    stubs.vec = stubs.vec[-stub.idx]
    
    ## decrement number of stubs
    num.stubs = num.stubs - 2
    
    print("END")
    print(stubs.vec)
  }
  
  return (adj.mat)
}

## A function to generate reps number of random networks 
## following a given degree sequence
## without self-loops or multi-edges
## returns a list of valid networks
config.valid.networks = function(deg.seq, reps)
{
  ## a list of matrices
  sim.matrices = list()
  i = 1
  while(i <= reps)
  {
    ## generate a random matrix
    rand.mat = config.model(deg.seq)
    
    ##check if the matrix is valid
    ##search for multi-edges
    is.valid.mat = TRUE
    for (j in 1:nrow(rand.mat))
    {
      for (k in 1:nrow(rand.mat))
      {
        ## if there is a multi-edge, matrix is invalid
        if(rand.mat[j, k] > 1)
        {
          is.valid.mat = FALSE
          break
        }
      }
    }
    
    ##check for self-loops
    for (j in 1:nrow(rand.mat))
    {
      if(rand.mat[j, j] != 0)
      {
        is.valid.mat = FALSE
        break
      }
    }
    
    ## if the matrix is valid, add it to the list and increment i
    if (is.valid.mat)
    {
      sim.matrices[[i]] = rand.mat
      i = i + 1
    }
  }
  ## return a list of adjacency matrices
  return (sim.matrices)
}


