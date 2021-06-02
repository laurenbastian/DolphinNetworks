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

configuration.model = function(deg.seq)
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
    ## randomly select two stubs
    stubs = sample(x = stubs.vec, size = 2)
  
    ## connect the two stubs to form an edge
    adj.mat[stubs[1], stubs[2]] = adj.mat[stubs[1], stubs[2]] + 1
    adj.mat[stubs[2], stubs[1]] = adj.mat[stubs[2], stubs[1]] + 1
  
    ## remove both stubs from stubs.vec
    stubs.vec = stubs.vec[-stubs[1]]
    stubs.vec = stubs.vec[-stubs[2]]
    
    ## decrement number of stubs
    num.stubs = num.stubs - 2
  }
  
  return (adj.mat)
}

## A function to generate reps number of random networks 
## following a given degree sequence
## without self-loops or multi-edges
## returns a list of valid networks
configure.valid.networks = function(deg.seq, reps)
{
  i = 1
  sim.matrices = list()
  while(i <= reps)
  {
    ## generate a random matrix
    rand.mat = configuration.model(deg.seq)
    
    ##check if the matrix is valid
    is.valid.mat = TRUE
    for (j in 1:nrow(rand.mat))
    {
      ##if there is a self-loop, matrix is invalid
      if(rand.mat[j, j] != 0)
      {
       is.valid.mat = FALSE
       break
      #}
      for (k in 1:ncol(rand.mat))
      {
        ## if there is a multi-edge, matrix is invalid
        if(rand.mat[j, k] > 1)
        {
          is.valid.mat = FALSE
          break
        }
      }
    }
    ## if the matrix is valid, add it to the list and increment i
    if (is.valid.mat)
    {
      sim.matrices[[i]] = rand.mat
      i = i + 1
    }
  }
  
  return (sim.matrices)
}

configure.valid.networks(dolphin.deg.seq, 1)

configure.valid.networks(test.deg.seq, 5)

## checking uniformity
##THIS IS VERY MUCH NOT UNIFORM SOMETHING IS UP
##LIKELY AN ERROR IN THE VALID NETWORKS FUNCTION??
a = matrix(data = c(0,0,0,1,0,0,1,1,0,1,0,1,1,1,1,0), nrow = 4)
b = matrix(data = c(0,0,1,1,0,0,0,1,1,0,0,1,1,1,1,0), nrow = 4)
c = matrix(data = c(0,1,0,1,1,0,0,1,0,0,0,1,1,1,1,0), nrow = 4)
d = matrix(data = c(0,1,1,1,1,0,1,0,1,1,0,0,1,0,0,0), nrow = 4)
e = matrix(data = c(0,1,1,1,1,0,0,1,1,0,0,0,1,1,0,0), nrow = 4)
f = matrix(data = c(0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0), nrow = 4)
g = matrix(data = c(0,1,1,0,1,0,1,1,1,1,0,0,0,1,0,0), nrow = 4)
h = matrix(data = c(0,1,0,1,1,0,1,1,0,1,0,0,1,1,0,0), nrow = 4)
i = matrix(data = c(0,1,0,0,1,0,1,1,0,1,0,1,0,1,1,0), nrow = 4)
j = matrix(data = c(0,1,1,0,1,0,1,0,1,1,0,1,0,0,1,0), nrow = 4)
k = matrix(data = c(0,0,1,1,0,0,1,0,1,1,0,1,1,0,1,0), nrow = 4)
l = matrix(data = c(0,0,1,0,0,0,1,1,1,1,0,1,0,1,1,0), nrow = 4)

reps = 1000 

test.nets = configure.valid.networks(test.deg.seq, reps)

test.freq = numeric(reps)

for(i in 1:reps)
{
  if(identical(test.nets[[i]], a))
  {
    test.freq[i] = 1
  }
  else if(identical(test.nets[[i]], b))
  {
    test.freq[i] = 2
  }
  else if(identical(test.nets[[i]], c))
  {
    test.freq[i] = 3
  }
  else if(identical(test.nets[[i]], d))
  {
    test.freq[i] = 4
  }
  else if(identical(test.nets[[i]], e))
  {
    test.freq[i] = 5
  }
  else if(identical(test.nets[[i]], f))
  {
    test.freq[i] = 6
  }
  else if(identical(test.nets[[i]], g))
  {
    test.freq[i] = 7
  }
  else if(identical(test.nets[[i]], h))
  {
    test.freq[i] = 8
  }
  else if(identical(test.nets[[i]], i))
  {
    test.freq[i] = 9
  }
  else if(identical(test.nets[[i]], j))
  {
    test.freq[i] = 10
  }
  else if(identical(test.nets[[i]], k))
  {
    test.freq[i] = 11
  }
  else if(identical(test.nets[[i]], l))
  {
    test.freq[i] = 12
  }
  else
  {
    test.freq[i] = 13
  }
}
hist(test.freq)

