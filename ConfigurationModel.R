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
    
    ## randomly select two stubs
    stubs = sample(stubs.vec, 2)
    
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
    
  }
  
  return (adj.mat)
}

## A function to generate reps number of random networks 
## following a given degree sequence
## without self-loops or multi-edges
## returns a list of valid networks
config.valid.networks = function(deg.seq, reps)
{
  ## make sure to sort descending
  deg.seq = sort(deg.seq, decreasing = TRUE)
  
  ## a list of matrices
  sim.matrices = list()
  i = 1
  while(i <= reps)
  {
    ## generate a random matrix
    rand.mat = config.model(deg.seq)
    
    
    
    ##check if the matrix is valid
    is.valid.mat = TRUE
    
    ##check for self-loops
    if(sum(diag(rand.mat)) != 0)
    {
      is.valid.mat = FALSE
    }
    
    ##check for multi-edges
    if (is.valid.mat)
    {
      if(length(rand.mat[rand.mat > 1]) > 0)
      {
        is.valid.mat = FALSE
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

##Checking that the random networks are about uniformly distributed
test = c(3,2,2,2,1)
test.nets = config.valid.networks(test,10000)
test.unique = unique(test.nets)
a = test.unique[[1]]
b = test.unique[[2]]
c = test.unique[[3]]
d = test.unique[[4]]
e = test.unique[[5]]
f = test.unique[[6]]

test.dist = c()
for (i in 1:length(test.nets))
{
  if (identical(test.nets[[i]], a))
  {
    test.dist = c(test.dist, "a")
  }
  if (identical(test.nets[[i]], b))
  {
    test.dist = c(test.dist, "b")
  }
  if (identical(test.nets[[i]], c))
  {
    test.dist = c(test.dist, "c")
  }
  if (identical(test.nets[[i]], d))
  {
    test.dist = c(test.dist, "d")
  }
  if (identical(test.nets[[i]], e))
  {
    test.dist = c(test.dist, "e")
  }
  if (identical(test.nets[[i]], f))
  {
    test.dist = c(test.dist, "f")
  }
}

barplot(height = table(test.dist), main = "Frequency of Randomly Generated Networks")

