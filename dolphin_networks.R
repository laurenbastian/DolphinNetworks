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


## set working directory
setwd("~/Blackwell Scholars Workshop/dolphins/DolphinNetworks")

## load the multiplex package and read file
library("multiplex")
pairs = read.gml("dolphins.gml")

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
dolphin.deg.seq = as.integer(dolphin.deg.seq)
dolphin.deg.seq = sort(dolphin.deg.seq, decreasing = TRUE)

##construct observed adjacency matrix
dolphin.observed.mat = matrix(data = 0, nrow = 62, ncol = 62)
key.names.df = data.frame("Name" = names, "Number" = c(1:62))
pairs.numbers.df = as.data.frame(pairs)

##replace every name with its number
for (i in 1:nrow(pairs.numbers.df))
{
  name = as.vector(key.names.df$Name[i])
  print(name)
  number = as.integer(key.names.df$Number[i])
  print(number)
  
  pairs.numbers.df[pairs.numbers.df == name] = number
}

##fill in values to adjacency matrix
for (i in 1:nrow(pairs.numbers.df))
{
  x = as.integer(pairs.numbers.df$S[i])
  y = as.integer(pairs.numbers.df$R[i])
  dolphin.observed.mat[x,y] = 1
  dolphin.observed.mat[y,x] = 1
}


## Generate networks for dolphin data
start.time = Sys.time()
dolphin.nets = config.valid.networks(dolphin.deg.seq, 1)
end.time = Sys.time()
