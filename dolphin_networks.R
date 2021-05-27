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

## construct a degree array
dolphin.deg.seq = numeric(length = 62)

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

## View degree sequence, confirm sum of degrees is even
dolphin.deg.seq = as.integer(dolphin.deg.seq)
dolphin.deg.seq
sum(dolphin.deg.seq)

## Generate random networks using Configuration Model
## stubs: counts the number of unpaired stubs
## adj.mat: a 62x62 adjacency matrix
## this algorithm will generate a 62x62 adjacency matrix 
## in which self-loops and multi-edges are not permitted
## if a matrix is generated with a self-loop or a multi-edge,
## discard the matrix and generate a new one
stubs = sum(dolphin.deg.seq)

adj.mat = matrix(0, nrow = 62, ncol = 62)

while (stubs != 0) {
  ##randomly select two nodes and connect them
  node1 = floor(runif(1, min = 1, max = length(names)))
  node2 = floor(runif(1, min = 1, max = length(names)))
  
  ## if the generated nodes are not equal, both nodes still have
  ## stubs, and there is not an edge between the two nodes, 
  ## connect them and update the stubs count
  ## otherwise, the stubs count is not updated
  if (node1 != node2 && 
      dolphin.deg.seq[node1] != 0 && dolphin.deg.seq[node2] != 0 && 
      adj.mat[node1, node2] == 0 && adj.mat[node2, node1] == 0)
  {
    adj.mat[node1, node2] = 1
    adj.mat[node2, node1] = 1
    dolphin.deg.seq[node1] = dolphin.deg.seq[node1] - 1
    dolphin.deg.seq[node2] = dolphin.deg.seq[node2] - 1
    stubs = stubs - 2
  }
}
