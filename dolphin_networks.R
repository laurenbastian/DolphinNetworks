## set working directory
setwd("~/Blackwell Scholars Workshop/dolphins/DolphinNetworks")

## load the multiplex package and read file
library("multiplex")
library("igraph")
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


## Use Markov Chain Monte Carlo simulation to calculate 
##the reduction in efficiency after the top three dolphins are removed
##***already completed simulation of 1000, results in "dolphin_network_efficiency.csv"
start = Sys.time()
efficiency.table = MCMC.eff(dolphin.observed.mat, 10000, burn = 10000, save.every = 100)
end = Sys.time()

##calculate the observed reduction in efficiency
##***results in "observed_efficiency_reduction.csv"
observed.eff.reduct = reduced.eff(dolphin.observed.mat)

##calculate the p-value for the proportion of random networks
##with efficiency reductions less than the observed reduction
perc.reduct.vec = as.numeric(efficiency.table[,"%_reduct"])
less.than.obs = numeric(length(perc.reduct.vec))
for(i in 1:length(less.than.obs))
{
  if(perc.reduct.vec[i] < observed.eff.reduct[, "%_reduct"])
  {
    less.than.obs[i] = 1
  }
}
p.val = mean(less.than.obs)

dolph.obs.graph = read.graph("dolphins.gml", format = "gml")
set.seed = 100
plot(dolph.obs.graph, 
     vertex.size = 8, 
     vertex.frame.color = "NA", 
     vertex.color = "#72aaff", 
     vertex.label.cex = 0.6, 
     vertex.label.color = "black", 
     vertex.label.family = "sans",
     edge.width = 1,
     edge.color = "light gray",
     layout = layout_with_lgl(dolph.obs.graph))


