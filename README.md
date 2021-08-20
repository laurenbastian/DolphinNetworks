# DolphinNetworks
A short project exploring network science using a data set on dolphin communication networks. We wanted to examine the percent reduction the average efficiency of the network after targeted attacks upon said network. In this case, the top 5% of dolphins were removed from the network and the efficiency compared to the "unharmed" network. During the project two different methods were tested to generate randomized networks with the same degree sequence as the observed network. First, the configuration model, which proved to have a lengthy runtime when selecting networks with no self-lopps or multiedges. Second, the Markov Chain Monte Carlo method, which takes "snapshots" of random network configurations. The second method was significantly faster and was the primary method used for this project.

To run the simulation:
1. Download dolphins.gml, dolphin_networks.R, ConfigurationModel.R, NetworkPropertyFunctions.R, and MarkovChainMC.R to a local folder
2. Run dolphin_networks.R

The script will produce a table of efficiencies for the the randomized networks as well as a p-value for the % reduction in efficiency

Configuration model check in dolphin_network.R is incomplete, but provides a glimpse into the accuracy of the test. This particular test takes a long time due to the randomization and selection criteria of the configuration model.
