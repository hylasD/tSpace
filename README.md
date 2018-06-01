# tSpace algorithm for unsupervised deteremination of multiple developmental branches in single cell data
By Denis Dermadi

Description

tSpace is the main function for trajectory analysis. The algorithm is described in the publication Dermadi et al. 2018 

Originally, it was developed for single cell analysis, however it can be applied on any type of large data.

Parameters needed for tSpace:

K	is an integer specifying the K-nearest-neighbors

L	is an integer specifying the random L out of K-nearest-neighbors, L < K, usually K = (4*L)/3

distance metric 

numer of graphs to calculate

an integer specifying how many L-K-NN graphs will be used for final trajectory calculation 

an integer indicating number of trajectories to calculate

an integer indicating how many waypoints to use for trajectory refinement

Soon more details how to use the algorithm
