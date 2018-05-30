#' @title Helper function: graphfinder
#'
#' @description graphfinder is a helper function to find a principal graph using KernelKnn
#' function as a base function
#' @return kNN graph
#' @importFrom foreach %dopar%
#' @keywords internal
#'
graphfinder <- function (x, k, distance, core_n){
  # % spdists = spdists_knngraph( x, k, distance, chunk_size, verbose )
  # %
  # % finds the nearest neighbor graph for data matrix x. the graph is represented as a sparse matrix. for each point in x,
  # % the k nearest neighbors are found. distance is the distance metric used by knnsearch (such as euclidean, cosine,
  #                                                                                         % etc.).
  # %
  # % if chunk_size is specified, x will be split to chunk_size-by-D submatrices, and the calculation will be run
  # % separately on each submatrix (in order to conserve memory).

  # cut data inot chunks of 1000
  chunk_size = 1000

  #shared nearest neighbour
  snn = 1

  core_n = as.numeric(core_n)

  n <- nrow(x)
  total_chunks <- ceiling( n / chunk_size )

  temp1 <- list()
  # iterate over submatrices
  from <- seq(from=1, to=n, by=chunk_size)


  cl <- parallel::makeCluster(core_n)
  doParallel::registerDoParallel(cl)

  temp <- foreach::foreach(i = 1:length(from), .combine = rbind) %dopar% {
    to <- min( from[i] + chunk_size - 1, n )
    rx <- from[i]:to
    x_from_to = x[rx, ]

    knn <- KernelKnn::knn.index.dist(x, x_from_to, k + 1, method = distance)

    idx <- knn[[1]][,-1] # remove self neighbor
    d <- knn[[2]][,-1] # remove self neighbor

    # update spdists
    js <- t(pracma::repmat(rx, k, 1))
    I <- vector()
    J <- vector()
    D <- vector()
    temp <- list()
    for(j in 1:ncol(js)){
      for(a in 1:nrow(idx)){
        I[a] <- js[a,j]
        J[a] <- idx[a,j]
        D[a] <- d[a,j]
      }
      temp[[j]] <- cbind(I,J,D)
    }
    temp1[[i]] <- do.call(rbind, temp)
  }

  temp <- temp[order(temp[,1]), ]


  if (snn != 0){

    nData <- nrow(x)
    rem <- list()
    rem <- foreach::foreach(ci=1:nData) %dopar% {
      #Finding neighbours
      from = (ci-1)*k+1
      to = ci*k
      i_inds = from:to
      i_neighs = temp[i_inds, 'J']

      tem_rem <- list()
      for(i_ind in i_inds){
        i_neigh=temp[i_ind, 'J']
        from = (i_neigh-1)*k+1
        to = i_neigh*k
        j_neighs = temp[from:to, 'J']
        if (sum(j_neighs %in% i_neighs) < snn) {
          # add them to remove list
          tem_rem <- append(tem_rem, i_ind)
        }
      }
      rem[[ci]] <- as.vector(unlist(tem_rem))
    }
    parallel::stopCluster(cl)
    rem <- unlist(rem)
    temp <- temp[-rem,]
  }

  return(temp)
}

##############################################
##############################################

#' @title Helper function: find_lknn
#'
#' @description find_lknn is a helper function to reduce a principal graph to l-kNN graph
#' This function works as in MATLAB, Note: try to optimize code for speed
#' @return l-kNN graph
#' @importFrom foreach %dopar%
#' @keywords internal
find_lknn <- function(knn, l, core_n){
  #remove_edges <- list()
  cl <- parallel::makeCluster(core_n)
  doParallel::registerDoParallel(cl)
  i <-  knn@i+1
  j <- findInterval(seq(knn@x)-1,knn@p[-1])+1
  vals <- knn@x
  new_test <- list()
  test <- foreach::foreach(idx = 1:dim(knn)[2], .combine = 'rbind') %dopar% { #,
    #remove l-k neighbors at random
    neighs = j[which(i == idx)] #find( knn( :, idx ) );
    K = length(neighs) #% count number of neighbors
    if(K <= l){
      #remove_edges = neighs[sample(length(neighs), 0 )]
      temp_pair <- cbind(I= i[which(i ==idx)], J = neighs, D = vals[which(i == idx)])
      #temp_pair1 <- temp_pair[-which(temp_pair[,2] %in% remove_edges),]
    }else{
      remove_edges = neighs[sample(length(neighs), K - l )]
      temp_pair <- cbind(I= i[which(i ==idx)], J = neighs, D = vals[which(i == idx)])
      temp_pair <- temp_pair[-which(temp_pair[,2] %in% remove_edges),]
    }
    # when K < l it removes all original data.... chekc that with the original code in
    # MATALB
    ###   new_test[[idx]] <- temp_pair
    #temp_pair <- cbind(I= i[which(i ==idx)], J = neighs, D = vals[which(i == idx)])
    #temp_pair1 <- temp_pair[-which(temp_pair[,2] %in% remove_edges),]
    return(temp_pair)
    #neighs = j[which(i == idx)]
    #idx_remove_edges = sub2ind( size( spdists ), remove_indices, ones( k - l, 1 ) * idx );
    #remove_edges[[idx]] <- remove_indices
  }
  parallel::stopCluster(cl)
  #test <- unlist(test)
  # if we go as collecting it in the list do.call(rbind, test)
  return(test)
}

##############################################
##############################################


#' @title Helper function: pathfinder
#'
#' @description pathfinder is a helper function to calculate trajectory distances from starting cell to
#' every other cell in the data set. This function is based on Dijkstra shorthest path
#' algortihm with implementation of waypoints as a correctives for distances
#' Notes: calculation of distances from waypoints to all other cells is needed for weighing scheme
#' this function merges two separate functions in MATALB version of the package (pathFinder and trajectory_waypoints_p)
#' output is a list, that can be exported (needs modification of the main function), however tSpace
#' function records only final_trajectory variable
#' @return a list with final trajectory, all unreachable data points, indices of waypoints,
#' a matrix of trajectory distances aligned to positions of the waypoints and
#' a matrix of original distances from the start cell and all waypoints.
#' @importFrom foreach %dopar%
#' @keywords internal

pathfinder <- function(data, lknn, s, waypoints, voting_scheme, distance = D){

  band_sample = 1
  flock_landmarks = 2
  partial_order = NULL
  # parameters:
  # band_sample = 1
  # flock_landmarks = 2
  # waypoints = 20
  # partial_order = NULL

  # if lknn is a three column matrix, reshape it into sparse matrix using below function.
  # head(lknn_mat)
  #     V1 V2      V3
  # 1   62  1 0.62844
  # 2  130  1 0.91527
  # 3 4859  1 0.63788
  # 4 5098  1 0.80191
  # 5 6470  1 0.71653
  # 6 6868  1 0.79455
  # Important caveat is that  i == V1, j == V2 and weights are V3
  # temporary use lknn_mat variable, which is lknn from MATALB for comparison purposes
  # lknn_mat <- read.csv('/Users/Denis/Documents/MATLAB/lKnn.csv', header = F)
  # lknn_mat_new <- read.csv('/Users/Denis/Documents/MATLAB/Flug/l_knn_fromMATLAB.csv', header = F)

  # create a directed graph from adjacency matrix, using mode "lower", which creates only
  # the lower left triangle (including the diagonal) and output of calcualtion is the same as in
  # MATALB when calcualting landmarks

  lknn_dir <- igraph::graph.adjacency(Matrix::sparseMatrix(i=lknn[,'J'], j=lknn[,'I'], x=lknn[,'D']), mode ='directed', weighted = TRUE)
  lknn_for_landm <- igraph::graph.adjacency(Matrix::sparseMatrix(i=lknn[,'J'], j=lknn[,'I'], x=lknn[,'D']), mode ='lower', weighted = TRUE)

  nl = waypoints #parameters.num_landmarks;

  # create a directed graph from adjacency matrix
  #lknn_g_d <- graph.adjacency(lknn, mode ='directed', weighted = TRUE)


  # lknn_g_u <- graph.adjacency(lknn, mode ='undirected', weighted = TRUE)

  if( length( nl ) == 1 ){
    #shortest_paths function is equivalent to graphshortestpath in MATLAB, it generates vertices along the shortest path.
    # not needed for now
    # shortest.paths.out <- igraph::shortest_paths(graph = lknn_g, from = V(lknn_g)[s_c], weights = NULL, mode = 'out', output = 'both')
    #distances function is equivalent to graphshortestpath in MATLAB, it generates shortest distances between vertices.
    shortest.dist.out <- igraph::distances(graph = lknn_dir, v = V(lknn_dir)[s], weights = NULL, mode = 'out',  algorithm = 'dijkstra')

    # if not given landmarks list, decide on random landmarks
    n_options = 1:nrow(data)
    if (band_sample == 1){
      n_options = NULL
      window_size = .1
      max_dist = max(shortest.dist.out)
      for(prc in seq(0.998, 0.08, -window_size)){
        band = which(shortest.dist.out>=(prc-window_size)*max_dist & shortest.dist.out <=prc*max_dist)
        n_options = append(n_options, sample( x=band, size=min(length(band), as.numeric(nl - 1 - length(partial_order))), replace = F ))
      }
    }
    nl = sample(x = n_options, size =as.numeric(nl - 1 - length(partial_order))) #loaded nl values form MATLAB

    if (flock_landmarks > 0){
      for(fl in 1:flock_landmarks){
        knn.landmarks <- KernelKnn::knn.index.dist(data, data[nl, ], k = 20, method = distance)[[1]]
        for(i in 1:length(nl)){
          #consider using robustbase package, it has colMedians function faster than apply option
          nl[i] <- KernelKnn::knn.index.dist(data, t(apply(data[knn.landmarks[i,], ],2, median)), k = 20, method = distance)[[1]][1]
        }
      }
    }
  }

  #diffdist <- matrix(data = NA, nrow = length(nl), ncol = length(nl))

  partial_order = c(s, partial_order)

  L = c(partial_order, nl)

  #paths_l2l <- list()
  unreachable_list <- list()
  short_dist_land <- matrix(data = NA, nrow= length(L), ncol = nrow(data))
  for(i in 1:length(L)){
    short_dist_land[i,] <- igraph::distances(graph = lknn_for_landm, v = V(lknn_for_landm)[L[i]], weights = NULL, mode = 'all', algorithm = 'dijkstra')
    unreachable <- which(short_dist_land[i, ] == Inf)


    # return unreacheble list as output
    unreachable_list[[i]] <- which(short_dist_land[i, ] == Inf)

    reachable = which(short_dist_land[i, ] != Inf)
    cou = 0
    while (!pracma::isempty(unreachable)) {
      cou = cou + 1
      unreac_knn = KernelKnn::knn.index.dist(data[reachable,], data[unreachable, ], k = as.integer(length(unreachable)))
      #for(i in 1:ncol(unreac_knn$test_knn_dist)){
      #closest_reachable = unreac_knn$test_knn_idx[which(unreac_knn$test_knn_dist[,i] == min(unreac_knn$test_knn_dist[,i])),i]
      #unreac_knn$test_knn_idx[which(unreac_knn$test_knn_dist %in% apply(unreac_knn$test_knn_dist, 2, min))]
      lknn_for_landm[from = unreachable, to = c(unreac_knn$test_knn_idx[which(unreac_knn$test_knn_dist %in% apply(unreac_knn$test_knn_dist, 2, min))]+1), attr="weight"] <- c(apply(unreac_knn$test_knn_dist, 2, min))
      lknn_for_landm[from = c(unreac_knn$test_knn_idx[which(unreac_knn$test_knn_dist %in% apply(unreac_knn$test_knn_dist, 2, min))]+1), to = unreachable, attr="weight"] <- c(apply(unreac_knn$test_knn_dist, 2, min))
      if( cou == 5){
        break
      }
    }
    #short_dist_temp <- igraph::distances(graph = lknn_for_landm, v = V(lknn_for_landm)[L[i]], weights = NULL, mode = 'all', algorithm = 'dijkstra')
    short_dist_land[i,] <- igraph::distances(graph = lknn_for_landm, v = V(lknn_for_landm)[L[i]], weights = NULL, mode = 'out', algorithm = 'dijkstra')

  }

  if (any(short_dist_land == Inf)){
    print('Warning: some points remained unreachable and their distance is set Inf')
  }
  #MATLAB function exchanges every Inf value for 1 ????
  # dist[dist == Inf] <- max(max(dist[dist != Inf]))
  #if (parameters.verbose)
  # print('Warning: some points remained unreachable and their distance is set Inf')
  #end

  ######################
  # adjust paths according to partial order by redirecting
  nPartialOrder = length(partial_order)
  for(r in 1:nPartialOrder){
    for(wp_r in 1:nPartialOrder){
      if (wp_r + r <= nPartialOrder){
        a = wp_r
        b = wp_r + (r-1)
        c = wp_r + r
        short_dist_land[a, partial_order[c]] = short_dist_land[a, partial_order[b]] + short_dist_land[b, partial_order[c]]
      }
      if (wp_r - r >= 1){
        a = wp_r
        b = wp_r - (r-1)
        c = wp_r - r
        short_dist_land[a, partial_order[c]] = short_dist_land[a, partial_order[b]] + short_dist_land[b, partial_order[c]]
      }
    }
  }
  ######################

  traj_dist <- short_dist_land

  ######################
  # This is the actual align for regular wanderlust
  if(length( L ) > length(partial_order)){

    for(idx in as.numeric(length(partial_order)+1):length( L )){
      # find position of landmark in dist_1
      idx_val = short_dist_land[ 1, L[idx ] ]
      # convert all cells before starting point to the negative
      before_indices = which( short_dist_land[ 1,  ] < idx_val )
      traj_dist[ idx, before_indices ] = -short_dist_land[ idx, before_indices ]
      # set zero to position of starting point
      traj_dist[ idx, ] = traj_dist[ idx, ] + idx_val
    }
  }

  #Voting scheme for weighing matrix

  if(voting_scheme == 'uniform'){
    T_full <- matrix(data = 1, nrow = length(L), ncol = ncol(short_dist_land))
  }
  if(voting_scheme == 'exponential'){
    sdv = mean ( apply( short_dist_land, 2, sd ))*3
    T_full = exp( -.5 * (short_dist_land / sdv)^2)
  }
  if(voting_scheme == 'linear'){
    temp <- list()
    for(i in 1:nrow(short_dist_land)){
      temp[[i]] <- apply(short_dist_land, 2, max)
    }
    T_full = do.call(rbind, temp)
    T_full = T_full - short_dist_land
  }
  if(voting_scheme == 'quadratic'){
    temp <- list()
    for(i in 1:nrow(short_dist_land)){
      temp[[i]] <- apply(short_dist_land, 2, function(x) max(x^2))
    }
    T_full = do.call(rbind, temp)
    T_full = T_full - (short_dist_land)^2
  }

  # The weghing matrix must be a column stochastic operator
  temp <- list()
  for(i in 1:nrow(T_full)){
    temp[[i]] <- apply(T_full, 2, sum)
  }
  T_full_sum <- do.call(rbind, temp)
  T_full = T_full/T_full_sum

  #T = T_full


  traj_w = rbind(traj_dist[1,], apply( traj_dist * T_full, 2, sum))

  # iteratively realign trajectory (because landmarks moved)
  converged = 0
  user_break = 0
  realign_iter = 2

  while(!converged && !user_break){
    realign_iter = realign_iter + 1
    dist_tem = short_dist_land
    for(idx in 1:nrow(dist_tem)){
      # find position of landmark in previous iteration
      idx_val = traj_w[ realign_iter - 1, L[idx] ]
      # convert all cells before starting point to the negative
      before_indices = which(traj_w[ realign_iter - 1, ] < idx_val )
      dist_tem[ idx, before_indices ] = -short_dist_land[ idx, before_indices]
      # set zero to position of starting point
      dist_tem[ idx, ] = dist_tem[ idx, ] + idx_val
    }


    # calculate weighed trajectory (same as in matlab)
    temp_mat <- dist_tem * T_full
    traj_w = rbind(traj_w, apply(temp_mat, 2, sum))
    correlation <- cor(as.numeric(traj_w[realign_iter,]), as.numeric(traj_w[c(realign_iter-1),]))
    converged = correlation > 0.9999

    if (realign_iter > 16){
      break
    }
    # break after too many alignments - something is wrong
    #user_break = true
    #fprintf('\nWarning: Force exit after %g iterations\n', realign_iter);
    #end
    #end
  }

  # save final trajectory for this graph
  traj_sum <- list(final_trajectory = as.numeric(traj_w[realign_iter, ]), traj=traj_dist, dist=short_dist_land, waypoints_idx = L, unreached = unreachable_list)

  return(traj_sum)
}




