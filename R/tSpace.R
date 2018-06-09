#' tSpace
#'
#' tSpace is the main function for trajectory analysis. The algorithm is described in the publication
#' Dermadi et al. 2018
#' Originally, it was developed for single cell analysis, however it can be applied on any type of large data.
#'
#' If you use it please cite:
#'
#' @param df a data frame or a matrix of expression values, which contain information on data straucture,
#' e.g. expression of variable genes, developmentally relevant genes/proteins
#' @param K an integer specifying the K-nearest-neighbors
#' @param L an integer specifying the random L out of K-nearest-neighbors, L < K, usually K = (4*L)/3
#' @param D a string specfying metric for distance calculation. Supported: ’euclidean’, ’pearson_correlation’,
#' ’manhattan’, ’chebyshev’, ’canberra’, ’braycurtis’,  ’simple_matching_coefficient’, ’minkowski’,
#' ’hamming’, ’mahalanobis’, ’jaccard_coefficient’, ’Rao_coefficient’
#' @param graph an integer specifying how many L-K-NN graphs will be used for final trajectory calculation
#' @param trajectories an integer specifying how many trajectories will be calculatedl. Default value is 100,
#' see ground_truth for more details
#' @param wp an integer specifying the number of waypoints for trajectory refinement
#' @param ground_truth a booolean (TRUE or FALSE) specifying if trajectories are calculated for every data point.
#' As default tSpace calculates an aproximation using 100 trajectories, which is usually sufficient for understanding
#' of developmental relations in single cell data. If set to TRUE, calculation time will be longer
#' and trajectories parameter will be overridden.
#' @param weights a string specfying method to calculate the weights for refinement of the trajectory distances. Supported:
#' uniform, linear, quadratic and exponential.
#' @param dr a string specifying type of embbeding for visualization. Options: 'pca', 'tsne' or 'both'.
#' 'pca' embbeds trajectory space matrix in principal components,
#' 'tsne' uses Rtsne function with parameters: perplexity = 25, max_iter = 2000, for details see documentation of Rtsne package,
#' 'both' calculates pca and tsne
#' @param seed an integer specifying seed for set.seed function in order to have reproducible tsne.
#' @param core_no and integer specifying number of cores for parallelization, check how many cores your machine has and adjust accordingly
#' @return tSpace returns a list of objects: 1. ts_file: a data frame of pca and/or t-SNE embbedings of trajectory space matrix and input data,
#' 2. pca and/or tsne objects. pca object contians all the outputs of pca analysis,
#' tsne contians all the outputs of the t-SNE analysis, see \code{\link[Rtsne:Rtsne]{Rtsne}}
#' 3. trajectory space matrix with calcualted distances
#' @importFrom foreach %dopar%
#' @export
tSpace <- function(df, K = 20,  L = 15, D = 'pearson_correlation', graph = 5, trajectories = 100, wp = 20, ground_truth = F, weights = 'exponential', dr = 'pca', seed = NULL, core_no = 1){

  if(any(sapply(df, function(x) is.numeric(x)) == F)){
    stop("Check that all values in your data frame are numeric")
  }
  # to do
  # Evaluate inputs, fix once all is running smootly
  if(!is.numeric(core_no)){
    stop("Number of cores is not numeric")
  }
  if(!is.numeric(K) | !is.numeric(L) | !is.numeric(graph) | !is.numeric(wp) | !is.numeric(trajectories)){
    stop("K, L, graph, waypoints or trajectories variables are not numbers")
  }
  if(!(D %in% c('euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 'minkowski',  'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'))){
    stop( "distance can be any of 'euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient', 'minkowski',  'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'" )
  }
  if(!(weights %in% c('uniform', 'linear', 'quadratic', 'exponential'))){
    stop( "weights can be any of 'uniform', 'linear', 'quadratic', 'exponential'" )
  }
  if(!(dr %in% c('pca', 'tsne', 'both'))){
    stop( "dimensionality reduction can be any of 'pca', 'tsne', 'both'" )
  }

  #########################

  if(ground_truth == T){
    numPop <- 1:nrow(df)

    tspacem <- matrix(data = NA, nrow = dim(df)[1], ncol = length(unique(numPop)))

    graph_panel <- list()

    s <- numPop

    trajectories <- length(numPop)
    Index <- numPop
  } else {
    numPop <- kmeans(df, centers = trajectories, iter.max = 10000)

    Index <- seq(1, nrow(df), by=1)

    tspacem <- matrix(data = NA, nrow = dim(df)[1], ncol = length(unique(numPop$cluster)))

    graph_panel <- list()

    s <- unlist(lapply(split(Index, as.factor(numPop$cluster)),
                       function(x) {
                         as.numeric(x[1])
                       }))
  }

  #########
  ## Functions
  cat(paste0('Finding graph'))
  knn <- graphfinder(x = df, k = K, distance = D, core_n = core_no)
  knn <- igraph::get.adjacency(igraph::graph.adjacency(Matrix::sparseMatrix(i=knn[,'I'], j=knn[,'J'], x=knn[,'D']), mode ='max', weighted = TRUE), attr = 'weight') # For comapriosn wiht MATLAB , index1 = F)
  cat(paste0('Finding trajectories in sub-graphs \nCalculation may take time, don\'t close R'))


  graph_panel <- list()
  tic('graphs_loop')
  for(graph_iter in 1:graph){
    if(K != L){
      l.knn = find_lknn(knn, l = L, core_n = core_no)
      # at this stage lknn is directed graph
    }
    else{
      l.knn = knn
    }

    cl <- parallel::makeCluster(core_no)
    doParallel::registerDoParallel(cl)

    tspacem <- foreach::foreach(i = 1:trajectories, .combine = cbind, .packages = c('igraph', 'Matrix', 'KernelKnn', 'pracma')) %dopar%{  # follwing ' , .export="pathfinder" ' used to be here
      #for(tt in 1:trajectories){
      s_c = as.numeric(s[i])
      tspacem <- pathfinder(data = df, lknn = l.knn, s = s_c, waypoints = wp, voting_scheme = weights, distance = D)$final_trajectory
      #test <- pathFinder(traj = test, voting_scheme = 'exponential')

      #tspacem <- pathFinder(traj = test)$final_trajectory #NULL #real shit of a function to be worked on, add all parameters from
      # tspacem[,tt] <- pathFinder(traj = test)$final_trajectory #NULL #real shit of a function to be worked on, add all parameters from
      #waypoints_paths
    }

    parallel::stopCluster(cl)
    graph_panel[[graph_iter]] <- tspacem

  }
  time <- toc()

  arr <- array( unlist(graph_panel) , c(nrow(graph_panel[[1]]),ncol(graph_panel[[1]]),graph) )
  tspace_mat <- rowMeans( arr , dims = 2 )

  if( dr == 'pca'){
    if(ncol(tspace_mat) > 20){
      set.seed(seed)
      pca_tspace <- prcomp(t(tspace_mat), rank. = 20)
    } else {
      set.seed(seed)
      pca_tspace <- prcomp(t(tspace_mat), center = T, scale. = T)
    }
    data.out = as.data.frame(cbind(Index = Index, pca_tspace$rotation, df))

    tspace_obj <- list(ts_file = data.out, pca_embbeding = pca_tspace, tspace_matrix = tspace_mat)
  }

  if( dr == 'tsne'){
    set.seed(seed)
    tsne_tspace <- Rtsne::Rtsne(tspace_mat, perplexity = 25, max_iter = 2000, theta)

    data.out <- as.data.frame(cbind(Index = Index,tsne_tspace$Y, df))

    tspace_obj <- list(ts_file = data.out, tsne_embbeding = tsne_tspace, tspace_matrix = tspace_mat)
  }

  if( dr == 'both'){
    if(ncol(tspace_mat) > 20){
      set.seed(seed)
      pca_tspace <- prcomp(t(tspace_mat), rank. = 20)
    } else {
      set.seed(seed)
      pca_tspace <- prcomp(t(tspace_mat), center = T, scale. = T)
    }
    tspace_pca = as.data.frame(pca_tspace$rotation)

    set.seed(seed)
    tsne_tspace <- Rtsne::Rtsne(tspace_mat, perplexity = 25, max_iter = 2000)
    data.out = as.data.frame(cbind(Index = Index,pca_tspace$rotation, tsne_tspace$Y, df))
    tspace_obj <- list(ts_file = data.out, pca = pca_tspace, tsne = tsne_tspace, tspace_matrix = tspace_mat)
  }

  return(tspace_obj)

}


