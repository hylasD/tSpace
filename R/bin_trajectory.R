#' @title Cell aligner, binning and summary statistics function on trajectory
#'
#' @description Post trajectory analysis function for calcualting statistics on expression values along the isolated trajectory
#' @param x a data frame of expression values to be summarized statictically along the trajectory. It does not contain trajectory variable.
#' It may contain factors (e.g. cell labels, clusters).
#' @param trajectory a vector of trajectory distances to be used for cell alignement and binning. This vector can be extracted
#' from the tspace_matrix object, a matrix of trajectory space distances. Trajectory vector has to correspond to cells in the data frame x.
#' @param n an integer specifying number of the bins along the trajectory, the higher the number the fewer cells within the bin.
#' If n = NULL, function will bin all the cells with the same distance into common bin. For this feature it may be useful to round decimal
#' values in trajectory vector.
#' @param stat a string defining to calculate median ('median'),
#' mean ('mean') or quibic spline ('smooth'). The default is median.
#' @param trim a boolean, TRUE or FALSE, if TRUE the first bin will have borders `<min(distance), 0.05*max(distance)]` and the last one `<0.95*max(distance), max(distance)]`, set as default to FALSE
#' @return This function returns data frame in which cells are binned along the trajectory and expressions are
#' summarized by selected statistics.
#' @importFrom foreach %dopar%
#' @export
#'
bin.trajectory <- function(x, trajectory, n=NULL, stat='median', trim = FALSE, ...){

  #Check the type of the variables
  if(class(x) != "data.frame"){
    stop("Your x is not a data frame, consider as.data.frame function")
  }
  if(class(trajectory) != "numeric"){
    stop("Your trajectory is not of class numeric, consider as.numeric function")
  }
  if(is.null(n) == FALSE){
    if(class(n) != "numeric"){
    stop("Number of bins (n) is not a number")
    }
    if(n <= 0){
      stop("Number of bins (n) cannot be a negative value or zero")
    }
  }
  #if(stat != 'median' | stat != 'mean' | stat != 'slope'){
 #   stop("Check your stat. Allowed are 'mean', 'median' and 'slope'.")
 # }
  if(nrow(x) != length(trajectory)){
    stop("Size of your data frame and length of the trajectory do not match.")
  }

  if(trim == TRUE){
  # if(is.null(qu2) == FALSE){
  #   if(class(qu2) != "numeric"){
  #     stop("qu2 is not a number, choose NULL or a positive integer between 0 and 1. Check documentation.")
  #   }
  # }
  # if(is.null(qu1) == FALSE){
  #   if(class(qu1) != "numeric"){
  #     stop("qu1 is not a number, choose NULL or a positive integer between 0 and 1. Check documentation.")
  #   }
  # }
    qu2 <- 0.95
    qu1 <- 0.05
    }

  # Order cells based on trajectory distance

  x <- x[order(trajectory), ]

  # Order trajectory distances from smaller to the larger
  trajectory <- trajectory[order(trajectory)]

  # Check if the data frame contains any variables as factors and separate them into two objects
  # non numeric and factor
  fact <- x[which(sapply(x, function(x) is.numeric(x)) == FALSE | sapply(x, function(x) is.factor(x)) == TRUE)]

  if(ncol(fact) != 0){
  # Convers everything to factors
    for(i in 1:ncol(fact)){
      fact[,i] <- as.factor(fact[,i])
    }
  }

  # numeric only data frame
  x <- x[,sapply(x, function(x) is.numeric(x))]

  # Determinig window & splitting data into intervals based on trajectory distances
  if(is.null(n) == TRUE){
    xs <- split(x, f = as.factor(trajectory))
    if(ncol(fact) != 0){
    xsf <- split(fact, f = as.factor(trajectory))
    }
  } else {
    window <- seq(qu1*max(trajectory), qu2*max(trajectory), max(trajectory)/n)#, max(trajectory), by = round(nrow(x)/n, digits = 0))
    window <- c(min(trajectory), window, max(trajectory))
    xs <- split(x, cut(trajectory, window))
    if(ncol(fact) != 0){
    xsf <- split(fact, cut(trajectory, window))
    }
  }

# if factors or characters are found in the data set this if statement takes care of them to calcualte the one with highest representation
    if(ncol(fact) != 0){

      temp <- list()
      for(i in 1:length(xsf)){

        temp[[i]] <- do.call(cbind,
                        lapply(xsf[[i]], function(x) {
                            as.character(levels(x))[as.vector(unlist(by(x, x, length))) %in% max(as.vector(unlist(by(x, x, length))), na.rm = T)]}))

        }
      # Problem of factors that are evenly represented, and there is no clear winner, combines all labels into one
      if(length(xsf) != nrow(as.data.frame(do.call(rbind, temp)))){

        temp <- lapply(temp, function(x){apply(x, 2, function(x) paste(unique(x), collapse = " "))})
        temp <- as.data.frame(do.call(rbind, temp))
        names(temp) <- names(fact)
        message("Some of the factor variables merged together in summarization process,\nbecause there was no clear over representation of one.
                \nPlease check your factor columns and labels.")
      }else{
      temp <- as.data.frame(do.call(rbind, temp))

      names(temp) <- names(fact)
      }
    }else{
      temp <- NULL
    }


# Numerical data
  # Median calculation
    if(stat == 'median'){
      temp_num <- as.data.frame(do.call(rbind, lapply(xs, function(x) apply(x, 2, function(x) median(x, na.rm = T)))))
      temp_num$Bins <- row.names(temp_num)
    }
  # Mean calculation
    if(stat == 'mean'){
      temp_num <- as.data.frame(do.call(rbind, lapply(xs, function(x) apply(x, 2, function(x) mean(x, na.rm = T)))))
      temp_num$Bins <- row.names(temp_num)
    }
  # Fitting the line in the bin, and calculating slope (beta coefficient)
      #TO DO - code does not work properly
  # if(stat == 'slope'){
    #   traj <- split(trajectory, cut(trajectory, window))
    #   temp_num <- list()
    #   for(i in 1:length(xs)){
    #     which(nrow(xs[[i]]) == 0)
    #     temp_num[[i]] <- as.data.frame(t(apply(xs[[i]], 2, function(x, traj, i) {
    #       if(length(traj[[i]]) == 0){
    #         NA
    #       }else{
    #         coef(lm(x ~ traj[[i]]))[2]
    #       }
    #     }, traj, i)))
    #
    #   }
    #   temp_num <- as.data.frame(do.call(rbind, temp_num))
    # }
  # smoothing using qubic spline
    if(stat == 'smooth'){
      cl <- parallel::makeCluster(parallel::detectCores() - 1)
      doParallel::registerDoParallel(cl)
      temp_sm <- foreach(i = 1:ncol(x), .combine = cbind) %dopar% {
        temp_sm <- smooth.spline(trajectory, x[,i], cv = T)$y
        #window_stat2(x = s2e_traj[,i], trajectory = s2e_traj$Traj_dist_SC_Enter, n = 400, stat = 'mean', w = "uniform")
      }
      parallel::stopCluster(cl)
      colnames(temp_sm) <- colnames(x)
      temp.x <- smooth.spline(trajectory, x[,1], cv = T)$x
      temp_num <- as.data.frame(cbind(temp_sm, traj.dist = temp.x))
    }
  if(is.null(temp)){
    temp <- as.data.frame(temp_num)
  }else{
    temp <- as.data.frame(cbind(temp_num, temp))
  }

  return(temp)

}
