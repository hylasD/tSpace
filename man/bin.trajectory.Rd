% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_trajectory.R
\name{bin.trajectory}
\alias{bin.trajectory}
\title{Cell aligner, binning and summary statistics function on trajectory}
\usage{
bin.trajectory(x, trajectory, n = NULL, stat = "median",
  trim = FALSE, ...)
}
\arguments{
\item{x}{a data frame of expression values to be summarized statictically along the trajectory. It does not contain trajectory variable.
It may contain factors (e.g. cell labels, clusters).}

\item{trajectory}{a vector of trajectory distances to be used for cell alignement and binning. This vector can be extracted
from the tspace_matrix object, a matrix of trajectory space distances. Trajectory vector has to correspond to cells in the data frame x.}

\item{n}{an integer specifying number of the bins along the trajectory, the higher the number the fewer cells within the bin.
If n = NULL, function will bin all the cells with the same distance into common bin. For this feature it may be useful to round decimal
values in trajectory vector.}

\item{stat}{a string defining to calculate median ('median'),
mean ('mean') or quibic spline ('smooth'). The default is median.}

\item{trim}{a boolean, TRUE or FALSE, if TRUE the first bin will have borders `<min(distance), 0.05*max(distance)]` and the last one `<0.95*max(distance), max(distance)]`, set as default to FALSE}
}
\value{
This function returns data frame in which cells are binned along the trajectory and expressions are
summarized by selected statistics.
}
\description{
Post trajectory analysis function for calcualting statistics on expression values along the isolated trajectory
}
