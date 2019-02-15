# tSpace algorithm for unsupervised determination of multiple developmental branches in single cell data
By Denis Dermadi

Description

tSpace is the main function for trajectory inference. The algorithm is described in the publication (https://doi.org/10.1101/336313) 

Originally, it was developed for single cell analysis, however it can be applied on any type of large data.

# Installation

To install tSpace you need devtools package, which can be installed by running code:

`install.packages("devtools")`

If devtools are already installed, or after devtools installation please run code:

`devtools::install_github('hylasD/tSpace', build = TRUE, build_opts = c("--no-resave-data", "--no-manual"), force = T)`

tSpace depends on several packages, at the present moment `igraph` installation has some issues on iOS and Linux, probably related to gcc compilers. If you run in to installation issues, try to install older versions of `igraph` prior to `tSpace` installation.


# How to use the algorithm

Vignette with a demo data is included in the package.

To open tutorial on tSpace from R please use function 

`vignette(package = 'tSpace', topic = 'introduction')`

or [online](http://denisdermadi.com/tspace-trajectory-inference-algorithm)


