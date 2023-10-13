# Functions to run PTrees ITC delineation algorithm

#' Ptrees initialization
#' @description Initialize PTrees for optimization
#' @param k integer vector. A serie of k-nearest neighbors to use. In this original paper a k refers to a 'scale' of analyse (see reference).
#' @param pc LAS. Point cloud data in LAS format
#' @param hmin scalar. This is an addition from the original paper to limit oversegmentation. Point below this threshold cannot initiate new trees or increase a hull (see details). Set to -Inf to strictly respect original paper.
#' @param nmax integer. This is an addition from the original paper to protect against uncomputable cases (see details). Set to +Inf to strictly respect the original paper (not recommended)
#' @return dataframe. X,Y,Z values corresponding to locations and heights of detected trees
#' @export ptrees.init
#'

ptrees.init <- function(k, pc, hmin, nmax=9L) {
  pt.trees <- find_trees(pc, ptrees(k, hmin, nmax))
  return(pt.trees)
}

#' PTrees optimization
#' @description Run PTrees optimization algorithm across sample areas
#' @param x list. List of point cloud datsets in LAS format
#' @param params list. List of optimizable values for k in ptrees
#' @return list. X,Y,Z locations of modeled trees in each sample area
#' @export ptrees.opt
#'
ptrees.opt <- function(x, params, hmin=1.3) {
  modtrees <- mclapply(params,
                       FUN=ptrees.init,
                       pc=x,
                       hmin=hmin,
                       nmax=9L,
                       mc.cores=getOption('mc.cores', 30)
  )
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(t){
    tl <- t[!st_is_empty(t),]
    tl
  })

  return(modtrees)
}
