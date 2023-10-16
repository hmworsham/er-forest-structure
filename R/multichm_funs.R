# Functions to run LayerStacking ITC delineation algorithm

#' LayerStacking initialization
#' @description Initialize LayerStacking for optimization
#' @export ls.init
#'
mc.init <- function(pc, res, layer_thickness, dist_2d, dist_3d, use_max=F, ws=5) {
  algo <-  multichm(res, layer_thickness, dist_2d, dist_3d, use_max, ws)
  mc.trees <- find_trees(pc, algo)
  return(mc.trees)
}

#' PTrees optimization
#' @description Run LayerStacking optimization algorithm across sample areas
#' @export ls.opt
#'
mc.opt <- function(x, params) {

  # Apply multichm algorithm using all parameter sets
  modtrees <- mcmapply(mc.init,
                       params[,1],
                       params[,2],
                       params[,3],
                       params[,4],
                       MoreArgs=list(pc=x, use_max=F),
                       mc.cores = getOption("mc.cores", length(workerNodes)-2)
  )

  # Clean results
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x){
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}
