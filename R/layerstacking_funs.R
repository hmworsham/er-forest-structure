# Functions to run LayerStacking ITC delineation algorithm

#' LayerStacking initialization
#' @description Initialize LayerStacking for optimization
#' @export ls.init
#'
ls.init <- function(pc, start, res, ws1, ws2, buf_size, hmin=1.3, hardwood=F) {
  algo <- LayerStacking(start, res, ws1, ws2, buf_size, hmin, hardwood)
  ls.trees <- find_trees(pc, algo)
  return(ls.trees)
}

#' PTrees optimization
#' @description Run LayerStacking optimization algorithm across sample areas
#' @export ls.opt
#'
ls.opt <- function(x, params) {

  # Apply LayerStacking algorithm using all parameter sets
  modtrees <- mcmapply(ls.init,
                       params[,1],
                       params[,2],
                       params[,3],
                       params[,4],
                       params[,5],
                       MoreArgs=list(pc=x, hmin=1.3, hardwood=F),
                       mc.cores = getOption('mc.cores', 30)
  )

  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x){
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)

}
