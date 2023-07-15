# Functions to run LMF fixed window ITC delineation algorithm

#' LMF fixed window initialization
#' @description Initialize LMF fixed window for optimization
#' @export ls.init
#'
lmf.fw.init <- function(pc, ws, shape, hmin=1.3){
  algo = lmf(ws=ws, shape=shape, hmin=hmin)
  lmf.fw.trees <- find_trees(pc, algo)
  return(lmf.fw.trees)
}

#' LMF fixed window optimization
#' @description Run LMF fixed window optimization algorithm across sample areas
#' @export ls.opt
#'
lmf.fw.opt <- function(x, params) {

  modtrees <- mcmapply(lmf.fw.init,
                       ws=params[,1][[1]],
                       shape=params[,2][[1]],
                       MoreArgs=list(pc=x, hmin=1.3),
                       mc.cores=getOption('mc.cores', 30)
  )

  # Clean up results
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })

  return(modtrees)
}
