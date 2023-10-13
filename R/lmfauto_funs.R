# Functions to run LMF fixed window ITC delineation algorithm

#' LMF fixed window initialization
#' @description Initialize LMF fixed window for optimization
#' @export lmf.auto.init
#'
lmf.auto.init <- function(pc, hmin=1.3){
  algo = lmfauto(hmin=hmin)
  lmf.auto.trees <- find_trees(pc, algo)
  return(lmf.auto.trees)
}

#' LMF fixed window optimization
#' @description Run LMF fixed window optimization algorithm across sample areas
#' @export lmf.auto.opt
#'

lmf.auto.opt <- function(x, hmin) {

  modtrees <- lmf.auto.init(x, hmin=hmin)
  modtrees <- st_as_sf(modtrees)

  # Clean up results
  # modtrees <- lapply(modtrees, st_as_sf)
  # modtrees <- lapply(modtrees, function(x) {
  #   tl <- x[!st_is_empty(x),]
  #   tl
  # })

  return(modtrees)
}
