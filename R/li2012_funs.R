# Functions to run Li 2012 ITC segmentation algorithm

#' Li 2012 initialization
#' @description Initialize Li 2012 for optimization
#' @param pc LAS. Point cloud data in LAS format
#' @param dt1 numeric vector. Set of values for threshold number 1. See reference page 79 in Li et al. (2012). Default is 1.5.
#' @param dt2 numeric vector. Set of values for threshold number 2. See reference page 79 in Li et al. (2012). Default is 2.
#' @param R numeric vector. Set of values for search radius. See page 79 in Li et al. (2012). Default is 2. If R = 0 all the points are automatically considered as local maxima and the search step is skipped (much faster).
#' @param Zu numeric. If point elevation is greater than Zu, dt2 is used, otherwise dt1 is used. See page 79 in Li et al. (2012). Default is 15.
#' @param hmin numeric. Minimum height of a detected tree. Default is 2.
#' @return dataframe. X,Y,Z values corresponding to locations and heights of detected trees
#' @export li2012.init
#'

li2012.init <- function(pc, dt1, dt2, R, Zu, hmin) {
  algo = li2012(dt1, dt2, R, Zu, hmin)
  li.trees = segment_trees(pc, algo) # segment point cloud
  crowns = crown_metrics(li.trees, func = .stdtreemetrics, geom = "convex")
  crowns = crowns[st_is_valid(crowns),]
  ttops = st_centroid(crowns)
  ttops = ttops[ttops$Z >= hmin,]
  # ttops$quad =
  return(ttops)
}


#' Li 2012 optimization
#' @description Run Li 2012 optimization algorithm across sample areas
#' @param x list. List of point cloud datsets in LAS format
#' @return list. X,Y,Z locations of modeled trees in each sample area
#' @export li2012.opt
#'

li2012.opt <- function(x, params, hmin=1.3){

  # Apply Li 2012 algorithm using all parameter sets
  modtrees <- mcmapply(li2012.init,
                       params[,1],
                       params[,2],
                       params[,3],
                       params[,4],
                       MoreArgs=list(pc=x, hmin=hmin),
                       mc.cores = getOption("mc.cores", length(workerNodes)-2))

  # Clean up results
  modtrees <- apply(modtrees, 2, data.frame)
  modtrees <- lapply(modtrees, st_as_sf)
  modtrees <- lapply(modtrees, function(x) {
    tl <- x[!st_is_empty(x),]
    tl
  })
  # modtrees <- Filter(function(x) nrow(x) > 0 , modtrees)

  return(modtrees)
}
