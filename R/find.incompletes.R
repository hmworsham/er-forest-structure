# Function to find incomplete items in tree crown detection runs

#' Find incompletes
#' @description Identify incomplete items in tree crown detection runs
#' @export find.incompletes
#'
find.incompletes <- function(big.grid, completed.files) {
  grids.incomp <- big.grid[!big.grid$id %in% completed.files,]
  grids.incomp.diss <- st_cast(st_union(st_buffer(grids.incomp, 0.1)), 'POLYGON') #Buffer to make the corners touch, union to dissolve adjacent borders
  dissolved <- st_sf(grids.incomp.diss) #Create a sf object from the geometries
  dissolved$clusterID=1:length(grids.incomp.diss) #Add cluster id to each row
  grids.incomp.cluster <- st_join(st_sf(grids.incomp), dissolved) #Join cluster id to the original polygons
  grids.incomp.a <- st_union(grids.incomp)
  grids.incomp.d <- st_cast(grids.incomp.a, 'POLYGON')

  # Plot aggregation options
  par(mfrow=c(2,3))
  plot(grids.incomp.d[as.integer(st_area(grids.incomp.d))>=10000], col='black', main='10000')
  plot(grids.incomp.d[as.integer(st_area(grids.incomp.d))>=50000], col='red', main='50000')
  plot(grids.incomp.d[as.integer(st_area(grids.incomp.d))>=80000], col='dodgerblue', main='80000')
  plot(grids.incomp.d[as.integer(st_area(grids.incomp.d))>=100000], col='green4', main='100000')
  plot(grids.incomp.d[as.integer(st_area(grids.incomp.d))>=150000], col='gold2', main='150000')

  return(grids.incomp.d)
}
