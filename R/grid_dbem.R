#' Matches shapefile with DBEM grid
#'
#' @author Juliano Palacios Abrantes | j.palacios@oceans.ubc
#' @param shapefile can be a shapefile or a df. if DF make sure longitude and latitude are present as "lon and "lat
#' @param dbem_grid the DBEM grid
#' @param out options of output df = for data frame sf for shapefile
#' @return A datafrmae with the

#' @export
#'
grid_dbem <- function(shapefile, dbem_grid, out = "df"){

  dbem_grid <- sf::st_as_sf(dbem_grid,
                            coords = c("lon","lat"),
                            crs = 4326)

  # if supplied a grid will rasterize
  if(inherits(shapefile, "sf")== FALSE){

    shapefile <- sf::st_as_sf(shapefile,
                              coords = c("lon","lat"),
                              crs = 4326) %>%
      tibble::rowid_to_column()
  }else{

    # Make sure shapefile is in the same coordinate system
    shapefile <- shapefile %>%
      st_set_crs(4326)
  }

  # Merge both
  if(out == "sf"){
    merge_grid <- sf::st_join(dbem_grid,
                              shapefile,
                              join = st_intersects)
  }else{
    merge_grid <- sf::st_join(dbem_grid,
                              shapefile,
                              join = st_intersects) %>%
      as.data.frame() %>%
      select(-geometry)
  }
  return(merge_grid)
}


