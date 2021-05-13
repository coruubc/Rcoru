#' Reads SAU distribution of taxons
#'
#' This function loads the SAU distribution of a taxon from DROBO. You have to be connected to the UBC network
#' and have access to the CORU-DROBO in order to use the function. You can choose to return the data or just a map
#' for visualization.
#' Note that no user/id will be required but the path wont be found.
#' Note that this function needs the CORU coordinate grid
#'
#' @author Juliano Palacios Abrantes | j.palacios@oceans.ubc.ca
#' @param taxon_key is the species to load as taxon key number.
#' @param box Expects a vector with four values in the following order: low lat, high lat, low long and high long to load geographical specific data.. if FALSE it will load global database
#' @param root_path Expects the root computer path before DROBO. Note: do not include /
#' @examples
#' Note needs coordinate system for running
#' output data
#' read_distribution(600004,output = "data",root_path = "/Volumes",coords = Lon_Lat_DBEM)
#' #' output plot
#' read_distribution(600004,output = "plot", root_path = "/Volumes",coords = Lon_Lat_DBEM)
#' @return A data frame or a map showing the distribution of a specific taxa according to the Sea Around Us
#'
#'#' @export
read_distribution <- function(taxon_key, root_path = NA, output = "plot", coords = FALSE){

  library <- c("ggplot2","dplyr")
  lapply(library, require, character.only = TRUE)

# Set distribution path
  dist_path <- paste0(root_path,"/DATA/SAU_SppDistributions/S",taxon_key,".csv")

  distribution <- data.table::fread(dist_path, head = F)
  colnames(distribution) <- "dis"

  if(output == "data"){
    return(distribution)
  }

  if(output == "plot"){

    if(hasArg(coords) == FALSE){
      print(paste("You need to load the DBEM coordinate system"))
      stop()
    }else{
      colnames(coords) <- c("index","lon","lat")
    }

    # Plot map
    map <- coords %>%
      bind_cols(distribution) %>%
      filter(dis > 0) %>%
      ggplot() +
      geom_tile(
        aes(
          x = lon,
          y = lat,
          fill = dis,
          col = dis
        )
      ) +
      coord_quickmap() +
      theme_minimal()

  }

  return(map)
}
