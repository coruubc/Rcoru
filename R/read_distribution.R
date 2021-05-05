#' Reads distribution of taxons
#'
#' This function loads the distribution of a taxon from DROBO. You have to be connected to the UBC network
#' and have access to the CORU-DROBO in order to use the function. You can choose to return the data or just a plot for visualization Note that no user/id will be
#' required but the path wont be found. Note that this function needs/lodas the CORU coordinate grid
#'
#'
#' @param taxon_key is the species to load as taxon key number.
#' @param box Expects a vector with four values in the following order: low lat, high lat, low long and high long to load geographical specific data.. if FALSE it will load global database
#' @param root_path Expects the root computer path before DROBO. Note: do not include /
#' @return A data frame or a map showing the distribution of the taxa
#'
#'#' @export
read_distribution <- function(taxon_key, root_path = NA, output = "plot", coords = FALSE){

  library <- c("ggplot2","dplyr")
  lapply(library, require, character.only = TRUE)

# Set distribution path
  dist_path <- paste0(root_path,"/DATA/DATA/DBEM/")

  distribution <- read.csv(dist_path, head = F)

  if(output == "data"){
    return(distribution)
  }

  if(output == "plot"){

    if(hasArg(coords) == FALSE){
      print(paste("You need to load the DBEM coordinate system"))
      stop()
    }

    # Plot map
    map <- coords %>%
      bind_cols(distribution) %>%
      filter(V1 > 0) %>%
      ggplot() +
      geom_tile(
        aes(
          x = lon,
          y = lat,
          fill = V1,
          col = V1
        )
      ) +
      coord_quickmap() +
      theme_minimal()

  }

  return(map)
}
