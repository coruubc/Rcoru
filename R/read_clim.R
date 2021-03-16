#' Reads ocean variables from ESMs
#'
#' This function loads reads oceanic climate variables from DROBO. You have to be connected to the UBC network
#' and have access to the CORU-DROBO in order to use the function. Note that no user/id will be
#' required but the path wont be found.
#'
#' @param cmip can be 5 for CMIP5 or 6 CMPI6. Note that CMPI5 only has GFDL, IPSL and MPI. CMPI6 has additional CNRM and UKESM
#' @param model is the ESM models to load; GFDL; IPSL, MPI, CNRM, UKESM. For all models select "All";
#' @param rcp expects "26" for RCP 2.6-low emission scenario and "85" for RCP 8.5-high emission scenario
#' @param variable expects a variable to be load
#' @param years expects a sequence of years to load the data from
#' @param root_path Expects the root computer path before DROBO. Note: do not include /
#' @param box Expects a vector with four values in the following order: low lat, high lat, low long and high long to load geographical specific data.. if FALSE it will load global database
#' @export
#'
read_clim <- function(
  cmip,
  model,
  rcp,
  variable,
  years,
  root_path,
  box = FALSE
){

  # ----------------#
  # Packages needed
  # ----------------#
  library <- c("tibble","dplyr","data.table")
  lapply(library, require, character.only = TRUE)

  if(cmip == 6){
    cmip <-  "CMIP6_DATA/for_DBEM/C6"
  }

  if(cmip == 5){
    cmip <-  "CMPI5_DATA/"
  }

  # -------------- #
  # Create data path
  # -------------- #

  # Totalphy is the only variable with a different format
  if(variable == "totalphy2"){
    data_path <- paste(root_path,"/DATA/DATA/Environmental data/",cmip,model,rcp,"/",variable,years,".txt",sep="")
  }else{
    data_path <- paste(root_path,"/DATA/DATA/Environmental data/",cmip,model,rcp,"/",variable,"_",years,".txt",sep="")
  }

  # Checking step
  if(file.exists(data_path) == "FALSE"){
    print(paste("Oh-oh, looks like your root path is wrong. Or maybe data was moved? Path:",data_path))
    stop()
  }

  # Message to user
  print(paste("You are loading",variable, "for the CMIP",cmpi,"ESM",model,"under rcp",rcp,"for",length(years), "years"))

  # -------------- #
  # Load raw data
  # -------------- #

  if(box == FALSE){
    clim_data <- bind_cols(
      lapply(data_path, fread)
    ) %>%
      mutate(variable = variable,
             model = paste0("C",cmip,model),
             rcp = rcp) %>%
      select(model,rcp,variable,everything())

    colnames(clim_data) <- c("model","rcp","variable",years)

  }else{
    # If you want a specific box in the world

    # Set box coordinate system
    lat_l <- box[1]
    lat_h <- box[2]
    lon_h <- box[3]
    lon_l <- box[4]

    # Load data
    clim_data <- bind_cols(
      lapply(data_path, fread)
    ) %>%
      mutate(variable = variable,
             model = paste0("C",cmip,model),
             rcp = rcp) %>%
      rowid_to_column("index") %>%
      left_join(lon_lat_grid,
                by = "index") %>%
      filter(
        lat >= lat_l, lat <= lat_h,
        lon >= -lon_h & lon <= -lon_l
      ) %>%
      select(model,rcp,variable,index,lon,lat,everything())

    colnames(clim_data) <- c("model","rcp","variable","index","lon","lat",years)

  }

  return(clim_data)

}
