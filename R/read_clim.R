#' Reads ocean variables from ESMs
#'
#' This function reads oceanic climate variables from DROBO. You have to be connected to the UBC network
#' and have access to the CORU-DROBO in order to use the function. Note that no user/id will be
#' required but the path wont be found.
#'
#' @author Juliano Palacios Abrantes | j.palacios@oceans.ubc
#' @param cmip can be 5 for CMIP5 or 6 cmip6. Note that CMPI5 only has GFDL, IPSL and MPI. CMPI6 has additional CNRM and UKESM
#' @param esm is the ESM esms to load; GFDL; IPSL, MPI, CNRM, UKESM. For all esms select "All";
#' @param rcp expects "26" for RCP 2.6-low emission scenario and "85" for RCP 8.5-high emission scenario
#' @param variable expects a variable to be load. Options include: "AdvectionU","AdvectionV", "bot_temp", "htotal_btm", "htotal_surf", "IceExt", "O2_btm","O2_surf","Salinity_btm","Salinity_surf","SST","totalphy2". Note: if you type "options" the function will print all available variables
#' @param years expects a sequence of years to load the data from
#' @param box Expects a vector with four values in the following order: low lat, high lat, low long and high long to load geographical specific data.. if FALSE it will load global database
#' @param coords This is the DBEM/CORU coordinate grid with index, lon and lat
#' @return A tibble with the environmental data requested for the world or a specific box
#' @examples
#' Change the root_path before running
#'
#' Get available variables
#' read_clim(variable = "options)
#'
#' One variable
#' read_clim(6,"GFDL",85,"SST",c(1951,1952), root_path = "/Volumes")
#'
#' Multiple variables with the lapply family
#' variables <-c ("O2_btm","htotal_btm")
#' lapply(variables,read_clim, cmip= 6, esm = "IPSL",rcp = 85, years = c(1951,1921), root_path = "/Volumes")
#'
#' For specific Box. Note requires coordinate system
#' read_clim("GFDL",85,"SST",c(1951,1952), root_path = "/Volumes", box = c(28,74,-110,-170), coords = Lon_Lat_DBEM)
#' @export
#'
#'
read_clim <- function(
    cmip = 6,
    esm,
    rcp,
    variable,
    years,
    root_path,
    box = FALSE,
    coords = FALSE
){

  if(variable == "options"){
    print(c("AdvectionU","AdvectionV", "bot_temp", "htotal_btm", "htotal_surf", "IceExt", "O2_btm","O2_surf","Salinity_btm","Salinity_surf","SST","totalphy2"))
  }else{


    # ----------------#
    # Packages needed
    # ----------------#
    library <- c("tibble","dplyr","data.table")
    lapply(library, require, character.only = TRUE)

    if(cmip == 6){
      cmip_path <-  "/DATA/DATA/Environmental data/CMIP6_DATA/for_DBEM/C6"
    }

    if(cmip == 5){
      cmip_path <-  "/DATA/DATA/CMIP5 txt/"
    }

    # -------------- #
    # Create data path
    # -------------- #

    # Totalphy is the only variable with a different format
    if(variable == "totalphy2"){
      data_path <- paste(root_path,cmip_path,esm,rcp,"/",variable,years,".txt",sep="")
    }else{
      data_path <- paste(root_path,cmip_path,esm,rcp,"/",variable,"_",years,".txt",sep="")
    }

    # Checking step
    if(file.exists(data_path)[1] == "FALSE"){
      print(paste("Oh-oh, looks like your root path is wrong. Or maybe data was moved? Path:",data_path))
      stop()
    }

    # Message to user
    print(paste("You are loading",variable, "for the CMIP",cmip,esm,"ESM","under rcp",rcp,"for",length(years), "years"))

    # -------------- #
    # Load raw data
    # -------------- #

    if(hasArg(box) == FALSE){
      suppressMessages(
        clim_data <- bind_cols(
          lapply(data_path, fread)
        ) %>%
          mutate(variable = variable,
                 esm = paste0("C",cmip,esm),
                 rcp = rcp) %>%
          select(esm,rcp,variable,everything())
      )

      colnames(clim_data) <- c("esm","rcp","variable",years)

    }else{
      # If you want a specific box in the world

      # Set box coordinate system
      lat_l <- box[1]
      lat_h <- box[2]
      lon_l <- box[3]
      lon_h <- box[4]

      # Load data

      # Stop in case you do not have coordinates
      if(hasArg(coords) == FALSE){
        print(paste("You need to load the DBEM coordinate system"))
        stop()
      }

      colnames(coords) <- c("index","lon","lat")

      suppressMessages( # new names message for multiple columns
        clim_data <- bind_cols(
          lapply(data_path, fread)
        ) %>%
          mutate(variable = variable,
                 esm = paste0("C",cmip,esm),
                 rcp = rcp) %>%
          rowid_to_column("index") %>%
          left_join(coords,
                    by = "index") %>%
          filter(
            lat >= lat_l, lat <= lat_h,
            lon >= lon_l & lon <= lon_h
          ) %>%
          select(esm,rcp,variable,index,lon,lat,everything())
      )

      colnames(clim_data) <- c("esm","rcp","variable","index","lon","lat",years)

    }

    return(clim_data)
  }
}
