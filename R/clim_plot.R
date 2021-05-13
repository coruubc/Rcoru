#' Reads ocean variables from ESMs
#'
#' This function loads reads oceanic climate variables from DROBO. You have to be connected to the UBC network
#' and have access to the CORU-DROBO in order to use the function. Note that no user/id will be
#' required but the path wont be found.
#'
#'
#' @param cmip can be 5 for CMIP5 or 6 CMPI6. Note that CMPI5 only has GFDL, IPSL and MPI. CMPI6 has additional CNRM and UKESM
#' @param model is the ESM models to load; GFDL; IPSL, MPI, CNRM, UKESM. For all models select "All";
#' @param rcp expects "26" for RCP 2.6-low emission scenario and "85" for RCP 8.5-high emission scenario
#' @param variable expects a variable to be load
#' @param years expects a sequence of years to load the data from
#' @param box Expects a vector with four values in the following order: low lat, high lat, low long and high long to load geographical specific data.. if FALSE it will load global database
#' @param plot_type determines the type of plot you want. Options include: time_line, a timeline of the environ data (geom_line)

#' @export
#'
summary_clim <- function(
  cmip,
  model,
  rcp,
  variable,
  years,
  root_path,
  box = FALSE,
  plot_type
){

  # ----------------#
  # Packages needed
  # ----------------#
  library <- c("tibble","dplyr","data.table","ggplot2","here")
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
    data_path <- paste(here("/DATA/Environmental data/",cmip,model,rcp,"/",variable,years,".txt",sep=""))
  }else{
    data_path <- paste(here("/DATA/Environmental data/",cmip,model,rcp,"/",variable,"_",years,".txt",sep=""))
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

  #--------------- #
  # Plot data
  #--------------- #

  min_y <- min(years)
  max_y <- max(years)

  if(plot_time == "time_line"){

  # Variable time-line
  line_plot <- clim_data %>%
    group_by(model,rcp,variable) %>%
    summarize_at(vars(min_y:max_y),
                 funs(min,max,mean,sd)
    ) %>%
    gather("fun","value",min:sd) %>%
    ggplot() +
    geom_line(
      aes(
        x = as.numeric(year),
        y = value
      )
    ) +
    facet_wrap(~fun, scales = "free.y") +
    ggtitle(paste(model,"CMIP",cmip, rcp,variable))

  return(line_plot)

  }

}
