#' Reads DBEM files
#'
#' This function loads the DBEM files from DROBO. You have to be connected to the UBC network
#' and have access to the CORU-DROBO in order to use the function. Note that no user/id will be
#' required but the path wont be found.
#'
#' @param taxon_key is the species to load as taxon key number.
#' @param year expects a sequence of years to load the data from
#' @param model is the ESM models to load; GFDL; IPSL, MPI, CNRM, UKESM. For all models select "All";
#' @param cmip can be 5 for CMIP5 or 6 CMPI6. Note that CMPI5 only has GFDL, IPSL and MPI. CMPI6 has additional CNRM and UKESM
#' @param rcp expects "26" for RCP 2.6-low emission scenario and "85" for RCP 8.5-high emission scenario
#' @param data_type select Catch or Abd data
#' @param root_path Expects the root computer path before DROBO. Note: do not include /
#' @param my_path If you have a completely different path for the data. Note that you still have to respect "model/rcp/taxonkey/" structure
#' @return A tidy table -tiddy- with the DBEM data selected
#' @export
#'
read_dbem <- function(taxon_key,
                      year,
                      cmip,
                      model = "All",
                      rcp,
                      data_type,
                      root_path,
                      my_path = FALSE
){

  # ----------------#
  # Packages needed
  # ----------------#
  library <- c("tibble","dplyr")
  lapply(library, require, character.only = TRUE)

  # ----------------#
  # Set paths
  # ----------------#

  # Set CMIP 5 or CMIP 6 paths on drobo
  if(cmip == 5){
    cmip_path <- "/DATA/DATA/DBEM/"
  }

  if(cmip == 6){
    cmip_path <- "/DATA/DATA/DBEM/"
  }

  # Set complete path
  if(my_path == FALSE){

    # Double check path provided is correct
    dbem_path <- paste(here(cmip_path),sep="")
    # dbem_path <- paste(path,sep="")

    if(file.exists(dbem_path) == "FALSE"){
      print(paste("Oh-oh, looks like your root path is wrong. Or maybe data was moved? Path:",dbem_path))
      stop()

    }

  }else{ # Else from my_path = F

    if(model == "All"){

      if(cmip == 5){

      D_Path <- c(paste(here(cmip_path,"/GFDL",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                        sep=""),
                  paste(here(cmip_path,"/IPSL",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                        sep=""),
                  paste(here(cmip_path,"/MPI",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                        sep="")
      )
      }

      if(cmip == 6){

        D_Path <- c(paste(here(cmip_path,"/GFDL",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                          sep=""),
                    paste(here(cmip_path,"/IPSL",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                          sep=""),
                    paste(here(cmip_path,"/MPI",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                          sep=""),
                    paste(here(cmip_path,"/CNRM",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                          sep=""),
                    paste(here(cmip_path,"/UKESM",rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                          sep="")
        )
      }

    }else{

      D_Path <- paste(here(cmip_path,"/",model,rcp,"/",taxon_key,"/",taxon_key,data_type,year,".txt"),
                      sep="")


    }
  } # end else of my_path


  # ----------------#
  #### Importing data
  # ----------------#

  # Step to make sure data for that species exists
  if(file.exists(D_Path[1])){

    cur <- lapply(D_Path, FUN=data.table::fread, na.strings="NA")
  }else{
    print(paste("Oh-oh, we have no data for taxon key",taxon_key))
    print(D_Path)
    df <- tibble()
    return(df)
  }

  if(length(cur)>0){

    cur <- cur[sapply(cur, function(d) nrow(d) >= 1)]
    colnames <- c("index", "value")

    cur <- lapply(cur, setNames, colnames)
    df <- bind_rows(cur, .id = "column_label")

    if(nrow(df)>0){

      frame_key <- tibble(column_label = seq(1,length(unlist(year)),1),
                          "year"=year) %>%
        mutate(column_label=as.character(column_label))

      df <- left_join(df, frame_key,
                      by="column_label") %>%
        dplyr::select(-column_label)

      df <- df %>% mutate(data_type=data_type,
                          taxon_key = taxon_key,
                          model = model,
                          rcp = rcp
      )
    }
  } else {
    df <- tibble()
  }

  # Function result
  # ----------------#

  return(df)
}
