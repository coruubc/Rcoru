#' Reads DBEM files from the ensemble member modeling with the GFDL
#'
#' This function loads the DBEM files from the GFDL 8.5 ensemble member from the CORU-DROBO.
#' You have to be connected to the UBC network and have access to the CORU-DROBO in order to use the function.
#' Note that user/id will not be required but the path wont be found.
#' #'
#' @param taxon_key is the species to load as taxon key number
#' @param year expects a year of sequence of years to load the data from 1951 to 2099
#' @param model is the ESM models to load; GFDL; IPSL, MPI. For all models select "All". Note that for now model is set to GFDL since is the only one we have
#' @param rcp expects "26" for RCP 2.6-low emission scenario and "85" for RCP 8.5-high emission scenario. For now set to 85 as we do not have other
#' @param ensemble Expects the ensemble number. Note, it wont load all ensembles at a time. Limited to 10 ensembles available
#' @param data_type expects Catch or Abd data
#' @param path Expects the computer head path before DROBO. Do not include /
#' @return A tibble with the data
#' @export
#'
read_dbem_ens <- function(taxon_key,
                          year,
                          model = "GFDL",
                          rcp = 85,
                          ensemble,
                          data_type,
                          root_path
){

  # ----------------#
  # Functions needed
  # ----------------#
  library <- c("data.table","dplyr","here")
  lapply(library, require, character.only = TRUE)

  # ----------------#
  #  Error messages for global variables
  # ----------------#

  if(model != "GFDL" |
     rcp != 85 |
     ensemble > 112 |
     ensemble < 102){
    print("Oh-oh, some variables are wrong. We only have ten ensemble members (102 to 111) results for the GFDL under the RCP 8.5 scenario from the years 1951 to 2099.")
    stop()
    return(df)
  }

  # ----------------#
  # Set paths
  # ----------------##
    D_Path <- paste(root_path,"mpa0F1ENS",ensemble,"/",taxon_key,"/",taxon_key,data_type,year,".txt",
                    sep="")

# Warning message if path does not exist
  if(file.exists(D_Path) == "FALSE"){
    print(paste("Oh-oh, looks like your path is wrong. Path:",D_Path))
    stop()
  }
  #----------------------------#

  # ----------------#
  #### Importing data
  # ----------------#

  # Step to make sure data for that species in that year exists
  if(file.exists(D_Path[1])){
    cur <- lapply(D_Path, FUN=data.table::fread, na.strings="NA")
  }else{
    print(paste("Oh-oh, we have no data for for taxon key",taxon_key))
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
        select(-column_label)

      df <- df %>% mutate(data_type=data_type,
                          taxon_key = taxon_key,
                          ensemble = ensemble
      )
    }
  } else {
    df <- tibble()
  }

  # Function result
  # ----------------#

  return(df)
}
