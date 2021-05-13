#' Transform NAs for th visual basic extrapolation
#'
#' This function transforms any NA value to -9999. It assumes the data is in the
#' DROBO under the folder Environmental data/CMIPx_DATA. You need to have access to the DROBO
#' in order to use this function. VB step 1/6
#'
#' @param yr A set of years to run the function
#' @param model The Earth System model that will be running. Part of the path. Needs to match the file name
#' @param variable Expects the name of the variable to be converted. Part of the path. Needs to match the filename
#' @param ssp The SSP to run. Part of the path. Needs to match the file name
#' @param cimip The number of the modeling project. Currently set to 6
#' @param overwrite The result of the function will overwrite the original file. Set to F if you do not want this option. If F, a save path needs to be provided. Normally set to T.
#' @param save_path Expects a path to save the data. Needs overwrite = T. Do not include / at the end of the path
#' @return It does not return anything. This function will overwrite the original file unless the parameter overwrite = F
#'
#' @export
vb_convert_na <- function(yr,model,ssp,variable, cimip = 6, overwrite = T, save_path = NA){

  # loop it for both ssp
  for(s in 1:length(ssp)){

    # Read data (NOTE specific path)
    raw_path <- paste(here("/DATA/Environmental data/CMIP",cimip,"_DATA/",model,"/Natural720_Annualaverage_txt/",variable,"_natural720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt"),sep="")


    # Convert NaN to -9999
    raw_data <- read.table(raw_path, quote="\"", comment.char="") %>%
      mutate(
        V1 = ifelse(V1=="NaN",-9999,V1)
      )

    colnames(raw_data) <- NULL

    if(overwrite == T){
      # Save data in the same place
      write.csv(x = raw_data,
                raw_path,
                row.names = F)
    }else{

      if(is.na(save_path)){
        print("You need to provide a save_path to store the new data. No need to inlcude /")
      }

      # Sets the new path
      new_path <- raw_path <- paste(here(save_path,"/",variable,"_natural720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt"),sep="")

      # Save data in a different place
      write.csv(x = raw_data,
                new_path,
                row.names = F)
    }
  }
}
