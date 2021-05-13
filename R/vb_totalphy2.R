#' Estimates totalphi2 for DBEM
#'#'
#' @author Juliano Palacios Abrantes | j.palacios@oceans.ubc
#' @param model The Earth System model that will be running. Part of the path. Needs to match the file name
#' @param ssp The SSP to run. Part of the path. Needs to match the file name
#' @param years A set of years to run the function
#' @param save_path Expects a path to save the data. Needs overwrite = F. Do not include / at the end of the path
#' @return It does not return anything. This function will overwrite the original file unless the parameter overwrite = F
#'
#' @export

vb_totalphy2 <- function(yr,model,ssp){

  # Raw data path
  nppdiat <- read.table(paste("Z:/DATA/Environmental data/CMIP6_DATA/",model,"/Processed720_Annualaverage_txt/NPPDIAT_processed720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt",sep="")) %>%
    rename(diat = V1)


  # Processed data path
  npppico <- read.table(paste("Z:/DATA/Environmental data/CMIP6_DATA/",model,"/Processed720_Annualaverage_txt/NPPPICO_processed720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt",sep="")) %>%
    rename(pico = V1)


  npp <- nppdiat %>%
    bind_cols(npppico) %>%
    mutate(npp = diat+0.1*pico) %>%
    select(npp)

  colnames(npp) <- NULL

  if(ssp == "ssp126"){
    folder = "C6CNRM26/"
  }else{
    folder = "C6CNRM85/"
  }

  save_name <- paste0(output_path,folder,"totalphy2_",yr,".txt")

  # Save data in the same place
  write.csv(x = npp,
            save_name,
            row.names = F)
}
