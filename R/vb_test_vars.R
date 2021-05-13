#' Check VB variables
#'
#' This part double checks that the VB runs were successful.
#' Specifically, it makes sure you VB all variables, ssps and years.
#' It will print out missing data.

#' @author Juliano Palacios Abrantes | j.palacios@oceans.ubc
#' @param cimip The number of the modeling project. Currently set to 6
#' @param model The Earth System model that will be running. Part of the path. Needs to match the file name
#' @param ssp The SSP to run. Part of the path. Needs to match the file name
#' @param variable Expects the name of the variable to be converted. Part of the path. Needs to match the filename
#' @param years A set of years to run the function
#' @param overwrite The result of the function will overwrite the original file. Set to F if you do not want this option. If F, a save path needs to be provided. Normally set to T.
#' @param save_path Expects a path to save the data. Needs overwrite = F. Do not include / at the end of the path
#' @return It does not return anything. This function will overwrite the original file unless the parameter overwrite = F
#'
#' @export
#'

vb_test_vars <- function(model){

#  read in original variables

original_var <- list.files(paste0("Z:/DATA/Environmental data/CMIP6_DATA/",model,"/Natural720_Annualaverage_txt/"))

or_var <- unique(str_extract(original_var,"[^_]+"))


# Read in processed variables
processed_var <- list.files(paste0("Z:/DATA/Environmental data/CMIP6_DATA/",model,"/Processed720_Annualaverage_txt/"))

pro_var <- unique(str_extract(processed_var,"[^_]+"))


#First check you got all variables
if(length(or_var) != length(pro_var)){

  missing_var <- tibble(or_var) %>%
    filter(!or_var %in% pro_var) %>%
    pull()


  print(paste("You forgot to VB variable(s)",missing_var, "from the",model,"model"))
}else{
  print(paste("All variables have been processed for model",model))
}


# Second check you got all ssps and years
matching_vars <- tibble(or_var) %>%
  filter(or_var %in% pro_var) %>%
  pull()

n_years <- tibble(processed_var) %>%
  mutate(filter_me = str_extract(processed_var,"[^_]+"),
         ssp = str_match(processed_var, "ssp\\s*(.*?)\\s*_")[,2]) %>%
  filter(filter_me %in% matching_vars) %>%
  group_by(variable=filter_me,ssp) %>%
  summarise(n_yr = n()) %>%
  ungroup()

missing_ssp <- n_years %>%
  group_by(variable) %>%
  summarise(n_ssp= n(),
            var = paste(ssp, collapse =";")
  ) %>%
  filter(n_ssp <2)

if(nrow(missing_ssp) > 0){


  print(paste("Varible", missing_ssp$variable, "only has output for ssp",missing_ssp$var))

}else{
  print("All variables have both SSPs")
}

max <- max(n_years$n_yr, na.rm = T)

missing_years <- n_years %>%
  filter(n_yr < max) %>%
  mutate(yrs_missing = max-n_yr)

if(nrow(missing_years) > 0){

  print(paste("Variable", missing_years$variable, "for ssp",missing_years$ssp, "is missing",missing_years$yrs_missing,"years"))
}else{
  print("All variables have all years")
}

}
