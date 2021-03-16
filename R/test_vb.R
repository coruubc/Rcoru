#' Tests the VB run
#'
#'This function tests the VB run by comparing the raw data to the VB data
#'It produces three plots with the option of rendering them; might take longer;
#'or save it.
#' #'
#' @param variable select the environmental variable to test
#' @param year expects a year of sequence of years to load the data from 1951 to 2099
#' @param model is the ESM models to load; GFDL, IPSL, MPI, CNRM, UKESM.
#' @param ssp expects ssp 126 or 585 rcp 126 585
#' @export
#'
test_vb <- function(year,variable, model,ssp){


  ## Set data paths ##

  # Raw data path
raw_path <- paste("Z:/DATA/Environmental data/CMIP6_DATA/",model,"/Natural720_Annualaverage_txt/",variable,"_natural720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt",sep="")

  #raw_path <- paste("Z:/DATA/Environmental data/CMIP6_DATA/Test_jepa/RawData/BPH_natural720_Omon_MPI-ESM1-2-HR_ssp585_annualaverage_year_",yr,".txt",sep="")


  # Processed data path
  processed_path <- paste("Z:/DATA/Environmental data/CMIP6_DATA/",model,"/Processed720_Annualaverage_txt/",variable,"_natural720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt",sep="")

  #   processed_path <- paste("Z:/DATA/Environmental data/CMIP6_DATA/Test_jepa/ProcessedData/BPH_natural720_Omon_MPI-ESM1-2-HR_ssp585_annualaverage_year_",yr,".txt",sep="")

  # Load raw data
  raw_data <- bind_cols(
    lapply(raw_path, read.table,  quote="\"", comment.char="")
  ) %>%
    mutate(ver = "raw") %>%
    rowid_to_column("index")

  colnames(raw_data) <- c("index",yr,"ver")

  # Load processed data


  # Load raw data
  processed_data <- bind_cols(
    lapply(processed_path, read.table,  quote="\"", comment.char="")
  ) %>%
    mutate(ver = "processed") %>%
    rowid_to_column("index")

  colnames(processed_data) <- c("index",yr,"ver")

  all_data <- raw_data %>%
    bind_rows(processed_data) %>%
    left_join(lon_lat_grid) %>%
    gather("year","value",2:12) %>%
    filter(value !=- 9999,
           year %in% c(seq(2000,2010,1))
    ) %>%
    group_by(index,ver,lat,lon) %>%
    summarise(
      mean = mean(value, na.rm= T)
    )

  #head(all_data)

  hist_data <- raw_data %>%
    bind_rows(processed_data) %>%
    left_join(lon_lat_grid) %>%
    gather("year","value",2:12) %>%
    filter(value !=- 9999) %>%
    group_by(year,ver) %>%
    summarise(
      mean = mean(value, na.rm= T),
      sum = sum(value,na.rm = T)
    ) %>%
    gather("var","val",mean:sum)

  head(hist_data)


  plot_name <-paste("Z:/DATA/Environmental data/CMIP6_DATA/Test_jepa/Plots/",model,"_",variable,"_2020_to_2030",sep="")

  # Gobal map
  all_data %>%
    ggplot() +
    geom_tile(
      aes(
        x = lon,
        y = lat,
        fill = mean,
        color = mean
      )
    ) +
    ggtitle(plot_name) +
    facet_wrap(~ver) +
    ggsave(
      plot =last_plot(),
      filename = paste(plot_name,"_glob.png",sep=""),
      width = 17,
      height = 10
    )

  # Zoom in Map (Mexico)
  #  all_data %>%
  #   filter(
  #    lat >= 16,
  #   lat <= 35,
  #  lon >= -116,
  # lon <= -80
  # ) %>%
  #  ggplot() +
  #  geom_tile(
  #   aes(
  #    x = lon,
  #   y = lat,
  #  fill = mean,
  # color = mean
  #)
  #  ) +
  # ggtitle(plot_name) +
  #facet_wrap(~ver) +
  #  ggsave(
  #   plot =last_plot(),
  #  filename = paste("./Plots/",plot_name,"_zoom.png",sep=""),
  # width = 17,
  #height = 10
  #)


  #  Dfferenc map
  all_data %>%
    spread(ver,mean) %>%
    mutate(dif= processed-raw) %>%
    ggplot() +
    geom_tile(
      aes(
        x = lon,
        y = lat,
        fill = dif,
        color =  dif
      )
    ) +
    ggtitle(plot_name) +
    ggsave(
      plot =last_plot(),
      filename = paste(plot_name,"_diff.png",sep=""),
      width = 17,
      height = 10
    )

  #  Hist plot
  hist_data %>%
    ggplot() +
    geom_line(
      aes(
        x = as.numeric(year),
        y = val,
        color =  ver
      )
    ) +
    facet_wrap(~var, scales =  "free") +
    ggtitle(plot_name) +
    ggsave(
      plot =last_plot(),
      filename = paste(plot_name,"_hist.png",sep=""),
      width = 8,
      height = 6
    )

}


yrs <- seq(2000,2010,1)
gc()
suppressMessages(
  test_fun(yrs,variable = "SST", model = "UKESM1", ssp = "ssp585")
)


```




## 2.2. Double check all VB runs

This part double checks that the VB runs were successful. Specifically, it makes sure you VB all variables, ssps and years. It will print out missing data.

### VB control function

**NOte: Double check path is correct**

  ```{r check_vb_fun, echo=FALSE}


matching_variables <- function(model){

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


devtools::document()
