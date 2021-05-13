#' Checking if the VB protocol worked properly
#'
#' This function compares the data before and after the VB run
#' to make sure it worked. It only provides a visual comparison
#' to see if the extrapolation was effective. All generated plots,
#' need to be different from each other, otherwise something went
#'  wrong with the VB simulation. Expects the processed data to be
#'  saved in the same root that raw data within a folder called
#'  Processed720_Annualaverage_txt. File names must be the same.
#'
#' @author Juliano Palacios Abrantes | j.palacios@oceans.ubc
#' @param model The Earth System model that will be running. Part of the path. Needs to match the file name
#' @param ssp The SSP to run. Part of the path. Needs to match the file name
#' @param variable Expects the name of the variable to be converted. Part of the path. Needs to match the file name
#' @param years A set of years to run the function
#' @return Returns three plots to visualize differences; a histogram, a historic line and a map of 10 years average
#'
#' @export
vb_test_run <- function(model, ssp, variable,years){


  ## Set data paths ##

  # Raw data path
  raw_path <- paste(here(),"/DATA/Environmental data/CMIP6_DATA/",model,"/Natural720_Annualaverage_txt/",variable,"_natural720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt",sep="")

  # Processed data path
  processed_path <- paste(here("/DATA/Environmental data/CMIP6_DATA/",model,"/Processed720_Annualaverage_txt/",variable,"_natural720_Omon_",model,"_",ssp,"_annualaverage_year_",yr,".txt",sep=""))

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
