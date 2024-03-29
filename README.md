# Rcoru
A package with functions related to CORU

# Instructions for new functions

- Include `#' @export` before the function
- Include `#' @param` for each parameter of the function
- Run devtools::document()
- Be happy, like a hippo


*See* [Fong Chun Chan's Blog](https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html) and [Rpackages](https://r-pkgs.org/man.html) for a great tutorial on how to start your own function

Each new function should have at least the following documentation:

- `#' @author`
- `#' @export` 
- `#' @param` 
- `#' @return`
- `#' examples`

# Instructions for loading the package

- devtools::install_github("jepa/Rcoru"), run this function to install the package
  -- set force = TRUE for updates and restart R
  
# Instructions for using the functions

- You will need to be connected to the UBC network for any function to work
- Paths are like the ocean, in constant change! Unfortunately, the functions are not that dynamic!

# Working functions

- `read_clim` function is working as of May 2020 ((Needs double checking
- `read_dbem` function is working as of May 2020 (Needs double checking)
- `read_dbem_ens` function is working as of May 2020 (Needs double checking)
- `clim_plot` function is working as of May 2020 (Needs double checking)


# Needs the following packages

- "tibble","dplyr","data.table","ggplot2","here"

# Contributors

- Juliano Palacios Abrantes
