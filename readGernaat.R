#' Read Hydro-Potential data from Gernaat et. al 2017
#' 
#' Read csv file copied from Supplementary table 1 of Gernaat et. al 2017. Units all in TWh/a, technical potential
#' defined as <0.5 $/kWh and economic potential as <0.1 $/kWh.
#' 
#' @return magpie object of aggregated hydropotential data from Gernaat et. al 2017
#' @author Atreya Shankar
#' @source Obtained from Nature Energy volume 2 (2017), pp 821–828, with following source: https://www.nature.com/articles/s41560-017-0006-y
#' @examples
#' \dontrun{a <- readGernaat()}

readGernaat <- function(){
  # row names for a have been modified to match the IMAGE3 official mapping
  a <- read.csv("input.csv", stringsAsFactors = FALSE)
  x <- as.magpie(a, spatial = 1)
  return(x)
}