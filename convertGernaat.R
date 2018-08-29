#' Convert Hydro-Potential data from Gernaat et. al 2017
#' 
#' Convert magpie object of hydropotential data from Gernaat et. al 2017. Units all in TWh a-1. Technical potential
#' defined as <0.5 $/kWh and economic potential as <0.1 $/kWh.
#' 
#' @param x magpie object derived from readGernaat()
#' @param subtype Possibilities for subtype are "Tech.Full", "Tech.Remaining", "Tech.Ecological",
#' "Eco.Full", "Eco.Remaining" or "Eco.Ecological"
#' @return magpie object of converted hydropotential data from Gernaat et. al 2017
#' @author Atreya Shankar
#' @source Obtained from Nature Energy volume 2 (2017), pp 821–828, with following source: https://www.nature.com/articles/s41560-017-0006-yy
#' @examples
#' \dontrun{a <- convertGernaat(x, "Eco.Ecological")}

convertGernaat <- function(x, subtype){
  
  sub <- getNames(x)
  
  if(length(subtype) == 1 & all(subtype %in% sub)){
    
    mapping <- read.csv2(paste0(getConfig()$mappingfolder, "/regional/regionMapping_Image3.csv"), stringsAsFactors = FALSE)
    irena <- readSource("IRENA", subtype = "Capacity", convert = TRUE)
    irena <- irena[mapping[which(mapping[,3] != "Extra"),2],2016,2]
    irena <- toolCountryFill(irena, fill = 0)
    y <- toolAggregate(x, mapping, irena)
    y <- y[,,subtype]
    
  } else stop("invalid subtype!")
  return(y)
}