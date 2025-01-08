#' Check code: none (is used by other functions)
#' Convert coordinates from SIRENO format to decimal
#' Sireno return coodinates in this format: -ddmmss, where:
#' - - indicates W
#' - dd are degrees
#' - mm are minutes in cegesimal
#' - ss are seconds in decimal.
#' For example: -034950 are 3º 49.50' W
#' @param x character or vector character with coordinates in SIRENO format.
#' @return character or vector character with coordinates in decimal format.
coordinates_SIRENO_to_decimal <- function (x) {
  
  # get seconds (decimal)
  s = as.numeric(substr(x, nchar(x)-1, nchar(x)))
  
  # get minutes (cegesimal)
  m = as.numeric(substr(x, nchar(x)-3, nchar(x)-2))
  
  # get degrees
  d = as.numeric(substr(x, nchar(x)-5, nchar(x)-4))
  
  # get East/West or North/south
  is_negative <- ifelse(substr(x, 0, 1) == "-", TRUE, FALSE)
  
  # create minutes in decimal (with seconds)
  ms <- as.numeric(paste0(m, ".", s))
  ms <- ms / 60
  
  # coordinate in decimal:
  coor <- d+ms
  
  ifelse(is_negative == TRUE, coor <- (-coor), coor)

  return(coor)
  
}


#' Check code: 2062
#' Add ICES statistical rectangle to hauls.
#' The IPD file contains a empty variable called "cuadricula" which is filled
#' with this function.
#' The statistical rectangle coordinates are obtained from ICES web page by .shp
#' file.
#' @param hauls_df Hauls dataframe obtained by importOabIpdFiles() function
#' @return the same hauls_df with the variable "cuadricula" filled.
add_ices_rectangle <- function(hauls_df) {
  # check if sf package is loaded
  if (!requireNamespace("sf", quietly = TRUE)) {
    library(sf)
  }
  
  hauls_df_mod <- hauls_df
  
  # convert longitude and latitude coordinates from SIRENO format to decimal format
  hauls_df_mod$longitud_virado_decimal <- coordinates_SIRENO_to_decimal(hauls_df_mod$longitud_virado)
  hauls_df_mod$latitud_virado_decimal <- coordinates_SIRENO_to_decimal(hauls_df_mod$latitud_virado)
  
  # we use this projection:
  crs <- "epsg:4326"
  
  # Convert the points dataframe to a spatial sf object
  hauls_points <- hauls_df_mod[, c("acronimo", "lance", "longitud_virado_decimal", "latitud_virado_decimal")]
  hauls_points <- sf::st_as_sf(hauls_points, coords = c("longitud_virado_decimal", "latitud_virado_decimal"), crs = crs)
  
  # Import the shp file
  rec_sta_pol_df_sf <- sf::st_read("data-raw/statistical_rectangle/ICES_StatRec_mapto_ICES_Areas/StatRec_map_Areas_Full_20170124.shp")
  
  
  hauls_rectangle <- sf::st_join(hauls_points, rec_sta_pol_df_sf )
  hauls_rectangle <- hauls_rectangle[, c("acronimo", "lance", "ICESNAME")]
  
  # fill "cuadricula" variable of hauls_df with statistical rectangle
  hauls_df[["cuadricula"]] <- hauls_rectangle[["ICESNAME"]]
  
  return(hauls_df)
}


