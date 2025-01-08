#' convert all the numeric variables to character variables and change its decimal
#' separator to comma
#' @param df dataframe to convert
#' @return the df with the numeric fields converted
decimalWithComma <- function(df){
  numeric_variables <- colnames(dplyr::select_if(df, is.numeric))
  
  numeric_variables <- which(colnames(df) %in% numeric_variables)
  # TODO: chage with sapmuebase:::replace_coma_with_dot()
  df[numeric_variables] <- lapply(df[numeric_variables], function(x){
    gsub("\\.", ",", x)
  })
  
  return(df)
}


#' export OAB dataframes to the fixed weight format, ready to import in SIRENO
#' @param df Dataframe to export
#' @param typeFile type of file to import. One of this options:"TRIPS", "HAULS",
#'  "CATCHES", "LENGTHS", "DISCARD_SAMPLE", "SAMPLES_GROUPED", "GEARS",
#'  "ACCIDENTALS", "MANDATORY_LANDINGS", "LITTER"
exportToTxtSireno <- function (df, typeFile, path = getwd()){
  
  if(!exists("NAME_FILE_VS_TYPE_FILE")){
    NAME_FILE_VS_TYPE_FILE <- createNameVsTypeDataset()
  }
  
  # Get the width of the fields
  widths_export <- as.character(getOabTableStruct(typeFile)[["length_field_export"]])
  
  
  # Fix decimal separation
  df <- decimalWithComma(df)
  
  # Get the name of the table to use it as part of the name of the file
  name_file <- 
    NAME_FILE_VS_TYPE_FILE[[
      which(NAME_FILE_VS_TYPE_FILE$TYPE_FILES_VARIABLES==typeFile) ,"NAME_FILES"]]
  
  
  name_file <- paste0(path, "/", name_file, "_import_", YEAR, "_",
                      MONTH_AS_CHARACTER, "_ICES.txt" )
  
  #export
  
  # in case the fileType = "HAULS", the justification of the field must be
  # to the right to import properly in SIRENO. Using a vector in the argument
  # justifiy in the write.fwd doens't work (and I don't know why), so I use
  # the sprintf to add a 0 to the left of the latitude variables:
  df[,grep("^latitud.+", colnames(df))] <- lapply(df[,grep("^latitud.+", colnames(df))], function(x){
        if (typeFile == "HAULS"){
          gsub(" ", "0", sprintf("%07s", x))
        } else {
          df[,grep("^latitud.+", colnames(df))] }
      }
      )

  #to import in sireno, is mandatory to have the encoding to ANSI
  # my_locale <- Sys.getlocale()
  # Sys.setlocale("LC_ALL", "Spanish_Spain.ISO8859-1")
  write.fwf(df, file=name_file,  width = widths_export, sep = "",
            colnames = F, justify = "left")
  # Sys.setlocale("LC_ALL", my_locale)
}


#' Export all the data frames of the oab_ipd list (which is returned by 
#' importOabIpdFiles() function) to txt format required in the SIRENO import
#' process.
#' @param oab_ipd_list oab_ipd list, containing all the tables of oab required
#' to the SIRENO import process.
#' @param path
exportOabListToSireno <- function(oab_ipd_list, path = getwd()){
  
  # Create errors subfolder in case it doesn't exists:
  if (!file.exists(file.path(PATH_EXPORT_FILES))){
    dir.create(file.path(PATH_EXPORT_FILES))
  }
  
  # Create NAME_FILE_VS_TYPE_FILE data set if it doesn't exists
  if(!exists("NAME_FILE_VS_TYPE_FILE")){
    
    NAME_FILE_VS_TYPE_FILE <- createNameVsTypeDataset()
    
  }
  
  
  lapply(seq(1:length(oab_ipd_list)), function(x, y){
    
    name_df <- names(y[x])
    
    if (!isFALSE(y[[x]])){
    
      exportToTxtSireno(y[[x]], name_df, path)
      
    } else {
      warning(paste0("Dataframe ", name_df, " is empty."))
    }
    
  }, oab_ipd_list)
  
  
}




