#' Check code: none (is a multipurpose function)
#' Generate the dataset NAME_FILE_VS_TYPE_FILE which relate the Type File 
#' variable with the name of the tables in IPD database
#' @return dataset with the filenames of the IPD database related with its
#' Type File variables
createNameVsTypeDataset <- function(){
  
  TYPE_FILES_VARIABLES <- c("TRIPS", "HAULS", "CATCHES", "RETAIN_LENGTHS",
                            "DISCARD_LENGTHS", "SAMPLES_GROUPED", "GEARS",
                            "ACCIDENTALS", "MANDATORY_LANDINGS", "LITTER")
  
  NAME_FILES <- c("mareas","lances","capturas","tallas_muestra_retenida",
                  "tallas_muestra_descartado","muestreos_agrupados","artes",
                  "accidentales","desembarcos_obligatorios","basura")

  NAME_FILE_VS_TYPE_FILE <- data.frame(NAME_FILES, TYPE_FILES_VARIABLES)
  
}

#' Check code: none (is a multipurpose function)
#' Return the TYPE_FILES_VARIABLES variable
getValidTypeFileDataset <- function(){
  
  if(!exists("NAME_FILE_VS_TYPE_FILE")){
    
    NAME_FILE_VS_TYPE_FILE <- createNameVsTypeDataset()
    
  }
  
  return(as.character(NAME_FILE_VS_TYPE_FILE$TYPE_FILES_VARIABLES))
  
} 

#' Check code: none (is part of other function)
#' Check if the variable typeFile is one of the OAB IPD types of file.
#'  @param typeFile type of file to import. One of this options:"TRIPS", "HAULS",
#'  "CATCHES", "LENGTHS", "DISCARD_SAMPLE", "SAMPLES_GROUPED", "GEARS",
#'  "ACCIDENTALS", "MANDATORY_LANDINGS", "LITTER"
#'  @return TRUE if typeFile is correct. Else, an error is throw.
isValidTypeFile <- function (typeFile){
  
  valid_typeFile <- getValidTypeFileDataset()
  
  if (typeFile %in% valid_typeFile){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

#' Check code: none (is part of other function)
#' get struct of AOB IPD files. The structure is stored in
#' format_variables_oab.csv file which is located in the working folder.
#' @param typeFile type of file to import. One of this options:"TRIPS", "HAULS",
#'  "CATCHES", "LENGTHS", "DISCARD_SAMPLE", "SAMPLES_GROUPED", "GEARS",
#'  "ACCIDENTALS", "MANDATORY_LANDINGS", "LITTER"
#'  @return dataframe with the struct of the typeFile
getOabTableStruct <- function(typeFile){
  
  if(!isValidTypeFile(typeFile)) {
    validTypeFile <- getValidTypeFileDataset()
    validTypeFile <- paste(validTypeFile, collapse = ", ")
    stop(paste("The typeFile ", typeFile, " is not valid. The valid options are:", validTypeFile))
  }
  
  # import format file dataset
  path_dataset <- paste(getwd(), 'format_variables_oab.csv', sep="/") 
  formato_variables_oab_ipd <- importCsvSAPMUE(path_dataset)
  
  
  out <- formato_variables_oab_ipd[, c("name_variable",
                                        "class_variable_final",
                                        "length_field_import",
                                        "length_field_export", typeFile)]
  out <- out[!is.na(out[[typeFile]]),]
  
  out <- out[order(out[[typeFile]]),]
  
  return(out)
  
}


#' Check code: none (is part of other function)
#' Generic function to import IPD files.
#' The format of every file are in formato_variables_oab_ipd.csv file
#' @param filename name of the file.
#' @param typeFile type of file to import. One of this options:"TRIPS", "HAULS",
#'  "CATCHES", "LENGTHS", "DISCARD_SAMPLE", "SAMPLES_GROUPED", "GEARS",
#'  "ACCIDENTALS", "MANDATORY_LANDINGS", "LITTER"
#'  @path path of the files. Working folder by default.
importOabIpd <- function(filename, typeFile, path=getwd()){

  full_path <- paste(path, filename, sep = "/")
  
  table_struct <- getOabTableStruct(typeFile)
  
  width_vector <- table_struct[["length_field_import"]]
  classes_vector <- as.character(table_struct[["class_variable_final"]])
  colnames_vector <- as.character(table_struct[["name_variable"]])
  
  records <- read.fwf(file = full_path,
                      widths = width_vector,
                      strip.white = TRUE,
                      dec = ",",
                      colClasses = classes_vector,
                      fileEncoding = "UTF-8"
                      )
  
  colnames(records) <- colnames_vector
  
  return(records)
  
}

#' Check code: none
#' Import al the OAB IPD files in a row. The name of the files, with extension,
#' must be stored in listOfFileNames as a character vector.
#' @param listOfFileNames vector with the names of the files to import.
#' @param path Path where the files are storaged. Working folder by default.
#' @return list of dataframes of the files imported. Every dataframe is named as
#' the name of the file.
importOabIpdFiles <- function(listOfFileNames, path=getwd()){
  
  g <- names(listOfFileNames)
  
  t <- lapply(g, function(x, y){
    
    fileType <- toupper(strsplit(x, "_file"))
    
    file <- paste(path, files[[x]], sep ="/")
    
    print(file)
    
    if(!file.exists(file)) {
      warning(paste0("File ", x, " does not exists"))
      return(FALSE)
    }
    
    info = file.info(file)
    
    if (info[["size"]] != 0){
      a <- importOabIpd(files[[x]], fileType, path = path)
      return(a)
    }else {
      warning(paste0("File ", x, " empty"))
      return(FALSE)
    }
    
    
  }, files)
  
  names_df <- toupper(g)
  names_df <- strsplit(names_df, "_FILE")
  names_df <- unlist(names_df)
  
  names(t) <- names_df
  
  return(t)
  
}

#' Check code: 2032
#' From the TRIPS table, get the acronyms of every hauls, and check if the
#' acronyms of the rest of the tables match with it.
#' @param listOfDataFrames list of dataframes imported from OAB IPD files
#' (with importOabIpdFiles function)
#' @return list of dataframes with errors of every table of AOB IPD files. When
#' a table doesn't contain errors, the object stored in the list is FALSE
#' instead of a dataframe.
acronyms_in_all_tables <- function(listOfDataFrames){
  
  acro_hauls <- unique(listOfDataFrames[["TRIPS"]][["acronimo"]])
  
  err <- lapply(oab_ipd, function(x){
    
    if (is.data.frame(x)){
      acronimos <- unique(x["acronimo"])
      
      errors <- acronimos[which(!(as.character(acronimos$acronimo) %in% acro_hauls)),,drop=FALSE]
      if (length(errors)>0){
        text_type_of_error <- paste0("ERROR: there aren't this acronym in the trips table.")
        errors <- addTypeOfError(errors, text_type_of_error)
        
        return(errors)
      }

    }
    
  })
  
}

#' Check code: none (is part of other function)
# function to add variable with type of error to a dataframe
addTypeOfError <- function(df, ...){
  
  arguments <- list(...)
  
  type <- paste(arguments, collapse = '', sep = " ")
  
  if(nrow(df)!=0){
    # df[["TIPO_ERROR"]] <- type
    df <- df %>% mutate(TIPO_ERROR = type)
  }
  return(df)
}

#' Check code: 2028, 2029, 2030, 2031
#' Check variables with masters
#' Check if the value of variables are consistents to the value in its SIRENO master.
#' It's only available for variables with a data source (master): ESTRATO_RIM, COD_PUERTO,
#' COD_ORIGEN, COD_ARTE
#' @param variable: one of this values: ESTRATO_RIM, COD_PUERTO, COD_ORIGEN or
#' COD_ARTE
#' @param specification: RIM or OAB
#' @return Return a dataframe with samples containing erroneus variables
check_variable_with_master <- function (df, variable, specification){
  
  if(variable != "ESTRATO_RIM" &&
     variable != "COD_PUERTO" &&
     variable != "COD_ORIGEN" &&
     variable != "COD_ARTE"){
    stop(paste("This function is not available for ", variable))
  }

  
  # If the variable begin with "COD_", the name of the data source
  # is the name of the variable without "COD_"
  variable_formatted <- variable
  if (grepl("^COD_", variable)){
    variable_formatted <- strsplit(variable, "COD_")
    variable_formatted <- variable_formatted[[1]][2]
  }
  name_dataset <- tolower(variable_formatted)

  #search the errors in variable
  
  dataset <- get(name_dataset)
  
  dataset["VALID"]<-TRUE
  
  df <- merge(x=df, y=dataset, by = variable , all.x = TRUE)
  
  errors <- df[is.na(df$VALID),]
  
  #prepare to return
  fields_to_filter <- c("acronimo", variable, variable_formatted)
  
  errors <- errors %>%
    select(one_of(fields_to_filter))%>%
    unique()
  
  
  text_type_of_error <- paste0("ERROR: ", variable, " no concuerda con los maestros de SIRENO")
  errors <- addTypeOfError(errors, text_type_of_error)
  
  #return
  return(errors)
}

#' Check code: none (is part of another function)
#' Check if a variable exists in a dataframe -----------------------------------
#' TODO: put in sapmuebase
#' @return TRUE if the variable exists. FALSE if doesn't.
#' @param variable: variable to check.
#' @param df: dataframe to check
#' @export
variable_exists_in_df <- function (variable, df){
  
  # get all the variables of df with the variable name = variable
  var_in_df <- colnames(df)[colnames(df) %in% variable]
  
  if (length(var_in_df) == 0) {
    return(FALSE)
  } else return (TRUE)
  
}



#' Check code: 2033
#' Check the species in grouped_species from IPD -------------------------------
#' Only the species contained in grouped_species dataset can be in the
#' samples_grouped table of IPD
#' @param df: dataframe to check
#' @return Dataframe with the erroneus data. A new a variable with the type
#' of error is added.
#' @export
species_in_grouped_species <- function(df) {

  if (is.null(nrow(df))) {
    warning("The dataframe is empty.")
    return()
  }

  gsipd <- df$SAMPLES_GROUPED

  grouped_species$VALID <- TRUE

  errors <- merge(gsipd, grouped_species, by.x = c("especie_tax", "COD_ESP"),
                  by.y = c("GROUPED_SPECIES", "SPECIES"), all.x = TRUE)

  errors <- errors[, c("acronimo", "lance", "especie_tax", "categoria",
                       "COD_ESP", "VALID")]

  errors <- errors[which(is.na(errors$VALID)),]

  errors <- unique(errors)

  if (length(errors) > 0) {
    errors <- addTypeOfError(errors, "This species can't be in the table samples_grouped")
    return(errors)
  }

}

#' Check code: none (is part of another function)
#' Check the month of a variable of dataframe ----------------------------------
#' @return Erroneus data dataframe with a new variable with the type of error.
#' @param df: dataframe to check
#' @param variable: Name of the character to check. Must have a date format like
#' dd/mm/yy whith a character class
#' @param month_to_check: month to check
#' @export
check_month <- function(df, variable, month_to_check){
  
  sapmuebase:::check_by_month_argument(month_to_check)
  
  if (variable_exists_in_df(variable, df)){
    
    dates <- strptime(df[[variable]], format = "%d/%m/%Y")
    
    months <-as.numeric(strftime(dates, "%m"))
    
    errors <- df[-which(months%in%month_to_check),]
    
    if(length(errors)>0){
      errors <- addTypeOfError(errors,
                               paste0("The month of the samples doesn't match with the month to upload: ", month_to_check))
      return(errors)
    }
    
  }
  
}

#' Check code: 2035
#' Check the month of all the oab_ipd dataframes -------------------------------
#' This check if the data sended by subcontracted company correspond with the
#' month they must to send.
#' @return Dataframe with erroneus registers and a new a variable with the type
#' of error.
#' @param oab_ipe: list of dataframe from the importOAB functions of the
#' sapmuebase package
#' @param month_to_check: the month to check
#' @export
check_all_month <- function(oab_ipd, month_to_check){
  
  # in this first apply, the arguments are a sequence of numbers of the oab_ipd (x),
  # which is a list of dataframes, and the oab_ipd list itself (y). I'm doing it in
  # this way because is important to add the name of the table to the returned dataframe
  
  err <- lapply(seq_along(oab_ipd), function(x, y){
    
    year_fields <- c("fecha", "fecha_inicio", "fecha_final", "fecha_firme", "fecha_virado")
    
    names_used <- names(y[[x]])[(names(y[[x]]) %in% year_fields)]
    
    # in this second laaply, the arguments are year_fields which is a vector with the
    # variable names with dates of all dataframes of oab_ipd (nu), and the dataframe
    # of the oab_ipd to check. The dataframe is y[[x]], the x index of the oab_ipd
    # list.
    errors <- lapply(names_used, function(nu, df){
      err <- check_month(df, nu, month_to_check )
      
      if(length(err)>0 && nrow(err) > 0){
        
        if("lance" %in% names(err)){
          err <- err[,c("acronimo", "lance", nu, "TIPO_ERROR")]
        } else {
          err <- err[,c("acronimo", nu, "TIPO_ERROR")]
        }
        
        # if (length(err) > 0 && nrow(err) > 0){
        err[, "TIPO_ERROR"] <- paste0(err[, "TIPO_ERROR"], " in table ", names(y[x]))
        
        # }
        
        return(err)
        
      }
      
      
    }, y[[x]])
    
    errors <- Reduce(bind_rows, errors)
    
    return(errors)
    
  }, oab_ipd)
  
  err <- Reduce(bind_rows, err)
  
  return(err)
}

#' Check code: none (is used in other function)
#' Check the year of a variable of dataframe -----------------------------------
#' @return Erroneus data dataframe with a new variable with the type of error.
#' @param df: dataframe to check
#' @param variable: Name of the character to check. Must have a date format like
#' dd/mm/yy whith a character class
#' @export
check_year <- function(df, variable, year_to_check){
  
  if (variable_exists_in_df(variable, df)){
    
    dates <- strptime(df[[variable]], format = "%d/%m/%Y")
    
    years <-as.numeric(strftime(dates, "%Y"))
    
    errors <- df[-which(years%in%year_to_check),]
    
    if(nrow(errors)>0){
      errors <- addTypeOfError(errors,
                               paste0("The samples year doesn't match with the 
                                      month to upload: ", year_to_check))
      errors <- errors[, c("acronimo", "lance", variable, "TIPO_ERROR")]
      return(errors)
    }
  }
}

#' Check code: 2036
#' Check the year of all the oab_ipd dataframes -------------------------------
#' This check if the data sended by subcontracted company correspond with the
#' year they must to send.
#' @param oab_ipd: list of dataframe from the importOAB functions of the
#' sapmuebase package
#' @param year_to_check: the year to check
#' @return Dataframe with erroneus registers and a new a variable with the type
#' of error.
#' @export
check_all_year <- function(oab_ipd, year_to_check){
  
  # in this first apply, the arguments are a sequence of numbers of the oab_ipd (x),
  # which is a list of dataframes, and the oab_ipd list itself (y). I'm doing it in
  # this way because is important to add the name of the table to the returned dataframe
  err <- lapply(seq_along(oab_ipd), function(x, y){
    
    year_fields <- c("fecha", "fecha_inicio", "fecha_final", "fecha_firme", "fecha_virado")
    
    names_used <- names(y[[x]])[(names(y[[x]]) %in% year_fields)]
    
    errors <- lapply(names_used, function(nu, df){
      err <- check_year(df, nu, year_to_check )
      
      if (length(err) > 0 && nrow(err) > 0){
        
        if("lance" %in% names(err)){
          err <- err[,c("acronimo", "lance", nu, "TIPO_ERROR")]
        } else {
          err <- err[,c("acronimo", nu, "TIPO_ERROR")]
        }
      
        err[, "TIPO_ERROR"] <- paste0(err[, "TIPO_ERROR"], " in table ", names(y[x]))
        
      }
      
      return(err)
      
    }, y[[x]])
    
    errors <- Reduce(bind_rows, errors)
    
    return(errors)
    
  }, oab_ipd)
  
  err <- Reduce(bind_rows, err)
  
}

#' Check code: none (is part of other function)
#' Get the trips of sireno aob trips file --------------------------------------
#' @return Dataframe with the erroneus data with a new a variable with the type
#' of error.
#' @param oab_trips_file: filename of the oab trips file
#' @param path_file: path of the oab trips file
#' @export
get_trips_sireno <- function(oab_trips_file, path_file){
  trips_sireno <- importOABTrips(oab_trips_file, path_file)
  # trips saved in sireno
  tsis <- unique(trips_sireno$COD_MAREA)
  return(tsis)
}

#' Check code: 2037
#' Check if the trip already exists in sireno--------------------------------------
#' @require Require the sireno oab trips file
#' @return Dataframe with the erroneus data with a new a variable with the type
#' of error.
#' @param trips_df: trips dataframe with the variable 'acronimo' to check
#' @export
trips_already_in_sireno <- function(trips_df){
  
  tis <- get_trips_sireno(OAB_TRIPS_FILE_NAME, PATH_IMPORT_FILES)
  
  trips <- trips_df$acronimo
  
  errors <- trips_df[which(trips %in% tis),"acronimo", drop=FALSE]
  
  if(length(errors)>0){
    errors <- addTypeOfError(errors, "This trip is already saved in SIRENO")
    return(errors)
  }
  
  return(errors)
}

#' Check code: 2041
#' Detect errors in caught weight and sampled caught weight when the sample
#' has been all measured
#' all catches measured' (todo_medido_captura) means that all the catch of a
#' species has been measured. In this cases, the fields 'peso' and 'peso_muestra'
#' must be 0.
#' @param df_catches catches dataframe
#' @return dataframe with errors.
error_weights_catches <- function (catches_df){
  
  # all catches measured 
  acm <- catches_df[which(catches_df$todo_medido_captura=="S"),]
  
  err <- acm[which(acm$peso != 0 | acm$peso_muestra != 0), ]
  
  if (length(err)>0){
    err <- addTypeOfError(err, "ERROR: The sample catch is marked as all measured, but the sample has caught weight and/or sampled weight catched distinct to 0.")
    return(err)
  }
  
}


#' Check code: 2042
#' Detect errors in weight discarded and sampled weight discarded when the
#' sample has been all measured.
#' All discard measured' (todo_medido_descarte) means that all the discard of a
#' species has been measured. In this cases, the fields 'peso_muestra_total' and 
#' peso_sub_muestra' must be 0. An exception is when there is a subsample,
#' in this cases only 'peso_sub_muestra' must be 0.
#' @param df_all_measured df with variable "todo_medido_descarte", which mark
#' the all discard measured samples
#' @param df_discards discards dataframe
#' @return dataframe with errors
error_weights_discards <- function (df_all_measured, discards_df){
  
  # get the discards all measured dataframe and add new column to reflect it:
  dam <- get_discard_all_measured(df_all_measured)
  # get_discard_all_measured return False if there are none discard all measured:
  if (isFALSE(dam)) {
    # only return the dataframe without rows. This is mandatory because the
    # returned dataframe must be reduce and need the dataframe structure.
    return(discards_df[0,])
  }
  dam$IS_AM <- T
  
  # add to the dataframe the column AM and fill the empty AM field with F
  discards_df <- merge(discards_df, dam, all.x = T)
  discards_df[which(is.na(discards_df$IS_AM)), "IS_AM"] <- F
  
  # create column IS_SUBSAMPLED only in the rows which are all measured
  discards_df <- discards_df %>%
    mutate(IS_SUBSAMPLED = ifelse(IS_AM == T & peso_muestra_total!=peso_sub_muestra, T, F))
  
  # if IS_SUBSAMPLED and IS_AM ==> peso_muestra_total != peso_sub_muestra & peso_sub_muestra = 0
  err_1 <- discards_df[which(
    discards_df$IS_SUBSAMPLED == T &
      discards_df$IS_AM == T &
      discards_df$peso_sub_muestra != 0),]
  err_1 <- addTypeOfError(err_1, "The discard is subsampled and all measured, but the peso_sub_muestra variable is distinct to 0")
  
  # if IS_SUBSAMPLED and NOT IS_AM ==> peso_muestra_total != peso_sub_muestra & peso_sub_muestra != 0
  err_2 <- discards_df[which(
    discards_df$IS_SUBSAMPLED == T &
      discards_df$IS_AM == F &
      discards_df$peso_sub_muestra == 0),]
  err_2 <- addTypeOfError(err_2, "The discard is subsampled and is not all measured, but the peso_sub_muestra variable is equal to 0")
  
  # if NOT IS_SUBSAMPLED and IS_AM ==> peso_muestra_total = peso_sub_muestra = 0
  err_3 <- discards_df[which(
    discards_df$IS_SUBSAMPLED == F &
      discards_df$IS_AM == T &
      discards_df$peso_sub_muestra != 0),]
  err_3 <- addTypeOfError(err_3, "The discard is not subsampled and is all measured, but the peso_muestra_total and peso_sub_muestra variables is distinct to 0")
  
  # if NOT IS_SUBSAMPLED and NOT IS_AM ==> peso_muestra_total = peso_sub_muestra != 0
  err_4 <- discards_df[which(
    discards_df$IS_SUBSAMPLED == F &
      discards_df$IS_AM == F &
      discards_df$peso_sub_muestra == 0),]
  err_4 <- addTypeOfError(err_4, "The discard is not subsampled and is not all measured, but the peso_sub_muestra variable is equal to 0")
  
  err <- rbind(err_1, err_2, err_3, err_4)
  
  return(err)
  
}


#' Check code: 2038
#' Find species of a haul with duplicated lengths in grouped species table
#' ATENTION: This errors must be fixed before the upload. When an error is
#' detected, the subcontracted company must be notified to correct it and send
#' it back to us fixed.
#' @param grouped_species_df grouped species dataframe
#' @return dataframe with errors
duplicate_lengtsh_in_grouped_species <- function(grouped_species_df) {

  if (is.null(nrow(grouped_species_df))) {
    warning("The grouped species dataframe is empty.")
    return()
    }

  dup <- aggregate( numero_ejemplares ~ acronimo + lance + especie_tax +
                    categoria + captura_descarte + COD_ESP + sexo + talla,
                    data = grouped_species_df,
                    FUN = length)

  if(nrow(dup) > 0){
    dup <- dup[which(dup$numero_ejemplares > 1), ]
    colnames(dup)[which(colnames(dup) == "numero_ejemplares")] <- "numero_registros_duplicados"
    return(dup)
  }

}

#' Check code: 2070
#' Occasionally the sampler sized all the individuals of the Chimaera monstrosa
#' and doesn't take the weight (because is all measured). But in SIRENO
#' we don't have SOP for it so, in the dumped process, the 'peso_muestra_total'
#' variable can't be updated an maintain the 0.
#' @param discards_lengths_df discard lengths dataframe
#' @return dataframe with errors
error_chimera_discards <- function(discards_lengths_df){
  errors <- discards_lengths_df[which(discards_lengths_df[["COD_ESP"]]=="10114" & discards_lengths_df[["peso_muestra_total"]]==0),]
  if(nrow(errors)>0){
    errors <- errors[, c("acronimo", "lance", "COD_ESP", "categoria", "sexo", "peso_muestra_total", "peso_sub_muestra", "numero_ejemplares_totales")]
    errors <- unique(errors)
    errors <- addTypeOfError(errors, "Chimera monstruosa sampled without total weight discard.")
    return(errors)  
  }
}

#' Check code: 2071
#' For us, right now this field must be as Y.
#' @param discards_lengths_df discard lengths dataframe
#' @return dataframe with errors
sub_muestra_field <- function(discards_lengths_df){
  err <- discards_lengths_df[discards_lengths_df$sub_muestra!="Y", c("acronimo", "lance", "COD_ESP", "categoria", "sexo")]
  if (nrow(err)>0){
    err <- addTypeOfError(err, "The field sub_muestra must be Y")
    return(err)  
  }
  
}

#' Check code: 2072
#' Same error as error_chimera_discards but with Torpedo marmorata
#' Occasionally the sampler sized all the individuals of the Torpedo marmorata
#' and doesn't take the weight (because is all measured). But in SIRENO
#' we don't have SOP for it so, in the dumped process, the 'peso_muestra_total'
#' variable can't be updated an maintain the 0.
#' @param discards_lengths_df discard lengths dataframe
#' @return dataframe with errors
error_torpedo_discards <- function(discards_lengths_df){
  errors <- discards_lengths_df[which(discards_lengths_df[["COD_ESP"]]=="10113" & discards_lengths_df[["peso_muestra_total"]]==0),]
  if(nrow(errors)>0){
    errors <- errors[, c("acronimo", "lance", "COD_ESP", "categoria", "sexo", "peso_muestra_total", "peso_sub_muestra", "numero_ejemplares_totales")]
    errors <- unique(errors)
    errors <- addTypeOfError(errors, "Torpedo marmorata sampled without total weight discard.")
    return(errors)  
  }
}

#' Check code:
#' Find duplicated litter and measure type in a haul of the survey.
#' ATENTION: THIS ERROR MUST BE FIXED BEFORE THE UPLOAD. When an error is
#' detected, the subcontracted company must be notified to correct it and send
#' it back to us fixed.
error_duplicated_litter <- function(litter_df) {

  if (is.null(nrow(litter_df))) {
    warning("The litter dataframe is empty.")
    return()
  }

  errors <- aggregate(x=litter_df[["acronimo"]],
                      by = litter_df[, c("acronimo", "lance", "medida", "basura_codigo")],
                      FUN = length)

  colnames(errors) <- c("acronimo", "lance", "medida", "basura_codigo", "n_duplicados")

  errors <- errors[errors[["n_duplicados"]] > 1, ]

  if (nrow(errors) > 0) {
    errors <- addTypeOfError(errors, "Duplicated litter and measure in a haul of the survey")
    return(errors)
  }

}


#' Filter oab_ipd list of data frames by month.
#' @param list_oab list of data frames with data to import returned by the
#' function importOabIpdFiles.
#' @param month month to filter, MONTH_AS_CHARACTER variables as default.
#' @return list_oab filtered by month. Read the note for detailed information.
#' @note
#' The function filter the trips acronym of the trips data frame included in the
#' list_oab list by the start date of the trip. Then, filter the rest of the
#' list_oab data frames by this acronym, without taking in consideration any
#' other dates (like final date of the haul). This mean that this function return
#' data of the trips which its start date match with the month to filter.
filter_by_month <- function(list_oab, month = MONTH_AS_CHARACTER){
  trips_dates <- list_oab[["TRIPS"]][, c("acronimo", "fecha_inicio")]
  trips_dates$fecha_inicio <- as.Date(trips_dates$fecha_inicio, format = "%d/%m/%Y")
  trips_dates$month <- format(trips_dates$fecha_inicio, "%m")
  trips_of_month <- trips_dates[trips_dates$month==month, "acronimo"]
  
  list_oab <- lapply(list_oab, function(x) {
    if(!is.null(nrow(x))) {
      filtered <- x[x$acronimo %in% trips_of_month,]
      return(filtered)
    } else {
      # mandatory to return the same value, even if it is NULL, for further
      # manipulations in the script.
      return (x)
    }
  })
}

#' Filter oab_ipd list by trips.
#' @param list_oab list of data frames with data to import returned by the
#' function importOabIpdFiles.
#' @param trips vector with trips to filter.
#' @return list_oab filtered by trips.
filter_by_trips <- function(list_oab, trips){
  list_oab <- lapply(list_oab, function(x) {
    if(!is.null(nrow(x))) {
      filtered <- x[x$acronimo %in% trips,]
      return(filtered)
    } else {
      # mandatory to return the same value, even if it is NULL, for further
      # manipulations in the script.
      return (x)
    }
  })
}
