#' Get samples which all measured discards
#' Get a list with the discard measured samples. To know which
#' samples are all measured, we have to use de dataframe CATCHES, where the
#' variables "todo_medido_descarte" is allocated.
#' @param df_all_measured df with variable "todo_medido_descarte", which mark
#' the all discard measured samples
#' @return samples with 'all discard measured' if there are any data to fix.
#' FALSE if there aren't data to fix.
get_discard_all_measured <- function(df_all_measured){
  
  if (variable_exists_in_df("todo_medido_descarte", df_all_measured)) {
    
    adm <- df_all_measured[which(df_all_measured$todo_medido_descarte=="S"),]
    if(nrow(adm) == 0) { return(FALSE) }
    adm <- adm[, c("acronimo", "lance", "COD_ESP", "categoria")]
    adm <- unique(adm)
    
    return(adm)
    
  } else {
    stop("Variable does not exist in the df_all_measured dataframe")
  }
  
}


#' Check code: 2039
#' FIX discards weights.
#' 1) When a sample is all measured and it isn't subsampled, the variables
#' peso_muestra_total and peso_sub_muestra must be 0.
#' 2) When a sample is all measured and it is subsampled, the variable
#' peso_sub_muestra must be 0.
#' In the dump process, when this variables are equal to 0, they are updated
#' with the SOP value.
#' 3) When a sample is not all measured and isn't subsampled, the
#' variable peso_sub_muestra must be equal to peso_muestra_total
#' @param df_all_measured df with variable "todo_medido_descarte", which mark
#' the all discard measured samples
#' @param df_discards discards dataframe
#' @return df_discards dataframe fixed
fix_discards_weights <- function(df_all_measured, df_discards){
  
  # get the discards all measured dataframe and add new column to reflect it:
  a_m_d <- get_discard_all_measured(df_all_measured)
  
  # it there are nothing to fix, the functions return the original dataframe
  if(isFALSE(a_m_d)) {
    
    print("Nothing to fix")
    
    return(df_discards)
    
  }
  
  a_m_d$AM <- TRUE
  
  if (variable_exists_in_df("peso_muestra_total", df_discards) &&
      variable_exists_in_df("peso_sub_muestra", df_discards)) {
    
    # add to the dataframe the column AM
    d_l_fixed <- merge(df_discards, a_m_d, all.x = T)
    d_l_fixed[which(is.na(d_l_fixed$AM)), c("AM")] <- F
    
    # create column IS_SUBSAMPLED only in the rows which are all measured
    d_l_fixed <- d_l_fixed %>%
      mutate(IS_SUBSAMPLED = ifelse(AM == T & peso_muestra_total!=peso_sub_muestra, T, F))
    
    # FIX 1) When a sample is all measured and it isn't subsampled, the variables
    # peso_muestra_total and peso_sub_muestra must be 0.
    # when the column AM is true and IS_SUBSAMPLED is false the peso_muestra_total
    # and the peso_sub_muestra must be 0
    d_l_fixed[which(d_l_fixed$AM==T &
                      d_l_fixed$IS_SUBSAMPLED==F),
              c("peso_muestra_total", "peso_sub_muestra")] <- c(0,0)
    
    # FIX 2) When a sample is all measured and it is subsampled, the variable
    # peso_sub_muestra must be 0.
    # when the column AM is true and IS_SUBSAMPLED is TRUE, the peso_sub_muestra
    # must be 0
    d_l_fixed[which(d_l_fixed$AM == T &
                      d_l_fixed$IS_SUBSAMPLED==T),
              c("peso_sub_muestra")] <- 0
    
    # FIX 3) When a sample is not all measured and isn't subsampled, the
    # variable peso_sub_muestra must be equal to peso_muestra_total
    d_l_fixed[which(d_l_fixed$AM == F &
                      d_l_fixed$IS_SUBSAMPLED == F),
              c("peso_sub_muestra")] <- d_l_fixed[which(d_l_fixed$AM == F &
                                                          d_l_fixed$IS_SUBSAMPLED == F),
                                                  c("peso_muestra_total")]
    
    
    d_l_fixed <- d_l_fixed[, -which(names(d_l_fixed) %in% c("IS_SUBSAMPLED", "AM"))]
    
    # return dataframe fixed
    return(d_l_fixed)
    
  } else {
    stop("The variable or variables does not exists in df_discards dataframe")
  }
  
}


#' Check code: 2040
#' FIX catches weights.
#' 1) When a sample is all measured, the fields 'peso' and 'peso_muestra' must
#' be 0.
#' In the dump process, when this variables are equal to 0, they are updated
#' with the SOP value.
#' @param df_catches catches dataframe
#' @return df_catches dataframe fixed
fix_catches_weights <- function (df_catches){
  # 1) When a sample is all measured, the fields 'peso' and 'peso_muestra' must
  # be 0.
  if (variable_exists_in_df("todo_medido_captura", df_catches) &&
      variable_exists_in_df("peso", df_catches)){
    df_catches[which(df_catches$todo_medido_captura == "S"), "peso"] <- 0
    df_catches[which(df_catches$todo_medido_captura == "S"), "peso_muestra"] <- 0
    return(df_catches)
  } else {
    stop("'peso' and/or 'todo_medido_captura' variable does not exists in this dataframe")
  }
  
}

#' Check code: 2043
#' In HAULS table, the field metier_ieo is not filled by the contracted company.
#' The value of this field depend of 'especie_objetivo' and 'ESTRATO_RIM'. A
#' dataset with this combinations is metier_target_species
#' @param df_hauls hauls dataframe
#' @return hauls dataframe with the metier_ieo filled. Is possible empty data
#' when there aren't match in metier_target_species dataset
add_metier_ieo_variable <- function(df_hauls){
  
  # check if metier_target_species dataset is loaded
  if(!exists("metier_target_species")){
    return("The metier_target_species dataset is required")
  }
  
  if(variable_exists_in_df("especie_objetivo", df_hauls) &
     variable_exists_in_df("ESTRATO_RIM", df_hauls) &
     variable_exists_in_df("metier_ieo", df_hauls)){
    
    # get the column names to maintain the order of columns
    col_names <- colnames(df_hauls)
    
    # add values to metier_ieo
    df_hauls <- merge(df_hauls, metier_target_species,
                      by.x = c("especie_objetivo", "ESTRATO_RIM"),
                      by.y = c("COD_ESP_OBJ", "ESTRATO_RIM"),
                      all.x = T)
    
    # reorder the columns of the dataframe
    df_hauls$metier_ieo <- df_hauls$METIER_IEO
    df_hauls <- df_hauls[, c(col_names)]
    
    return(df_hauls)
    
  } else {
    return("especie_objetivo, ESTRATO_RIM and/or metier_ieo variables does not exists")
  }
  
  
}


#' Check code: 2044
#' Detect and fix hauls with discarded species which has number of measured
#' individuals but the weight sampled, weight sub-sampled and number of
#' individuals by lenght is 0.
#' In some hauls, discarded species has number of measured individuals but the weight
#' sampled, weight sub-sampled and number of individuals by length is 0. It can
#' be an error in the sampling, an error in the keyed process o the SOP is 0.
#' This species must be removed because the discards process break down if this
#' errors exists.
#' An errors file is exported to share with the Area Coordinators in order to 
#' check it.
#' @param lengths_discards lengths discards dataframe
#' @return legths discards dataframe without the erroneus species. A xls file is
#' exported with the erroneus species.
error_lengths_discards <- function(lengths_discards){
  
  #check if package openxlsx is instaled:
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Openxlsx package needed for this function to work. Please install it.",
         call = FALSE)
  }
  
  
  # Create errors subfolder in case it doesn't exists:
  if (!file.exists(file.path(PATH_ERRORS_FILES))){
    dir.create(file.path(PATH_ERRORS_FILES))
  }
  
  register_errors <- lengths_discards %>%
    filter(numero_ejemplares_totales != 0) %>%
    group_by(acronimo, lance, COD_ESP, categoria, sexo) %>%
    mutate(suma = sum(numero_ejemplares)) %>%
    filter(peso_muestra_total == 0, peso_sub_muestra == 0, suma == 0)
  
  
  # export file
  to_export <- addTypeOfError(register_errors, "WARNING: Esta especie tiene número de ejemplares descartados, pero no tiene ni ejemplares por talla, ni peso muestra, ni peso sumbmuestra. No se ha volcado en SIRENO.")

  openxlsx::write.xlsx(to_export, file.path(PATH_ERRORS_FILES, paste0("species_to_check_", YEAR, "_", MONTH, ".xlsx")))
  
  
  # remove registers from dataframe and return it    
    registers_to_remove <- register_errors[, c("acronimo", "lance", "COD_ESP",
                                             "categoria", "sexo")]
  
  clean_lengths_discards <- anti_join(lengths_discards, registers_to_remove)
  
  return(clean_lengths_discards)
}

#' Check code: 2057
#' Fix total weight discarded in not measured hauls.
#' When a haul is not measured, the total weight discarded must be empty (and
#' not 0)
fix_discarded_weights_not_measured_haul <- function(df_hauls){
  
  if (variable_exists_in_df("muestreado", df_hauls) &&
      variable_exists_in_df("peso_descarte", df_hauls)){
    
    df_hauls[df_hauls[["muestreado"]] == "N", "peso_descarte"] <- NA
    
    
    return(df_hauls)
    
  } else {
    
    stop("The variable 'muestreado' and 'peso descarte' must exists in hauls
    dataframe")
    
  }
}

