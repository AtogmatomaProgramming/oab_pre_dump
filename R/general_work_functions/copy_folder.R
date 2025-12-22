#' Copy folder and contents
#'
#' @description
#' Copies all files and subfolders from a source directory to a destination 
#' directory, maintaining the folder structure. If files already exist in the 
#' destination, prompts the user to confirm overwrite.
#'
#' @param folder_from Character. Path to the source directory containing the 
#'   files and folders to be copied.
#' @param folder_to Character. Path to the destination directory where files 
#'   and folders will be copied.
#'
#' @return NULL. The function is called for its side effects (copying files).
#'
copy_folder <- function(folder_from, folder_to){
          
      files_from <- list.files(FOLDER_FROM, recursive = TRUE)
      files_to <- list.files(FOLDER_TO, recursive = TRUE)

      # ALBERTO: ponemos el overwrite que va a ir en la función de copiar ficheros a false por defecto,
      # en vez de ponerlo igual a "" como hacías tu.
      # Así, luego si el usuario decide sobreescribir, lo ponemos a TRUE, pero por defecto a FALSE
      # y así no tener que hacer un else para ponerlo a FALSE.
      overwrite <- FALSE

      # Check for existing files in destination
      if (any(files_from %in% files_to)){ # ALBERTO: comprobamos que haya archivos 
        # que se llamen igual. En tu script sólo compruebas que haya algún archivo, sea cual sea.
        # Si compruebas que haya cualquier archivo, no tiene sentido preguntar si quieres sobreescribir,
        # porque no tiene por qué coincidir ninguno. Detectar si existe algún archivo estaría bien
        # para avisar al usuario por si prefiere cancelar la operación en caso que se esté equivocando
        # de carpeta.
        message("WARNING: Files already exist in the destination folder.")

        answer <- ""

        # ALBERTO: Con este bucle while nos aseguramos que el usuario introduzca una respuesta válida
        while(!(answer %in% c("Y", "y", "N", "n"))){
          answer <- readline(prompt="Do you want to overwrite existing data?(Y/N):")
        }

        # ALBERTO: en el caso de que el usuario decida sobreescribir, ponemos overwrite a TRUE
        if(answer == "Y" | answer == "y"){
          overwrite <- TRUE
        }
      }
  
      # Create subfolders in destination
      folders <- list.dirs(folder_from,
        recursive = TRUE, # ALBERTO: con el recursive nos aseguramos que tengamos
                          # las subcarpetas, si no me equivoco en tu script sólo
                          # tenías en cuenta las primeras carpetas, pero si hubiera
                          # alguna dentro de otra no la copiaría. Es importante
                          # si queremos reutilizar la función en otros scripts.
        full.names = FALSE)
  
      lapply(file.path(folder_to, folders), 
             dir.create, 
             recursive = TRUE)
  
      # Copy files
      file.copy(file.path(folder_from, files_from),
        file.path(folder_to, files_from),
        overwrite = overwrite)
}

# EJEMPLO DE USO:
# copy_folder(folder_from = "C:/TEST_FROM/2025_06",
#             folder_to = "C:/TEST_TO/2025_06")
