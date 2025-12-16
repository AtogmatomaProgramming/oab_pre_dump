#' Make a copy of file from one directory to other 
#'
#' @description
#' This function makes a copy of files from a source directory to a destination directory.
#'
#' @param path_files_from Character. Path to the source directory containing the files to be copied.
#' @param path_files_to Character. Path to the destination directory where files will be copied
#'
#' @return NULL

copy_files_to_folder <- function (path_files_from, path_files_to){

  tryCatch(
    expr = {
      # First, take a list with the internal direcories name
      dir_list_from <- list.dirs(path_files_from, 
                                 recursive = FALSE,
                                 full.names = FALSE)
      
      # Second, check if there are previous files into the destination folder
      dir_list_to <- list.files(path_files_to)


      # Third, take a list of the files present into the source folder
      files_list_from <- list.files(path_files_from, 
                                   recursive = TRUE,
                                   full.names = FALSE)
      
      # Fourth, set the variable overwrite if you want to overwrite existing files or not
      # with empty value

      over_write <- ""

      
      if(length(dir_list_to) != 0){

        message("WARNING: Already exists previous files into the share folder")

        # In the case there are previous files into the destiny folder, ask the user what to do
        answer <- readline(prompt="Do you want to overwrite existing data?(Y/N):")

      }

      
      if(length(dir_list_to) == 0 || answer == "Y"){

        if(length(dir_list_to) == 0){

        # In the case the destiny folder is empty, create the internal directories

        lapply(file.path(path_files_to, dir_list_from), 
              dir.create, 
              recursive = TRUE)
          
        over_write <- FALSE
          
        } else{

          over_write <- TRUE

        }

      # List all the files present into the source folder
      # files_list_from <- list.files(path_files_from, 
      #                              recursive = TRUE,
      #                              full.names = FALSE)
      
      # Make the final copy into the destiny folder
      file.copy(from = file.path(path_files_from, files_list_from),
                to = file.path(path_files_to, files_list_from),
                overwrite = over_write)
        
      message("MESSAGE: Files copied successfully.")
        
      }else{

        message("Nothing has been done.")

      }

    }, error = function(e){

      message("Error in copy_files_to_folder: ", e$message)

    }, warning = function(w){

      message("Warning in copy_files_to_folder: ", w$message)
      
    }
  
  
  )
  

}

