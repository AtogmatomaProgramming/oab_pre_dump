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
      
      # Fourth, set the variable make_copy if you want to make one or not

      make_copy <- FALSE

      #' Fifth, set the variable any_file to use like argument in file.copy function
      #' in the case is necesary or not to overwrite existing files

      any_file <- FALSE

      if(length(dir_list_to) == 0){

        # In the case the destiny folder is empty, create the internal directories

        make_copy <- TRUE

        any_file <- FALSE

        dir_list_to <- file.path(path_files_to, dir_list_from)

        lapply(dir_list_to, dir.create, recursive = TRUE)

        message(paste0("The copy of the files will be made at: ", path_files_to))

      } else {

        # In the case there are previous files into the destiny folder, ask the user what to do

        message("WARNING: Already exists previous files into the share folder")
        
        # Set the start number of attempts to introduce a correct answer
        number_of_attemps <- 1

        # Variable to set if the user has introduce the correct format of answer
        correct_answer <- ""

        # Loop to ask the user until a correct answer is introduced or the number of attempts is exceeded
        while(correct_answer != TRUE & number_of_attemps <= 3){

          overwrite <- readline(prompt="Do you want to overwrite existing data?(Y/N):")

          # Standardize the answer
          overwrite <- as.character(overwrite)
          
          overwrite <- toupper(overwrite)
          
          if (overwrite == "Y") {
            correct_answer <- TRUE
            any_file <- TRUE
            make_copy <- TRUE
            message("You have decided to overwrite existing files")
          } else if (overwrite == "N") {
            correct_answer <- TRUE
            make_copy <- FALSE
            message("You have aborted to overwrite existing files")
          } else {
            correct_answer <- FALSE
            number_of_attemps <- number_of_attemps + 1
            remaining_attemps <- 4 - number_of_attemps
            message(paste0("You have introduce a wrong answer. Rest attemps: ", remaining_attemps))
          }

      }
        
      if(make_copy){

      # List all the files present into the source folder
      files_list_from <- list.files(path_files_from, 
                                   recursive = TRUE,
                                   full.names = FALSE)
      
      # Make the final copy into the destiny folder
      file.copy(from = file.path(path_files_from, files_list_from),
                to = file.path(path_files_to, file_list_from),
                overwrite = any_file)
        
      }else{

        message("Nothing has been done.")

      }
    }
        
    }, error = function(e){

      message("Error in copy_files_to_folder: ", e$message)

    }, warning = function(w){

      message("Warning in copy_files_to_folder: ", w$message)
      
    }
  
  
  )
  

}


