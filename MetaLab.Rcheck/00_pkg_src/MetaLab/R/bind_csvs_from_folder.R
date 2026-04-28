#' Bind csvs from folder
#'
#' This function row binds all CSV files from a specified folder
#'
#' @param folder_path folder path
#' @param participant_id ID variable to check for duplicates
#' @return A combined dataframe with all the CSVs'
#' @export
bind_csvs_from_folder <- function(folder_path, participant_id = "participant_id") {
  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    stop("The specified folder does not exist")
  }

  # List all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

  # Check if any CSV files were found
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified folder")
  }

  # Create an empty list to store dataframes
  df_list <- list()

  # Read each CSV and store in the list
  for (file in csv_files) {
    tryCatch({
      df <- read.csv(file, stringsAsFactors = FALSE)
      df_list[[basename(file)]] <- df
      message(paste("Successfully read:", basename(file)))
    }, error = function(e) {
      warning(paste("Failed to read file:", basename(file), "- Error:", e$message))
    })
  }

  # Check if any files were successfully read
  if (length(df_list) == 0) {
    stop("Failed to read any CSV files")
  }

  # Get all unique column names across all dataframes
  all_cols <- unique(unlist(lapply(df_list, colnames)))

  # Function to add missing columns and reorder
  standardize_columns <- function(df, all_columns) {
    missing_cols <- setdiff(all_columns, colnames(df))

    # Add missing columns with NA values
    for (col in missing_cols) {
      df[[col]] <- NA
    }

    # Return dataframe with columns in the same order
    return(df[, all_cols])
  }

  # Standardize all dataframes
  standardized_dfs <- lapply(df_list, standardize_columns, all_columns = all_cols)

  # Combine all dataframes
  combined_df <- do.call(rbind, standardized_dfs)

  # Add a column indicating source file if needed
  combined_df$source_file <- rep(names(standardized_dfs), sapply(standardized_dfs, nrow))

  # Check if duplicates
  find_duplicate_attempts(combined_df, id_col = participant_id)

  return(combined_df)
}
