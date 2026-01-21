#' MetaForge API Functions for R
#'
#' Download experiment data from MetaForge (Protocol.js) directly into R.
#'
#' @author Protocol.js Team
#' @version 1.0.0

library(httr)
library(jsonlite)

# Base URL for the API
METAFORGE_API_BASE <- "https://us-central1-protocol-js.cloudfunctions.net"

#' Set MetaForge API Token
#'
#' Store your API token for use in subsequent API calls.
#' Get your token from your Profile in MetaForge (metaforge.live).
#'
#' @param token Your API token string
#' @return Invisibly returns the token
#' @export
#' @examples
#' metaforge_set_token("your-api-token-here")
metaforge_set_token <- function(token) {
  options(metaforge_api_token = token)
  invisible(token)
}

#' Get stored MetaForge API Token
#'
#' @return The stored API token or NULL if not set
#' @keywords internal
metaforge_get_token <- function() {
  token <- getOption("metaforge_api_token")
  if (is.null(token)) {
    stop("API token not set. Use metaforge_set_token('your-token') first.\n",
         "Get your token from Profile in MetaForge (metaforge.live).")
  }
  return(token)
}

#' List MetaForge Projects
#'
#' Get a list of all your projects in MetaForge.
#'
#' @param token Optional API token. If not provided, uses stored token.
#' @return A data frame with project IDs, titles, and creation dates
#' @export
#' @examples
#' metaforge_set_token("your-api-token")
#' projects <- metaforge_list_projects()
#' print(projects)
metaforge_list_projects <- function(token = NULL) {
  if (is.null(token)) {
    token <- metaforge_get_token()
  }

  url <- paste0(METAFORGE_API_BASE, "/apiListProjects")

  response <- GET(
    url,
    add_headers(Authorization = paste("Bearer", token)),
    content_type_json()
  )

  if (status_code(response) != 200) {
    error_content <- tryCatch(
      content(response, "parsed"),
      error = function(e) list(error = content(response, "text"))
    )
    error_msg <- if (is.list(error_content)) error_content$error else as.character(error_content)
    stop("API Error (", status_code(response), "): ", error_msg %||% "Unknown error")
  }

  result <- tryCatch(
    content(response, "parsed"),
    error = function(e) {
      stop("Failed to parse API response: ", e$message)
    }
  )

  if (length(result$projects) == 0) {
    message("No projects found.")
    return(data.frame(id = character(), title = character(), createdAt = character()))
  }

  projects_df <- do.call(rbind, lapply(result$projects, function(p) {
    data.frame(
      id = p$id %||% NA,
      title = p$title %||% NA,
      createdAt = p$createdAt %||% NA,
      stringsAsFactors = FALSE
    )
  }))

  return(projects_df)
}

#' Download MetaForge Project Data
#'
#' Download experiment data from a specific project.
#'
#' @param project_id The project ID (get this from metaforge_list_projects())
#' @param token Optional API token. If not provided, uses stored token.
#' @return A data frame containing the experiment data
#' @export
#' @examples
#' metaforge_set_token("your-api-token")
#'
#' # List projects to find the ID
#' projects <- metaforge_list_projects()
#' print(projects)
#'
#' # Download data from a specific project
#' data <- metaforge_get_data("your-project-id")
#' head(data)
metaforge_get_data <- function(project_id, token = NULL) {
  if (is.null(token)) {
    token <- metaforge_get_token()
  }

  if (missing(project_id) || is.null(project_id) || project_id == "") {
    stop("project_id is required. Use metaforge_list_projects() to see available projects.")
  }

  url <- paste0(METAFORGE_API_BASE, "/apiGetProjectData")

  # Request CSV format for better handling of large files
  response <- GET(
    url,
    query = list(projectId = project_id, format = "csv"),
    add_headers(Authorization = paste("Bearer", token))
  )

  if (status_code(response) != 200) {
    error_content <- tryCatch(
      content(response, "parsed"),
      error = function(e) list(error = content(response, "text"))
    )
    error_msg <- if (is.list(error_content)) error_content$error else as.character(error_content)
    stop("API Error (", status_code(response), "): ", error_msg %||% "Unknown error")
  }

  # Check if response is CSV (text/csv) or JSON
  content_type <- headers(response)[["content-type"]]

  if (grepl("text/csv", content_type, fixed = TRUE)) {
    # Parse CSV directly
    csv_text <- content(response, "text", encoding = "UTF-8")

    if (nchar(trimws(csv_text)) == 0) {
      message("No data found for project.")
      return(data.frame())
    }

    data_df <- read.csv(text = csv_text, stringsAsFactors = FALSE)
    message("Downloaded ", nrow(data_df), " rows (source: uploaded)")
    return(data_df)
  }

  # Otherwise parse as JSON
  result <- tryCatch(
    content(response, "parsed"),
    error = function(e) {
      stop("Failed to parse API response: ", e$message)
    }
  )

  if (length(result$data) == 0) {
    message("No data found for project: ", result$projectTitle %||% project_id)
    # Return empty data frame with columns if available
    if (length(result$columns) > 0) {
      empty_df <- as.data.frame(matrix(ncol = length(result$columns), nrow = 0))
      colnames(empty_df) <- result$columns
      return(empty_df)
    }
    return(data.frame())
  }

  # Convert list of lists to data frame
  data_df <- do.call(rbind, lapply(result$data, function(row) {
    as.data.frame(lapply(row, function(x) if (is.null(x)) NA else x), stringsAsFactors = FALSE)
  }))

  message("Downloaded ", nrow(data_df), " rows from '", result$projectTitle, "' (source: ", result$source, ")")

  return(data_df)
}

#' Download All MetaForge Project Data
#'
#' Download data from all your projects at once.
#'
#' @param token Optional API token. If not provided, uses stored token.
#' @param combine If TRUE, combines all data into one data frame with a 'project' column.
#'                If FALSE, returns a named list of data frames.
#' @return Either a combined data frame or a named list of data frames
#' @export
#' @examples
#' metaforge_set_token("your-api-token")
#'
#' # Get all data as separate data frames
#' all_data <- metaforge_get_all_data(combine = FALSE)
#'
#' # Get all data combined
#' combined <- metaforge_get_all_data(combine = TRUE)
metaforge_get_all_data <- function(token = NULL, combine = FALSE) {
  if (is.null(token)) {
    token <- metaforge_get_token()
  }

  projects <- metaforge_list_projects(token)

  if (nrow(projects) == 0) {
    message("No projects found.")
    return(if (combine) data.frame() else list())
  }

  all_data <- list()

  for (i in seq_len(nrow(projects))) {
    project_id <- projects$id[i]
    project_title <- projects$title[i]

    tryCatch({
      data <- metaforge_get_data(project_id, token)
      if (nrow(data) > 0) {
        if (combine) {
          data$project_id <- project_id
          data$project_title <- project_title
        }
        all_data[[project_title]] <- data
      }
    }, error = function(e) {
      warning("Failed to download data for '", project_title, "': ", e$message)
    })
  }

  if (combine && length(all_data) > 0) {
    # Combine all data frames, filling missing columns with NA
    all_cols <- unique(unlist(lapply(all_data, names)))

    combined_data <- do.call(rbind, lapply(all_data, function(df) {
      missing_cols <- setdiff(all_cols, names(df))
      for (col in missing_cols) {
        df[[col]] <- NA
      }
      df[, all_cols, drop = FALSE]
    }))

    return(combined_data)
  }

  return(all_data)
}

# Null coalescing operator helper
`%||%` <- function(a, b) if (is.null(a)) b else a
