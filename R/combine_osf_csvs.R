#' Row‑bind every top‑level CSV file in an OSF component
#'
#' @param osf_id  5‑character GUID or full OSF URL (e.g. "u28tr").
#' @param pat     Personal‑access‑token (defaults to Sys.getenv("OSF_PAT")).
#' @param verbose Print progress messages?  Default = TRUE.
#' @return A tibble containing all rows from every *.csv file, padded with NA
#'         for missing columns, plus a `.source_file` provenance column.
#' @export
combine_osf_csvs <- function(osf_id,
                             pat     = Sys.getenv("OSF_PAT"),
                             verbose = TRUE) {
  
  for (pkg in c("osfr", "readr", "purrr", "dplyr"))
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Please install the {", pkg, "} package.")
  
  if (nzchar(pat)) osfr::osf_auth(token = pat)
  
  guid <- sub("^https?://osf.io/([A-Za-z0-9]{5}).*$", "\\1", osf_id)
  node <- osfr::osf_retrieve_node(guid)
  
  csv_tbl <- osfr::osf_ls_files(node,
                                type    = "file",
                                pattern = ".csv", n_max = 2)   # <- your simpler pattern
  
  if (nrow(csv_tbl) == 0)
    stop("No CSV files found at the top level of this component.")
  
  if (verbose) message("Found ", nrow(csv_tbl), " CSV file(s); downloading …")
  
  tmp <- tempfile("osf_csvs_"); dir.create(tmp)
  dl  <- osfr::osf_download(csv_tbl, path = tmp,
                            conflicts = "skip", verbose = verbose)
  
  files <- dl$local_path
  print("files")
  
  out <- bind_csvs_from_folder(tmp)
  
  if (verbose) message("Done – returning ",
                       nrow(out), " rows and ",
                       ncol(out) - 1, " data column(s).")
  out
}

