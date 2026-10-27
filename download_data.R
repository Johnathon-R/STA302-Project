# Run from the project folder: source("download_data.R")

download_etfs <- function() {
  destination <- file.path("data", "ETFs.csv")
  if (file.exists(destination) && file.info(destination)$size > 0) {
    message("Already downloaded: ", destination)
    return(invisible(destination))
  }

  url <- paste0(
    "https://www.kaggle.com/api/v1/datasets/download/",
    "stefanoleone992/mutual-funds-and-etfs/ETFs.csv",
    "?datasetVersionNumber=4"
  )

  # Kaggle sends this CSV inside a ZIP file.
  temporary <- tempfile("etf-download-")
  dir.create(temporary)
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  archive <- file.path(temporary, "ETFs.zip")
  download.file(url, archive, mode = "wb", method = "libcurl")
  contents <- unzip(archive, list = TRUE)
  if (!("ETFs.csv" %in% contents$Name)) {
    stop("Kaggle did not return ETFs.csv. Download it manually from the dataset page.")
  }
  unzip(archive, files = "ETFs.csv", exdir = temporary)

  dir.create("data", showWarnings = FALSE)
  if (!file.copy(file.path(temporary, "ETFs.csv"), destination, overwrite = TRUE)) {
    stop("Could not save ", destination)
  }
  message("Saved: ", destination)
  invisible(destination)
}

download_etfs()
