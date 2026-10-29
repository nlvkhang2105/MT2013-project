
# Load raw dataset
load_data <- function() {
  file_path <- "MT2013/data/raw/add.csv"

  if (!file.exists(file_path)) {
    stop("File not found at: ", file_path)
  }

  df <- read.csv(
    file_path,
    na.strings = c("?", "NA", "", " ?"),
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )

  return(df)
}
