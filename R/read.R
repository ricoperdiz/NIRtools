#' Read NIR parameters file from disk
#'
#' @param file A path to a file.
#'
#' @return A data.frame
#'
#' @importFrom data.table dcast setDT
#' @importFrom stringr str_trim
#' @importFrom tidyr separate
#' @export
#' @examples
#' params_file_path <- system.file("extdata", "test-NIRparams.txt", package = "NIRtools")
#' read_NIRparams(params_file_path)
read_NIRparams <- function(file) {
  file_text <-
    data.frame(text = readLines(file))
  file_text_sbset <-
    subset(file_text, grepl("##", text))
  parameters_separated <-
    separate(file_text_sbset, text, into = c("value", "key", "definition"), sep = paste(c("## ", "\\: "), collapse = "|"))

  setDT(parameters_separated)

  parameters <-
    parameters_separated[, list(value, key)]

  parameters$key <-
    str_trim(gsub("\\[|\\]", "", parameters$key), "both")


  parameters$value <-
    str_trim(parameters$value, "both")

  parameters[, `:=`(dataset_index = cumsum(key == "dataset_name"))]

  parameters <-
    dcast(parameters, dataset_index ~ key, value.var = "value")

  return(parameters)
}

#' Read near infrared (NIR) spectroscopy data in raw format
#'
#' @param file A path to a file containing near infrared (NIR) spectroscopy raw data. This means that it expects to be a file with 1557 rows, in which first column contains identification of spectrum number, and second column the NIR absorbance.
#'
#' @return A data.frame in `wide` format in which each row contains 1557 columns of near infrared spectroscopy data.
#' @importFrom tidyr spread
#' @importFrom data.table fread
#' @export
#' @examples
#'
#' # read file with raw data
#' nir_raw <- read_NIRraw("data/nir_raw.csv")
#' # check first rows and 5 first columns of data
#' dim(nir_raw)
#' names(nir_raw)
#' head(nir_raw)[, 1:10]
read_NIRraw <- function(file, add_nir_id = TRUE) {
  nir_raw <- fread(file, header = FALSE, blank.lines.skip = TRUE)
  message(paste0("Raw NIR file dimension:\n", paste(dim(nir_raw), collapse = " ")))

  if (dim(nir_raw)[2] != 2) {
    stop("Raw NIR file does not have two columns. Please, check your data to see if it really contains two columns only.")
  }

  names(nir_raw) <- c("key", "value")

  if (add_nir_id == TRUE) {
    message("Adding letter 'X' before NIR spectra")
    nir_raw[, key := paste0("X", key)]
  }

  nir_raw_spread <-
    spread(as.data.frame(nir_raw), key, value)

  return(nir_raw_spread)
}