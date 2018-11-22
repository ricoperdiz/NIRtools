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
#' nir_raw <- read_NIRraw('data/nir_raw.csv')
#' # check first rows and 5 first columns of data
#' dim(nir_raw)
#' names(nir_raw)
#' head(nir_raw)[,1:10]
read_dirNIRraw <- function(file, add_nir_id = TRUE) {

  dirNIRraw <-

  nir_raw <- fread(file, header = FALSE, blank.lines.skip = TRUE)
  message(paste0('Raw NIR file dimension:\n', paste(dim(nir_raw), collapse = ' ')))

  if(dim(nir_raw)[2] != 2) {
    stop('Raw NIR file does not have two columns. Please, check your data to see if it really contains two columns only.')
  }

  names(nir_raw) <- c('key', 'value')

  if(add_nir_id == TRUE) {
    message("Adding letter 'X' before NIR spectra")
    nir_raw[, key := paste0('X', key)]
  }

  nir_raw_spread <-
    spread(as.data.frame(nir_raw), key, value)

  return(nir_raw_spread)
}
