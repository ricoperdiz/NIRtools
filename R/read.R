#' Lê metadados de um subconjunto de dados NIR a partir de um arquivo de texto
#'
#' @param arq Vetor de texto indicando caminho a um arquivo.
#'
#' @return Um data.frame
#'
#' @importFrom data.table dcast setDT
#' @importFrom tidyr separate
#' @export
#' @examples
#' params_file_path <- system.file("extdata", "conjunto1-NIRparams.txt", package = "NIRtools")
#' read_NIRparams(arq = params_file_path)
read_NIRparams <- function(arq) {
  file_text <-
    data.frame(text = readLines(arq))
  file_text_sbset <-
    subset(file_text, grepl("##", text))
  parameters_separated <-
    separate(file_text_sbset, text, into = c("value", "key", "definition"), sep = paste(c("## ", "\\: "), collapse = "|"))

  setDT(parameters_separated)

  parameters <-
    parameters_separated[, list(value, key)]

  parameters$key <-
    trimws(gsub("\\[|\\]", "", parameters$key), "both")


  parameters$value <-
    trimws(parameters$value, "both")

  parameters[, `:=`(dataset_index = cumsum(key == "dataset_name"))]

  parameters <-
    dcast(parameters, dataset_index ~ key, value.var = "value")

  return(parameters)
}

#' Lê dados NIR em formato bruto obtido em um espectrofotômetro Thermo Nicollet \(Thermo Fischer Scientific, Waltham, Massachusetts, USA\), modelo Antaris II FT-NIR Analyzer, hospedado no herbário do INPA, Amazonas, Brasil
#'
#' @param arq Vetor de texto contendo caminho para um arquivo contendo dados de infravermelho próximo em formato bruto. Geralmente o dado será um arquivo de 1557 linhas, em que a primeira coluna conterá o número identificador do espectro, e a segunda coluna conterá o valor de absorbância NIR.
#'
#'@param add_nir_id Vetor lógico, `TRUE` ou `FALSE`, para saber se deve ser adicionado a letra `X` antes de cada variável NIR no arquivo final
#' @return Um data.frame em formato amplo (`wide`  em inglês) em que cada linha conterá as 1557 colunas de dados NIR..
#' @importFrom tidyr spread
#' @importFrom data.table fread
#' @export
#' @examples
#' # read file with raw data
#' nir_raw <- system.file("extdata", "nir_raw.csv", package = "NIRtools")
#' nir_raw <- read_dirNIRraw(arq = nir_raw, add_nir_id = TRUE)
#' # check first rows and 5 first columns of data
#' dim(nir_raw)
#' names(nir_raw)
#' head(nir_raw)[,1:10]
read_dirNIRraw <- function(arq, add_nir_id = TRUE) {

  nir_data_read <- data.table::fread(arq, blank.lines.skip = TRUE)
  message(paste0('Raw NIR file dimension:\n', paste(dim(nir_data_read), collapse = ' ')))

  if(dim(nir_data_read)[2] != 2) {
    stop('Raw NIR file does not have two columns. Please, check your data to see if it really contains two columns only.')
  }

  names(nir_data_read) <- c('key', 'value')

  if(add_nir_id == TRUE) {
    message("Adding letter 'X' before NIR spectra")
    nir_data_read[, key := paste0('X', key)]
  }

  nir_raw_spread <-
    tidyr::spread(as.data.frame(nir_data_read), key, value)

  return(nir_raw_spread)
}
