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
