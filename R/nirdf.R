#' Cria um objeto nirdf
#'
#' @param x Um data.frame, ou um data.table.
#' @param category Um nome apontando o nome da variável categórica dentro do argument x.
#' @param measure_columns Um vetor contendo nomes de colunas que contêm as variáveis NIR.
#' @param measure_columns_prefix Um nome apontando qual prefixo foi utilizado para indicar as colunas que contêm variáveis NIR. Opcional. Caso haja, este nome será retirado do nome das colunas com o uso da função `gsub()`.
#'
#' @return Um objeto nirdf.
#' @import data.table
#' @export
#'
#' @examples
#' # Load nir data
#' library("NIRtools")
#' data(nir_data)
#' nirdad <- nirdf(nir_data, "SP1",
#'  measure_columns = grep("^X", names(nir_data), value = TRUE),
#'   measure_columns_prefix = "X")
nirdf <- function(x, category, measure_columns, measure_columns_prefix = NULL) {

  # Check base object
  stopifnot(is.data.frame(x))


  # Check arguments - category:
  # Category NULL?
  if (is.null(category)) {
    stop(paste0("Argument `category` can't be `NULL`. You must indicate the name of a column of your nirdf object to represent the categories of your nir dataset."))
  }
  # Category empty
  if (length(category) == 1 && category == "") {
    stop(paste0("Argument `category` can't be empty. You must indicate the name of a column of your nirdf object to represent the categories of your nir dataset."))
  }
  # Category empty
  if (length(category) > 1) {
    stop(paste0("Argument `category` must have length 1. Provide only one name of a column of your nirdf object to represent the categories of your nir dataset."))
  }

  # Check arguments - measure_columns
  # measure_columns NULL?
  if (is.null(measure_columns)) {
    stop(paste0("Argument `measure_columns` can't be `NULL`. You must indicate all columnn names that represent the variables that contain NIR measurements."))
  }
  # measure_columns empty
  # think more - result is not ok.
  # if (measure_columns == "") {
  #     stop(paste0("Argument `measure_columns` can't be empty. You must indicate all columnn names that represent the variables that contain NIR measurements."))
  # }
  # measure_columns of length 1
  if (length(measure_columns) == 1) {
    stop(paste0("Argument `measure_columns` can't have length 1. Provide all columnn names that represent the variables that contain NIR measurements."))
  }

  columns_to_keep <- c(category, measure_columns)
  columns_to_keep_pos <- which(names(x)  %in% columns_to_keep)
  # Coerce x to a data.table
  nirdf <- as.data.table(x)
  nirdf <- nirdf[, columns_to_keep_pos, with = FALSE]


  if (is.null(measure_columns_prefix)) {

    class(nirdf) <- c("nirdf", class(nirdf))
    message("\nReturning a nirdf object.")
    return(nirdf)

  } else {
    measure_columns_prefix <- "X"
    message(paste0("\nRemoving measurement column prefix: ", measure_columns_prefix))
    names(nirdf) <- gsub(paste0("^",measure_columns_prefix), "", names(nirdf))
    class(nirdf) <- c("nirdf", class(nirdf))
    message("\nReturning a nirdf object.")
    return(nirdf)

  }


}

#' Testa se o objeto é um nirdf
#'
#' Esta função retorna `TRUE` para nirdf, e `FALSE` para outros objetos que não se adequam ao conceito de um nirdf.
#'
#' @param x Um objeto.
#'
#' @return `TRUE` se o objeto herda da classe `nirdf`
#' @export
is_nirdf <- function(x) {
  inherits(x, "nirdf")
}


# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.nirdf <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- "data.frame"
  x
}