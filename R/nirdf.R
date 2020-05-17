#' @importFrom methods setOldClass
#' @exportClass nir_df
setOldClass(c("nirdf", "data.table", "data.frame"))

#' Title
#'
#' @param x A data.frame, or a data.table object.
#' @param category A string pointing the name of the category variable of nirdf object.
#' @param measure_columns A vector containing column names that contain NIR measurements.
#'
#' @return A nirdf object.
#' @export
#'
#' @examples
nirdf <- function(x, category, measure_columns, measure_columns_prefix = NULL) {

  # nir_data <- data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
  # library(magrittr)
  # x = nir_data
  # x %>% names
  # category = "SP1"
  # measure_columns <- gsub("^X", "", names(nir_data))
  # length(grep("^X", names(nir_data), value = TRUE))


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
  nirdf <- data.table::as.data.table(x)
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