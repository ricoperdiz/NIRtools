#' Write NIR parameters file to disk
#'
#' @param file Dataset name.
#' @param wd Location of working directory. If not provided, current working directory will be used one.
#' @param surface Which leaf surface (abaxial, adaxial, both).
#' @param reads Do all the reads will be taken into account (all, mean)?
#' @param nir_variables Which NIR variables will be used (all, subset).
#' @param subset_file Location of NIR variable subset file, in case option selected in `nir_variables` was `subset`.
#' @param individual_id Which variable name corresponds to the individual?
#' @param individual_list Location of a file containing list of specimens to subset data when building datasets with `build_NIRdataset` function.
#' @param surface_id Which variable name corresponds to the leaf surface?
#' @param group_id Which variable name corresponds to the group category?
#' @param nir_id A string that can be used to grep column names containing NIR data. Default value is '`X`, which precedes all columns with NIR data.
#'
#' @return A message that indicates the file has been saved to disk.
#' @export
#' @examples
#' \dontrun{
#' write_NIRparams(file = "teste", wd = ".",
#'  reads = "mean", surface = "abaxial",
#'   nir_variables = "all", surface_id = "face",
#'    individual_id = "especimenid",
#'    individual_list = NULL, group_id = "SP1",
#'     nir_id = "X")
#' read_NIRparams("teste-NIR.txt")
#' readLines("teste-NIR.txt")
#' }
write_NIRparams <- function(file = "", wd = ".", surface = "", reads = "", nir_variables = "", subset_file = "", individual_id = "", individual_list = NULL, surface_id = "", group_id = "", nir_id = "X") {
  surface_var <- c("abaxial", "adaxial", "both")
  reads_var <- c("all", "mean")
  nir_var <- c("all", "subset")

  #### file ####
  if (file == "") {
    file_name <- "NIR_dataset"
  } else {
    file_name <- file
  }

  #### wd ####
  if (wd == ".") {
    wd <- getwd()
  }

  #### surface ####
  if (surface == "") {
    surface <- "          "
  } else if (!surface %in% surface_var) {
    stop("Invalid  `surface` value")
  }

  if (surface %in% surface_var[1:2]) {
    if (surface_id == "") {
      stop("Argument `surface_id` is empty. You must supply a value for `surface_id`!")
    }
  }


  #### reads ####
  if (reads == "") {
    reads <- "          "
  } else if (!reads %in% reads_var) {
    stop("Invalid `reads` value")
  }

  #### nir_variables ####
  if (nir_variables == "") {
    nir_variables <- "          "
  } else if (!nir_variables %in% nir_var) {
    stop("Invalid `nir_variables` value")
  }

  #### subset_file ####
  if (subset_file == "") {
    subset_file <- "          "
  }

  #### individual_id ####
  if (individual_id == "") {
    individual_id <- "          "
  }

  #### individual_list ####
  if (!is.null(individual_list)) {
    if (file.access(individual_list) != 0) {
      stop("If supplied, `individual_list` must be a path to a file containing a specimen identifier, one per line.")
    }
  } else {
    individual_list <- "          "
  }

  #### surface_id ####
  if (surface_id == "") {
    surface_id <- "          "
  }

  #### group_id ####
  if (group_id == "") {
    group_id <- "          "
  }


  params_file <- paste0(file_name, "-NIRparams.txt")
  file_full <- paste0(wd, "/", params_file)


  # write params file
  sink(file_full, split = FALSE, append = FALSE)

  cat("# NIR dataset description - Dataset name and variables for its construction", sep = "\n")
  cat(paste0(file_name, " ## [dataset_name]: Dataset name."), sep = "\n")
  cat(paste0(wd, " ## [working_dir]: Location of working directory. If not provided, current working directory will be used one"), sep = "\n")
  cat(paste0(surface, " ## [surface]: Which leaf surface (abaxial, adaxial, both)"), sep = "\n")
  cat(paste0(reads, " ## [reads]: Do all the reads will be taken into account (all, mean)?"), sep = "\n")
  cat(paste0(nir_variables, " ## [nir_variables]: Which NIR variables will be used (all, subset)"), sep = "\n")
  cat(paste0(subset_file, " ## [subset_file]: Location of NIR variable subset file, in case option selected in `nir_variables` was `subset`"), sep = "\n")
  cat(paste0(individual_id, " ## [individual_id]: Which variable name corresponds to the individual?"), sep = "\n")
  cat(paste0(individual_list, " ## [individual_list]: Location of a file containing list of specimens to subset data when building datasets with `build_NIRdataset` function."), sep = "\n")
  cat(paste0(surface_id, " ## [surface_id]: Which variable name corresponds to the leaf surface?"), sep = "\n")
  cat(paste0(group_id, " ## [group_id]: Which name corresponds to the group category?"), sep = "\n")
  cat(paste0(nir_id, " ## [nir_id]: A string that can be used to grep column names containing NIR data. Default value is '`X`, which precedes all columns with NIR data."), sep = "\n")
  sink(file = NULL)
  closeAllConnections()


  message("New file '", params_file, "' written in directory ", wd)
}
