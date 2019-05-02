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
#' # Write to disk a parameter file
#' write_NIRparams(file = "test", wd = getwd(), reads = "mean", surface = "abaxial", nir_variables = "all", surface_id = "face", individual_id = "especimenid", individual_list = NULL, group_id = "SP1", nir_id = "X")
#'
#' # Read parameter file called "test" with function `read_NIRparams()`. It returns a data.frame
#' read_NIRparams("test-NIR.txt")
#'
#' # You can also check parameter file with funciont `readLines()`
#' readLines("test-NIR.txt")
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
      stop("If supplied, `individual_list` must be apath to a file containing a specimen identifier, one per line.")
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

#' Build NIR dataset based on a parameters file.
#'
#' @param df A dataframe in long format, that is, each NIR variable must be stored in its own column, and other columns containing identifications for individuals, and leaf surface, at least.
#' @param params_file A path for a NIR parameter file.
#' @param save_RDS If you want to save a .RDS file with final result, just use TRUE instead. Default value is FALSE.
#' @param save_txt If you want to save a text delimited file with final result, just use TRUE instead. Default value is FALSE.
#' @return A dataframe.
#' @importFrom data.table as.data.table dcast fwrite melt
#' @export
build_NIRdataset <- function(dframe, params_file_path, save_RDS = FALSE, save_txt = FALSE) {

  stopifnot(is.data.frame(dframe))

  dframe_tbl <- as.data.table(dframe)

  nir_params <- read_NIRparams(params_file_path)
  dataset_name <- nir_params$dataset_name
  individual_id <- nir_params$individual_id
  individual_id_pos <- which(names(dframe_tbl) == individual_id)
  individual_list <- nir_params$individual_list
  group_id <- nir_params$group_id
  nir_id <- nir_params$nir_id
  nir_cols <- grep(nir_id, names(dframe_tbl))
  nir_cols_names <- grep(nir_id, names(dframe_tbl), value = TRUE)
  reads <- nir_params$reads
  surface <- nir_params$surface
  surface_id <- nir_params$surface_id
  wd <- nir_params$working_dir

  #### If statements ####
  if (individual_id == "") {
    stop("Variable `individual_id` is empty. You must supply a value for it!")
  }

  if (is.na(individual_id)) {
    stop("Variable `individual_id` is empty. You must supply a value for it!")
  }

  if (surface_id %in% names(dframe)) {
    message(paste0("Variable `surface_id`: ", surface_id))
  } else {
    stop("Supplied variable `surface_id` is not present in `dframe`. You must supply a correct value for it!")
  }

  if (group_id == "" | is.na(group_id)) {
    message("\n\n\n######## Variable `group_id` is empty ########\n\n\n")
  }


  ## Both surfaces
  if (surface == "both") {
    message(paste0("Which `surface` was chosen: ", surface, " sides"))

    ## All reads
    if (reads == "all") {
      message(paste0("Variable `reads`: ", reads))
      message(paste0("Building dataset: ", dataset_name, ".\nAll reads from both surfaces grouped by variable `", individual_id, "`"))

      if (group_id == "" | is.na(group_id)) {
        dframe_res <-
          dframe_tbl[, .SD, .SDcols = c(individual_id, surface_id, nir_cols_names)]
      } else {
        message(paste0("Variable `group_id`: ", group_id))
        dframe_res <-
          dframe_tbl[, .SD, .SDcols = c(individual_id, group_id, surface_id, nir_cols_names)]
      }
    }
    ## Only mean of reads
    if (reads == "mean") {
      message(paste0("Variable `reads`: ", reads))
      message(paste0("Building dataset: ", dataset_name, ".\nMean of reads from both sides grouped by variable `", individual_id, "`"))

      if (group_id == "" | is.na(group_id)) {
        dframe_tbl_melted <-
          melt(dframe_tbl, id.vars = individual_id, measure.vars = nir_cols)

        dframe_long <-
          dframe_tbl_melted[, list(mean = mean(value)), by = c(individual_id, "variable")]

        dframe_res <-
          dcast(dframe_long, ... ~ variable, value.var = "mean")
      } else {
        message(paste0("Variable `group_id`: ", group_id))
        dframe_tbl_melted <-
          melt(dframe_tbl, id.vars = c(individual_id, group_id), measure.vars = nir_cols)

        dframe_long <-
          dframe_tbl_melted[, list(mean = mean(value)), by = c(individual_id, group_id, "variable")]

        dframe_res <-
          dcast(dframe_long, ... ~ variable, value.var = "mean")
      }
    }
  }

  ## Abaxial or adaxial surface
  if (surface != "both") {
    message(paste0("Which `surface` was chosen: ", surface, " side"))

    # subset data for only surface of interest
    dframe_tbl_surfaceFiltered <-
      subset(dframe_tbl, get(surface_id) == surface)

    ## All reads
    if (reads == "all") {
      message(paste0("Variable `reads`: ", reads))
      message(paste0("Building dataset: ", dataset_name, ".\nAll reads from ", surface, " side grouped by variable `", individual_id, "`"))

      if (group_id == "" | is.na(group_id)) {
        dframe_res <-
          dframe_tbl_surfaceFiltered[, .SD, .SDcols = c(individual_id, surface_id, nir_cols_names)]
      } else {
        message(paste0("Variable `group_id`: ", group_id))
        dframe_res <-
          dframe_tbl_surfaceFiltered[, .SD, .SDcols = c(individual_id, group_id, surface_id, nir_cols_names)]
      }
    }

    ## Only mean of reads
    if (reads == "mean") {
      message(paste0("Variable `reads`: ", reads))
      message(paste0("Building dataset: ", dataset_name, ".\nMean of reads from both sides grouped by variable `", individual_id, "`"))

      if (group_id == "" | is.na(group_id)) {
        dframe_tbl_surfaceFiltered_melted <-
          melt(dframe_tbl_surfaceFiltered, id.vars = individual_id, measure.vars = nir_cols_names)
        dframe_long <-
          dframe_tbl_surfaceFiltered_melted[, list(mean = mean(value)), by = c(individual_id, "variable")]
        dframe_res <-
          dcast(dframe_long, ... ~ variable, value.var = "mean")
      } else {
        message(paste0("Variable `group_id`: ", group_id))

        dframe_tbl_surfaceFiltered_melted <-
          melt(dframe_tbl_surfaceFiltered, id.vars = c(individual_id, group_id), measure.vars = nir_cols_names)

        dframe_long <-
          dframe_tbl_surfaceFiltered_melted[, list(mean = mean(value)), by = c(individual_id, group_id, "variable")]

        dframe_res <-
          dcast(dframe_long, ... ~ variable, value.var = "mean")
      }
    }
  }


  if (save_RDS == TRUE) {
    message(paste0("Saving object dframe_res as .RDS file"))

    saveRDS(dframe_res, file = paste0(wd, "/", dataset_name, ".RDS"))
  }

  if (save_txt == TRUE) {
    message(paste0("Saving object dframe_res as .txt file"))

    fwrite(dframe_res, file = paste0(wd, "/", dataset_name, ".txt"), sep = "\t")
  }

  # If individual_list is specified, then it will be used to filter dframe_res

  if (individual_list != '') {
    ind_list <- scan(individual_list)
    if (!is.vector(ind_list)) {
      stop('Variable `individual_list` must point to a file containing list of specimens and it must return a vector.')
    }

    dframe_res_filtered <- subset(dframe_res, get(individual_id)  %in%  ind_list)
    # return data.frame with results
  return(dframe_res_filtered)
  } else {
    # return data.frame with results
    return(dframe_res)
  }

}

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
#' # Input file on vignette folder
#' read_NIRparams("test-NIR.txt")
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