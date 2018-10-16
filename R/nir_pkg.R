#' Write NIR parameters file to disk
#'
#' @param file Dataset name.
#' @param wd Location of working directory. If not provided, current working directory will be used one.
#' @param surface Which leaf surface (abaxial, adaxial, both).
#' @param reads Do all the reads will be taken into account (all, mean)?
#' @param nir_variables Which NIR variables will be used (all, subset).
#' @param subset_file Location of NIR variable subset file, in case option selected in `nir_variables` was `subset`.
#' @param individual_id Which variable name corresponds to the individual?
#' @param surface_id Which variable name corresponds to the leaf surface?
#' @param group_id Which variable name corresponds to the group category?
#' @param nir_id A string that can be used to grep column names containing NIR data. Default value is '`X`, which precedes all columns with NIR data.
#'
#' @return A message that indicates the file has been saved to disk.
#' @export
write_NIRparams <- function(file = "", wd = '.', surface = "", reads = "", nir_variables = "", subset_file = "", individual_id = "", surface_id = "", group_id = "", nir_id = "X") {


  surface_var <- c('abaxial', 'adaxial', 'both')
  reads_var <- c('all', 'mean')
  nir_var <- c('all', 'subset')

  #### file ####
  if (file == "")  {
    file_name <- 'NIR_dataset'
  } else {
    file_name <- file
  }

  #### wd ####
  if(wd == '.') {
    wd <- getwd()
  }

  #### surface ####
  if(surface == "") {
    surface <- '          '
    } else if(!surface %in% surface_var) {
    stop("Invalid  `surface` value")
    }

   if(surface  %in% surface_var[1:2]) {
     if(surface_id == "") {
       stop("Argument `surface_id` is empty. You must supply a value for `surface_id`!")
     }
   }


  #### reads ####
  if(reads == "") {
    reads <- '          '
    } else if(!reads  %in% reads_var) {
    stop("Invalid `reads` value")
  }

  #### nir_variables ####
  if(nir_variables == "") {
  nir_variables <- '          '
    } else if(!nir_variables %in% nir_var) {
    stop("Invalid `nir_variables` value")
  }

  #### subset_file ####
  if(subset_file == "") {
  subset_file <- '          '
  }

  #### individual_id ####
  if(individual_id == "") {
  individual_id <- '          '
  }

  #### surface_id ####
  if(surface_id == "") {
  surface_id <- '          '
  }

  #### group_id ####
  if(group_id == "") {
  group_id <- '          '
  }


  params_file <- paste0(file_name, '-NIR.txt')
  file_full <- paste0(wd, '/', params_file)


  # write params file
  sink(file_full,split=FALSE,append = FALSE)

  cat("# NIR dataset description - Dataset name and variables for its construction", sep = '\n')
  cat(paste0(file_name, " ## [dataset_name]: Dataset name."), sep = '\n')
  cat(paste0(wd, " ## [working_dir]: Location of working directory. If not provided, current working directory will be used one"), sep = '\n')
  cat(paste0(surface, " ## [surface]: Which leaf surface (abaxial, adaxial, both)"), sep = '\n')
  cat(paste0(reads, " ## [reads]: Do all the reads will be taken into account (all, mean)?"), sep = '\n')
  cat(paste0(nir_variables, " ## [nir_variables]: Which NIR variables will be used (all, subset)"), sep = '\n')
  cat(paste0(subset_file, " ## [subset_file]: Location of NIR variable subset file, in case option selected in `nir_variables` was `subset`"), sep = '\n')
  cat(paste0(individual_id, " ## [individual_id]: Which variable name corresponds to the individual?"), sep = '\n')
  cat(paste0(surface_id, " ## [surface_id]: Which variable name corresponds to the leaf surface?"), sep = '\n')
  cat(paste0(group_id, " ## [group_id]: Which name corresponds to the group category?"), sep = '\n')
  cat(paste0(nir_id, " ## [nir_id]: A string that can be used to grep column names containing NIR data. Default value is '`X`, which precedes all columns with NIR data."), sep = '\n')
  sink(file=NULL)
  closeAllConnections()


  message("New file '", params_file, "' written in directory ", wd)

  }

#' Build NIR dataset based on a parameters file.
#'
#' @param df A dataframe in long format, that is, each NIR variable must be stored in its own column, and other columns containing identifications for individuals, and leaf surface, at least.
#' @param params_file A path for a NIR parameter file.
#' @param save_RDS If you want to save a .RDS file with final result, just use TRUE instead. Default value is FALSE.
#' @param save_txt If you want to save a text delimited file with final result, just use TRUE instead. Default value is FALSE.
#' @param wd Working directory. Default value points to current working directory.
#' @return A dataframe.
#' @import data.table
#' @export
build_NIRdataset <- function(dframe, params_file, save_RDS = FALSE, save_txt = FALSE, wd = '.') {

  stopifnot(is.data.frame(dframe))

  #### wd ####
  if(wd == '.') {
    wd <- getwd()
  } else {
    wd <- nir_params$working_dir
  }
  params_file_path <- paste0(wd, '/', params_file)
  dframe_tbl <- as.data.table(dframe)

  nir_params <- read_NIRparams(params_file_path)
  dataset_name <- nir_params$dataset_name
  individual_id <- nir_params$individual_id
  individual_id_pos <- which(names(dframe_tbl) == individual_id)
  nir_id <- nir_params$nir_id
  nir_cols <- grep(nir_id, names(dframe_tbl))
  nir_cols_names <- grep(nir_id, names(dframe_tbl), value = TRUE)
  reads <- nir_params$reads
  surface <- nir_params$surface
  surface_id <- nir_params$surface_id



  #### If statements ####
  if(individual_id == "") {
    stop('Variable `individual_id` is empty. You must supply a value for it!')
  }

  if(is.na(individual_id)) {
    stop('Variable `individual_id` is empty. You must supply a value for it!')
  }

  if(surface_id %in% names(dframe)) {
    message(paste0('Variable `surface_id`: ', surface_id))
  } else {
    stop('Supplied variable `surface_id` is not present in `dframe`. You must supply a correct value for it!')
  }


  ## Both surfaces
  if(surface == 'both') {
    message(paste0('Which `surface` was chosen: ', surface, ' sides'))

    ## All reads
    if(reads == 'All') {

      message(paste0('Variable `reads`: ', reads))
      message(paste0('Building dataset: ', dataset_name, '.\nAll reads from both surfaces grouped by variable `', individual_id, '`'))

      dframe_res <-
        dframe_tbl[, .SD, .SDcols = c(individual_id, surface_id, nir_cols_names)]

    }

    ## Only mean of reads
    if(reads == 'mean') {

      message(paste0('Variable `reads`: ', reads))
      message(paste0('Building dataset: ', dataset_name, '.\nMean of reads from both sides grouped by variable `', nir_params$individual_id, '`'))

      dframe_tbl_melted <-
        melt(dframe_tbl, id.vars = individual_id, measure.vars = nir_cols)
      dframe_long <-
        dframe_tbl_melted[,list(mean=mean(value)), by = c(individual_id, 'variable')]

      dframe_res <-
        dcast(dframe_long, ... ~ variable, value.var = 'mean')
    }
  }

  ## Abaxial or adaxial surface
  if(surface != "both") {
    message(paste0('Which `surface` was chosen: ', surface, ' side'))

    # subset data for only surface of interest
    dframe_tbl_surfaceFiltered <-
      subset(dframe_tbl, get(surface_id) == surface)

    ## All reads
    if(reads == 'all') {
      message(paste0('Variable `reads`: ', reads))
      message(paste0('Building dataset: ', dataset_name, '.\nAll reads from ', surface,' side grouped by variable `', individual_id, '`'))

      dframe_res <-
        dframe_tbl_surfaceFiltered[, .SD, .SDcols = c(individual_id,surface_id, nir_cols_names)]

    }

    ## Only mean of reads
    if(reads == 'mean') {

      message(paste0('Variable `reads`: ', reads))
      message(paste0('Building dataset: ', dataset_name, '.\nMean of reads from both sides grouped by variable `', nir_params$individual_id, '`'))

      dframe_tbl_surfaceFiltered_melted <-
        melt(dframe_tbl_surfaceFiltered, id.vars = individual_id, measure.vars = nir_cols_names)
      dframe_long <-
        dframe_tbl_surfaceFiltered_melted[ , list(mean=mean(value)), by = c(individual_id, 'variable')]
      dframe_res <-
        dcast(dframe_long, ... ~ variable, value.var = 'mean')
    }
  }

  if(save_RDS == TRUE) {
    message(paste0('Saving object dframe_res as .RDS file'))

    saveRDS(dframe_res, file = paste0(wd, nir_params$dataset_name, '.RDS'))
  }

  if(save_txt == TRUE) {
    message(paste0('Saving object dframe_res as .txt file'))

    fwrite(dframe_res, file = paste0(wd, nir_params$dataset_name, '.txt'), sep= '\t')
  }

  # return data.frame with results
  return(dframe_res)

}

#' Read NIR parameters file from disk
#'
#' @param file A path to a file.
#'
#' @return
#' @import data.table
#' @import stringr
#' @import tidyr
#' @export
read_NIRparams <- function(file) {

  file_text <-
    data.frame(text = readLines(file))
  file_text_sbset <-
    subset(file_text, grepl('##', text))
  parameters_separated <-
    tidyr::separate(file_text_sbset, text, into = c('value', 'key', 'definition'), sep = paste(c('## ', '\\: '), collapse = '|'))

  data.table::setDT(parameters_separated)

  parameters <-
    parameters_separated[,list(value, key)]

    parameters$key <-
      str_trim(gsub('\\[|\\]', '', parameters$key), 'both')


    parameters$value <-
      str_trim(parameters$value, 'both')

    parameters[, `:=`(dataset_index = cumsum(key == 'dataset_name'))]

    parameters <-
      dcast(parameters, dataset_index ~ key, value.var = 'value')

  return(parameters)


}
