#' Sumariza conjuntos de dados NIR.
#'
#' @param dframe Dados brutos NIR.
#' @param params_file_path Localização do arquivo de parâmetros NIR.
#' @param use_cwd Usa o atual diretório de trabalho?
#' @return Um data.frame contendo o nome do conjunto de dados especificado no arquivo de metadados, número de amostras, número de taxa, e número de variáveis NIR.
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' library("NIRtools")
#' dados <-
#' read.table(system.file("extdata", "nir_data.csv", package = "NIRtools"), sep = "\t", header = TRUE)
#' arq_metadado <-
#' system.file("extdata", "teste_comListaEspecimes-NIRparams.txt", package = "NIRtools")
#' ## Teste com indicacao de uma lista de especimes
#' nirset_summarised <-
#' summarise_NIRdataset(dframe = dados, params_file_path = arq_metadado)
summarise_NIRdataset <- function(dframe, params_file_path, use_cwd = TRUE) {
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


  nirset <- build_NIRdataset(dframe, params_file_path)

  n_samples <- length(unique(nirset[[individual_id]]))
  n_taxa <- length(unique(nirset[[group_id]]))
  n_variables <- length(nir_cols_names)

  res <- data.table(
    `Dataset` = dataset_name,
    `N samples` = n_samples,
    `N taxa` = n_taxa,
    `N variables` = n_variables
  )
  return(res)
}