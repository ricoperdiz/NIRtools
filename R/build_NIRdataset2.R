#' Build NIR dataset based on a parameters file.
#'
#' @param dframe A dataframe in long format, that is, each NIR variable must be stored in its own column, and other columns containing identifications for individuals, and leaf surface, at least.
#' @param params_file_path A path for a NIR parameter file.
#' @param save_RDS If you want to save a .RDS file with final result, just use TRUE instead. Default value is FALSE.
#' @param save_txt If you want to save a text delimited file with final result, just use TRUE instead. Default value is FALSE.
#' @return A dataframe.
#' @importFrom collapse fselect pivot fmean get_vars fgroup_by fsubset
#' @export
build_NIRdataset2 <- function(dframe, params_file_path, save_RDS = FALSE, save_txt = FALSE) {
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
          fselect(dframe_tbl, individual_id, surface_id, nir_cols_names)
      } else {
        message(paste0("Variable `group_id`: ", group_id))
        dframe_res <-
          fselect(dframe_tbl, individual_id, group_id, surface_id, nir_cols_names)
      }
    }
    ## Only mean of reads
    if (reads == "mean") {
      message(paste0("Variable `reads`: ", reads))
      message(paste0("Building dataset: ", dataset_name, ".\nMean of reads from both sides grouped by variable `", individual_id, "`"))
      var_name <- "variable"
      if (group_id == "" | is.na(group_id)) {
        dframe_tbl_melted <-
          pivot(dframe_tbl,
            ids = individual_id,
            values = nir_cols
          )

        dframe_long <-
          fmean(get_vars(fgroup_by(dframe_tbl_melted, c(individual_id, var_name)), vars = "value"))

        dframe_res <-
          pivot(dframe_long,
            how = "wider"
          )
      } else {
        message(paste0("Variable `group_id`: ", group_id))

        dframe_tbl_melted <-
          pivot(dframe_tbl,
            ids = c(individual_id, group_id),
            values = nir_cols
          )

        dframe_long <-
          fmean(get_vars(fgroup_by(dframe_tbl_melted, c(individual_id, group_id, var_name)), vars = "value"))

        dframe_res <-
          pivot(dframe_long,
            how = "wider"
          )
      }
    }
  }

  ## Abaxial or adaxial surface
  if (surface != "both") {
    message(paste0("Which `surface` was chosen: ", surface, " side"))
    var_name <- "variable"

    # subset data for only surface of interest
    dframe_tbl_surfaceFiltered <-
      fsubset(dframe_tbl, get(surface_id) == surface)

    ## All reads
    if (reads == "all") {
      message(paste0("Variable `reads`: ", reads))
      message(paste0("Building dataset: ", dataset_name, ".\nAll reads from ", surface, " side grouped by variable `", individual_id, "`"))

      if (group_id == "" | is.na(group_id)) {
        dframe_res <-
          fselect(dframe_tbl, individual_id, surface_id, nir_cols_names)
      } else {
        message(paste0("Variable `group_id`: ", group_id))
        dframe_res <-
          fselect(dframe_tbl, individual_id, group_id, surface_id, nir_cols_names)
      }
    }

    ## Only mean of reads
    if (reads == "mean") {
      message(paste0("Variable `reads`: ", reads))
      message(paste0("Building dataset: ", dataset_name, ".\nMean of reads from both sides grouped by variable `", individual_id, "`"))

      if (group_id == "" | is.na(group_id)) {
        dframe_tbl_surfaceFiltered_melted <-
          pivot(dframe_tbl_surfaceFiltered,
            ids = individual_id,
            values = nir_cols_names
          )
        dframe_long <-
          fmean(get_vars(fgroup_by(dframe_tbl_surfaceFiltered_melted, c(individual_id, var_name)), vars = "value"))
        dframe_res <-
          pivot(dframe_long,
            how = "wider"
          )
      } else {
        message(paste0("Variable `group_id`: ", group_id))

        dframe_tbl_surfaceFiltered_melted <-
          pivot(dframe_tbl_surfaceFiltered,
            ids = c(individual_id, group_id),
            values = nir_cols_names
          )

        dframe_long <-
          fmean(get_vars(fgroup_by(dframe_tbl_surfaceFiltered_melted, c(individual_id, group_id, var_name)), vars = "value"))

        dframe_res <-
          pivot(dframe_long,
            how = "wider"
          )
      }
    }
  }


  if (save_RDS == TRUE) {
    message(paste0("Saving object dframe_res as .RDS file"))
    cd <- getwd()
    # Check current directory - is it different from wd?
    if (wd != cd) {
      message(
        sprintf(
          "Working directory pointed by params_file is different than current directory.\n
Working diretory in params_file: %s\n
Current directory: %s\n
File will be saved then in current directory.",
          wd,
          cd
        )
      )
      wd <- cd
      saveRDS(dframe_res, file = paste0(wd, dataset_name, ".RDS"))
    }
    # If not different, then use wd from params file
    saveRDS(dframe_res, file = paste0(wd, "/", dataset_name, ".RDS"))
  }

  if (save_txt == TRUE) {
    message(paste0("Saving object dframe_res as .txt file"))
    cd <- getwd()
    # Check current directory - is it different from wd?
    if (wd != cd) {
      message(sprintf(
        "Working directory pointed by params_file is different than current directory.\n
        Working diretory in params_file: %s\n
        Current directory: %s\n
        File will be saved then in current directory.",
        wd,
        cd
      ))
      wd <- cd
      fwrite(dframe_res, file = paste0(wd, dataset_name, ".txt"), sep = "\t")
    }
    # If not different, then use wd from params file
    fwrite(dframe_res, file = paste0(wd, "/", dataset_name, ".txt"), sep = "\t")
  }

  # If individual_list is specified, then it will be used to filter dframe_res

  if (individual_list != "") {
    ind_list <- scan(individual_list, what = "character")
    if (!is.vector(ind_list)) {
      stop("Variable `individual_list` must point to a file containing list of specimens and it must return a vector.")
    }

    dframe_res_filtered <- fsubset(dframe_res, get(individual_id) %in% ind_list)
    # return data.frame with results and class "nir_df"

    class(dframe_res_filtered) <- c("nirdf", class(dframe_res_filtered))

    return(dframe_res_filtered)
  } else {
    # return data.frame with results and class "nir_df"
    class(dframe_res) <- c("nirdf", class(dframe_res))
    return(dframe_res)
  }
}
