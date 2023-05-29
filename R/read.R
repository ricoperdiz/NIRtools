#' Lê metadados de um subconjunto de dados NIR a partir de um arquivo de texto
#'
#' @param arq Vetor de texto indicando caminho a um arquivo contendo dados NIR.
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

#' Importa arquivo da Texas Instruments
#'
#' @param file Vetor de texto indicando caminho a um arquivo com dados de leitura espectroscópica obtidos com um aparelho Texas Instruments, modelo DLPNIRNANOEVM.
#' @param sep Separador
#'
#' @return Um data.frame
#' @export
#'
#' @examples
#' arq <- system.file("extdata", "nir_raw3.csv", package = "NIRtools")
#' read_dlp_nano(file = arq)
read_dlp_nano <- function(file = "", sep = "\n") {
  dad <- scan(file, what = "character", sep = sep)
  search_pt <-
    grep("^wavelength|^absorbance", dad, ignore.case = TRUE)
  dad_df <- dad[search_pt:length(dad)]
  pkg <- "data.table"
  if (system.file(package = pkg) != "") {
    dad_out <- as.data.frame(data.table::fread(text = dad_df, sep = ","))
  } else {
    dad_out <- read.table(textConnection(dad_df), sep = ",", header = TRUE)
  }
  return(dad_out)
}

#' Importa arquivo da INNO, modelo NIR-S-G1
#'
#' @param file Vetor de texto indicando caminho a um arquivo com dados de leitura espectroscópica obtidos com um aparelho INNO, modelo NIR-S-G1.
#' @param sep Separador
#'
#' @return Um data.frame
#' @export
#'
#' @examples
#' arq <- system.file("extdata", "nir_raw4.csv", package = "NIRtools")
#' read_inno(file = arq)
read_inno <- function(file = "", sep = "\n") {
  dad <- scan(file, what = "character", sep = sep)
  search_pt <-
    grep("^wavelength|^absorbance", dad, ignore.case = TRUE)
  dad_df <- dad[search_pt:length(dad)]
  pkg <- "data.table"
  if (system.file(package = pkg) != "") {
    dad_out <- as.data.frame(data.table::fread(text = dad_df, sep = ","))
  } else {
    dad_out <- read.table(textConnection(dad_df), sep = ",", header = TRUE)
  }
  return(dad_out)
}

#' Title
#'
#' @param list_files
#' @param source
#' @param wavelength_var_name_partial
#' @param absorbance_var_name_partial
#'
#' @return Um data.frame
#' @export
#'
#' @examples
read_dirNIRraw_alt <- function(list_files, source = c("nano", "inno"), wavelength_var_name_partial = "Wavelength", absorbance_var_name_partial = "Absorbance") {

  # Lista arquivos na pasta

  pkg <- "data.table"
  if (system.file(package = pkg) != "") {
    # usa pacote data.table caso esteja instalado no computador
    dad_binded <- data.table::rbindlist(lapply(list_files, function(x) {
      if(length(source) == 2) {
        dad <- read_dlp_nano(x)
      } else {
        if (source == "nano") {
          dad <- read_dlp_nano(x)
        } else if (source == "inno") {
          dad <- read_inno(x)
        } else {
          stop("Argument `source` is wrong. Please fix it! and run again this function!")
        }
      }

      id_var <- "id"
      dad$id <- x
      wavelength_var <- grep(wavelength_var_name_partial, names(dad), value = TRUE)
      absorbance_var <- grep(absorbance_var_name_partial, names(dad), value = TRUE)
      pos_names <- which(names(dad) %in% c(id_var, absorbance_var, wavelength_var))
      dad_df <- dad[, pos_names]
      names(dad_df)[names(dad_df) == absorbance_var] <- "absorbance"
      names(dad_df)[names(dad_df) == wavelength_var] <- "wavelength"
      return(as.data.frame(dad_df))
    }))
    # agora converte do formato longo para o formato amplo
    dad_binded$wavelength <- paste0("X", dad_binded$wavelength)
    dad_wide <- data.table::dcast(dad_binded, id ~ wavelength, value.var = "absorbance")
    return(dad_wide)
  } else {
    # Usando R base
    ## Primeiro, une arquivos
    dad_binded <- do.call(rbind, lapply(list_csv_files, function(x) {
      if(length(source) == 2) {
        dad <- read_dlp_nano(x)
      } else {
        if (source == "nano") {
          dad <- read_dlp_nano(x)
        } else if (source == "inno") {
          dad <- read_inno(x)
        } else {
          stop("Argument `source` is wrong. Please fix it! and run again this function!")
        }
      }
      id_var <- "id"
      dad$id <- x
      wavelength_var <- grep(wavelength_var_name_partial, names(dad), value = TRUE)
      absorbance_var <- grep(absorbance_var_name_partial, names(dad), value = TRUE)
      pos_names <- which(names(dad) %in% c(id_var, absorbance_var, wavelength_var))
      dad_df <- dad[, pos_names]
      names(dad_df)[names(dad_df) == absorbance_var] <- "absorbance"
      names(dad_df)[names(dad_df) == wavelength_var] <- "wavelength"
      return(dad_df)
    }))
    # agora converte do formato longo para o formato amplo
    dad_wide <- reshape(dad_binded, direction = "wide", idvar = id_var, timevar = "wavelength")
    names(dad_wide) <- gsub("absorbance.", "X", names(dad_wide))
    return(dad_wide)
  }
}

