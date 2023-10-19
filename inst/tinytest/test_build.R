library("tinytest")
library("NIRtools")
library("data.table")
library("magrittr")


## Build NIR dataset for means of abaxial surface
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "conjunto1-NIRparams.txt", package = "NIRtools")
nir_params <- read_NIRparams(arq = params_file_path)

#### tests
expect_true(nir_params$dataset_index == 1)
expect_true(nir_params$dataset_name == "conjunto1")
expect_true(nir_params$group_id == 'SP1')
expect_true(nir_params$individual_id == "especimenid")
expect_true(nir_params$individual_list == "")
expect_true(nir_params$nir_id == "X")
expect_true(nir_params$nir_variables == "all")
expect_true(nir_params$reads == "mean")
expect_true(nir_params$subset_file == "")
expect_true(nir_params$surface == "abaxial")
expect_true(nir_params$surface_id == "face")
expect_true(nir_params$working_dir == "inst/extdata")

### usando especimenid == 10194
x10194 <- dplyr::filter(x, especimenid == 10194)
dim(x10194)
medias <-
  dplyr::group_by(x10194, face) %>%
  dplyr::summarise(
    mean_X10001.03 = mean(X10001.03),
    mean_X3999.64 = mean(X3999.64)) %>%
  dplyr::filter(face == nir_params$surface)
nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)
nir10194 <-
  dplyr::filter(nir , especimenid == 10194) %>% dplyr::select(X10001.03, X3999.64)

#### tests
expect_true(nir10194$X10001.03 == medias$mean_X10001.03)
expect_true(nir10194$X3999.64 == medias$mean_X3999.64)

## Build NIR dataset for means of adaxial surface
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "teste_adaxial-NIRparams.txt", package = "NIRtools")
nir_params <- read_NIRparams(arq = params_file_path)

#### tests
expect_true(nir_params$dataset_index == 1)
expect_true(nir_params$dataset_name == "teste_adaxial")
expect_true(nir_params$group_id == 'SP1')
expect_true(nir_params$individual_id == "especimenid")
expect_true(nir_params$individual_list == "")
expect_true(nir_params$nir_id == "X")
expect_true(nir_params$nir_variables == "all")
expect_true(nir_params$reads == "mean")
expect_true(nir_params$subset_file == "")
expect_true(nir_params$surface == "adaxial")
expect_true(nir_params$surface_id == "face")
expect_true(nir_params$working_dir == "inst/extdata")

### usando especimenid == 10194
x10194 <- dplyr::filter(x, especimenid == 10194)
medias <-
  dplyr::group_by(x10194, face) %>%
  dplyr::summarise(
    mean_X10001.03 = mean(X10001.03),
    mean_X3999.64 = mean(X3999.64)) %>%
  dplyr::filter(face == nir_params$surface)
nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)
nir10194 <-
  dplyr::filter(nir, especimenid == 10194) %>%
  dplyr::select(X10001.03, X3999.64)

#### tests
expect_equal(dim(x10194)[1], 8)
expect_equal(dim(x10194)[2], 1562)
expect_equal(dim(nir10194)[1], 1)
expect_equal(dim(nir10194)[2], 2)
expect_true(nir10194$X10001.03 == medias$mean_X10001.03)
expect_true(nir10194$X3999.64 == medias$mean_X3999.64)


## Build NIR dataset for all reads of both surfaces
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "testeAR-NIRparams.txt", package = "NIRtools")

### usando especimenid == 10194
x10194 <-
  dplyr::filter(x, especimenid == 10194) %>% dplyr::select(X10001.03, X3999.64)
nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)
nir10194 <-
  dplyr::filter(nir, especimenid == 10194) %>% dplyr::select(X10001.03, X3999.64)

#### tests
expect_equal(dim(x10194)[1], 8)
expect_equal(dim(x10194)[2], 2)
expect_equal(dim(nir10194)[1], 8)
expect_equal(dim(nir10194)[2], 2)
expect_equal(x10194$X10001.03, nir10194$X10001.03)
expect_equal(x10194$X3999.64, nir10194$X3999.64)


## Subset data based without a list of specimens
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "conjunto1-NIRparams.txt", package = "NIRtools")
read_NIRparams(arq = params_file_path)
nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)

# tests
expect_true(is_nirdf(nir))
expect_true(nrow(nir) == 6)
expect_true(ncol(nir) == 1559)


## Build NIR dataset for mean reads of both surfaces
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "teste_media_duasfaces-NIRparams.txt", package = "NIRtools")

### usando especimenid == 10194
x10194 <- dplyr::filter(x, especimenid == 10194)
medias <- dplyr::group_by(x10194) %>%
  dplyr::summarise(
    mean_X10001.03 = mean(X10001.03),
    mean_X3999.64 = mean(X3999.64))
nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)
nir10194 <-
  dplyr::filter(nir, especimenid == 10194) %>%
  dplyr::select(X10001.03, X3999.64)

#### tests
expect_true(nrow(x10194) == 8)
expect_true(ncol(x10194) == 1562)
expect_true(nrow(nir10194) == 1)
expect_true(ncol(nir10194) == 2)
expect_true(nir10194$X10001.03 == medias$mean_X10001.03)
expect_true(nir10194$X3999.64 == medias$mean_X3999.64)

## Subset data based without a list of specimens
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "conjunto1-NIRparams.txt", package = "NIRtools")
nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)

#### tests
expect_true(is_nirdf(nir))
expect_true(nrow(nir) == 6)
expect_true(ncol(nir) == 1559)

## Subset data based with a list of specimens
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata",
              "teste_comListaEspecimes-NIRparams.txt",
              package = "NIRtools")
nir_params <- read_NIRparams(arq = params_file_path)
individual_list <- nir_params$individual_list
ind_list <- scan(individual_list)
nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)

#### tests
expect_true(is_nirdf(nir))
expect_true(individual_list != "")
expect_true(is.vector(ind_list))
expect_equal(nrow(nir), 3)
expect_equal(ncol(nir), 1559)
expect_true(all(nir$especimenid %in% as.numeric(ind_list)))

## Check if working directory pointed by params file refers to the current directory. IF not, it will then use the current directory to save the file
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "conjunto1-NIRparams.txt", package = "NIRtools")

nir_params <- read_NIRparams(arq = params_file_path)
dataset_name <- nir_params$dataset_name
wd <- nir_params$working_dir
cd <- getwd()
expect_false(wd == cd)

## Test if build_NIRdataset works as expected if I wish to save a TXT file in this current directory, different than the one pointed by params_file
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))

## Test with indication of a list of specimens
params_file_path <-
  system.file("extdata",
              "teste_comListaEspecimes-NIRparams.txt",
              package = "NIRtools")
nir_params <- read_NIRparams(arq = params_file_path)
dataset_name <- nir_params$dataset_name
wd <- nir_params$working_dir
cd <- getwd()

nir <-
  build_NIRdataset(dframe = x,
                   params_file_path = params_file_path,
                   save_txt = TRUE)
wd <- cd
file <- paste0(dataset_name, ".txt")
expect_true(any(list.files() %in% file))

# remove arquivo gerado neste teste
fs::file_delete(file)
expect_false(any(list.files() %in% file))
