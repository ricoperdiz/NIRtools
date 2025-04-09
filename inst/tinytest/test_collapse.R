library("NIRtools")
library("tinytest")
library("data.table")
library("magrittr")
library("collapse")

## Objetos para testar na funcao
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "conjunto1-NIRparams.txt", package = "NIRtools")
nir_params <- read_NIRparams(arq = params_file_path)

nir <-
  build_NIRdataset(dframe = x, params_file_path = params_file_path)
nir2 <-
  build_NIRdataset2(dframe = x, params_file_path = params_file_path)

microbenchmark::microbenchmark(
  collapse = build_NIRdataset2(dframe = x, params_file_path = params_file_path),
  dt = build_NIRdataset(dframe = x, params_file_path = params_file_path)
)


nir2 %>% fselect(1:6) %>% head(6)
nir %>% fselect(1:6) %>% head(6)


nirdad <- nirdf(nir, "SP1", measure_columns = grep("^X", names(nir), value = TRUE), measure_columns_prefix = "X")
nirdad2 <- nirdf2(nir2, "SP1", measure_columns = grep("^X", names(nir2), value = TRUE), measure_columns_prefix = "X")


#### tests
expect_equivalent(nir, nir2)
expect_equal(dim(nir), dim(nir2))
expect_equivalent(nirdad, nirdad2)

#### Benchmarks
microbenchmark::microbenchmark(
  collapse = dframe_tbl_melted %>% fgroup_by(c(individual_id, var_name)) %>%  get_vars("value") %>%  fmean,
  collapse2 = dframe_tbl_melted |> fgroup_by(c(individual_id, var_name)) |> get_vars(vars = "value") %>% fmean(),
  collapse3 = fmean(get_vars(fgroup_by(dframe_tbl_melted, c(individual_id, var_name)), vars = "value"))
,
  dt = dframe_tbl_melted[, list(mean = mean(value)), by = c(individual_id, "variable")]
  )

microbenchmark::microbenchmark(
  collapse = pivot(dframe_long2,
                   how = "wider"),
  dt = dcast(dframe_long, ... ~ variable, value.var = "mean")
)
microbenchmark::microbenchmark(
  collapse = fsubset(dframe_tbl, get(surface_id) == surface),
  dt = subset(dframe_tbl, get(surface_id) == surface)
)

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



