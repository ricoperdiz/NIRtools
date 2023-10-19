library("tinytest")
library("NIRtools")
library("data.table")

## Checa número de amostras
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
params_file_path <-
  system.file("extdata", "teste_comListaEspecimes-NIRparams.txt", package = "NIRtools")
## Test with indication of a list of specimens
nirset_summarised <- summarise_NIRdataset(x, params_file_path)
nir_params <- read_NIRparams(params_file_path)

#### tests
expect_true(length(scan(nir_params$individual_list)) == nirset_summarised$`N samples`)
