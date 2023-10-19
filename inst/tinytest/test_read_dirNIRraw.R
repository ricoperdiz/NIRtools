library("tinytest")
library("NIRtools")

## read_dirNIRraw funciona com variaveis nir em 1557 linhas?
file_name <-
  system.file("extdata", "nir_raw.csv", package = "NIRtools")
nir_raw <- read_dirNIRraw(arq = file_name, add_nir_id = TRUE)
#### tests
expect_equal(dim(nir_raw)[1], 1)
expect_equal(dim(nir_raw)[2], 1557)

## read_dirNIRraw funciona com variaveis nir em 3112 linhas?
nir_raw2 <-
  read_dirNIRraw(arq = "C:/Users/ricop/Documents/PROFISSIONAL/producao_cientifica/09_R_pkgs/NIRtools/data-raw/Sample_MP_CBM003.csv")

#### tests
expect_equal(dim(nir_raw2)[1], 1)
expect_equal(dim(nir_raw2)[2], 3112)

