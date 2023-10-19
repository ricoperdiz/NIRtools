library("tinytest")
library("NIRtools")
library("data.table")
library("magrittr")
library("tidyr")

## Testa a leitura de metadados de dados NIR
x <-
  data.table::fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
arq <-
  system.file("extdata", "conjunto1-NIRparams.txt", package = "NIRtools")

file_text <-
  data.frame(text = readLines(arq))
file_text_sbset <-
  subset(file_text, grepl("##", text))
parameters_separated <-
  separate(
    file_text_sbset,
    text,
    into = c("value", "key", "definition"),
    sep = paste(c("## ", "\\: "), collapse = "|")
  )

setDT(parameters_separated)
nir_params <- read_NIRparams(arq = arq)

#### tests
expect_true(nir_params$surface == "abaxial")
expect_equal(length(file_text_sbset), 1)
expect_equal(as.integer(nchar(file_text_sbset)), 1059)
expect_true('data.frame' %in% class(parameters_separated))
expect_true('data.table' %in% class(parameters_separated))
expect_equal(nrow(parameters_separated), 11)
expect_equal(ncol(parameters_separated), 3)
