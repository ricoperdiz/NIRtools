library("tinytest")
library("NIRtools")
library("data.table")
library("purrr")

## Grava um arquivo de metadados NIR sem uma lista de espécimes
surface_values <- 'both'
surface_id <- 'face'
reads_values <- 'mean'
subset_names <-
  paste('test_without_listSpecimens',
        reads_values,
        surface_values,
        'all',
        sep = '_')
individual_id <- 'especimenid'
group_id <- 'SP1'
nir_id <- 'X'
metadata <-
  data.frame(
    subset_names,
    surface_values,
    reads_values,
    individual_id,
    surface_id,
    group_id,
    nir_id
  )

expect_message(pwalk(
  list(
    subset_names,
    surface_values,
    reads_values,
    individual_id,
    surface_id,
    group_id,
    nir_id
  ),
  ~ write_NIRparams(
    file = ..1,
    surface = ..2,
    reads = ..3,
    individual_id = ..4,
    surface_id = ..5,
    group_id = ..6,
    nir_id = ..7
  )
))


# remove file created in this test
fs::file_delete(paste0(subset_names, "-NIRparams.txt"))



## Grava metadados NIR com uma lista de especimes
surface_values <- 'both'
surface_id <- 'face'
reads_values <- 'mean'
subset_names <-
  paste('test_with_listSpecimens',
        reads_values,
        surface_values,
        'all',
        sep = '_')
individual_id <- 'especimenid'
individual_list <-
  system.file("extdata", "individual_list.txt", package = "NIRtools")
group_id <- 'SP1'
nir_id <- 'X'
metadata <-
  data.frame(
    subset_names,
    surface_values,
    reads_values,
    individual_id,
    surface_id,
    group_id,
    nir_id
  )

expect_message(pwalk(
  list(
    subset_names,
    surface_values,
    reads_values,
    individual_id,
    individual_list,
    surface_id,
    group_id,
    nir_id
  ),
  ~ write_NIRparams(
    file = ..1,
    surface = ..2,
    reads = ..3,
    individual_id = ..4,
    individual_list = ..5,
    surface_id = ..6,
    group_id = ..7,
    nir_id = ..8
  )
))

# remove file created in this test
fs::file_delete(paste0(subset_names, "-NIRparams.txt"))