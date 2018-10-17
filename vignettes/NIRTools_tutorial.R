## ------------------------------------------------------------------------
head(nir_data)[,1:10]

## ---- eval = FALSE-------------------------------------------------------
#  write_NIRparams(
#    file = 'test',
#    wd = getwd(),
#    reads = 'mean',
#    surface = 'abaxial',
#    nir_variables = 'all',
#    surface_id = 'face',
#    individual_id = 'especimenid',
#    group_id = 'SP1',
#    nir_id = 'X')

## ------------------------------------------------------------------------
readLines('test-NIR.txt')

## ---- eval = FALSE-------------------------------------------------------
#  read_NIRparams('test-NIR.txt')

## ---- cache = TRUE-------------------------------------------------------
subset01 <- build_NIRdataset(df = nir_data, params_file = 'test-NIR.txt', wd = '.')

## ------------------------------------------------------------------------
head(subset01)[,1:10]

## ---- cache = TRUE-------------------------------------------------------
library(dplyr)
library(magrittr)
library(purrr)

subset_names <- sprintf('subset%02d', 1:3)
surface_values <- c('abaxial', 'adaxial', 'both')
surface_id <- 'face'
reads_values <- c('all', 'all', 'mean')
individual_id <- 'especimenid'
group_id <- 'SP1'
nir_id <- 'X'

metadata <- data_frame(subset_names, surface_values, reads_values, individual_id, surface_id, group_id, nir_id)


## ---- eval = FALSE-------------------------------------------------------
#  pwalk(list(subset_names, surface_values, reads_values, individual_id, surface_id, group_id, nir_id), ~write_NIRparams(file = ..1, wd = '.', surface = ..2, reads = ..3, individual_id = ..4, surface_id = ..5, group_id = ..6, nir_id = ..7))

## ------------------------------------------------------------------------
subset_files <- list.files(pattern = 'subset')
# subset 01
subset01 <- build_NIRdataset(nir_data, subset_files[1])
# subset 02
subset02 <- build_NIRdataset(nir_data, subset_files[2])
# subset 03
subset03 <- build_NIRdataset(nir_data, subset_files[3])

## ------------------------------------------------------------------------
dim(subset01)

## ------------------------------------------------------------------------
dim(subset02)

## ------------------------------------------------------------------------
dim(subset03)

## ------------------------------------------------------------------------
readLines('subset03-NIR.txt')

## ------------------------------------------------------------------------
read_NIRparams('subset03-NIR.txt')

## ------------------------------------------------------------------------
subsets <- walk2(list(nir_data), subset_files, ~build_NIRdataset(df = .x, .y))
subsets %>% str

## ------------------------------------------------------------------------
nir_params <- map_dfr(subset_files, read_NIRparams)

## ------------------------------------------------------------------------
nested_df <-
  nir_params %>% 
  mutate(
    main_dataset = list(nir_data),
    data = map2(main_dataset, subset_files, ~build_NIRdataset(df = .x, .y))
  )

## ------------------------------------------------------------------------
# subset 01
dim(nested_df$data[[1]])

## ------------------------------------------------------------------------
# subset 02
dim(nested_df$data[[2]])

## ------------------------------------------------------------------------
# subset 03
dim(nested_df$data[[3]])

