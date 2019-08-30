## ------------------------------------------------------------------------
library(NIRtools)
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
readLines('test-NIRparams.txt')

## ------------------------------------------------------------------------
read_NIRparams('test-NIRparams.txt')

## ------------------------------------------------------------------------
head(subset01)[,1:10]

## ---- eval = FALSE-------------------------------------------------------
#  pwalk(list(subset_names, surface_values, reads_values, individual_id, surface_id, group_id, nir_id), ~write_NIRparams(file = ..1, wd = '.', surface = ..2, reads = ..3, individual_id = ..4, surface_id = ..5, group_id = ..6, nir_id = ..7))

