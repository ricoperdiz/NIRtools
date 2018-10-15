## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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
#    group_id = 'SP1')

## ------------------------------------------------------------------------
readLines('test-NIR.txt')

## ---- eval = FALSE-------------------------------------------------------
#  read_NIRparams('test-NIR.txt')

## ---- eval = FALSE-------------------------------------------------------
#  build_NIRdataset(df = nir_data, params_file = 'test-NIR.txt', save_RDS = TRUE, save_txt = TRUE, wd = '.')

## ------------------------------------------------------------------------
nir_test <- readRDS('vignettestest.RDS')
str(nir_test)

