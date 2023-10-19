# test_that("Confusion matrix LABOTAM", {
#     ## Test with indication of a list of specimens
#   library(NIRtools)
#
#
#   dframe <- nir_df
#   params_file_path <- './tests/testthat/test_with_listSpecimens_mean_both_all-NIRparams.txt'
#   dad_lda_loo <- lda_loo(df = dframe, nir_params_path = params_file_path)
#   classe_abrev <-
#     dad_lda_loo$PorAmostra$Classe
#   predito_como <-
#     dad_lda_loo$PorAmostra$PreditoComo
#   xtab <- table(classe_abrev, predito_como)
#   confusao_lab(xtab, add_CP = TRUE)
#     confu
#   nirset_summarised <- summarise_NIRdataset(dframe, params_file_path)
#   nir_params <- read_NIRparams(params_file_path)
#
# expect_true(length(unique(dframe[[nir_params$individual_id]])) == nirset_summarised$`N samples`)
#
#   }
#
# )