#' LDA Leave One Out
#'
#' @param df
#' @param nir_params_path A path that points to a NIR parameter file.
#' @param save.csv
#'
#' @return
#' @importFrom utils write.table
#' @importFrom stats filter prcomp predict
#' @importFrom glue glue
#' @export
#'
#' @examples
lda_loo <- function(df, nir_params_path, save.csv = FALSE) {

  # converte para data.frame
  dad <- as.data.frame(df)
  cat(paste0("Dimensoes dos dados: ", dim(dad)), sep = "\n")

  # indice das colunas que correspondem aos dados NIR
  cls <- grep("X", colnames(dad), ignore.case = F)

  # IMPLEMENTANDO O LEAVE ONE OUT (deixe um de fora)
  res.loo <- NULL # objeto para salvar os resultados

  # pega dados do nir_params_file
  nir_params <- read_NIRparams(nir_params_path)
  nome_grupo <- nir_params$dataset_name
  categoria <- nir_params$group_id
  colunaind <- nir_params$individual_id
  wd <- paste0(nir_params$working_dir, "/")

  # para cada individuo (linha em dad), faz o teste
  for (i in 1:nrow(dad)) {
    # i = 1
    individuo <- dad[i, colunaind]

    # seleciona os dados do individuo sendo testado
    teste <- dad[i, cls]
    teste.sp <- dad[i, categoria]

    # seleciona os demais individuos como modelo
    modelo <- dad[-i, cls]
    modelo.sp <- dad[-i, categoria]

    # fazer modelo discriminante
    meuLDA <- lda(modelo, grouping = modelo.sp)
    predicao <- predict(meuLDA, teste)

    p <- as.vector(predicao$posterior)
    names(p) <- colnames(predicao$posterior)

    pc <- names(p[p > 0.95]) # classes preditas com valor de p<=0.05
    cl <- as.vector(predicao$class) # classe predita
    pv <- p[cl] # valor de p da classe predita
    if (length(pc) > 0) {
      if (pc == teste.sp) {
        acerto <- "acertou significativo"
      } else {
        acerto <- "errou significativo"
      }
    } else {
      if (cl == teste.sp) {
        acerto <- "acertou não significativo"
      } else {
        acerto <- "errou não significativo"
      }
    }

    rr <-
      data.frame(
        Individuo = individuo,
        Classe = teste.sp,
        Posterior = round(predicao$posterior, 2),
        Acerto = acerto,
        PreditoComo = names(pv),
        stringsAsFactors = F
      )
    res.loo <- rbind(res.loo, rr)

    cat(glue(
      "Amostra {ind}-{nrow(dad)}, individuo {individuo} {teste_sp} predito {predicao} p={pv}% {acerto_perc}",
      ind = i,
      dad = dad,
      individuo = individuo,
      teste_sp = teste.sp,
      predicao = cl,
      pv = (round(pv, 2) * 100),
      acerto_perc = toupper(acerto)
    ),
    sep = "\n"
    )
  }

  # Tabela com resultados por amostra
  PorAmostra <- data.frame(EspectroTestado = rownames(res.loo), res.loo, stringsAsFactors = F)

  # Resultados resumidos
  tb <- table(res.loo$Acerto)
  # em porcentagem
  tbb <- round((tb / sum(tb)) * 100, 1)
  # Tabela com resumo
  resumo <- as.data.frame(cbind(Tipo = names(tb), N.Individuos = tb, Porcentagem = tbb))

  # salva os resultados
  if (save.csv == TRUE) {
    if (wd == ".") {
      save_where <- ""
      message("Working directory: ", getwd())
    } else {
      save_where <- wd
      message("Working directory: ", wd)
    }
    # resultado da loo
    write.table(res.loo, paste0(save_where, "lda_tabelaLOO_GERAL_dataset-", nome_grupo, ".csv"), sep = "\t", row.names = FALSE)
    # tabela de resultados por Amostra
    write.table(PorAmostra, paste0(save_where, "lda_tabelaLOO_poramostra_dataset-", nome_grupo, ".csv"), sep = "\t", row.names = FALSE)
    # Tabela com resumo
    write.table(resumo, paste0(save_where, "lda_tabelaLOO_resumo_dataset-", nome_grupo, ".csv"), sep = "\t", row.names = FALSE)
  }

  resultado <-
    list(
      geral = res.loo,
      PorAmostra = PorAmostra,
      resumo = resumo
    )

  return(resultado)
}
