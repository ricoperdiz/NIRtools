#' Cria uma funcao que seleciona 1 spectro aleatorio para cada uind
#'
#' @param x
#' @param tb
#' @param id
#' @author Alberto Vicentini (INPA), \email{alberto.vicentini@@inpa.gov.br}
#' @return
#' @export
#'
#' @examples
pegaespectro <- function(x, tb, id) {
  xx <- rownames(tb[id == x, ])
  xx <- sample(xx, 1)
  return(xx)
}

#' Cria uma funcao que seleciona 1 spectro aleatorio para cada uind
#'
#' @param x
#' @param dd
#' @param ind
#' @author Alberto Vicentini (INPA), \email{alberto.vicentini@@inpa.gov.br}
#' @return
#' @export
#'
#' @examples
pegateste <- function(x, dd, ind, teste_n = 0.3) {
  xx <- rownames(dd[ind == x, ])
  trinta <- floor(length(xx) * teste_n)
  xx <- sample(xx, trinta)
  return(xx)
}

#' Executa LDA 70-30 por padrão
#'
#' @param df
#' @param categoria
#' @param colunaind
#' @param aleatorizacao
#' @param teste_n
#' @author Alberto Vicentini (INPA), \email{alberto.vicentini@@inpa.gov.br}
#' @return
#' @export
#'
#' @examples
lda_lab <- function(df, nir_params_path, aleatorizacao, teste_n = 0.3) {

  # converte para data.frame
  dados <- as.data.frame(df)

  # pega dados do nir_params_file
  nir_params <- read_NIRparams(nir_params_path)
  nome_grupo <- nir_params$dataset_name
  categoria <- nir_params$group_id
  colunaind <- nir_params$individual_id

  # cria novos nomes de linha para auxiliar a aleatorizacao dos espectros
  rownames(dados) <- paste0("Espectro", 1:nrow(dados))

  # dados devem estar no forma amplo == WIDE
  # seus dados devem conter uma coluna para categoria a ser testada
  # e um coluna para individuos

  # REPETE n vezes A ANALISE SELECIONANDO DIFERENTES ESPECTROS E DIFERENTE INDIVIDUOS COMO DADO TESTE E DADO MODELO
  acertosporpermutacao <- NULL # objeto vazio para salvar resultados
  resultado_bruto <- NULL
  cat(paste0("Grupo: ", nome_grupo, "\n"))

  for (idx in 1:aleatorizacao) {
    # idx = 1
    # SELECIONANDO 1 ESPECTRO ALEATORIO POR INDIVIDUO
    ind <- dados[, colunaind]

    # valores unicos de individuos
    uind <- unique(ind)

    # aplica essa funcao 1 vez
    # aplica a cada valor único de individuo a funcao que irá retornar o nome da linha selecionada para cada individuo
    ll <- lapply(uind, pegaespectro, tb = dados, id = ind)
    # converte o resultado num vetor (o output da funcao ll é uma lista)
    ll <- as.vector(ll, mode = "character")

    # para filtrar dos dados brutos as linhas correspondentes, basta usar o vetor
    dat <- dados[ll, ]

    # selecionando dado teste e dado modelo
    # seleciona a coluna das categorias
    sp <- dat[, categoria]

    # extrai valores unicos de cada categoria
    usp <- unique(sp)

    # aplica a funcao pegateste
    ll <- lapply(usp, pegateste, dd = dat, ind = sp, teste_n = teste_n)

    # linhas do dado teste
    rn <- NULL
    for (l in 1:length(ll)) {
      rn <- c(rn, ll[[l]])
    }

    # linhas do dado modelo
    rn.mod <- rownames(dat)
    rn.mod <- rn.mod[!rn.mod %in% rn]

    # filtrando dados teste e modelo
    n1 <- grep("X", colnames(dat), ignore.case = F)
    teste <- dat[rn, n1]
    teste_sp <- dat[rn, categoria]

    modelo <- dat[rn.mod, n1]
    modelo.sp <- dat[rn.mod, categoria]

    # fazer modelo discriminante
    meuLDA <- lda(modelo, grouping = modelo.sp)
    predicao <- predict(meuLDA, teste)

    pt <- predicao$posterior
    id <- dat[rownames(pt), colunaind]
    nz <- as.numeric(predicao$class == teste_sp)
    perm <- rep(idx, nrow(pt))
    pt <- data.frame(
      Permutacao = perm,
      EspectroTestado = rownames(pt),
      Individuo = id,
      Categoria = teste_sp,
      Predicao = predicao$class,
      Acertou = nz,
      round(pt, 2)
    )
    rownames(pt) <- NULL
    resultado_bruto <- rbind(resultado_bruto, pt)
    poracerto <- (sum(predicao$class == teste_sp) / length(teste_sp)) * 100
    acertosporpermutacao <- c(acertosporpermutacao, poracerto)
    print(paste("Teste", idx, "teve", poracerto, "% de acerto"))
  }

  resultado_geral <-
    list(
      acertosporpermutacao = acertosporpermutacao,
      resultado.bruto = resultado_bruto,
      predicao = predicao,
      train_spp = modelo.sp,
      train_set = modelo,
      train_model = meuLDA
    )

  return(resultado_geral)
}

#' Gera um resumo dos dados da LDA 70-30 a partir do resultado gerado pela função beto_lda7.
#'
#' @param resultado.bruto
#' @param acertosporpermutacao
#' @author Alberto Vicentini (INPA), \email{alberto.vicentini@@inpa.gov.br}
#' @return
#' @export
#'
#' @examples
resumo_lda <- function(resultado.bruto, acertosporpermutacao) {
  res <- resultado.bruto
  cat("Resultado bruto - LDA")
  cat("", sep = "\n")
  print(res)
  cat("", sep = "\n")
  vl <- res$Acertou == 0 # vetor lógico dos erros
  erros <- (sum(vl) / nrow(res)) * 100 # quantos erros em%
  cat(paste0("Quantos erros em% -> ", erros))
  cat("", sep = "\n")
  cat("", sep = "\n")
  (100 - erros) == mean(acertosporpermutacao) # essa expressao precisa ser verdadeira
  cat("Espectros preditos errados")
  cat("", sep = "\n")
  table(as.vector(res[vl, "EspectroTestado"])) # spectros preditos errados
  cat("", sep = "\n")
  cat("Individuos preditos errados")
  cat("", sep = "\n")
  table(as.vector(res[vl, "Individuo"])) # individuos preditos errados
  cat("", sep = "\n")
  cat("Acertos por individuo")
  cat("", sep = "\n")
  tbb <- table(as.vector(res[, c("Individuo", "Acertou")]))
  tb <- data.frame(Erros = tbb[, 1], Acertos = tbb[, 2], stringsAsFactors = F)
  rownames(tb) <- rownames(tbb)
  tb$PorAcerto <- round(tb[, 2] / (tb[, 1] + tb[, 2]), 2) * 100 # adiciona porcentagem das permutacoes que foram acertos para cada individuo
  print(tb)
  cat("", sep = "\n")
  return(tb)
}


#' Plota resultado de uma LDA LOO.
#'
#' Copiado desta [postagem](https://www.r-bloggers.com/linear-quadratic-and-regularized-discriminant-analysis/).
#'
#' @param train_set
#' @param train_model
#'
#' @return
#' @export
#'
#' @examples
lda_lab_plot <- function(train_set, train_model, train_spp, col = FALSE, color_p = "") {
  train_prediction <-
    predict(train_model, train_set)
  lda_df <-
    data.frame(train_prediction$x, "outcome" = train_spp)

  if (all(c("LD1", "LD2") %in% names(lda_df))) {
    if (col == TRUE) {
      lda_plot <-
        ggplot(lda_df, aes(x = LD1, y = LD2, color = outcome)) +
        geom_point() +
        scale_color_manual(values = color_p)
    } else {
      lda_plot <-
        ggplot(lda_df, aes(x = LD1, y = LD2, color = outcome)) +
        geom_point() +
        scale_colour_brewer(palette = "Paired")
    }
    return(lda_plot)
  } else {
    lda_plot <-
      ggplot(lda_df, aes(x = LD1, y = outcome)) +
      ggridges::geom_density_ridges() +
      ggridges::theme_ridges()
    return(lda_plot)
  }
}

#' Executa LDA 70-30 padrão em um for loop. Apropriado para listas de data.frames.
#'
#' @param df
#' @param wd
#' @author Ricardo de Oliveira Perdiz, \email{ricardoperdiz@@yahoo.com}; Alberto Vicentini (INPA), \email{alberto.vicentini@@inpa.gov.br}
#' @return
#' @export
#'
#' @examples
lda_lab_batch <- function(df, aleatorizacao, teste_n = NULL) {
  lda_lab_list <- vector("list", nrow(df))

  if (is.null(teste_n)) {
    teste_n <- 0.3
  } else if (!is.numeric(teste_n)) {
    stop("teste_n must be a number between 0.1 and 0.4.")
  } else if (is.numeric(teste_n)) {
    if (teste_n >= 0.1 & teste_n <= 0.4) {
      teste_n <- teste_n
    } else {
      stop("teste_n must be a number between 0.1 and 0.4.")
    }
  }
  lda_type <- paste(c(1 - teste_n, teste_n) * 100, collapse = "-")


  for (i in 1:nrow(df)) {

    # i = 2
    colunaind <- df$individual_id[[i]] # coluna contendo o identificador dos individuos
    categoria <- df$group_id[[i]] # categoria (nome da coluna em dados que contem a categoria a ser testada)
    nome_grupo <- df$dataset_name[[i]]

    # executa
    lda_res <- lda_lab(df = df$data[[i]], nome_grupo = nome_grupo, categoria = categoria, colunaind = colunaind, aleatorizacao = aleatorizacao, teste_n = 0.3)
    acertosporpermutacao <- lda_res$acertosporpermutacao
    resultado.bruto <- lda_res$resultado.bruto

    # prepara dados para plotar histograma
    resultado <- resumo_lda(resultado.bruto, acertosporpermutacao)

    # concatena resultados na lisat lda_lab_list
    lda_lab_list[[i]] <- list(nome_grupo = nome_grupo, lda_type = lda_type, aleatorizacao = aleatorizacao, lda_res = lda_res, resultado = resultado)
  }

  return(lda_lab_list)
}

#' Salva o resultado da LDA 70-30 (padrão) na forma de um pdf com histograma, e o conjunto de dados em um RDS.
#'
#' @param lda_lab_list
#' @param save.pdf
#' @param save.RDS
#' @param wd
#' @author Ricardo de Oliveira Perdiz, \email{ricardoperdiz@@yahoo.com}; Alberto Vicentini (INPA), \email{alberto.vicentini@@inpa.gov.br}
#' @return
#' @export
#'
#' @examples
save_lda <- function(lda_lab_list, save.pdf = TRUE, save.RDS = TRUE, wd = NULL) {

  # pasta para salvar dados
  if (is.null(wd)) {
    wd <- getwd()
  } else {
    wd <- wd
  }

  for (i in seq_along(lda_lab_list)) {
    if (save.pdf == TRUE) {
      # plota histograma
      pdf(paste0(wd, "plot_lda_", lda_lab_list[[i]]$lda_type, "_", lda_lab_list[[i]]$aleatorizacao, "x_DATASET-", lda_lab_list[[i]]$nome_grupo, ".pdf"), width = 8, height = 4)
      hist(lda_lab_list[[i]]$lda_res$acertosporpermutacao, xlab = "% de Acerto", col = "red", main = paste0("Resultado das Permutações \n Dataset ", lda_lab_list[[i]]$nome_grupo), sub = paste0("Acerto médio ", mean(lda_lab_list[[i]]$resultado$PorAcerto)))
      abline(v = mean(lda_lab_list[[i]]$lda_res$acertosporpermutacao), lwd = 3, col = "blue")
      dev.off()
    }
    if (save.RDS == TRUE) {
      # salve resultados em um RDS
      saveRDS(lda_lab_list[[i]], paste0(wd, "lda_", lda_lab_list[[i]]$lda_type, "_", lda_lab_list[[i]]$aleatorizacao, "x_DATASET-", lda_lab_list[[i]]$nome_grupo, ".RDS"))
    }
  }
}

#' Faz LDA LOO em batch
#'
#' @param df
#' @param wd
#' @param times
#' @author Ricardo de Oliveira Perdiz, \email{ricardoperdiz@@yahoo.com}; Alberto Vicentini (INPA), \email{alberto.vicentini@@inpa.gov.br}
#' @return
#' @export
#'
#' @examples
lda_loo_batch <- function(df, times = 1, dataset_name = NULL, wd = wd) {
  lda_loo_list <- vector("list", nrow(df))

  for (i in 1:nrow(df)) {
    var_grp <- df$individual_id[[i]] # coluna contendo o identificador dos individuos
    species <- df$group_id[[i]] # categoria (nome da coluna em dados que contem a categoria a ser testada)
    lda_loo_res <- list()

    # POR ENQUANTO ESTA ABANDONADA ESTA IDEIA DE FAZER VARIAS VEZES
    # ----------------------------------------------------------------------
    # for(quant in seq_along(times)) {

    # executa
    # lda_loo_res[[quant]] <- lda_loo(df = df$data[[i]], categoria = species, colunaind = var_grp, save.csv = TRUE, wd = wd)
    # }
    # ----------------------------------------------------------------------

    lda_loo_res <- lda_loo(df = df$data[[i]], categoria = species, colunaind = var_grp, save.csv = FALSE, wd = wd)

    # salve resultados em um RDS
    # saveRDS(lda_loo_res, paste0(wd,'lda_LOO_', times, 'xDATASET-', df$dataset_name[i], '.RDS'))

    # cria objetos com o resultado da lda_loo
    # esses mesmos objetos serao salvos em sua pasta de trabalha
    # buscar pelos arquivos .csv que contenham os nomes "lda_tabelaLOOresumo_dataset" e "lda_tabelaLOOporamostra_dataset"
    por_amostra <- lda_loo_res$PorAmostra
    resumo <- lda_loo_res$resumo
    xtab <- table(por_amostra$Classe, por_amostra$PreditoComo)

    # plota matriz de confusao
    ## Use o resultado da lda leave one out para plotar a mtriz de confusao abaixo
    ## E necessario utilizar os pacotes caret e ggplot2
    mtrx_confusao <- confusao(por_amostra)

    ggsave(paste0(wd, "fig-confusao_lda_LOO_DATASET-", df$dataset_name[i], ".pdf"), device = "pdf", width = 8, height = 8)

    lda_loo_list[[i]] <- list(lda_loo_res = lda_loo_res, xtab = xtab, mtrx_confusao = mtrx_confusao)
  }

  # salva todos resultados em um RData
  if (is.null(dataset_name)) {
    dataset_name <- "geral"
    save(list = c("lda_loo_list"), file = paste0(wd, "lda_LOO_", times, "xDATASET-", dataset_name, ".RData"))
  } else {
    save(list = c("lda_loo_list"), file = paste0(wd, "lda_LOO_", times, "xDATASET-", dataset_name, ".RData"))
  }

  return(lda_loo_list)
}

#' Tabela com resultados de acertos por espécie da LDA
#'
#' @param resultado_bruto
#'
#' @return
#' @export
#'
#' @examples
visualiza_res_modelos <- function(resultado_bruto) {
  # resultado_bruto <- lda_result[[1]]$lda_res$resultado.bruto

  # quais spp
  spp <- sort(unique(as.character(resultado_bruto$Categoria)))

  species <- NULL
  certainty <- NULL
  # model <- NULL
  for (i in seq_along(spp)) {
    # i <- 1
    sp1 <- spp[i]
    sp1
    tb <- resultado_bruto[resultado_bruto$Categoria == sp1, ]
    acerto <- nrow(tb[tb$Acertou == 1, ])
    acerto_persp1 <- round(acerto / nrow(tb) * 100, 2)

    species <- c(species, sp1)
    certainty <- c(certainty, acerto_persp1)
  }
  res <- data.frame(species = species, certainty = certainty)

  return(res)
}

#' Prepara dados para PCA
#'
#' @param df
#' @param var_grp
#' @param var_color_grp
#' @author Ricardo de Oliveira Perdiz
#' @return
#' @export
#'
#' @examples
prep_pca <- function(df, var_grp, var_color_grp = NULL) {
  rownames_dad <- df[[var_grp]]

  # prepara matriz DADOS COMPLETOS
  dad_mtrx <-
    as.data.frame(dplyr::select(df, var_grp, contains("X")))

  dad_mtrx <-
    as.matrix(dad_mtrx[, -which(names(dad_mtrx) %in% var_grp)])
  rownames(dad_mtrx) <- rownames_dad
  # grupo
  if (!is.null(var_color_grp)) {
    color_grp <- factor(df[[var_color_grp]])
  }
  return(dad_mtrx)
}

#' Roda PCA
#'
#' @param prep_pca_df
#'
#' @return
#' @export
#'
#' @examples
do_pca <- function(prep_pca_df) {
  pca_res <- prcomp(prep_pca_df, scale. = TRUE)
  return(pca_res)
}

#' Run Near Infra Red Analysis (NIRA) and plot results
#'
#' @param dataset
#' @param nir_params_path
#' @param outfig
#' @param run_analysis
#' @param aleatorizacao
#' @param add_CP
#' @return
#' @export
#'
#' @examples
run_NIRA <- function(dataset, nir_params_path, outfig = ".", run_analysis = c("plot_raw", "PCA", "PCA_plot", "LDA", "LDA_plot", "LOO", "LOO_plot"), aleatorizacao = 200, save.csv = TRUE, add_CP = FALSE, use_date = FALSE, col_p = cores, use_col = TRUE, outgroup = "") {
  # run_analysis = 'LL'
  # if (run_analysis  %in% c('plot_raw', 'PCA', 'LDA', 'LOO'))
  # stop('Only values allowed in `run_analysis` are "PCA", "LDA", and "LOO"')
  # dataset = dad
  # nir_params_path = nir_params_path
  # outfig = "./figure/cap01"
  # aleatorizacao = 1
  # run_analysis = c("plot_raw", "PCA", 'PCA_plot', "LDA", "LDA_plot", "LOO", "LOO_plot")
  #

  nir_params <- read_NIRparams(nir_params_path)
  categoria <- nir_params$group_id
  dataset_name <- nir_params$dataset_name
  group_id <- nir_params$group_id
  nir_id <- nir_params$nir_id
  colunaind <- nir_params$individual_id
  wd <- nir_params$working_dir

  dad <- dataset
  if (use_date == TRUE) {
    outfig_name <- paste0(outfig, "/NIR_", dataset_name, "_", gsub(" ", "_", Sys.time() %>% gsub(":", "-", .)))
  } else {
    outfig_name <- paste0(outfig, "/NIR_", dataset_name)
  }
  ## ------------------------------------------------------------------------
  # Espectros brutos por especie
  ####### prepara variaveis
  if ("plot_raw" %in% run_analysis) {
    plot_raw <-
      dad %>%
      seplyr::gather_se(
        key = "key",
        value = "value",
        columns = grep("X", names(dad), value = TRUE)
      ) %>%
      mutate_at("key", ~ gsub("X", "", .x) %>%
        as.numeric()) %>%
      group_by(SP1, key) %>%
      summarise(
        value = mean(value)
      )
    plot_raw %>% names()
    plot_raw
    eixo_x <-
      plot_raw$key %>%
      unique() %>%
      range()
    eixo_y <-
      plot_raw$value %>%
      range()
    xlab.en <- parse(text = "Wavenumber (cm^-1)")
    ylab.en <- "Absorbance"
    png(paste0(outfig_name, "_SpeciesSpectra.png"),
      res = 600,
      width = 13.8,
      height = 10,
      units = "cm"
    )
    plot(plot_raw$key, plot_raw$value, type = "n", xlab = xlab.en, ylab = ylab.en)
    if (outgroup != "") {
      # outgroup = "heptaphyllum"
      especies <- plot_raw$SP1 %>% unique() %>% sort()
      pos_og <- which(especies == outgroup)
      especies <- c(especies[-pos_og], outgroup)
    } else {
      especies <- plot_raw$SP1 %>% unique() %>% sort()
    }
    cores2 <- c(brewer.pal(8, "Dark2"), "black")
    col_legend <- NULL
    for (i in seq_along(especies)) {
      cat_plot <- filter(plot_raw, SP1 == especies[i])
      if (especies[i] %in% names(col_p)) {
        pos_cor <- which(names(col_p) == especies[i])
        col_plot <- col_p[pos_cor]
        points(cat_plot$key, cat_plot$value, cex = 0.05, col = col_plot, ylim = eixo_y)
        col_legend <- c(col_legend, col_plot)
      } else {
        points(cat_plot$key, cat_plot$value, cex = 0.05, col = cores2[i], ylim = eixo_y)
        col_legend <- c(col_legend, cores2[i])
      }
    }
    legend("topright", legend = especies, pch = 16, col = col_legend)
    dev.off()
  }

  if ("PCA" %in% run_analysis) {
    ## ------------------------------------------------------------------------
    # PCA para apresentacao
    ## ----pca_nested_df-------------------------------------------------------
    dad_pca <- do_pca(prep_pca(dad, var_grp = group_id))
    ## ----checa_pca-------------------------------------------------------
    dad_pca
  }

  if ("PCA_plot" %in% run_analysis) {
    library(ggfortify)
    ## -----plota pca-------------------------------------------------------
    var_color_grp <- group_id
    color_grp <- factor(dad[[var_color_grp]])
    if (use_col == TRUE) {
      pca_plot <-
        autoplot(dad_pca, data = dad, colour = var_color_grp) +
        scale_color_manual(values = col_p)
    } else {
      pca_plot <-
        autoplot(dad_pca, data = dad, colour = var_color_grp)
    }

    ggsave(
      filename = paste0(outfig_name, "_PCA.pdf"),
      plot = pca_plot,
      device = "pdf",
      width = 8,
      height = 8
    )
  }

  if ("LDA" %in% run_analysis) {
    ## ----run_LDA-------------------------------------------------------------
    ### LDA 70-30
    dad_lda <- lda_lab(df = dad, nir_params_path = nir_params_path, aleatorizacao = aleatorizacao)
    #### chece resultado da lda
  }

  if ("LDA_plot" %in% run_analysis) {
    # salva plots da LDA
    if (use_col == TRUE) {
      dad_lda_plot <-
        lda_lab_plot(dad_lda$train_set, dad_lda$train_model, dad_lda$train_spp, col = TRUE, color = cores) +
        ggtitle(paste0("Dataset ", dataset_name))
    } else {
      dad_lda_plot <-
        lda_lab_plot(dad_lda$train_set, dad_lda$train_model, dad_lda$train_spp, col = FALSE, color = "") +
        ggtitle(paste0("Dataset ", dataset_name))
    }
    teste_n <- 0.3
    lda_type <- paste(c(1 - teste_n, teste_n) * 100, collapse = "-")
    if (use_date == TRUE) {
      ggsave(filename = paste0(outfig, "/NIR_", lda_type, "_", as.character(aleatorizacao), "x_DATASET-", dataset_name, "_", gsub(" ", "_", Sys.time() %>% gsub(":", "-", .)), ".pdf"), device = "pdf", width = 8, height = 8)
    } else {
      ggsave(filename = paste0(outfig, "/NIR_", lda_type, "_", as.character(aleatorizacao), "x_DATASET-", dataset_name, ".pdf"), device = "pdf", width = 8, height = 8)
    }
  }

  if ("LOO" %in% run_analysis) {
    ### LDA LOO
    # apenas um dataset
    dad_lda_loo <- lda_loo(df = dad, nir_params_path = nir_params_path, save.csv = save.csv)
  }

  if ("LOO_plot" %in% run_analysis) {
    # plota matriz de confusao
    classe_abrev <-
      dad_lda_loo$PorAmostra$Classe
    predito_como <-
      dad_lda_loo$PorAmostra$PreditoComo
    xtab <- table(classe_abrev, predito_como)
    png(
      paste0(outfig_name, "_confusao.png"),
      res = 300,
      width = 13.8,
      height = 10,
      units = "cm"
    )
    par(mar = c(1, 4, 4, 1))
    confusao_lab(xtab, add_CP = add_CP)
    dev.off()
  }

  ## salva dados
  save(
    list = c("dad_pca", "pca_plot", "dad_lda", "dad_lda_plot", "dad_lda_loo", "dad", "nir_params"),
    file = paste0(wd, "/", dataset_name, "_analyses.RData")
  )

  res_list <- list(
    dad_pca = dad_pca,
    pca_plot = pca_plot,
    dad_lda = dad_lda,
    dad_lda_plot = dad_lda_plot,
    dad_lda_loo = dad_lda_loo,
    dad = dad,
    nir_params = nir_params
  )

  return(res_list)
}

#' Save Near Infra Red Analysis (NIRA) results
#'
#' @param file_output
#' @author Ricardo de Oliveira Perdiz, \email{ricardoperdiz@@yahoo.com}
#' @return
#' @export
#'
#' @examples
save_NIRAresults <- function(file_output = "", run_NIRA = FALSE) {
  if (file_output == "") {
    list_files <- list.files(path = getwd(), pattern = "NIRAresults")
    if (is_empty(list_files)) {
      file_output <- sprintf("%s/NIRAresults_1.RData", getwd())
    } else {
      file_suffix_max <- max(as.numeric(gsub("NIRAresults_", "", list_files)))
      file_output <- sprintf("%s/NIRAresults_%d.RData", getwd(), file_suffix_max + 1)
    }
  }
  if (run_NIRA == TRUE) {
    save(list = ls(), file = file_output)
  } else {
    save(list = c("dad_pca", "dad_lda", "dad_lda_plot", "dad_lda_loo", "erros", "poramostra", "dad"), file = file_output)
  }
  message("RData File saved in:\n", file_output)
}


#' Predict identity of a group of samples based on a model
#'
#' @param nir_data
#' @param nir_params_path_predict
#' @param nir_params_path_model
#' @param add_to_model
#' @param save.csv
#'
#' @return
#' @export
#'
#' @examples
test_pred_lda_loo <- function(nir_data, nir_params_path_predict, nir_params_path_model, df_with_names, add_to_model = FALSE, save.csv = FALSE) {

  #
  # nir_params_path_model = "/Users/ricoperdiz/Documents/DOC/PROJETO_DOC/Dados_Complexo_Aracouchini/thesis/data/cap01/nir_datasets/PASC_test02D_mean_both_all-NIRparams.txt"
  # nir_params_path_predict =  '/Users/ricoperdiz/Documents/DOC/PROJETO_DOC/Dados_Complexo_Aracouchini/thesis/data/cap01/nir_datasets/PASC_predict_test02E_mean_both_all-NIRparams.txt'
  # add_to_model = TRUE


  #           MODELO                #
  # pega dados do nir_params_file
  nir_params <- read_NIRparams(nir_params_path_model)
  categoria <- nir_params$group_id
  nir_id <- nir_params$nir_id
  colunaind <- nir_params$individual_id

  # gera dataset modelo
  message("#--------------------#\n")
  message("Reading MODEL data.\n")
  message("#--------------------#\n")
  modelo <-
    build_NIRdataset(
      dframe = nir_data,
      params_file_path = nir_params_path_model
    ) %>%
    as.data.frame(.) %>%
    dplyr::select(-SP1) %>%
    left_join(., dplyr::select(df_with_names, especimenid, SP1))
  message("Number of rows: ", dim(modelo)[1])
  message("Number of columns: ", dim(modelo)[2])
  message("#--------------------#\n")
  # indice das colunas que correspondem aos dados NIR
  cls <- grep(nir_id, colnames(modelo))

  #           TESTE                #
  message("#--------------------#\n")
  message("Reading TEST data.\n")
  message("#--------------------#\n")
  # pega dados do nir_params_path_predict
  nir_params_predict <- read_NIRparams(nir_params_path_predict)
  nome_grupo_p <- nir_params_predict$dataset_name
  categoria_p <- nir_params_predict$group_id
  nir_id_p <- nir_params_predict$nir_id
  colunaind_p <- nir_params_predict$individual_id
  wd_p <- paste0(nir_params_predict$working_dir, "/")

  # gera dataset TESTE
  teste <-
    build_NIRdataset(
      dframe = nir_data,
      params_file_path = nir_params_path_predict
    ) %>%
    as.data.frame(.)

  message("Number of rows: ", dim(teste)[1])
  message("Number of columns: ", dim(teste)[2])
  message("#--------------------#\n")
  # gera vetor com identificadores de amostras que serao testadas
  test_vector <- teste[[colunaind_p]]

  if (length(test_vector) == 0) {
    stop("Samples to test prediction equals zero! Check your dataset generated by `nir_params_path_predict`.")
  }

  # indice das colunas que correspondem aos dados NIR em full_data
  cls2 <- grep(nir_id_p, colnames(teste))

  # IMPLEMENTANDO O LEAVE ONE OUT (deixe um de fora)
  res_loo <- NULL # objeto para salvar os resultados
  test_to_add <- list()
  # para cada individuo (amostra em test_vector), faz o teste de predicao
  for (i in seq_along(test_vector)) {
    # i = 1

    # -------- CHANGE ONLY BELOW ----- #
    individuo <- test_vector[i]

    # seleciona os dados do individuo sendo testado
    teste_dad <- teste[i, cls2]
    teste_sp <- teste[i, categoria_p]

    # Usa os dados NIR de modelo para predizer a id de teste_dad
    if (add_to_model == TRUE) {
      if (i == 1) {
        modelo_dad <- modelo[, cls]
        modelo_sp <- modelo[[categoria]]
        test_to_add[[paste0("i_", i)]] <- list(
          df = modelo_dad,
          sp = modelo_sp
        )
      } else {
        modelo_dad_pre <- test_to_add[[paste0("i_", i - 1)]]$df
        modelo_sp_pre <- test_to_add[[paste0("i_", i - 1)]]$sp
        modelo_dad <- bind_rows(modelo_dad_pre, teste[i - 1, cls2])
        modelo_sp <- c(modelo_sp_pre, teste[i - 1, categoria_p])
        test_to_add[[paste0("i_", i)]] <-
          list(
            df = modelo_dad,
            sp = modelo_sp
          )
      }
    } else {
      modelo_dad <- modelo[, cls]
      modelo_sp <- modelo[[categoria]]
    }
    # fazer modelo discriminante
    meuLDA <- lda(modelo_dad, grouping = modelo_sp)
    predicao <- predict(meuLDA, teste_dad)

    p <- as.vector(predicao$posterior)
    names(p) <- colnames(predicao$posterior)

    pc <- names(p[p > 0.95]) # classes preditas com valor de p<=0.05
    cl <- as.vector(predicao$class) # classe predita
    pv <- p[cl] # valor de p da classe predita
    if (length(pc) > 0) {
      if (pc == teste_sp) {
        acerto <- "acertou significativo"
      } else {
        acerto <- "errou significativo"
      }
    } else {
      if (cl == teste_sp) {
        acerto <- "acertou não significativo"
      } else {
        acerto <- "errou não significativo"
      }
    }

    rr <-
      data.frame(
        Individuo = individuo,
        Classe = teste_sp,
        Posterior = round(predicao$posterior, 2),
        Acerto = acerto,
        PreditoComo = names(pv),
        stringsAsFactors = F
      )
    res_loo <- rbind(res_loo, rr)

    cat(glue::glue(
      "Amostra {ind}-{nrow(dad)}, individuo {individuo} {teste_sp} predito {predicao} p={pv}% {acerto_perc}",
      ind = i,
      dad = teste_dad,
      individuo = individuo,
      teste_sp = teste_sp,
      predicao = cl,
      pv = (round(pv, 2) * 100),
      acerto_perc = toupper(acerto)
    ),
    sep = "\n"
    )
  }

  # Tabela com resultados por amostra
  PorAmostra <- data.frame(EspectroTestado = rownames(res_loo), res_loo, stringsAsFactors = F)

  # Resultados resumidos
  tb <- table(res_loo$Acerto)
  # em porcentagem
  tbb <- round((tb / sum(tb)) * 100, 1)
  # Tabela com resumo
  resumo <- as.data.frame(cbind(Tipo = names(tb), N.Individuos = tb, Porcentagem = tbb))

  # salva os resultados
  if (save.csv == TRUE) {
    if (wd_p == ".") {
      save_where <- ""
      message("Working directory: ", getwd())
    } else {
      save_where <- wd_p
      message("Working directory: ", wd_p)
    }
    # resultado da loo
    write.table(res_loo, paste0(save_where, "lda_tabelaLOO_GERAL_dataset-", nome_grupo_p, ".csv"), sep = "\t", row.names = FALSE)
    # tabela de resultados por Amostra
    write.table(PorAmostra, paste0(save_where, "lda_tabelaLOO_poramostra_dataset-", nome_grupo_p, ".csv"), sep = "\t", row.names = FALSE)
    # Tabela com resumo
    write.table(resumo, paste0(save_where, "lda_tabelaLOO_resumo_dataset-", nome_grupo_p, ".csv"), sep = "\t", row.names = FALSE)
  }

  resultado <-
    list(
      geral = res_loo,
      PorAmostra = PorAmostra,
      resumo = rr
    )

  return(resultado)
}
