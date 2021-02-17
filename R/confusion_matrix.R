#' Plota matriz de confusão, estilo LABOTAM
#'
#' @param xtab Tabela cruzamento de dados observados versus dados preditos.
#' @param add_CP Vetor lógico que, se TRUE, adiciona a taxa de predição correta como última coluna da matrix, dispondo os valores em porcentagem. Comportamento padrão é não adicionar essa coluna.
#' @param ... Argumentos adicionais a serem utilizados na função `plotamatriz()`.
#'
#' @author Alberto Vicentini, \email{vicentini.beto@@gmail.com}; Ricardo O. Perdiz, \email{ricoperdiz@@gmail.com}
#'
#' @return Um plot.
#'
#' @export
#'
#' @examples
#' observado <- factor(sample(letters[1:10], 200, replace = TRUE), levels = letters[1:10])
#' predito <- factor(sample(letters[1:10], 200, replace = TRUE), levels = letters[1:10])
#' dad <- data.frame(observ = observado, pre = predito)
#' # com predicao correta como ultima coluna
#' confusao_lab(table(dad$observ, dad$pre), add_CP = TRUE)
#' # sem predicao correta como ultima coluna
#' confusao_lab(table(dad$observ, dad$pre), add_CP = FALSE)
confusao_lab <- function(xtab, add_CP = FALSE, ...) {

  mt <- xtab
  mt2 <- as.matrix.data.frame(xtab)
  rownames(mt2) <- rownames(mt)
  colnames(mt2) <- colnames(mt)
  mt2

  if (add_CP == TRUE) {
    if( length(diag(mt2)) == ncol(mt2) ) {
      mt2_cp <- cbind(mt2, "%CP" = round(diag(mt2) / rowSums(mt2), 2) * 100)
      mt2_cp
      rn_cp <- sort(rownames(mt2_cp), decreasing = T)
      mt2_cp <- mt2_cp[rn_cp, ]
      mt2_cp
      return(plotamatriz(mt2_cp))
    } else {
      colunas_valores <- which(colnames(mt2)  %in% row.names(mt2))
      porcentagem <- diag(mt2[,colunas_valores]/rowSums(mt2))

      mt2_cp <- cbind(mt2, "%CP" =  round(porcentagem, 2) * 100)
      mt2_cp
      rn_cp <- sort(rownames(mt2_cp), decreasing = T)
      mt2_cp <- mt2_cp[rn_cp, ]
      mt2_cp
      return(plotamatriz(mt2_cp, ...))
    }
  } else {
    rn <- sort(rownames(mt2), decreasing = T)
    mt2 <- mt2[rn, ]
    mt2
    return(plotamatriz(mt2, ...))
  }
}

#' Plota matriz de confusão
#'
#' @param matriz matrix com colnames e rownames
#' @param bg.cols matrix com valores de cores para background das células
#' @param txt.cols matrix com valores de cores para o texto dos valores das células
#' @param valcex cex do texto dos valores das células
#' @param cexaxis cex do texto dos eixos = nomes de colunas e linhas
#' @param lasx Orientação do texto do eixo x.
#' @param lasy Orientação do texto do eixo y.
#'
#' @author Alberto Vicentini, \email{vicentini.beto@@gmail.com}
#'
#' @return Um plot.
#' @return
#' @export
#'
#' @examples
plotamatriz <- function(matriz, bg.cols = NULL, txt.cols = NULL, valcex = 1, cexaxis = 1, lasx = NULL, lasy = NULL) {

  if(is.null(lasx)) {
    lasx = 2
  } else {
    lasx = lasx
  }
  if(is.null(lasy)) {
    lasy = 2
  } else {
    lasy = lasy
  }
  # matriz = mt2_cp
  tb <- matriz
  if (any(colnames(matriz) == "%CP")) {
    rm_col <- which(colnames(matriz) == "%CP")
    tb <- round(matriz[, -rm_col] / rowSums(matriz[, -rm_col]), 2) * 100
    tb <- cbind(tb, "%CP" = matriz[, rm_col])
    tb
  } else {
    tb <- round(matriz / rowSums(matriz), 2) * 100
  }
  tb
  if (is.null(bg.cols)) {
    bg.cols <- matriz
    txt.cols <- matriz
    # se for 0 % é branco
    bg.cols[tb == 0] <- gray(level = 1)
    txt.cols[tb == 0] <- gray(level = 1)
    # se for ENTRE 0 e 60% é cinza claro
    bg.cols[tb > 0 & tb <= 60] <- gray(level = 0.8)
    txt.cols[tb > 0 & tb <= 60] <- gray(level = 0)
    # se for entre 60 e 80% um pouco mais escuro
    bg.cols[tb > 60 & tb <= 80] <- gray(level = 0.6)
    txt.cols[tb > 60 & tb <= 80] <- gray(level = 0)
    # se for entre 80 e 95% um pouco mais escuro
    bg.cols[tb > 80 & tb <= 95] <- gray(level = 0.4)
    txt.cols[tb > 80 & tb <= 95] <- gray(level = 1)
    # se for MAIOR que 95% o fundo é preto e o texto branco
    bg.cols[tb > 95] <- gray(level = 0)
    txt.cols[tb > 95] <- gray(level = 1)
    # one for 0 coloca NA, isso não é plotado
    tb[tb == 0] <- NA
  }
  # plota uma matriz vazia
  xx <- 1:ncol(tb)
  yy <- seq(1, nrow(tb), length.out = length(xx))
  plot(xx, yy, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", pty = "m", xlim = c(0.5, max(xx) + .5), ylim = c(0.5, max(yy) + 0.5))
  ## ax <- yy[1:(length(yy)-1)]+(diff(yy)/2)
  ## ay <- xx[1:(length(xx)-1)]+(diff(xx)/2)
  ay <- 1:ncol(tb)
  ax <- 1:nrow(tb)
  abline(v = ay, h = ax, lty = "dotted", lwd = 0.5)
  nn <- rownames(tb)
  nncl <- colnames(tb)
  for (l in 1:nrow(tb)) {
    for (cc in 1:ncol(tb)) {
      cl <- bg.cols[l, cc]
      txtc <- txt.cols[l, cc]
      val <- matriz[l, cc]
      if (!is.na(val)) {
        rect(xleft = cc - 0.5, ybottom = l - 0.5, xright = cc + 0.5, ytop = l + 0.5, density = -1, border = NA, col = cl)
        text(cc, l, labels = val, cex = valcex, col = txtc)
      }
    }
  }
  axis(side = 3, at = ay, labels = colnames(tb), cex.axis = cexaxis, las = lasy)
  axis(side = 2, at = ax, labels = rownames(tb), cex.axis = cexaxis, las = lasx)
}
