#' Plota um objeto nirdf.
#'
#' @description
#' \lifecycle{experimental}
#'
#' @param x Um objeto nirdf.
#' @param category Nome da coluna contendo a variável categórica do objeto nirdf.
#' @param remove_cols Vetor de texto contendo os nomes das colunas a serem removidos.
#' @param xlabel Texto do eixo X.
#' @param ylabel Texto do eixo Y.
#' @param legend_position Posição da legenda, como adotado na função `legend()`.
#' @param cex_pt Tamanho adotado no argumento cex da função `points()`.
#' @param cex_leg Tamanho adotado no argumento cex da função `legend()`.
#' @param text_font Tipo de fonte a ser utilizado em  `legend()`. Por padrão, opção 1, que corresponde em texto simples, sem formatação. Outras opções são `2`, negrito, `3`, itálico, e `4`, negrito e itálico.
#' @param plot_legend Plota a legenda? Se não plotar, ao utilizar `FALSE`, a função retorna um `data.frame` para ser utilizado a posteriori na construção de uma legenda utilizando a função `layout`.
#' @param color Vetor de cores. Se não fornecido, a função providenciará uma seleção automaticamente. Atenção ao comprimento deste vetor. Ele deve ser de tamanho mínimo ao número de categorias do objeto nirdf.
#' @param ... Funções adicionais a serem fornecidas à função `plot()`.
#'
#' @return Um plot.
#' @importFrom graphics abline axis hist legend par plot points rect text
#' @importFrom grDevices cm.colors gray
#' @export
#'
#' @examples
#' # Load nir data
#' library("NIRtools")
#' data(nir_data)
#' nirdad <- nirdf(nir_data, "SP1",
#'  measure_columns = grep("^X", names(nir_data), value = TRUE),
#'   measure_columns_prefix = "X")
#' plot(nirdad, "SP1")

plot.nirdf <- function(x, category, remove_cols = NULL, xlabel = parse(text = "Wavenumber (cm^-1)"), ylabel = "Absorbance", legend_position = "topright", cex_pt = 0.05, cex_leg = 0.5, text_font = 1, plot_legend = TRUE, color = NULL, ...) {


  stopifnot(is_nirdf(x))

  # Category NULL?
  if (is.null(category)) {
    stop(paste0("Argument `category` can't be `NULL`. You must indicate the name of a column of your nirdf object to represent the categories of your nir dataset."))
  }
  # Category empty
  if (category == "") {
    stop(paste0("Argument `category` can't be empty. You must indicate the name of a column of your nirdf object to represent the categories of your nir dataset."))
  }
  # Category empty
  if (length(category) > 1) {
    stop(paste0("Argument `category` must have length 1. Provide only one name of a column of your nirdf object to represent the categories of your nir dataset."))
  }

  # select columns to keep
  if (is.null(remove_cols)) {

    measure_columns <- names(x)[which(!names(x) == category)]

  } else {

    stopifnot(is.character(remove_cols))

    columns_to_keep <- which(!names(x) %in%  remove_cols)
    nirdfplot <- x[, columns_to_keep, with = FALSE]

    measure_columns <- names(nirdfplot)[which(!names(nirdfplot) == category)]
  }

  # melt nirdf to long format
  df_to_plot <- melt(x, id.vars = category, measure.vars = measure_columns)
  df_to_plot$variable <- as.numeric(as.character(df_to_plot$variable))

  # X axis
  x_axis <- range(unique(df_to_plot$variable))

  # Y axis
  y_axis <- range(df_to_plot$value)

  # plot empty nirdf
  plot(df_to_plot$variable, df_to_plot$value, type = "n", xlab = xlabel, ylab = ylabel, ...)

  # prepare to plot categories
  ## a vector for categories
  categories <- sort(unique(df_to_plot[[category]]))

  # color vector NULL?
  if (is.null(color)) {
    # palette("default")
    # palette
    # pal <- terrain.colors(length(categories))
    pal <- cm.colors(length(categories))
  } else {
    # check if length(color) is acceptable
    # should have minimum length == length(categories)
    if (length(color) >= length(categories)) {
      pal <- color
    } else {
      # color <- 1
      stop(paste0("Argument color has length ", length(color), ". It should have at least length equal to the number of your categories."))
    }
  }

  col_legend <- NULL
  for (i in seq_along(categories)) {
    # i = 1
    cat_plot <- df_to_plot[df_to_plot[[category]] == categories[i], ]

    points(cat_plot$variable, cat_plot$value, pch = 16, cex = cex_pt, col = pal[i], ylim = y_axis)
    col_legend <- c(col_legend, pal[i])
  }

  if (plot_legend) {
    legend(legend_position, legend = categories, pch = 16, cex = cex_leg, col = col_legend, text.font = text_font)
  } else {
    df_legend <- data.frame(categories = categories, pch = 16, cex = cex_leg, col = col_legend, text_font = text_font)
    invisible(df_legend)
  }


}
