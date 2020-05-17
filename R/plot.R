#' Plot a nirdf object.
#'
#' @description
#' \lifecycle{experimental}
#'
#' @param x A nirdf object.
#' @param category A string pointing the name of the category variable of nirdf object.
#' @param remove_cols Character vector pointing name of columns to be removed.
#' @param xlabel X axis label.
#' @param ylabel Y axis label.
#' @param legend_position Legend position, as done in function legend.
#' @param color Vector of colors. If not provided, function will provide one. Attention to the length of this vector. It should be the same length as the number of categories.
#' @param ...
#'
#' @return A plot
#' @export
#'
#' @examples
#' str(nirdf)
#' # nirdf object is a data.frame containing a column for categories, `SP1`, an id column, `especimenid`, and all the rest of columns containg Near Infrared data.
#' plot(nirdf, category = "SP1", remove_cols = "especimenid")
plot.nirdf <- function(x, category, remove_cols = NULL,  xlabel = parse(text = "Wavenumber (cm^-1)"), ylabel = "Absorbance", legend_position = "topright", color = NULL, ...) {


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


  # names(nir2) <- gsub("^X", "", names(nir2))
  # nirdf <- nir2
  # category = "SP1"
  # remove_cols = c("especimenid", "coletor")


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
  # str(df_to_plot)

  # plot mean of values or not?
  # value = mean(value)

  # X axis
  x_axis <- range(unique(df_to_plot$variable))

  # Y axis
  y_axis <- range(df_to_plot$value)

  # plot empty nirdf
  plot(df_to_plot$variable, df_to_plot$value, type = "n", xlab = xlabel, ylab = ylabel)

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
    cat_plot <- df_to_plot[SP1 == categories[i], ]

    points(cat_plot$variable, cat_plot$value, cex = 0.05, col = pal[i], ylim = y_axis)
    col_legend <- c(col_legend, pal[i])
  }

  legend(legend_position, legend = categories, pch = 16, cex = 0.5, col = col_legend)

}
