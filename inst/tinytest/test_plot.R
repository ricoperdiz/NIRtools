library("tinytest")
library("NIRtools")
library("data.table")
library("RColorBrewer")
library("magrittr")

## Seleciona colunas
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
xx <-
  nirdf(x,
        category = "SP1",
        measure_columns = grep("^X", names(x), value = TRUE))
class(xx)
xx[1:10, 1:10]
category <- "SP1"
measure_columns <- names(xx)[which(!names(xx) == category)]

#### tests
expect_true(length(measure_columns) == 1557)


## Transforma objeto nirdf em formato comprido (=='long')
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
# class(x)
# head(names(x))
xx <-
  nirdf(x,
        category = "SP1",
        measure_columns = grep("^X", names(x), value = TRUE))
class(xx)
xx[1:10, 1:10]

category <- "SP1"
measure_columns <- names(xx)[which(!names(xx) == category)]


# melt nirdf to long format
df_to_plot <-
  melt(xx, id.vars = category, measure.vars = measure_columns)

#### tests
expect_true(ncol(df_to_plot) == 3)


## Checa objeto nirdf 'comprido'
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
xx <-
  nirdf(
    x,
    category = "SP1",
    measure_columns = grep("^X", names(x), value = TRUE),
    measure_columns_prefix = "X"
  )
category <- "SP1"
measure_columns <- names(xx)[which(!names(xx) == category)]


# melt nirdf to long format
df_to_plot <-
  melt(xx, id.vars = category, measure.vars = measure_columns)

#### tests
expect_true(is.factor(df_to_plot$variable))


df_to_plot$variable <-
  as.numeric(as.character(df_to_plot$variable))
expect_true(is.numeric(df_to_plot$variable))


## Testa comportamento de objeto de cores frente a um vetor de texto ou de fator
set.seed(123)
category_chr <-
  c(sample(LETTERS[1:5], size = 10, replace = TRUE), LETTERS[1:5])
category_fct <-
  factor(c(sample(
    LETTERS[1:5], size = 10, replace = TRUE
  ), LETTERS[1:5]))

# When character
sort(unique(category_chr))
# when factor
sort(unique(category_fct))

#### tests
expect_equal(sort(unique(category_chr)), as.character(sort(unique(category_fct))))

## Testa categorias selecionadas
data("nir_data", package = "NIRtools")
df_to_plot <-
  nir_data %>%
  dplyr::filter(especimenid == 10194) %>%
  nirdf(
    .,
    category = "face",
    measure_columns = grep("X", names(.), value = TRUE),
    measure_columns_prefix = "X"
  )
category = "face"
categories <- sort(unique(df_to_plot[[category]]))
res <- df_to_plot[df_to_plot[[category]] == categories[1],]

#### tests
expect_equal(nrow(res), 4)
expect_equal(ncol(res), 1558)

df_to_plot2 <-
  dplyr::filter(nir_data, especimenid == 10194) %>%
  nirdf(
    .,
    category = "SP1",
    measure_columns = grep("X", names(.), value = TRUE),
    measure_columns_prefix = "X"
  )
category2 = "SP1"
categories2 <- sort(unique(df_to_plot2[[category2]]))
res2 <- df_to_plot2[df_to_plot2[[category2]] == categories2[1],]

#### tests
expect_equal(nrow(res2), 8)
expect_equal(ncol(res2), 1558)


## Plota objeto nirdf
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
class(x)
head(names(x))
xx <-
  nirdf(
    x,
    category = "SP1",
    measure_columns = grep("^X", names(x), value = TRUE),
    measure_columns_prefix = "X"
  )
class(xx)
head(names(xx))
plotout <- plot(xx, category = "SP1")

#### tests
expect_true(length(plotout) == 2)


## Plota objeto nirdf com a adição de um vetor de cores
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
xx <-
  nirdf(
    x,
    category = "SP1",
    measure_columns = grep("^X", names(x), value = TRUE),
    measure_columns_prefix = "X"
  )
colors <- brewer.pal(length(unique(xx$SP1)) + 1, "Paired")
plot_paired <- plot(xx, category = "SP1", color = colors)

#### tests
expect_true(length(plot_paired) == 2)
colors2 <- brewer.pal(length(unique(xx$SP1)) + 1, "Set2")
plot_set2 <- plot(xx, category = "SP1", color = colors2)
expect_true(length(plot_set2) == 2)

