library("tinytest")
library("NIRtools")
library("data.table")

## Check base object
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))

#### tests
expect_true(is.data.frame(x))

data(nir_data)
expect_true(is.data.frame(nir_data))

## Coerce x to a data.table
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
xx <- as.data.frame(x)
xxx <- as.data.table(xx)


#### tests
expect_false(is.data.table(xx))
expect_true(is.data.table(xxx))

data(nir_data)
nir_data2 <- as.data.frame(nir_data)
nir_data3 <- as.data.table(nir_data2)

expect_false(is.data.table(nir_data2))
expect_true(is.data.table(nir_data3))


## Select columns in a data.table - csv
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
head(names(x))
category <- "SP1"
measure_columns <- grep("^X", names(x), value = TRUE)

columns_to_keep <- c(category, measure_columns)
columns_to_keep_pos <- which(names(x) %in% columns_to_keep)


#### tests
expect_true(class(columns_to_keep) == "character")
expect_true(class(columns_to_keep_pos) == "integer")

xx <- x[, columns_to_keep_pos, with = FALSE]
dim(xx)
head(names(xx))

#### tests
expect_true(nrow(xx) == 48)
expect_true(ncol(xx) == 1558)
expect_false(is_nirdf(xx))


## Select columns in a data.table - rda
data(nir_data)
x <- nir_data
category <- "SP1"
measure_columns <- grep("^X", names(x), value = TRUE)

columns_to_keep <- c(category, measure_columns)
columns_to_keep_pos <- which(names(x) %in% columns_to_keep)

#### tests
expect_true(class(columns_to_keep) == "character")
expect_true(class(columns_to_keep_pos) == "integer")

xx <- x[, columns_to_keep_pos, with = FALSE]
dim(xx)
head(names(xx))


#### tests
expect_true(nrow(xx) == 48)
expect_true(ncol(xx) == 1558)
expect_false(is_nirdf(xx))


## Remotion of prefix in measure_columns - csv
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
xx <-
  nirdf(x,
        category = "SP1",
        measure_columns = grep("^X", names(x), value = TRUE))

measure_columns_prefix <- "X"
names(xx) <-
  gsub(paste0("^", measure_columns_prefix), "", names(xx))

#### tests
expect_warning(as.numeric(names(xx)))
expect_true(sum(is.na(expect_warning(as.numeric(names(xx))))) == 1)


## Remotion of prefix in measure_columns - rda
data(nir_data)
x <- nir_data
xx <-
  nirdf(x,
        category = "SP1",
        measure_columns = grep("^X", names(x), value = TRUE))

measure_columns_prefix <- "X"
names(xx) <-
  gsub(paste0("^", measure_columns_prefix), "", names(xx))

#### tests
expect_warning(as.numeric(names(xx)))
expect_true(sum(is.na(expect_warning(as.numeric(names(xx))))) == 1)

## Use nirdf to see it it works - no remotion of prefix - csv
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
xx <-
  nirdf(x,
        category = "SP1",
        measure_columns = grep("^X", names(x), value = TRUE))

#### tests
expect_true(nrow(xx) == 48)
expect_true(ncol(xx) == 1558)
expect_true(is_nirdf(xx))


## Use nirdf to see it it works - no remotion of prefix - rda
data(nir_data)
x <- nir_data
xx <-
  nirdf(x,
        category = "SP1",
        measure_columns = grep("^X", names(x), value = TRUE))

#### tests
expect_true(nrow(xx) == 48)
expect_true(ncol(xx) == 1558)
expect_true(is_nirdf(xx))


## Use nirdf to see it it works - with remotion of prefix - csv
x <-
  fread(system.file("extdata", "nir_data.csv", package = "NIRtools"))
xx <-
  nirdf(
    x,
    category = "SP1",
    measure_columns = grep("^X", names(x), value = TRUE),
    measure_columns_prefix = "X"
  )

#### tests
expect_true(class(xx$SP1) == "character")
expect_true(nrow(xx) == 48)
expect_true(ncol(xx) == 1558)
expect_true(is_nirdf(xx))


## Use nirdf to see it it works - with remotion of prefix - rda
data(nir_data)
x <- nir_data
xx <-
  nirdf(
    x,
    category = "SP1",
    measure_columns = grep("^X", names(x), value = TRUE),
    measure_columns_prefix = "X"
  )

#### tests
expect_true(class(xx$SP1) == "character")
expect_true(nrow(xx) == 48)
expect_true(ncol(xx) == 1558)
expect_true(is_nirdf(xx))

