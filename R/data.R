#' Conjunto de dados NIR processado
#'
#' Um subconjunto de dados oriundos da tese do autor, que consiste em uma série de leituras do espectro infravermelho de folhas de um grupo de árvores de distribuição neotropical.
#'
#' @format Um data.frame de `r nrows(nir_data)` linhas e `r ncol(nir_data)` colunas.
#' \describe{
#' \item{especimenid}{Identificador da amostras.}
#' \item{SP1}{Nome da espécie da amostras.}
#' \item{face}{De qual face da folha a leitura foi obtida? Abaxial (inferior) ou adaxial (superior)?}
#' \item{coletor}{Nome do coletor da amostra.}
#' \item{number}{Número da amostra coletada.}
#' \item{Colunas que começam com X}{Cada uma corresponde a uma faixa do espectro, e possuem valores de absorbância.}
#' }
"nir_data"

#' Dataset of Near Infra Red data
#'
#' A dataset containing a subset from the author's thesis, that consists of reads of  near infrared spectrum of leaves of a group of neotropical plants
#'
#' @format A dataframe with 541 rows and 1561 columns
#' \describe{
#' \item{especimenid}{Specimen identification}
#' \item{SP1}{Species id}
#' \item{face}{Which leaf surface?}
#' \item{coletor}{Nome do coletor da amostra}
#' \item{number}{Número da amostra coletada}
#' \item{Columns that starts with X}{each one of these is a spectrum}
#' }
"nir_raw"

#' Conjunto de dados NIR processado e convertido em objeto nirdf
#'
#' Um subconjunto de dados oriundos da tese do autor, que consiste em uma série de leituras do espectro infravermelho de folhas de um grupo de árvores de distribuição neotropical.
#'
#' @format Um data.frame de `r nrows(nir_data)` linhas e `r ncol(nir_data)` colunas.
#' \describe{
#' \item{especimenid}{Identificador da amostras.}
#' \item{SP1}{Nome da espécie da amostras.}
#' \item{face}{De qual face da folha a leitura foi obtida? Abaxial (inferior) ou adaxial (superior)?}
#' \item{coletor}{Nome do coletor da amostra.}
#' \item{number}{Número da amostra coletada.}
#' \item{Colunas que começam com X}{Cada uma corresponde a uma faixa do espectro, e possuem valores de absorbância.}
#' }
"nirdad"