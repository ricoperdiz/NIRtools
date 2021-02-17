#' Conjunto de dados NIR em formato bruto
#'
#' Subconjunto de dados utilizado na tese do autor do pacote ([Perdiz, 2020](https://repositorio.inpa.gov.br/handle/1/36948)), consistindo em leituras espectrais do infravermelho próximo de um grupo de plantas de distribuição Neotropical. Formato dos dados consiste em uma observação de leitura espectral por linha.
#'
#' `r (nir_raw)`
#' @format Um dataframe com 1557 linhas e 2 colunas
#' \describe{
#' \item{wavenumber}{Número da onda}
#' \item{absorbance}{Absorbância}
#' }
"nir_raw"

#' Conjunto de dados NIR processado
#'
#' Um subconjunto de dados oriundos da tese do autor ([Perdiz, 2020](https://repositorio.inpa.gov.br/handle/1/36948)), que consiste em uma série de leituras do espectro infravermelho de folhas de um grupo de árvores de distribuição neotropical.
#'
#' @format Um data.frame de 48 linhas e 1562 colunas.
#' \describe{
#' \item{especimenid}{Identificador da amostras.}
#' \item{SP1}{Nome da espécie da amostras.}
#' \item{face}{De qual face da folha a leitura foi obtida? Abaxial (inferior) ou adaxial (superior)?}
#' \item{coletor}{Nome do coletor da amostra.}
#' \item{number}{Número da amostra coletada.}
#' \item{Colunas que começam com X}{Cada uma corresponde a uma faixa do espectro, e possuem valores de absorbância.}
#' }
"nir_data"

#' Conjunto de dados NIR processado e convertido em objeto nirdf
#'
#' Um subconjunto de dados oriundos da tese do autor ([Perdiz, 2020](https://repositorio.inpa.gov.br/handle/1/36948)), que consiste em uma série de leituras do espectro infravermelho de folhas de um grupo de árvores de distribuição neotropical. Dados convertidos para um objeto nirdf com a função `nirdf()`.
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
"nir_dad"