<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# NIRtools
**************

# Panorama geral

NIRTools foi desenhado para auxiliar taxonomistas e ecólogos de plantas em **documentar** e **construir** subconjuntos de dados do infravermelho próximo (NIR, do inglês *Near infrared spectroscopy*), por meio do usos de arquivos de parâmetros que servirão como metadados de cada subconjunto. Também estão inclusas funções para auxiliar a plotar os resultados.

<!-- NIRTools was specifically designed to help plant taxonomist and ecologists in documenting and building near infrared (NIR) spectroscopy subsets from a dataset based on parameter files that will serve as a metadata of each subset.-->

# O que é o NIR?

Para mais detalhes, [leia este trabalho](http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0103-50532003000200006).

# Instalação
**************

Você pode instalar esta versão em desenvolvimento por meio do pacote `remotes`. 

```
install.packages("remotes")
library(remotes)
remotes::install_github("ricoperdiz/NIRtools")
```

Após instalar o NIRtools, digite o comando abaixo para ver o arquivo contendo o tutorial do pacote.

<!--After installing NIRtools, type the command below to see the vignette with NIRtools tutorial.-->

```
vignette("NIRTools_tutorial", package = "NIRtools")
```
