#####################################
# ATENCAO                           #
#####################################
# E necessario instalar os pacotes abaixo:
# install.packages(c("shiny", "DT", "RColorBrewer"), dependencies = TRUE)

# Caso ja os possua instalados, e so executar os comandos abaixo
library("shiny")
library("DT")
library("NIRtools")
library("RColorBrewer")

### Desenho do app
dados <-
  read.table(
    system.file("extdata", "nir_data.csv", package = "NIRtools"),
    header = TRUE,
    sep = "\t"
  )
head(dados[, 1:6], 10)
codigos_identificadores <- names(dados)[1:5]

ui <- fluidPage(#   # Titulo
  titlePanel("NIRTools - Plota imagens"),
  fluidRow(column(
    6,
    selectInput("id", "Identificador", choices = codigos_identificadores)
  )),
  fluidRow(column(
    12,
    plotOutput("nirplot")
    ))
  )

server <- function(input, output, session) {
  selected <-
    reactive(
      NIRtools::nirdf(
        dados,
        category = input$id,
        measure_columns = grep("^X", names(dados), value = TRUE),
        measure_columns_prefix = "X"
      )
    )

  colors <-
    reactive(brewer.pal(length(unique(dados[[input$id]])) + 1, "Paired"))

  output$nirplot <- renderPlot({
    plot(selected(), category = input$id, color = colors())
  }, res = 96)
}

shinyApp(ui, server)
