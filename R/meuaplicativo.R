#' Executa um aplicativo Shiny
#'
#' @return
#' @export
#'
#' @examples
meuaplicativo <- function(display.mode = "normal") {
  runApp(
    appDir = system.file('shiny', package = 'NIRtools'),
    display.mode = "normal")
}
