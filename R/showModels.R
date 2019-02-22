#' Run process macro shiny app
#' @importFrom shiny runApp
#' @importFrom shinyWidgets radioGroupButtons
#' @export
showModels=function(){
    shiny::runApp(system.file('showModels',package='processR'))
}
