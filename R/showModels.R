#' Run process macro shiny app
#' @importFrom utils install.packages
#' @export
showModels=function(){

    if(!requireNamespace("shiny")) install.packages("shiny")
    if(!requireNamespace("shinyWidgets")) install.packages("shinyWidgets")
    shiny::runApp(system.file('showModels',package='processR'))
}
