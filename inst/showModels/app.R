library(shiny)
library(shinyWidgets)
library(processR)

ui=fluidPage(
h2("Select Process Macro Model Number"),
fluidRow(
    column(2,
           selectInput("modelno","Model No",choices=sort(pmacro$no),selectize=FALSE,size=28)
    ),
    column(10,

           radioGroupButtons(
               inputId = "plotChoice",
               label = "Select Plot",
               choices = c("Conceptual Diagram"=1, "Statistical Diagram"=2),
               status = "primary"
           ),

           plotOutput("modelPlot",height="500px",width="700px")
    )
)
)

server=function(input,output,session){
    output$modelPlot=renderPlot({
        par(family=input$font)

        if(input$plotChoice==1) {
            pmacroModel(as.numeric(input$modelno))
        } else{
            statisticalDiagram(as.numeric(input$modelno))
        }

    })
}
shinyApp(ui,server)
