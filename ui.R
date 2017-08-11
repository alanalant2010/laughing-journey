library(shiny)
shinyUI(fluidPage(
  titlePanel("PSG Search Engine"),
  textInput("search", "What keyword would you like to search: "),
  tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
            ),
  dataTableOutput("result")
))