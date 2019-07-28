library(shiny)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
    titlePanel("BC Liquor Store prices"),
    sidebarLayout(
        sidebarPanel("our inputs will go here"),
        mainPanel("the results will go here")
    )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)