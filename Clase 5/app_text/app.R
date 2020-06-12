#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Titulo Del Panel"),
  sidebarLayout(
                sidebarPanel("Sidebar Title"),
                mainPanel(
                  h1("Primer Nivel", align = "center"),
                  h2("Segundo Nivel"),
                  h3("Tercer nivel"),
                  p("esto funciona para hacer nuevos parrafos",
                    strong("strong() funciona para hacer negrita la letra"), 
                    em("em() crea texto en italica"), 
                    code("@lazarus")),
                  p("texto en color", style = "color:red"))
                )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)

