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
  titlePanel("Shiny"),
  sidebarLayout(
    sidebarPanel("Author",
    img(src= "ancira.jpg", height = 100, width = 100)
  ),

    
    mainPanel(
      h1("Informacion sobre shiny", align = "center"),
      p("Shiny is an R package that makes it easy to build interactive web apps straight from R. You can host standalone apps on a webpage or embed them in R Markdown documents or build dashboards. You can also extend your Shiny apps with CSS themes, htmlwidgets, and JavaScript actions. ",
        strong("Author infomration"), 
        em("em() Contact"), 
        code("ancira.perez@gmail.com")),
      p("Shiny combines the computational power of R with the interactivity of the modern web", style = "color:red"),
      img(src= "shiny.png", height = 200, width = 200)
      
))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)

