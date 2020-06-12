
# https://www.tidymodels.org/
#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
library(shiny)
library(tidymodels)

d_broom <- "broom summarizes key information about models in tidy tibble()s. 
broom provides three verbs to make it convenient to 
interact with model objects:
- tidy() summarizes information about model components
- glance() reports information about the entire model
- augment() adds informations about observations to a dataset
For a detailed introduction, please see vignette(\"broom\").

broom tidies 100+ models from popular modelling packages and almost all of the model
objects in the stats package that comes with base R. vignette(\"available-methods\")
lists method availability.

If you aren’t familiar with tidy data structures and want to know how they
can make your life easier, we highly recommend reading Hadley Wickham’s Tidy Data.
"

d_dplyr <- "dplyr is a grammar of data manipulation, providing a consistent set of 
verbs that help you solve the most common data manipulation challenges.

    mutate() adds new variables that are functions of existing variables
    select() picks variables based on their names.
    filter() picks cases based on their values.
    summarise() reduces multiple values down to a single summary.
    arrange() changes the ordering of the rows.
"
d_ggplot2 <- " ggplot2 is a system for declaratively creating graphics, based on The Grammar 
of Graphics. You provide the data, tell ggplot2 how to map variables to aesthetics, what graphical
primitives to use, and it takes care of the details."

d_infer <- "The objective of this package is to perform inference using an expressive 
statistical grammar that coheres with the tidy design framework.
"
d_parsnip <- "It is designed to solve a specific problem related to model fitting in R,
the interface. Many functions have different interfaces and arguments names and parsnip
standardizes the interface for fitting models as well as the return values. When using parsnip, 
you don't have to remember each interface and its unique set of argument names to easily 
move between R packages."

d_purrr <- "Purrr is the tidyverse's answer to apply functions for iteration. It's one of 
those packages that you might have heard of, but seemed too complicated to sit down and learn. 
Starting with map functions, and taking you on a journey that will harness the power of the
list, this post will have you purrring in no time."

d_recipes <- "The recipes package is an alternative method for creating and preprocessing
design matrices that can be used for modeling or visualization"

overviews_keys <- c("broom","dplyr", "ggplot2", "infer", "parsnip", "purrr", "recipes")
overviews_descriptions <- c(d_broom, d_dplyr, d_ggplot2, d_infer, d_parsnip, d_purrr, d_recipes)
names(overviews_descriptions) <- overviews_keys

# Define UI for application
ui <- fluidPage(
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("Tidymodels", style="color:firebrick", href="https://www.tidymodels.org/"),
        tabPanel("Installation",
                 fluidRow(
                     column(5, img(src = "tidymodels.png", hight=300, width = 300)),
                     column(3,
                            h3("TIDYMODELS"), br(),
                            p("The tidymodels framework is a collection of packages for modeling 
                            and machine learning using", strong("tidyverse"),"principles."), 
                            p("Install tidymodels with:"),
                            br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            p("Run", em("library(tidymodels)"), "to load the core packages and make 
                              them available in your current R session"))
                 ),
                 br(),
                 code("by joseluisancira")
                ),
        tabPanel("Packages", 
                 h3("CORE TIDYMODELS"),
                 uiOutput(outputId = "logo"),
                 #h1("Favor de incluir el logo correspondiente a cada paquete elegido."),
                 p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
                 selectInput("state", "Choose a tidymodel library:",
                             list(`package` = tidymodels_packages()) # hacer una lista limpia con los paquetes principales
                             ),
                 verbatimTextOutput("result")
                 #htmlOutput("result")
                 
                 ),
        tabPanel("Learn", "This panel is intentionally left blank",
                     tabsetPanel(
                         tabPanel("Statistical Analysis",
                                  tabsetPanel(
                                    tabPanel("Tab A", "Correlation and regression fundamentals with tidy data principles"),
                                      #h4("Correlation and regression fundamentals with tidy data principles"),
                                    tabPanel("Tab B", "k-means clustering with tidy data principles")),
                                      #h4("K-means clustering with tidy data principles"),
                                  tableOutput("table"),
                                  h4("Analyze the results of correlation tests and simple regression models 
                                     for many data sets at once."),
                                  verbatimTextOutput("txtout"),
                                  #h1("Correlation and regression fundamentals with tidy data principles"),
                                  p("While the tidymodels package broom is useful for summarizing the 
                                    result of a single analysis in a consistent format, it is really designed
                                    for high-throughput applications, where you must combine results from
                                    multiple analyses. These could be subgroups of data, analyses using different
                                    models, bootstrap replicates, permutations, and so on. In particular, it plays
                                    well with the nest()/unnest() functions from tidyr and the map() function in purrr"),
                                
                         ),
                         tabPanel("Robust Models", "This panel is intentionally left blank"),
                         tabPanel("Tune compare & Work with models", "This panel is intentionally left blank"),
                         tabPanel("Custom Modeling Tools", "This panel is intentionally left blank",
                                  tabsetPanel(
                                    tabPanel("Tab A", "This panel is intentionally left blank"),
                                    tabPanel("Tab B", "This panel is intentionally left blank")
                                    
                                  ))
                         
                     )
                 
                 )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
        head(cars, 4)
    })
    output$result <- renderText({
        paste(overviews_descriptions[input$state])
        #HTML(overviews_descriptions[input$state])
    })
    output$logo <-renderUI({
      tags$img(src=paste0(input$state, ".png"))
      
    })
}

# Run the application 
shinyApp(ui, server)
