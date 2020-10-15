

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text prediction"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("Instruction"),
            h5("1. Input data to the box"),
            h5("2. The prediction print below the box"),
            h5("3. The wordcloud tab show wordcloud of 1, 2, 3 words")
                
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("predict",
                textInput("textinput","Input value"),
                textOutput("text")),
                tabPanel("wordcloud",
                         h2("1 word"),
                         uiOutput(outputId="wordcloud"),
                         h2("2 words"),
                         uiOutput(outputId="wordcloud2"),
                         h2("3 words"),
                         uiOutput(outputId="wordcloud3")))
        )
    )
))
