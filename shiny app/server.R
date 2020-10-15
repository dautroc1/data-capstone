

library(shiny)
library(dplyr)
library(markovchain)
library(tidyr)
library(stringr)
library(curl)
library(httr)


top5 <- c("said","will","one", "new","last")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
        
        mydata <- reactive({
            
            githubURL <- "https://github.com/dautroc1/data-capstone/raw/master/rds/unigram.rds"
            download.file(githubURL,"unigram.rds")
            fit_markov_uni <- readRDS("unigram.rds")
            
            #fit_markov_uni <- readRDS("https://github.com/dautroc1/data-capstone/raw/master/rds/unigram.rds")
            #url <- GET("https://github.com/dautroc1/data-capstone/raw/master/rds/unigram.rds")
            #fit_markov_uni <- readRDS(url)
            
            
            return(fit_markov_uni)
        })
        mydata1 <- reactive({
            
            
            
            githubURL <- "https://github.com/dautroc1/data-capstone/raw/master/rds/bigram.rds"
            download.file(githubURL,"bigram.rds")
            fit_markov_bi <- readRDS("bigram.rds")
            #fit_markov_bi <- readRDS("bigram.rds")
            
            return(fit_markov_bi)
        })
        mydata2 <- reactive({
            
            
            
            githubURL <- "https://github.com/dautroc1/data-capstone/raw/master/rds/trigram.rds"
            download.file(githubURL,"trigram.rds")
            fit_markov_tri <- readRDS("trigram.rds")
            #fit_markov_tri <- readRDS("trigram.rds")
            
            return(fit_markov_tri)
        })
        '%ni%' <- Negate('%in%')
        predictive_text_uni <- function(text){
            if(tolower(text) %ni% names(mydata()$estimate))
                return (FALSE)
            suggest <- mydata()$estimate[ tolower(text), ] %>%
                sort(decreasing = T) %>% 
                head(5) 
            suggest <- suggest[ suggest > 0] %>% 
                names() %>% 
                str_remove("[ ]")
            
            
            return(suggest)
        }
        
        predictive_text_bi <- function(text){
            if(tolower(text) %ni% names(mydata1()$estimate))
                return (FALSE)
            suggest <- mydata1()$estimate[ tolower(text), ] %>%
                sort(decreasing = T) %>% 
                head(5) 
            suggest <- suggest[ suggest > 0] %>% 
                names() %>% 
                str_remove("[ ]")
            
            
            return(suggest)
        }
        
        predictive_text_tri <- function(text){
            if(tolower(text) %ni% names(mydata2()$estimate))
                return (FALSE)
            suggest <- mydata2()$estimate[ tolower(text), ] %>%
                sort(decreasing = T) %>% 
                head(5) 
            suggest <- suggest[ suggest > 0] %>% 
                names() %>% 
                str_remove("[ ]")
            
            
            return(suggest)
        }
        
        
        
        Predict <- function(text){
            text <- tolower(text)
            result <- TRUE
            n <- str_count(text)
            if (n >= 3)
            {
                temp <- paste(word(text,-3) ,word(text,-2) ,word(text,-1), sep = " ")
                result <- predictive_text_tri(temp)
                
            }
            if (n == 2 | result == FALSE)
            {
                temp <- paste(word(text,-2) ,word(text,-1), sep = " ")
                result <- predictive_text_bi(temp)
            }
            if (n == 1 | result == FALSE)
            {
                temp <- paste(word(text,-1), sep = " ")
                result <- predictive_text_uni(temp)
            }
            if(result == FALSE)
            {
                return (top5)
            }
            else
            {
                return (result)
            }
            
        }
    
    output$text <- renderText({
        result <- Predict(input$textinput)
        if(result == TRUE)
        {
            print("...")
            
        }
        else
        {
        print(result)
        
        }
    })
    output$wordcloud <- renderUI({
        
        tags$img(src = "https://raw.githubusercontent.com/dautroc1/data-capstone/master/image/wordcloud.png")
    })
    output$wordcloud2 <- renderUI({
        tags$img(src = "https://raw.githubusercontent.com/dautroc1/data-capstone/master/image/wordcloud2.png")
    })
    output$wordcloud3 <- renderUI({
        tags$img(src = "https://raw.githubusercontent.com/dautroc1/data-capstone/master/image/wordcloud3.png")
    })
    

})
