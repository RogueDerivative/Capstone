

library(shiny)
library(shinycssloaders)
source("predict_word_redo.r")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    tabsetPanel(
        # First tab to show off the app
        tabPanel("App",
                 titlePanel("Word Prediction App"),
                 fluidRow(
                     column(width = 6,
                            textInput(inputId = "user_input",
                                      label = "Enter a phrase",
                                      placeholder = "e.g. I'll dust them off and be on my")
                     )
                 ),
                 fluidRow(
                     column(width = 4,
                            actionButton(inputId = "go", label = "predict"),
                            offset = 1
                     )
                     
                 ),
                 fluidRow(
                     column(
                         width=6,
                         h2("The top 3 predicted words are: "),
                         withSpinner(verbatimTextOutput("word"))
                     )
                 )
        ),
        # Second tab tells the user how to use the app
        tabPanel("How to use the app",
                 p(h2("Enter words or incomplete phrase:")), 
                 p("Enter any number of words. They do not need to be sensical."),
                 p(h2("Tap the predict button:")),
                 p("Upon hitting the predict button the algorithm will run
                   and return the top three predictions for the next word.")
        ),
        # Third tab gives a description of what the app does
        tabPanel("Description",
                 h1("My Word Prediction App"),
                 p(style = "font-family:Impact", "See presentaton",
                   a("the slides",href="http://")),
                 p("This is the Capstone project for the Data Science 
                Specialization through Coursera and Johns Hopkins.
                  Our goal was to create a word prediction application using
                  the R programming language and to then post that application 
                  onto a Shiny.io."),
                 p("Our first goal was to clean-up a series of data sets that were 
                provided by SwiftKey. The data is samples of blog posts,
                news articles, and Twitter posts. From these files we were
                tasked to create a library of n_grams that could be used for 
                prediction. Unsurprisingly (at least in terms of blog posts and Tweets)
                the data sets provided had flaws: expletives, non-ascii
                characters, non-English words, etc. My approach was to create a 
                  library of token words to then filter the higher-order 
                  n_grams by these acceptible words."),
                 p("The method of prediction was the Katz-backoff algorithm. From
                  Wikipedia: Katz back-off is a generative n-gram language model
                  that estimates the conditional probability of a word given
                  its history in the n-gram. It accomplishes this estimation
                  by backing off through progressively shorter history models
                  under certain conditions. By doing so, the model with
                  the most reliable information about a given history
                  is used to provide the better results. This isn't a true
                  Katz back-off since the discounting factor was given to be 
                  equal to 0.5 rather than determined using Good-Turing."),
                 p("The predictive algorithm was tweeked many times. One of the
                  largest time complexities was the loading of progressive n_grams.
                  Cleaning up the initial set of token words and then filtering
                  the larger n_grams by these token words significantly decreased
                  the amount of time of the prediction."),
                 p("The Shiny application is REACTIVE. This means that the app remains in 
                  a static state until the predict button is pressed. The user is
                  then free to change the words again and again to get new predictions.")
        )
    )
))
