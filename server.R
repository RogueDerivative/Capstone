library(shiny)


shinyServer(function(input, output) {

    re <- eventReactive(
        input$go,
        {last_word(input$user_input)})
    
    output$word <- renderPrint({
        word <- re()
    })

})
