
#Capstone dashboard using shiny application

library(dplyr)
library(shiny)
library(DT)
library(data.table)

library(randomForest)



#JD's model

#
X1 = fread("df.csv",
              select =c("Agencia_ID","Cliente_ID","NombreCliente", "Producto_ID","NombreProducto" , "Town" , "State" , "Target" , "Predict") )

colnames(X1)[colnames(X1) == 'Predict'] <- "Prediction"
#X1=X2

#$$$$$$$$$$$$$$$$$$$$$


X1 = arrange(X1, NombreCliente )


##### Shiny up code starts here#########

ui = fluidPage(
    fluidRow( br(),column(4 ),
              column(5,    titlePanel( h4 ("Demand Model Analysis for Groupo Bimbo" )  )),
              column(3, tags$img( height = 60, width = 100, src ="logo2.png"))),

    #column(2,tags$img( height = 60, width = 120, src ="logo2.png")),
    sidebarLayout(
        sidebarPanel( width = 3,

                      selectInput("Agent", "Select an Agent", c('All', sort(unique((X1$Agencia_ID)))    )  ),
                      selectInput("CLient", "Select a Client", c(" All",  sort(unique((X1$NombreCliente)))))
        ),
        mainPanel(

            br(),

            tabsetPanel(
                tabPanel( 'Actual vs. Predicted Demand',
                          fluidRow( div(  DT::dataTableOutput("table"), style = "font-size : 80%; width : 80%" )   )  ),

                tabPanel( 'Total Predicted Demand by Product',
                          fluidRow (  div(  DT::dataTableOutput("table2"), style = "font-size : 80%; width : 80%" ) )
                ),

                tabPanel( 'Prediction Plot' , plotOutput('plot',  width = "700px", height = "500px") )

            )

        )   )   )

server = function(input, output, session) {

    observe({
        sel_Ag <- input$Agent

        updateSelectInput(session, "CLient", choices = c("All",sort(X1$NombreCliente[X1$Agencia_ID == sel_Ag] )))

        if (input$Agent == "All" ) {
            updateSelectInput(session, "CLient", choices = c("All",sort(X1$NombreCliente )
            ) )  }
    }   )

    #
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        data <- X1
        if (input$Agent != "All" & input$CLient == "All") {
            data <- data[data$Agencia_ID == input$Agent,] }
        if (input$Agent != "All" & input$CLient != "All") {
            data <- data[data$NombreCliente == input$CLient,] }

        if (input$Agent == "All" & input$CLient != "All") {
            data <- data[data$NombreCliente == input$CLient,] }


        #  data$predReturns = data$Predicted - data$Target
        #  data$Cost.predReturns = (data$Predicted - data$Target )*data$Dev_proxima/data$Dev_uni_proxima
        data$Cliente_ID = NULL
        data$Producto_ID = NULL
        data
    })
    )

    # Summary data based grouped by Products
    output$table2 <- DT::renderDataTable(DT::datatable({
        data2 <- X1
        if (input$Agent == "All" & input$CLient == "All") {
            data2 <- data2 }
        if (input$Agent != "All" & input$CLient == "All") {
            data2 <- data2[data2$Agencia_ID == input$Agent,] }
        if ( input$CLient != "All") {
            data2 <- data2[data2$NombreCliente == input$CLient,] }


        data2 = data2 %>% group_by(NombreProducto) %>%
            summarize( Predicted.Total_Demand = round(sum(Prediction), 0))


        data2
    })
    )

    output$plot <- renderPlot({

        plot(X1$Target,X1$Prediction,  xlab = "Target" ,ylab="Prediction", main = "  randomForest Model", col =4)
        abline(a= 0, b= 1,  col =2)

    })


}

shinyApp(ui = ui, server = server)


