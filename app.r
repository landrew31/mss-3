library(shiny)
library(rmutil)
library(DT)
library(plotly)

ui <- dget("ui.r")
MNK_frame <- dget("mnk_frame.r")


server <- function(input, output) {
    
    output$R_table <- renderDataTable({
        R_tbl <-
            data.frame(
                X1 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X2 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X3 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X4 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X5 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                )
            )
        theta_real <- c(3, 2, 1, 0, 0)
        R_tbl <-
            cbind(
                R_tbl,
                as.matrix(R_tbl) %*% theta_real,
                rnorm(input$input_n2, sd = input$input_ksi_sd)
            )
        colnames(R_tbl) <- c("X1", "X2", "X3", "X4", "X5", "Y0", "ksi")
        R_tbl$Y <- R_tbl$Y0 + R_tbl$ksi
        R_tbl <- round(R_tbl, 5)
        datatable(R_tbl,
                  class = "compact",
                  options = list(pageLength = 10, dom = 'tp'))
        
    })
    
    output$Criteria_plot <- renderPlotly({
        R_tbl <-
            data.frame(
                X1 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X2 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X3 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X4 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                ),
                X5 = runif(
                    input$input_n2,
                    min = input$input_ab[1],
                    max = input$input_ab[2]
                )
            )
        theta_real <- c(3, 2, 1, 0, 0)
        R_tbl <-
            cbind(
                R_tbl,
                as.matrix(R_tbl) %*% theta_real,
                rnorm(input$input_n2, sd = input$input_ksi_sd)
            )
        colnames(R_tbl) <- c("X1", "X2", "X3", "X4", "X5", "Y0", "ksi")
        R_tbl$Y <- R_tbl$Y0 + R_tbl$ksi
        res <- MNK_frame(R_tbl[, c(1:5, 8)])
        pl <-
            plot_ly(
                x = seq(5),
                y = res$RSS,
                name = "RSS(s)",
                type = "scatter",
                mode = 'lines+markers'
            ) %>%
            add_trace(x = seq(5),
                      y = res$Cp,
                      name = "Cp(s)") %>%
            add_trace(x = seq(5),
                      y = res$FPE,
                      name = "FPE(s)") %>%
            layout(
                title = "Criteria vs Complexity",
                xaxis = list(title = "s"),
                yaxis = list(title = "Criteria")
            )
        pl
    })
}

# Run the application
shinyApp(ui = ui, server = server)
