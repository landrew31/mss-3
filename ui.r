
ui <- navbarPage(
    title = "Lab2",
    tabPanel(
        'Part2',
        fluidRow(
            column(6, wellPanel(
                numericInput("input_n2", "Input sample length", value = 30),
                sliderInput(
                    "input_ab",
                    "Input interval [a,b] for matrix X:",
                    min = 0,
                    max = 5,
                    value = c(0, 1),
                    step = 0.5
                ),
                numericInput("input_ksi_sd", "Input sd of ksi", value = 1)
            )),
            column(6, wellPanel(plotlyOutput("Criteria_plot")))
        ),
        fluidRow(wellPanel(
            dataTableOutput("R_table")
        ))
    )
)
