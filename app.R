library(shiny)
library(quantreg)
library(Ecdat)
data(Workinghours)

ui <- fluidPage(
  titlePanel("Quantile Regression Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("tau", "Percentile of Income by Age:",
                  min = 0.01, max = 0.99, value = 0.5, step = 0.01),
      sliderInput("lambda", "Lambda Parameter:",
                  min = 1, max = 500, value = 1, step = 1),
      checkboxGroupInput("variables", "Select Variables to Include:",
                         choices = setdiff(colnames(Workinghours), c("income", "age")),
                         selected = setdiff(colnames(Workinghours), c("income", "age")))
    ),
    mainPanel(
      plotOutput("quantilePlot")
    )
  )
)

server <- function(input, output) {
  # Debugging: Print package versions
  print(packageVersion("quantreg"))
  print(packageVersion("Ecdat"))
  
  output$quantilePlot <- renderPlot({
    lambdasmooth <- input$lambda
    tau <- input$tau
    selected_vars <- input$variables
    formula <- as.formula(paste("income~qss(age, lambda=lambdasmooth) +",
                                paste(selected_vars, collapse="+")))
    quantfit <- rqss(formula, tau = tau, data = Workinghours)
    plot(quantfit, xlim=c(0,64), ylim=c(0, 500))  # Plotting the quantile regression results
  })
}

shinyApp(ui = ui, server = server)
