library(shiny)
library(dplyr)
library(xgboost)
library(ggplot2)
library(plotly)
library(reshape2)

data <- read.csv("D:/Downloads/cleaned_data_woman_crime2.csv")

# total crimes sum
data$Total.Crimes <- rowSums(data[, c("Rape", "Kidnapping.and.Abduction", 
                                      "Dowry.Deaths", "Assault.on.women.with.intent.to.outrage.her.modesty",
                                      "Insult.to.modesty.of.Women", "Cruelty.by.Husband.or.his.Relatives",
                                      "Importation.of.Girls")])


median_total_crime <- median(data$Total.Crimes)
threshold_high <- quantile(data$Total.Crimes, probs = 0.75)
threshold_low <- quantile(data$Total.Crimes, probs = 0.25)

# risk levels defined
data$RiskLevel <- cut(data$Total.Crimes, 
                      breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                      labels = c("Low", "Medium", "High"))

# xgboost matrix
dtrain <- xgb.DMatrix(as.matrix(data$Total.Crimes), label = as.numeric(data$RiskLevel) - 1)

# xgboost parameters
params <- list(objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss")

# train 
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 10)

# UI
ui <- fluidPage(
  titlePanel("Crime Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("With Total Crimes Prediction",br(),
                 numericInput("total_crimes", "Enter Total Crimes:", value = 100),
                 sliderInput("train_test_split", "Training Data Percentage:",
                             min = 0, max = 100, value = 80),
                 actionButton("predict_button", "Predict"),
                 hr(),
                 verbatimTextOutput("prediction_text_total")
        ),
        tabPanel("With Crime Type Prediction",br(),
                 numericInput("rape", "Rape:", value = 0),
                 numericInput("kidnapping_abduction", "Kidnapping and Abduction:", value = 0),
                 numericInput("dowry_deaths", "Dowry Deaths:", value = 0),
                 numericInput("assault_on_women", "Assault on Women:", value = 0),
                 numericInput("insult_to_modesty", "Insult to Modesty:", value = 0),
                 numericInput("cruelty_by_husband", "Cruelty by Husband:", value = 0),
                 numericInput("importation_of_girls", "Importation of Girls:", value = 0),
                 hr(),
                 actionButton("predict_crime_button", "Predict"),
                 verbatimTextOutput("prediction_text_crime")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Exploratory Data Analysis",br(),
                 plotlyOutput("risk_plot"),
                 br(),  # Add a line break here
                 plotlyOutput("total_cases_plot"),
                 br(),
                 plotlyOutput("bubble_chart_cases_by_state"),
                 br(),  # Add a line break here
                 plotlyOutput("accuracy_plot"),
                 
        )
      )
    )
  )
)

# server logic
server <- function(input, output) {
  
    #total crime 
    observeEvent(input$predict_button, {

    new_observation <- data.frame(Total.Crimes = input$total_crimes)
    
    new_probabilities <- predict(xgb_model, as.matrix(new_observation))
    new_prob_matrix <- matrix(as.numeric(new_probabilities), ncol = 3, byrow = TRUE)
    new_predicted_labels <- apply(new_prob_matrix, 1, which.max) - 1
    new_predicted_risk <- factor(new_predicted_labels, levels = 0:2, labels = c("Low", "Medium", "High"))
    
    output$prediction_text_total <- renderPrint({
      paste("Predicted Risk Level:", new_predicted_risk)
    })
  })

  #input crime wise  
  observeEvent(input$predict_crime_button, {

      total_crimes <- input$rape + input$kidnapping_abduction + input$dowry_deaths +
      input$assault_on_women + input$insult_to_modesty + input$cruelty_by_husband +
      input$importation_of_girls
    
    new_observation <- data.frame(Total.Crimes = total_crimes)
    
    new_probabilities <- predict(xgb_model, as.matrix(new_observation))
    new_prob_matrix <- matrix(as.numeric(new_probabilities), ncol = 3, byrow = TRUE)
    new_predicted_labels <- apply(new_prob_matrix, 1, which.max) - 1
    new_predicted_risk <- factor(new_predicted_labels, levels = 0:2, labels = c("Low", "Medium", "High"))
    
    output$prediction_text_crime <- renderPrint({
      paste("Predicted Risk Level:", new_predicted_risk)
    })
  })
  
  # eda
 
  output$risk_plot <- renderPlotly({
    yearly_risk <- data %>%
      group_by(Year, RiskLevel) %>%
      summarise(Count = n()) 
    plot_ly(yearly_risk, x = ~Year, y = ~Count, color = ~RiskLevel, type = 'bar', 
            hoverinfo = 'y+name', text = ~Count, 
            colors = c('Low' = 'green', 'Medium' = 'orange', 'High' = 'red')) %>%
      layout(title = 'Risk Level Over Time',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Number of Districts'),
             barmode = 'stack',
             legend = list(title = 'Risk Level'))
  })
  
  output$total_cases_plot <- renderPlotly({
    
    total_cases <- aggregate(cbind(Rape, Kidnapping.and.Abduction, Dowry.Deaths,
                                   Assault.on.women.with.intent.to.outrage.her.modesty,
                                   Insult.to.modesty.of.Women, Cruelty.by.Husband.or.his.Relatives,
                                   Importation.of.Girls) ~ Year, data = data, FUN = sum)
    
    total_cases_melted <- melt(total_cases, id.vars = "Year", variable.name = "Crime", value.name = "Cases")
    
    plot_ly(total_cases_melted, x = ~Year, y = ~Cases, color = ~Crime, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Trend of Different Types of Crimes Over the Years", xaxis = list(title = "Year"), yaxis = list(title = "Number of Cases"))
  })
  
  output$bubble_chart_cases_by_state <- renderPlotly({
    total_cases_by_state <- aggregate(cbind(Rape, Kidnapping.and.Abduction, Dowry.Deaths, 
                                            Assault.on.women.with.intent.to.outrage.her.modesty,
                                            Insult.to.modesty.of.Women, Cruelty.by.Husband.or.his.Relatives,
                                            Importation.of.Girls) ~ STATE.UT, data = data, FUN = sum)
    
    total_cases_melted <- melt(total_cases_by_state, id.vars = "STATE.UT", variable.name = "Crime", value.name = "Cases")
    bubble_chart_cases_by_state <- plot_ly(total_cases_melted, x = ~STATE.UT, y = ~Cases, 
                                           color = ~Crime, size = ~Cases, 
                                           type = "scatter", mode = "markers")
    
    bubble_chart_cases_by_state <- bubble_chart_cases_by_state %>% layout(title = "Bubble Chart of Total Cases by State/UT",
                                                                          xaxis = list(title = "State/UT"),
                                                                          yaxis = list(title = "Number of Cases"),
                                                                          showlegend = TRUE)
    
    bubble_chart_cases_by_state
  })
  
  
    }

# app
shinyApp(ui = ui, server = server)
