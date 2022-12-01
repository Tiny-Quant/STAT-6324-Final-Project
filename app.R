#
# Heart Health App
# Delaney Helgeson and Art Tay
# STAT 6324 - Computing in R: Final Project
#
#


library(shiny)
library(fontawesome)
library(shinythemes)
library(shinydashboard)

setwd("C:/Users/delan/Downloads/STAT6324 Computing in R/HeartDiseaseApp")

heart_xgb_tidy_optimal_fit <- readRDS("heart_xgb_tidy_optimal_fit.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin='red',
                    dashboardHeader(title = 'Heart Health'),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = 'home_tab', icon = icon('home')))),
                    dashboardBody(    fluidRow(box(
                      title = "Check your Heart Health!", background = 'red', solidHeader = TRUE,
                      "Upload a csv with information related to your health history
    to receive a predicted probability that you're at risk for heart disease. 
    For more details on the appropriate column structure of your file, 
     see the example dataset in the documentation.", width = 12)),
    
    fluidRow(box(
      title = "Upload your health history data", background = 'navy',
      fileInput(inputId = "user_file", label = "Choose File", accept = c(".csv")),
      actionButton(inputId = "Go", label = "Go", icon("refresh")),
      width = 6
    ), 
    valueBoxOutput("projected_heart_disease_box", width = 6)))

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # PREDICTION
  store_predicted <- reactiveValues()
  
  # Event for user file upload
  observeEvent(input$Go, {
    inFile <- input$user_file
    user_data <- read_csv(inFile$datapath)
    demo_obs <- user_data %>% 
      mutate(HeartDisease = as_factor(HeartDisease),
             AgeCategory1 = case_when(
               AgeCategory == "18-24" ~ 21,
               AgeCategory == "25-29" ~ 27,
               AgeCategory == "30-34" ~ 32,
               AgeCategory == "35-39" ~ 37,
               AgeCategory == "40-44" ~ 42,
               AgeCategory == "45-49" ~ 47,
               AgeCategory == "50-54" ~ 52,
               AgeCategory == "55-59" ~ 57,
               AgeCategory == "60-64" ~ 62,
               AgeCategory == "65-69" ~ 67,
               AgeCategory == "70-74" ~ 72,
               AgeCategory == "75-79" ~ 77,
               AgeCategory == "80 or older" ~ 82,
             ),
             AgeCategory = factor(AgeCategory1, ordered = TRUE),
             GenHealth =  factor(case_when(
               GenHealth == "Poor" ~ 1,
               GenHealth == "Fair" ~ 2,
               GenHealth == "Good" ~ 3,
               GenHealth == "Very good" ~ 4,
               GenHealth == "Excellent" ~ 5
             ), ordered = TRUE),
             MentalHealth = factor(MentalHealth, ordered = TRUE),
             PhysicalHealth = factor(PhysicalHealth, ordered = TRUE)) %>%
      select(-c("AgeCategory1"))
    store_predicted$user_pred <- round(predict(heart_xgb_tidy_optimal_fit,
                                               demo_obs, type = "prob")$.pred_Yes, 3)
  })
  
  # Heart Disease Score output Box
  output$projected_heart_disease_box <- renderValueBox({ 
    if(is.null(store_predicted$user_pred)) {
      user_pred_print <- c(0.0)
    } else{
      user_pred_print <- store_predicted$user_pred
    }
    valueBox(
      user_pred_print, "Predicted Probability of Heart Disease",
      icon = icon("heart-pulse", lib = "font-awesome"),
      color = "red"
    )
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
