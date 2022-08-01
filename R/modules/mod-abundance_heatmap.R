library(plyr)
library(dplyr)
library(ggmap)
source("R/moosepop.R")
library(shiny)


abundance_heatmap_ui <- function(id) {

  fluidRow(
    column(11, plotOutput(NS(id, "plot"))),
    column( 1, downloadButton(NS(id, "dnld"), label = ""))
  )

}


abundance_heatmap_server <- function(id, df, metric) {

  moduleServer(id, function(input, output, session) {

    plot <- reactive({abundance_heatmap(df(), metric)})
    output$plot <- renderPlot({plot()})
    output$dnld <- downloadHandler(
      filename = function() {paste0(metric, '.png')},
      content = function(file) {ggsave(file, plot())}
    )

  })
}

abundance_heatmap_demo <- function() {

  data<- read.csv("data/All_20E_Surveys.csv")

  df <-data %>% filter(SurveyID == SurveyID[1])
  ui <- fluidPage(abundance_heatmap_ui("x"))
  server <- function(input, output, session) {
    abundance_heatmap_server("x", reactive({df}), "totalmoose")
  }
  shinyApp(ui, server)

}
abundance_heatmap_demo()
