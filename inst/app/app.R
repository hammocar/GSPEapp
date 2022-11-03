
library(shiny)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(jsonlite)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggmap)
library(mrds)
library(raster)
library(plyr)
library(gdata)
library(reshape)
library(knitr)
library(gridExtra)
library(gtools)
library(stringr)
library(PracTools)
library(purrr)
library(dbplyr)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(kableExtra)
library(sp)
library(readr)
library(DT)
library(tools)
library(DBI)
library(odbc)
library(lubridate)
library(bslib)


source("moosepop.R")
# source("D:/Projects/Moose/Surveys/John's Code/GSPEandMoosePopCode.r")
source("SCF_apply_functions.R")

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'









ui <- fluidPage(shinyjs::useShinyjs(),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                  tags$script(src = "script.js", type="text/javascript"),
                  tags$style(".shiny-notification {
                  height: 100%;
                  width: 100%;
                  position:fixed;
                  top: 50px;;
                  left: 0;;}"))
                ,
                theme=bs_theme(version = 4,
                               bootswatch = "default"),

                uiOutput("body"))


source("moosepop.R")
# source("D:/Projects/Moose/Surveys/John's Code/GSPEandMoosePopCode.r")
source("SCF_apply_functions.R")

textStyle <- element_text(face = "bold.italic", color = "black", size = 20)

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Define server logic




##################################################################


server<-shinyServer(function(input, output, session) {

########################################################################
# reactive UI elements
########################################################################


    observe({
        if (values$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (input$Login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    moose <- odbc::dbConnect(odbc(),
                                             Driver = "SQL Server",
                                             Server = "DWCDBP",
                                             Database= "WC_moosepop",
                                             UID = Username,
                                             PWD = Password,
                                             trusted_connection = "true",
                                             Port = 1443,
                                             TDS_Version = 7.2)
                    if (class(moose)[1] == "Microsoft SQL Server"){
                      values$Logged <- TRUE
                    }
                }
            }
        }
    })

    output$body <- renderUI({
        if (values$Logged == TRUE) {

          navbarPage("MOOSE MANAGER",
                     theme = bs_theme(bootswatch = "lux"),
                     inverse = TRUE,
                     tabPanel("Home",
                              fluidRow(
                                column(4), column(4,imageOutput("home_img")), column(4)),
                              hr(),
                              h1(strong("Moose Manager")),
                              p(style="text-align: justify; font-size = 45px",
                                "Moose Manager is a moose data dashboard made with
          the purpose of replacing and improving upon the Winfonet survey and inventory tools, creating a one-stop-shop for all
                                 moose S&I data."),

                              tags$blockquote("Moose Manager is still under continuous development.
           Please look forward to future updates!"),
                              hr()),
                     tabPanel("GSPE",
                              tabsetPanel(
                                id = "abundance_tabs",
                                tabPanel(
                                  value = "Survey Search",
                                  title = "Survey Search",
                                  h3('Browse for Surveys'),
                                  box(width = 12,
                                      title = "",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      h4("Select a survey in the table"),
                                      column(width = 6,
                                      DT::dataTableOutput("tbl"),
                                      uiOutput("columns")),
                                      uiOutput("survey_action")%>% withSpinner(color="#0dc5c1")
                                      )))),
                             tabPanel("Survival"),
                             tabPanel("Twinning"),
                             tabPanel("Browse"))
        } else {
          navbarPage("MOOSE MANAGER",
                box(    width = 6,
                        textInput("userName", "Username"),
                        passwordInput("passwd", "Password"),
                        br(),
                        actionButton("Login", "Log in")))
        }
    })

########################################################################
# DEFINE REACTIVE VALUES
########################################################################

    values<-reactiveValues(Logged = F, # T/F logged in variable
                           moose = NULL, # database connection
                           authorized_surveys = NULL, # List of all surveys the biologist has access to
                           survey_ids = NULL, # ID from selected survey
                           moose.dat = NULL, # Data from the selected survey
                           trend_data = NULL,
                           tab_change = 0) # Data from all surveys with area matches to the selected survey

    survey_data_idcount <- reactiveVal(0)
    single_GSPE_idcount <- reactiveVal(0)
    trend_idcount <- reactiveVal(0)

# On login, establish data connection and get available surveys
    observeEvent(input$Login > 0, {
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)

      moose <- DBI::dbConnect(
        odbc::odbc(),
        Driver      = "SQL Server",
        Server = "DFGJNUSQL-DB72P",
        Database= "WC_moosepop",
        Trusted_Connection = "True",
        Port        = 1433
      )

        query<-paste("SELECT surveyname, surveyyear, surveyid
    FROM v_wc_moosepop_reprospreadsheet
    GROUP BY surveyname, surveyyear, surveyid")

      values$authorized_surveys<-dbGetQuery(moose,query)
      values$moose<-moose
      })

observeEvent(input$tbl_rows_selected,{
    # Look at the row of the table that was clicked, get the survey id
    values$survey_ids <- values$authorized_surveys[input$tbl_rows_selected,"surveyid"]
    values$moose.dat<- dbGetQuery(values$moose, paste("exec spr_wc_moosepop_reprospreadsheet @surveyIDlist = '", values$survey_ids,"'", sep = ""))

})






########################################################################
# HOME PAGE SERVER ELEMENTS
########################################################################

    output$home_img <- renderImage({
      list(src = "moose_pic.jpg",
           width = 500)
    }, deleteFile = F)

########################################################################
# SURVEY SEARCH
########################################################################


    output$tbl <- DT::renderDataTable({
      values$authorized_surveys <- values$authorized_surveys %>% arrange(desc(surveyyear))
        datatable(values$authorized_surveys[,c("surveyname", "surveyyear")],
                  filter = 'top',
                  selection = 'single',
                  options = list(dom = "Blrtip",
                                 iDisplayLength = 5),
                  rownames = FALSE)
    })

    output$survey_action<-renderUI({
      req(input$tbl_rows_selected)
      actionGroupButtons(
        inputIds = c("survey_data", "GSPE", "Trend", "Plan"),
        labels = c("View/download survey data", "Single survey GSPE details", "Trend analysis", "Plan a new survey for this area"),
        status = "primary"
        )})

    output$columns<-renderUI({
      req(input$tbl_rows_selected)
      pickerInput(inputId = "columns", label = "Identify analysis area columns (if any)", choices = names(values$moose.dat), multiple = TRUE)
      })

########################################################################
# ADD TABS BASED ON WHAT THE USER WANTS TO DO WITH THE SELECTED SURVEY
########################################################################
    ## tab title with close button
    tab_title <- function(name, type = "data") {
      tags$span(
        name,
        tags$span(icon("times"),
                  style = "margin-left: 5px;",
                  onclick = paste0("Shiny.setInputValue(\"", paste0("remove_", type, "_tab"), "\", \"", name, "\", {priority: \"event\"})"))
      )
    }
    ############################
    # View/download survey data
    ############################
    # When the button is clicked

    observeEvent(input$survey_data , {


      survey_data_thisid <- survey_data_idcount() + 1
      survey_data_idcount(survey_data_thisid)
      survey_data_thisid <- paste0("survey_data", survey_data_thisid)




      # Add the view survey tab
      appendTab("abundance_tabs",
                tabPanel(
                  value =paste("View data", values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"]),
                  title = tab_title( paste("View data", values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"])),
                  box(width = 12,
                      title = "Survey Data",
                      status = "primary",
                      solidHeader = TRUE,
                      div(id = survey_data_thisid)
                  )), select=TRUE)


      insertUI(selector = paste("#",survey_data_thisid, sep = ""), where = "beforeEnd",
               ui = DT::dataTableOutput(survey_data_thisid))

      # This part is CRUCIAL for making new tabs not overwrite old tabs
      # It takes the survey data out of a reactive context
      values[[survey_data_thisid]]<-values$moose.dat
      survey_data<-values[[survey_data_thisid]]


      output[[survey_data_thisid]]<- DT::renderDataTable(server = FALSE, {
        survey_data
        },
        extensions = 'Buttons',
        options = list(dom = "Blfrtip",
                       scrollX = T,
                       iDisplayLength = 10,
                       buttons = list("copy",
                                      list(extend = "collection" ,
                                           buttons = c("csv", "excel", "pdf"),
                                           text = "Download",
                                           exportOptions = list(modifier = list(page = "all"))))),
        rownames = FALSE)


      observeEvent(input$remove_data_tab, {
       isolate({ removeTab(inputId = "abundance_tabs", target = input$remove_data_tab)})
        })


    })



    ############################
    # Single GSPE
    ############################

    observeEvent(input$GSPE ,{
      single_GSPE_thisid <- single_GSPE_idcount() + 1
      single_GSPE_idcount(single_GSPE_thisid)
      single_GSPE_thisid <- paste0("GSPE", single_GSPE_thisid)


      # Add option for sightability correction using trials H/L
      output[[paste(single_GSPE_thisid, "sightability_factor", sep = "")]]<-renderUI({
        if(input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]] == "Constant" & input[[paste(single_GSPE_thisid, "sightability", sep = "")]]){
          fluidRow(
            box(width = 3,
                numericInput(paste(single_GSPE_thisid, "SCF_standard", sep = ""), "Enter a constant SCF (e.g. 1.2)", value = 1, min = 1, step = .05 )))
        }
        else if(input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]] == "Trials" & input[[paste(single_GSPE_thisid, "sightability", sep = "")]]){
          fluidRow(
            box(width = 10,
                numericInput(paste(single_GSPE_thisid, "high_trials", sep = ""), "Number of High Stratum Sightability Trials", value = 0, min = 1, step = 1),
                numericInput(paste(single_GSPE_thisid, "high_missed", sep = ""), "Number of High Stratum Moose Missed", value = 0, min = 1, step = 1),
                numericInput(paste(single_GSPE_thisid, "low_trials", sep = ""), "Number of Low Stratum Sightability Trials", value = 0, min = 1, step = 1),
                numericInput(paste(single_GSPE_thisid, "low_missed", sep = ""), "Number of Low Stratum Moose Missed", value = 0, min = 1, step = 1)
            ))
        }
        else{
          NULL
        }

      })

      # If we want to use sightability, toggle on the sightability type choice UI
      observeEvent(input[[paste(single_GSPE_thisid, "sightability", sep = "")]],{
        toggle(
          paste(single_GSPE_thisid, "sightability_type", sep = "")
        )
      })

      # reactive sightability type choice UI
      output[[paste(single_GSPE_thisid, "sightability_type", sep = "")]]<-renderUI({
        fluidRow(
          box(width = 4,
              radioButtons(paste(single_GSPE_thisid, "sightability_type", sep = ""), "Type of sightability", choices = c("Trials", "Constant"), inline = T, selected = "Trials")))
      })

      # Add the UI for constant SCF
      output[[paste(single_GSPE_thisid, "sightability_constant", sep = "")]]<-renderUI({
        fluidRow(
          box(width = 4,
              numericInput(paste(single_GSPE_thisid, "sightability_constant", sep = ""),
                           "Enter a constant SCF (e.g., 1.2)",
                           value = 1, min = 1, step = .05 )))

        })

      # Add the UI for trials
      output[[paste(single_GSPE_thisid, "sightability_trials", sep = "")]]<-renderUI({
        fluidRow(
          column(width = 3,
                 box(width = 10, title = "High Strat",
                     numericInput(paste(single_GSPE_thisid, "high_trials", sep = ""), "Trials", value = 0, min = 1, step = 1),
                     numericInput(paste(single_GSPE_thisid, "high_missed", sep = ""), "Moose Missed", value = 0, min = 1, step = 1))),
          column(width = 3,
                 box(width = 10, title = "Low Strat",
                     numericInput(paste(single_GSPE_thisid, "low_trials", sep = ""), "Trials", value = 0, min = 1, step = 1),
                     numericInput(paste(single_GSPE_thisid, "low_missed", sep = ""), "Moose Missed", value = 0, min = 1, step = 1))),
          column(width = 6))
        })

      # If we want to use sightability, toggle on the sightability type choice UI
      observeEvent(c(input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]]  , input[[paste(single_GSPE_thisid, "sightability", sep = "")]] ),{
        toggle(
          paste(single_GSPE_thisid, "sightability_trials", sep = ""), condition = input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]] == "Trials" & input[[paste(single_GSPE_thisid, "sightability", sep = "")]]
        )
          toggle(
            paste(single_GSPE_thisid, "sightability_constant", sep = ""), condition = input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]] == "Constant" & input[[paste(single_GSPE_thisid, "sightability", sep = "")]]
          )

      })


      appendTab("abundance_tabs",
                tabPanel(
                  value = paste("GSPE details:", values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"]),
                  title =tab_title( paste("GSPE details:", values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"])),
                  tags$script(
                    "$( document ).ready(function() {
               $(\".tab-content [type='actionButton']\").on('click', function(){
                  setTimeout(function() {
                    window.scrollTo(0,document.body.scrollHeight);
                }, 200)
               })
             })"
                  ),
                  h1(strong( "GeoSpatial Population Estimate")),
                  checkboxInput(paste(single_GSPE_thisid, "sightability", sep = ""), "Use sightability correction factor", value = FALSE),
                  div(id = paste(single_GSPE_thisid, "sightability_type", sep = ""),
                      uiOutput(paste(single_GSPE_thisid, "sightability_type", sep = ""))),
                  div(id = paste(single_GSPE_thisid, "sightability_trials", sep = ""),
                      uiOutput(paste(single_GSPE_thisid, "sightability_trials", sep = ""))),
                  div(id = paste(single_GSPE_thisid, "sightability_constant", sep = ""),
                      uiOutput(paste(single_GSPE_thisid, "sightability_constant", sep = ""))),
                  h4(strong("Total Abundance")),
                  div(id = paste(single_GSPE_thisid, 1, sep = "")),
                  h4(strong("Bull:Cow Composition")),
                  div(id = paste(single_GSPE_thisid, 2, sep = "")),
                  h4(strong("Calf:Cow Composition")),
                  div(id = paste(single_GSPE_thisid, 3, sep = "")),
                  h4(strong("Maps")),
                    div(style="display: inline-block;vertical-align:top; width: 200px;",
                  selectInput(paste(single_GSPE_thisid, "metric", sep = ""), "",
                              choices = c("Predicted total moose" = "totalmoose",
                                          "Predicted bull moose" = "TotalBulls",
                                          "Predicted cow moose" = "TotalCows",
                                          "Predicted calf moose" = "TotalCalves",
                                          "Stratification map" = "strat_map",
                                          "Stratification + counts" = "strat_map_w_counts"),
                              selected = NULL ,selectize = T,
                              multiple = FALSE)),
                  div(style="display: inline-block;vertical-align:top; width: 150px;",
                      br(),
                  actionButton(paste(single_GSPE_idcount(), "maps", sep = ""), "Create map")),
                  uiOutput(paste(single_GSPE_thisid, "metric", sep = "")),
                  div(id = paste(single_GSPE_thisid, 4, sep = ""))

                          ), select=TRUE)


      observeEvent(input$remove_data_tab, {
        isolate({ removeTab(inputId = "abundance_tabs", target = input$remove_data_tab)})
      })


    withProgress(message = "Computing results", detail = "fetching data", value = 0, {


    incProgress(0.25, detail = "computing abundance")

    # Run the GSPE code on the data
    values[[paste(single_GSPE_thisid, "table_data", sep = "")]]<-AA_tables(values$moose.dat, input$columns)
    values[[paste(single_GSPE_thisid, "table_data", sep = "")]]$moose.dat<-values$moose.dat
    table_data<-values[[paste(single_GSPE_thisid, "table_data", sep = "")]]


    # Abundance table output

    insertUI(selector = paste("#",single_GSPE_thisid, 1, sep = ""), where = "beforeEnd",
               ui = DT::dataTableOutput(paste(single_GSPE_thisid, "GSPE_table_abundance_SCF", sep = "")) %>% withSpinner(color="#0dc5c1"))


    output[[paste(single_GSPE_thisid, "GSPE_table_abundance_SCF", sep = "")]]<-DT::renderDataTable({
      abundance_data<-
        as.data.frame(rbind(table_data[["total_abundance"]],
                            table_data[["cow_abundance"]],
                            table_data[["bull_abundance"]],
                            table_data[["calf_abundance"]])) %>%
        mutate(Metric = rep(c("Total", "Cows", "Bulls", "Calves"), each = length(input$columns)+1)) %>%
        relocate(Metric, .after = Area)
      if(is.null(input[[paste(single_GSPE_thisid, "sightability", sep = "")]]))

        return(abundance_data)

      if(input[[paste(single_GSPE_thisid, "sightability", sep = "")]] == FALSE | is.null(input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]]))

        return(abundance_data)

      if(input[[paste(single_GSPE_thisid, "sightability", sep = "")]] == TRUE & is.null(input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]]) )
        return(abundance_data)

      if(input[[paste(single_GSPE_thisid, "sightability", sep = "")]] == TRUE & input[[paste(single_GSPE_thisid, "sightability_type", sep = "")]] == "Trials"){
        scf<-map(1:(length(input$columns)+1), ~
                 scf.apply1(hi.est = as.numeric(as.data.frame(table_data[["total_abundance"]])$High.Est[.x]),
                            lo.est = as.numeric(as.data.frame(table_data[["total_abundance"]])$Low.Est[.x]),
                            hi.se = as.numeric(as.data.frame(table_data[["total_abundance"]])$High.SE[.x]),
                            lo.se = as.numeric(as.data.frame(table_data[["total_abundance"]])$Low.SE[.x]),
                            n.trials.hi = input[[paste(single_GSPE_thisid, "high_trials", sep = "")]],
                            n.trials.lo = input[[paste(single_GSPE_thisid, "low_trials", sep = "")]],
                            n.seen.hi = input[[paste(single_GSPE_thisid, "high_trials", sep = "")]]-input[[paste(single_GSPE_thisid, "high_missed", sep = "")]],
                            n.seen.lo = input[[paste(single_GSPE_thisid, "low_trials", sep = "")]]-input[[paste(single_GSPE_thisid, "low_missed", sep = "")]]))

      scf <- ldply(scf, data.frame)
      SCF<-as.data.frame(table_data[["total_abundance"]])
      SCF$Area<-as.data.frame(table_data[["total_abundance"]])$Area
      SCF$Total.Est<-scf$tot.moose
      SCF$High.Est<-scf$tot.hi
      SCF$Low.Est<-scf$tot.lo
      SCF$Total.SE<-scf$tot.moose.se
      SCF$High.SE<-scf$tot.hi.se
      SCF$Low.SE<-scf$tot.lo.se
      SCF$`Tot.RP@90`<-scf$rp.tot
      SCF$`High.RP@90`<-scf$rp.90.hi
      SCF$`Low.RP@90`<-scf$rp.90.lo
      SCF[,2:7]<-round(SCF[,2:7])
      SCF[,8:10]<-round(SCF[,8:10],2)
      SCF
      }
      else{
        SCF<-as.data.frame(table_data[["total_abundance"]])[,c("Area", "Total.Est", "High.Est", "Low.Est")]
        SCF[,c("Total.Est", "High.Est", "Low.Est")]<-round(SCF[,c("Total.Est", "High.Est", "Low.Est")]*input[[paste(single_GSPE_thisid, "sightability_constant", sep = "")]])
        SCF
      }
    }, options = list(dom = 't'),rownames = FALSE)

    # Bull:Cow table output

    incProgress(0.25, detail = "computing bull:cow ratios")

    insertUI(selector = paste("#",single_GSPE_thisid, 2, sep = ""), where = "beforeEnd",
             ui = DT::dataTableOutput(paste(single_GSPE_thisid, "GSPE_table_bullcow", sep = "")) %>% withSpinner(color="#0dc5c1"))

    output[[paste(single_GSPE_thisid, "GSPE_table_bullcow", sep = "")]]<-DT::renderDataTable({
      table_data$bullcow}, options = list(dom = 't'),rownames = FALSE)

    # Calf:Cow table output

    incProgress(0.25, detail = "computing calf:cow ratios")

    insertUI(selector = paste("#",single_GSPE_thisid, 3, sep = ""), where = "beforeEnd",
             ui = DT::dataTableOutput(paste(single_GSPE_thisid, "GSPE_table_calfcow", sep = "")) %>% withSpinner(color="#0dc5c1"))

    output[[paste(single_GSPE_thisid, "GSPE_table_calfcow", sep = "")]]<-DT::renderDataTable({
      table_data$calfcow}, options = list(dom = 't', selection = "none"),rownames = FALSE)


    updateTabsetPanel(session, "abundance_tabs",
                      selected = paste("GSPE details:", values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"])
    )

})
    })


observeEvent(input[[paste(single_GSPE_idcount(), "maps", sep = "")]],{

  single_GSPE_thisid <- single_GSPE_idcount()
  single_GSPE_idcount(single_GSPE_thisid)
  single_GSPE_thisid <- paste0("GSPE", single_GSPE_thisid)

  table_data<-values[[paste(single_GSPE_thisid, "table_data", sep = "")]]
  # If we want a different map (This takes some time, depending on the zoom)
  # Google map API key workaround
  height <- max(table_data$moose.dat$centrlat) - min(table_data$moose.dat$centrlat)
  width <- max(table_data$moose.dat$centrlon) - min(table_data$moose.dat$centrlon)
  borders <- c(bottom  = min(table_data$moose.dat$centrlat)  - 0.1 * height,
               top     = max(table_data$moose.dat$centrlat)  + 0.1 * height,
               left    = min(table_data$moose.dat$centrlon) - 0.1 * width,
               right   = max(table_data$moose.dat$centrlon) + 0.1 * width)


  insertUI(selector = paste("#",single_GSPE_thisid, 4, sep = ""), where = "beforeEnd",
           ui = plotOutput(paste(single_GSPE_thisid, "GSPE_plot", sep = ""))%>% withSpinner(color="#0dc5c1") )

  output[[paste(single_GSPE_thisid, "GSPE_plot", sep = "")]] <- renderPlot({
    # Abundance heat map



    A <- get_stamenmap(borders, zoom = 3, maptype = "terrain")


    plot_data_filter<-case_when(input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "totalmoose" ~ "total_results",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalBulls" ~ "bull_results",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCows" ~ "cow_results",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCalves" ~ "calf_results",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map" ~ "total_results",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map_w_counts" ~ "total_results")

    plot_data<- cbind(table_data$moose.dat,
                      table_data[[plot_data_filter]][[1]]$predictions[,4:5])


    plot_fill_metric<-case_when(input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "totalmoose" ~ "log(Est+1)",
                           input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalBulls" ~ "log(Est+1)",
                           input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCows" ~ "log(Est+1)",
                           input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCalves" ~ "log(Est+1)",
                           input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map" ~ "Stratname",
                           input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map_w_counts" ~ "Stratname")

    plot_unit_label_metric<-case_when(input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "totalmoose" ~ "totalmoose",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalBulls" ~ "TotalBulls",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCows" ~ "TotalCows",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCalves" ~ "TotalCalves",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map" ~ "NA",
                                input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map_w_counts" ~ "totalmoose")




    metric_label<-case_when(input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "totalmoose" ~ "Total",
                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalBulls" ~ "Bull",
                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCows" ~ "Cow",
                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCalves" ~ "Calf")



    if(is.element(input[[paste(single_GSPE_thisid, "metric", sep = "")]], c("totalmoose","TotalBulls","TotalCows","TotalCalves"))){
      scale_fill <- scale_fill_gradient(breaks = log(c(0, 5, 15, 30, 60, 120, 200)+1),
                                        labels = c(0, 5, 15, 30, 60, 120, 200))
      }
    else{
      scale_fill <- scale_fill_discrete()
    }


    pred_plot<-ggmap(A)+geom_tile(data = plot_data, aes_string(x="centrlon",y="centrlat",fill= plot_fill_metric),alpha= 1,colour="black")+
      geom_text(data=subset(plot_data, Counted== TRUE),aes_string(x="centrlon",y="centrlat",label= plot_unit_label_metric), color = "white", size = 5)+
      ggtitle(paste(plot_data$SurveyName),subtitle = paste(plot_data$Surveyyear, plot_data$Season, case_when(input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "totalmoose" ~ paste("Predicted", metric_label,"Moose Abundance"),
                                                                                                             input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalBulls" ~ paste("Predicted", metric_label,"Moose Abundance"),
                                                                                                             input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCows" ~ paste("Predicted", metric_label,"Moose Abundance"),
                                                                                                             input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCalves" ~ paste("Predicted", metric_label,"Moose Abundance"),
                                                                                                             input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map" ~ "Stratification",
                                                                                                             input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map_w_counts" ~ "Stratification and Total Moose Counts")))+
      labs(x = "", y = "", fill = case_when(input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "totalmoose" ~ "Moose",
                                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalBulls" ~ "Moose",
                                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCows" ~ "Moose",
                                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "TotalCalves" ~ "Moose",
                                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map" ~ "Strat",
                                            input[[paste(single_GSPE_thisid, "metric", sep = "")]] == "strat_map_w_counts" ~ "Strat"))+
      scale_fill+
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0),
            plot.subtitle  = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0),
            legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
            axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
            legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
            axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
            legend.position = "bottom",
            panel.background = element_rect(fill = "white", colour = "#666666"),
            panel.grid.major = element_line(size = 0.0005, linetype = 'solid',
                                            colour = "grey"),
            legend.key.size = unit(2.5, "cm"))
        pred_plot
      }, height = 1000, units = "px")


})


    ############################
    # Trend Analysis
    ############################
    observeEvent(input$Trend , {
      withProgress(message = "Computing results", detail = "fetching data", value = 0, {

      trend_thisid <- trend_idcount() + 1
      trend_idcount(trend_thisid)
      trend_thisid <- paste0("trend", trend_thisid)

      observeEvent(input$remove_data_tab, {
        isolate({ removeTab(inputId = "abundance_tabs", target = input$remove_data_tab)})
      })

    appendTab("abundance_tabs",
    tabPanel(
      value = paste(values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"], "Trend analysis"),
      title = tab_title(paste(values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"], "Trend analysis")),
      box(width = 12,
          title = "Trend Analysis",
          status = "primary",
          solidHeader = TRUE,
          DT::dataTableOutput(paste(trend_thisid, "trend_data_selection_table", sep = ""))%>% withSpinner(color="#0dc5c1"),
          actionButton(paste(trend_thisid, "remove_from_trend", sep = ""), "Remove from trend ") ,
          plotOutput(paste(trend_thisid, "matching_abundance_plot", sep = ""))
          )), select=TRUE)



      # Extract Unit IDs to search
      values[[paste(trend_thisid, "moose_dat", sep = "")]]<-values$moose.dat

      unit_IDs<-values[[paste(trend_thisid, "moose_dat", sep = "")]]$UnitID

      l<-length(unit_IDs)


      # Find surveys that at least PARTIALLY include area of interest
      incProgress(0.1, detail = "Finding matching surveys")

      partial <-
        tbl(values$moose, "v_wc_moosepop_reprospreadsheet") %>%
        filter(UnitID %in% unit_IDs) %>%
        collect() %>%
        dplyr::select(SurveyID)

      survey_IDs_partial<- unique(partial$SurveyID)

      # Get all survey data from the partial matches
      incProgress(0.05, detail = "Finding matching surveys")

      partial_match_data <- dbGetQuery(values$moose,
                                       paste("exec spr_wc_moosepop_reprospreadsheet @SurveyIDList = '",
                                             paste(as.character(survey_IDs_partial), sep="' '", collapse=", "),
                                         "'", sep = ""))

      # Identify only the surveys that include the entire AA
      incProgress(0.05, detail = "Filtering to only matched area")

      partial_match_data<-
        partial_match_data %>%
        filter(UnitID %in% unit_IDs) %>%
        group_by(SurveyID, SurveyName, Surveyyear) %>%
        dplyr::summarise(n = n()) %>%
        ungroup() %>%
        dplyr::filter(n == l)

      # Get the survey data for the surveys that include the entire AA
      incProgress(0.1, detail = "Fetching abundance estimates. This may take a while!")

      exact_match_data <- dbGetQuery(values$moose,
                                     paste("exec spr_wc_moosepop_reprospreadsheet @SurveyIDList = '",
                                           paste(as.character(partial_match_data$SurveyID), sep="' '", collapse=", "),
                                       "'", sep = ""))

      exact_match_data$AA<-0
      exact_match_data[is.element(exact_match_data$UnitID, unit_IDs),"AA"]<-1

      # Create a function that will run and can be mapped even if there's an error in a survey
      flexible_AA_tables<-possibly(AA_tables, otherwise = NULL)

      # Run all of the estimates
      out.all <- dlply(exact_match_data, .(SurveyID),.fun=function(x)flexible_AA_tables(x, column_names = "AA"))

      # Extract just the total abundance estimate
      trend_data<-map_df(out.all,  1)

      trend_data$SurveyID<-rep(as.integer(names(out.all[!sapply( out.all, function(x) length(x) == 0 )])), each = 2)
      trend_data<-left_join(trend_data, partial_match_data) %>% filter(Area == "AA")


      # Deal with multiple surveys in the same year
      incProgress(0.7, detail = "Creating table")

      trend_data<-
        trend_data %>%
        group_by(Surveyyear) %>%
        dplyr::mutate(redundant_name = SurveyName,
                      redundant = length(Surveyyear))


      values[[paste(trend_thisid, "trend_data", sep = "")]]<-trend_data %>%
        arrange(Surveyyear)


    output[[paste(trend_thisid, "trend_data_selection_table", sep = "")]]<-DT::renderDataTable(server = FALSE,{
      datatable(values[[paste(trend_thisid, "trend_data", sep = "")]][,c("SurveyName",
                                                                         "Surveyyear",
                                                                         "Total.Est",
                                                                         "High.Est",
                                                                         "Low.Est",
                                                                         "Total.SE",
                                                                         "High.SE",
                                                                         "Low.SE",
                                                                         "Tot.RP@90",
                                                                         "High.RP@90",
                                                                         "Low.RP@90")] %>%
                  arrange(Surveyyear),
                extensions = 'Buttons',
                selection = 'single',
                options = list(dom = "Blfrtip",
                               iDisplayLength = 10,
                               scrollX = TRUE,
                               buttons = list("copy",
                                              list(extend = "collection" ,
                                                   buttons = c("csv", "excel", "pdf"),
                                                   text = "Download",
                                                   exportOptions = list(modifier = list(page = "all"))))),
                rownames = FALSE)
    })

    observeEvent(input[[paste(trend_thisid, "remove_from_trend", sep = "")]],{

      if (!is.null(input[[paste(trend_thisid, "trend_data_selection_table_rows_selected", sep = "")]])) {

        values[[paste(trend_thisid, "trend_data", sep = "")]] <- values[[paste(trend_thisid, "trend_data", sep = "")]][-as.numeric(input[[paste(trend_thisid, "trend_data_selection_table_rows_selected", sep = "")]]),]
      }
    })

    output[[paste(trend_thisid, "matching_abundance_plot", sep = "")]]<- renderPlot({

      data<-values[[paste(trend_thisid, "trend_data", sep = "")]] %>% filter(!is.na(Total.Est))
      labels<-as.character(seq(min(data$Surveyyear), max(data$Surveyyear), by = 1))

      labels[(seq(min(data$Surveyyear), max(data$Surveyyear), by = 1)%%2 == 1)]<-''


      myColors <- c(brewer.pal(length(unique(data$redundant_name)),"Set1"))
      names(myColors) <- c(unique(data$redundant_name))
      colScale <- scale_colour_manual(name = names(myColors),values = myColors)


      if(sum(na.omit(values[[paste(trend_thisid, "trend_data", sep = "")]]$redundant)) == length(na.omit(values[[paste(trend_thisid, "trend_data", sep = "")]]$redundant))){
        colScale <- NULL
        colvar<- NULL
      }else{
        colScale <- scale_colour_manual(name = names(myColors),values = myColors)
        colvar<-"redundant_name"
      }

      ggplot(data, aes_string(x = "Surveyyear",
                       color = colvar))+
        geom_point(aes( y = Total.Est),
                   position = position_dodge(width = .5))+
        geom_errorbar(aes(ymin = Total.Est - Total.SE, ymax = Total.Est + Total.SE),
                      position = position_dodge(width = .5))+
        scale_y_continuous(limits = c(0,max(data$Total.Est) + max(data$Total.SE)),
                           breaks = seq(0, max(data$Total.Est) + max(data$Total.SE), by = 1000))+
        scale_x_continuous(limits=c(min(data$Surveyyear)-1,max(data$Surveyyear)+1),
                           breaks=seq(min(data$Surveyyear), max(data$Surveyyear), by = 1),
                           labels = labels)+
        colScale+
        labs(y = "Moose", x = "", color  = "Survey Name")+
        ggtitle(paste("All matches for" ,values[[paste(trend_thisid, "moose_dat", sep = "")]]$SurveyName[1], values[[paste(trend_thisid, "moose_dat", sep = "")]]$Surveyyear[1], "survey area"))+
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0),
              plot.subtitle  = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0),
              axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
              axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "right",
              legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=5),
              legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=5),
              legend.key.size = unit(1, "cm"),
              panel.background = element_rect(fill = "white", colour = "#666666"),
              panel.grid.major = element_line(size = 0.0005, linetype = 'solid', colour = "grey"))
    })
    })

    })
    ############################
    # Survey planning
    ############################
    observeEvent(input$Plan , {
      appendTab("abundance_tabs",
                tabPanel(
                  value = paste("Plan a survey like", values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"]),
                  title = paste("Plan a survey like", values$authorized_surveys[input$tbl_rows_selected,"surveyname"], " ", values$authorized_surveys[input$tbl_rows_selected,"surveyyear"])
                ), select=TRUE)})

})

# Run the application
shinyApp(ui = ui, server = server)



