
library(shiny)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
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









header <- dashboardHeader(title = "Moose Manager")
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"), collapsed = T)
body <- dashboardBody(uiOutput("body"))
ui <- dashboardPage(header, sidebar, body)


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

login <- box(
    textInput("userName", "Username"),
    passwordInput("passwd", "Password"),
    br(),
    actionButton("Login", "Log in")
)


server<-shinyServer(function(input, output, session) {

    # To logout back to login page
    login.page = paste(
        isolate(session$clientData$url_protocol),
        "//",
        isolate(session$clientData$url_hostname),
        ":",
        isolate(session$clientData$url_port),
        sep = ""
    )

    USER <- reactiveValues(Logged = F)
    observe({
        if (USER$Logged == FALSE) {
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
                            USER$Logged <- TRUE
                    }
                }
            }
        }
    })

    output$sidebarpanel <- renderUI({
        if (USER$Logged == TRUE) {
            div(
                shinydashboard::dashboardSidebar(collapsed = TRUE,
                        sidebarMenu(
                            menuItem("GSPE Moose Predictions", tabName="GSPE")

                        )

                )
            )
        }
    })

    output$body <- renderUI({
        if (USER$Logged == TRUE) {
                dashboardBody(
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    value = "Survey Search",
                    title = "Survey Search",

                    box(    width = 12,
                            title = "Browse for Surveys",
                            status = "primary",
                            solidHeader = TRUE,
                    h3("Select a survey in the table, then use the tabs above to navigate functions"),
                    textInput("keyword", "Search in column surveyname:",12),
                    fluidRow(column(width = 4,
                                    airDatepickerInput("surveyyear_min",
                                                       label = "Select date range",
                                                       value = "1990",
                                                       maxDate = format(Sys.Date(),"%Y"),
                                                       minDate = "1990",
                                                       view = "years", #editing what the popup calendar shows when it opens
                                                       minView = "years", #making it not possible to go down to a "months" view and pick the wrong date
                                                       dateFormat = "yyyy")),
                             column(width = 1, br(),p("to")),
                             column(width = 4, airDatepickerInput("surveyyear_max",
                                                                  label = "",
                                                                  value = format(Sys.Date(),"%Y"),
                                                                  maxDate = format(Sys.Date(),"%Y"),
                                                                  minDate = "1990",
                                                                  view = "years", #editing what the popup calendar shows when it opens
                                                                  minView = "years", #making it not possible to go down to a "months" view and pick the wrong date
                                                                  dateFormat = "yyyy"))),
                    DT::dataTableOutput("tbl"))),
                tabPanel(
                    value = "View/download survey data",
                title = "View/download survey data",
                  box(width = 12,
                      title = "Survey Data",
                      status = "primary",
                      solidHeader = TRUE,
                    DT::dataTableOutput("survey_data"))),
                tabPanel(
                    value = "Single survey GSPE details",
                    title = "Single survey GSPE details",
                  box(width = 12,
                      title = "GeoSpatial Population Estimate",
                      status = "primary",
                      solidHeader = TRUE,
                    uiOutput("columns"),
                    box(width = 12,
                        checkboxInput("sightability", "Use sightability correction factor", value = FALSE),
                        column(width=3,
                               numericInput("high_trials", "Number of High Stratum Sightability Trials", value = 0, min = 1, step = 1)
                               ),
                        column(width=3,
                               numericInput("high_missed", "Number of High Stratum Moose Missed", value = 0, min = 1, step = 1)
                               ),
                        column(width=3,
                               numericInput("low_trials", "Number of Low Stratum Sightability Trials", value = 0, min = 1, step = 1)
                               ),
                        column(width=3,
                               numericInput("low_missed", "Number of Low Stratum Moose Missed", value = 0, min = 1, step = 1)
                               )
                        ),
                    box(width = 12,
                        h4(strong("Total Abundance")),
                        DT::dataTableOutput("GSPE_table_abundance_SCF")%>% withSpinner(color="#0dc5c1") ,
                        h4(strong("Bull:Cow Composition")),
                        DT::dataTableOutput("GSPE_table_bullcow")%>% withSpinner(color="#0dc5c1"),
                        h4(strong("Calf:Cow Composition")),
                        DT::dataTableOutput("GSPE_table_calfcow")%>% withSpinner(color="#0dc5c1")),
                    box(width = 12,
                        h4(strong("Abundance Heatmap")),
                        selectInput("metric", "Which demographic?", choices = c("totalmoose", "TotalBulls", "TotalCows", "TotalCalves"), selected = "totalmoose", multiple = FALSE),
                        plotOutput("GSPE_plot", height = "1000px") %>% withSpinner(color="#0dc5c1"))
                  )),
                tabPanel(
                    value = "Trend analysis",
                    title = "Trend analysis"),
                tabPanel(
                    value = "Plan a survey like this one",
                    title = "Plan a survey like this one"
                )
                )
                )



        } else {
            login
        }
    })




    moose<-reactive({
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)

        moose <- DBI::dbConnect(
          odbc::odbc(),
          Driver      = "SQL Server",
          Server = "DFGJNUSQL-DB72P",
          Database= "WC_moosepop",
          # Uid         = Username,
          # Pwd         = Password,
          Trusted_Connection = "True",
          Port        = 1433,
          # TDS_Version = 8.0
        )
        moose
    })

    searched_surveys <- reactive({
        query<-paste("SELECT DISTINCT surveyid, surveyname, surveyyear
    FROM v_wc_moosepop_reprospreadsheet
    WHERE surveyname like '", input$keyword,"%' AND surveyyear BETWEEN ",year(input$surveyyear_min), " AND ", year(input$surveyyear_max),sep="")

        all.id.list <- dbGetQuery(moose(),query)
        all.id.list

    })

    output$tbl <- DT::renderDataTable({
        datatable(searched_surveys(), extensions = 'Buttons',
                  selection = 'single'
                  , options = list(
                      dom = "Blfrtip"
                      , iDisplayLength = 10,
                      buttons =
                          list("copy", list(
                              extend = "collection" ,
                              buttons = c("csv", "excel", "pdf"),
                              text = "Download",
                              exportOptions = list(
                                  modifier = list(page = "all")
                              )
                          ) )), rownames = FALSE)


    })

    moose.dat<-reactive({
        req(input$tbl_rows_selected)
        survey_ids <- searched_surveys()[input$tbl_rows_selected,"surveyid"]

        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        moose <- DBI::dbConnect(
          odbc::odbc(),
          Driver      = "SQL Server",
          Server = "DFGJNUSQL-DB72P",
          Database= "WC_moosepop",
          # Uid         = ,
          # Pwd         = ,
          Trusted_Connection = "True",
          Port        = 1433,
          # TDS_Version = 8.0
        )

        query<-paste("exec spr_wc_moosepop_reprospreadsheet @surveyIDlist = '", survey_ids,"'", sep = "")
        moose.dat <- dbGetQuery(moose(), query)

        moose.dat})

        # out <- by(data=moose.dat,as.factor(moose.dat$Surveyyear),FUN=function(x){
        #     results(x)
        # })



        output$columns<-renderUI({
pickerInput(inputId = "columns", label = "Identify analysis area columns", choices = names(moose.dat()), multiple = TRUE)})

        output$survey_data<- DT::renderDataTable(server = FALSE, {
            validate(need(!is.null(input$tbl_rows_selected), "\nSelect a survey from the Survey Search tab."))
            moose.dat()},
            options = list(
          dom = "Blfrtip",
          scrollX = T,
          iDisplayLength = 10,
          buttons =
            list("copy", list(
              extend = "collection" ,
              buttons = c("csv", "excel", "pdf"),
              text = "Download",
              exportOptions = list(
                modifier = list(page = "all")
              )
            ) )), rownames = FALSE)




    td<-reactive({
        req(input$tbl_rows_selected)

        AA_tables(moose.dat(), input$columns)})

    metric<-reactive(input$metric)


    output$GSPE_plot <- renderPlot({
        validate(need(!is.null(input$tbl_rows_selected), "\nSelect a survey from the Survey Search tab."))

        plot_data<-moose.dat()
        table_data<-td()


        prediction_data<-case_when(input$metric == "totalmoose" ~ "total_results",
                                   input$metric == "TotalBulls" ~ "bull_results",
                                   input$metric == "TotalCows" ~ "cow_results",
                                   input$metric == "TotalCalves" ~ "calf_results")


        # If we want a different map (This takes some time, depending on the zoom)
        # Google map API key workaround
        height <- max(plot_data$centrlat) - min(plot_data$centrlat)
        width <- max(plot_data$centrlon) - min(plot_data$centrlon)
        borders <- c(bottom  = min(plot_data$centrlat)  - 0.1 * height,
                     top     = max(plot_data$centrlat)  + 0.1 * height,
                     left    = min(plot_data$centrlon) - 0.1 * width,
                     right   = max(plot_data$centrlon) + 0.1 * width)

        A <- get_stamenmap(borders, zoom = 10, maptype = "terrain")
        metric<-input$metric
        metric_label<-case_when(metric == "totalmoose" ~ "Total",
                                metric == "TotalBulls" ~ "Bull",
                                metric == "TotalCows" ~ "Cow",
                                metric == "TotalCalves" ~ "Calf")


        surv_results_predictions<-table_data[[prediction_data]][[1]]$predictions
        # surv_results_predictions<-AA_abundance(plot_data, input$columns, metric = input$metric)$results_list[[1]]$predictions

        plot_data<- cbind(plot_data, surv_results_predictions[,4:5])





        pred_plot<-ggmap(A)+geom_tile(data = plot_data, aes(x=centrlon,y=centrlat,fill= log(Est+1)),alpha= 1,colour="black")+
            geom_text(data=subset(plot_data, Counted== TRUE),aes_string(x="centrlon",y="centrlat",label= input$metric), color = "white", size = 5)+
            ggtitle(paste(plot_data$SurveyName),subtitle = paste(plot_data$Surveyyear, plot_data$Season, "Predicted", metric_label,"Moose Abundance"))+
            labs(x = "", y = "", fill = "Moose")+
            scale_fill_gradient(breaks = log(c(0, 5, 15, 30, 60, 120, 200)+1),
                                labels = c(0, 5, 15, 30, 60, 120, 200))+
            theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0),
                  plot.subtitle  = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0),
                  legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
                  axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
                  legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
                  axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
                  legend.position = "right",
                  panel.background = element_rect(fill = "white", colour = "#666666"),
                  panel.grid.major = element_line(size = 0.0005, linetype = 'solid',
                                                  colour = "grey"),
                  legend.key.size = unit(2.5, "cm"))
        pred_plot

    }, height = 1000, units = "px")



    output$GSPE_table_abundance_SCF<-DT::renderDataTable({
        validate(need(!is.null(input$tbl_rows_selected), "\nSelect a survey from the Survey Search tab."))


        table_data<-td()

        abundance_data<-
        as.data.frame(rbind(table_data[["total_abundance"]],
                            table_data[["cow_abundance"]],
                            table_data[["bull_abundance"]],
                            table_data[["calf_abundance"]])) %>%
            mutate(Metric = rep(c("Total", "Cows", "Bulls", "Calves"), each = length(input$columns)+1)) %>%
            relocate(Metric, .after = Area)


        scf<-map(1:(length(input$columns)+1), ~
                     scf.apply1(hi.est = as.numeric(as.data.frame(table_data[["total_abundance"]])$High.Est[.x]),
                                lo.est = as.numeric(as.data.frame(table_data[["total_abundance"]])$Low.Est[.x]),
                                hi.se = as.numeric(as.data.frame(table_data[["total_abundance"]])$High.SE[.x]),
                                lo.se = as.numeric(as.data.frame(table_data[["total_abundance"]])$Low.SE[.x]),
                                n.trials.hi = input$high_trials,
                                n.trials.lo = input$low_trials,
                                n.seen.hi = input$high_trials-input$high_missed,
                                n.seen.lo = input$low_trials - input$low_missed))

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

        if(input$sightability == FALSE)
            return(abundance_data)
        SCF
    }, options = list(dom = 't'),rownames = FALSE)


    output$GSPE_table_bullcow<-DT::renderDataTable({

        table_data<-td()

        table_data$bullcow}, options = list(dom = 't'),rownames = FALSE)

    output$GSPE_table_calfcow<-DT::renderDataTable({
        table_data<-td()

        table_data$calfcow}, options = list(dom = 't'),rownames = FALSE)

})

# Run the application
shinyApp(ui = ui, server = server)



