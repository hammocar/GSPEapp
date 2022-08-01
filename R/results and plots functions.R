areamap_gg <- function(data) {
  min_lat <- min(data[,"centrlat"])
  max_lat <- max(data[,"centrlat"])
  min_lon <- min(data[, "centrlon"])
  max_lon <- max(data[,"centrlon"])

  height <-max_lat - min_lat
  width <- max_lon - min_lon
  borders <- c(bottom  = min_lat  - 0.1 * height,
               top     = max_lat  + 0.1 * height,
               left    = min_lon - 0.1 * width,
               right   = max_lon + 0.1 * width)

  get_stamenmap(borders, zoom = 10, maptype = "terrain")
}


abundance_heatmap <- function(survey_data, metric) {

  A<-areamap_gg(survey_data)

  pred_data<-results(survey_data, metric)$GSPE_calc.out$predictions


  ggmap(A)+
  geom_tile(pred_data, mapping = aes(x=centrlon,y=centrlat,fill= log(Est+1)),alpha= 1,colour="black")+
  geom_text(data=subset(pred_data, Counted== TRUE),aes_string(x="centrlon",y="centrlat",label= "Est"), color = "white", size = 2)+
    ggtitle(paste(pred_data[,"SurveyName"]),subtitle = paste(pred_data[,"Surveyyear"],  "Predicted", metric,"Abundance"))+
    labs(x = "", y = "", fill = "Moose")+
    scale_fill_gradient(breaks = log(c(0, 5, 15, 30, 60, 120, 200)+1),
                        labels = c(0, 5, 15, 30, 60, 120, 200))+
    theme_minimal()
}




