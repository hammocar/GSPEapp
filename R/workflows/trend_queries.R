library(plyr)
library(ggplot2)
library(ggmap)
library(purrr)
library(tidyr)
library(kableExtra)
library(sp)
library(readr)
library(DT)
library(tools)
library(DBI)
library(odbc)
library(lubridate)
library(dplyr)
library(dbplyr)
library(RColorBrewer)

source("inst/app/moosepop.R")

moose <- DBI::dbConnect(
  odbc::odbc(),
  Driver      = "SQL Server",
  Server = "DFGJNUSQL-DB72P",
  Database= "WC_moosepop",
  # Uid         = Username,
  # Pwd         = Password,
  Trusted_Connection = "True",
  Port        = 1433
  # TDS_Version = 8.0
)


keyword<-"20E TAYLOR CORRIDOR"
year_min<- 2000
year_max<-2020



# Query to search for survey ID's that match keyword and year inputs

query<-paste("SELECT DISTINCT surveyid, surveyname, surveyyear
    FROM v_wc_moosepop_reprospreadsheet
    WHERE surveyname like '", keyword,"%' AND surveyyear BETWEEN ",year_min, " AND ", year_max,sep="")

all.id.list <- dbGetQuery(moose,query)
all.id.list

# Query to select individual survey using unique survey ID

survey_id<- 361

moose.dat <- dbGetQuery(moose,
                        paste("exec spr_wc_moosepop_reprospreadsheet @surveyIDlist = '",
                              survey_id,
                              "'",
                              sep = "")
)

# Extract Unit IDs to search
unit_IDs<-moose.dat$UnitID
l<-length(unit_IDs)


# Find surveys that at least PARTIALLY include area of interest
partial <-
  tbl(moose, "v_wc_moosepop_reprospreadsheet") %>%
  try %>%
  filter(UnitID %in% unit_IDs) %>%
  collect() %>%
  dplyr::select(SurveyID)



survey_IDs_partial<- unique(partial$SurveyID)


  # Get all survey data from the partial matches
partial_match_data <- dbGetQuery(moose,
              paste("exec spr_wc_moosepop_reprospreadsheet @SurveyIDList = '",
                    paste(as.character(survey_IDs_partial), sep="' '", collapse=", "),
                    "'", sep = ""))

# Identify only the surveys that include the entire AA
partial_match_data<-
  partial_match_data %>%
  filter(UnitID %in% unit_IDs) %>%
  group_by(SurveyID, SurveyName, Surveyyear) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  dplyr::filter(n == l)

# Get the survey data for the surveys that include the entire AA
exact_match_data <- dbGetQuery(moose,
                paste("exec spr_wc_moosepop_reprospreadsheet @SurveyIDList = '",
                      paste(as.character(partial_match_data$SurveyID), sep="' '", collapse=", "),
                      "'", sep = ""))


exact_match_data$AA<-0
exact_match_data[is.element(exact_match_data$UnitID, unit_IDs),"AA"]<-1

results(moose.dat)
# Create a function that will run and can be mapped even if there's an error in a survey
flexible_AA_tables<-possibly(AA_tables, otherwise = NULL)

# Run all of the estimates
out.all <- dlply(exact_match_data, .(SurveyID),.fun=function(x)flexible_AA_tables(x, column_names = "AA"))

# Extract just the total abundance estimate
trend_data<-map_df(out.all,  1)


trend_data$SurveyID<-rep(as.integer(names(out.all[!sapply( out.all, function(x) length(x) == 0 )])), each = 2)
trend_data<-left_join(trend_data, partial_match_data) %>% filter(Area == "AA")

# Deal with multiple surveys in the same year
trend_data<-
trend_data %>%
  group_by(Surveyyear) %>%
  dplyr::mutate(redundant_name = SurveyName,
                redundant = length(Surveyyear),
                equal_estimates =length(unique(Total.Est)) == 1 & length(unique(Total.SE)) == 1)

trend_data[trend_data$redundant == 1 |trend_data$equal_estimates == TRUE , "redundant_name"]<-""


labels<-as.character(seq(min(trend_data$Surveyyear), max(trend_data$Surveyyear), by = 1))

labels[(seq(min(trend_data$Surveyyear), max(trend_data$Surveyyear), by = 1)%%2 == 1)]<-''


myColors <- c("black", brewer.pal(length(unique(trend_data$redundant_name)[-1]),"Set1"))
names(myColors) <- c("", (unique(trend_data$redundant_name)[which(unique(trend_data$redundant_name) != "")]))
colScale <- scale_colour_manual(name = names(myColors),values = myColors, breaks = )

ggplot(trend_data, aes(x = Surveyyear,
                       color = redundant_name))+
  geom_point(aes( y = Total.Est),
             position = position_dodge(width = .5))+
  geom_errorbar(aes(ymin = Total.Est - Total.SE, ymax = Total.Est + Total.SE),
                position = position_dodge(width = .5))+
  scale_y_continuous(limits = c(0,max(trend_data$Total.Est) + max(trend_data$Total.SE)),
                     breaks = seq(0, max(trend_data$Total.Est) + max(trend_data$Total.SE), by = 1000))+
  scale_x_continuous(limits=c(min(trend_data$Surveyyear)-1,max(trend_data$Surveyyear)+1),
                     breaks=seq(min(trend_data$Surveyyear), max(trend_data$Surveyyear), by = 1),
                     labels = labels)+
  colScale+
  labs(y = "Moose", x = "", color  = "")+
  ggtitle(paste("All matches for" ,moose.dat$SurveyName[1], moose.dat$Surveyyear[1], "survey area"))+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22, hjust=0),
        plot.subtitle  = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0),
        legend.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        legend.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=15),
        axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        panel.background = element_rect(fill = "white", colour = "#666666"),
        panel.grid.major = element_line(size = 0.0005, linetype = 'solid', colour = "grey"),
        legend.key.size = unit(2.5, "cm"))



