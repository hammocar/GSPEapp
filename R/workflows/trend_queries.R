library(dplyr)


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

# Query to select indivival survey using unique survey ID

survey_id<- 211

moose.dat <- dbGetQuery(moose,
                        paste("exec spr_wc_moosepop_reprospreadsheet @surveyIDlist = '",
                              survey_id,
                              "'",
                              sep = "")
)

# Extract Unit IDs to search
unit_IDs<-moose.dat$UnitID
l<-length(unit_IDs)

# Query to find surveys with common area
query<-paste("SELECT DISTINCT surveyid, surveyname, surveyyear
    FROM v_wc_moosepop_reprospreadsheet
    WHERE UnitID IN (",paste(shQuote(unit_IDs, type="sh"), collapse=", "), ")",sep="")

try <- tbl(moose, "v_wc_moosepop_reprospreadsheet")
try<-
try %>%
  group_by(SurveyID, SurveyName, SurveyYear, UnitID) %>%
  summarise(unit.in.searched.survey = ifelse(UnitID %in% unit_IDs, "yes", "no")) %>%
  ungroup() %>%
  group_by(SurveyID, SurveyName, SurveyYear, unit.in.searched.survey) %>%
  summarise(n = n()) %>%
  filter(unit.in.searched.survey == "yes") %>%
  filter(n == l) %>%
  select(SurveyID) %>% collect()


# Query to get the surveys with common areas
S <- dbGetQuery(moose,
              paste("exec spr_wc_moosepop_reprospreadsheet @SurveyIDList = '",
                    paste(as.character(try$SurveyID), sep="' '", collapse=", "),
                    "'", sep = ""))

S$AA<-0
S[is.element(S$UnitID, unit_IDs),"AA"]<-1

flexible_AA_tables<-possibly(AA_tables, otherwise = NULL)

out.all <- dlply(S, .(SurveyID),.fun=function(x)flexible_AA_tables(x, column_names = "AA"))

trend_data<-map_df(out.all,  1)

trend_data$SurveyID<-rep(as.integer(names(out.all[!sapply( out.all, function(x) length(x) == 0 )])), each = 2)
trend_data<-left_join(trend_data, try)

ggplot(trend_data %>% filter(Area == "AA"), aes(x = SurveyYear))+
  geom_point(aes( y = Total.Est))+
  geom_errorbar(aes(ymin = Total.Est - Total.SE, ymax = Total.Est + Total.SE))+
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



