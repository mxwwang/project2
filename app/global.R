library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(corrplot)
library(lubridate)
library(ggpubr)
library(patchwork)


############
### Data ###
############
mobility_nyc <- read.csv(file = "./data/mobility_nyc.csv")
covid_nyc <- read.csv(file = "./data/covid_nyc.csv")

mobility_nyc = mobility_nyc %>% 
  mutate(date = as.Date(date))

covid_nyc = covid_nyc %>% 
  mutate(date = as.Date(date))

nyc <- left_join(mobility_nyc,covid_nyc,by=c("date","borough"))



####################
### ui.R Choices ###
### (function para)#
####################
categories_colname = colnames(mobility_nyc)[10:15] #colnames(mobility_nyc)[c(10:13,17:20)]

categories = c("Retail and Recreation",
               "Grocery and Pharmacy",
               "Parks",
               "Transit Stations",
               #"Workplaces",    
               "Workplaces (Weekdays)",    
               "Workplaces (Weekends)",    
               #"Residential"
               "Residential (Weekdays)",
               "Residential (Weekends)"
               )

categories1 = c("Retail and Recreation",
               "Grocery and Pharmacy",
               "Parks",
               "Transit Stations",
               "Workplaces",    
               #"Workplaces (Weekdays)",    
               #"Workplaces (Weekends)",    
               "Residential"
               #"Residential (Weekdays)",
               #"Residential (Weekends)"
)


# covid_stats = c("case_count",
#                 "hosp_count",
#                 "death_count",
#                 "vaccine"
#                )

covid_stats = c("Cases",
                "Hospitalizations",
                "Deaths",
                "Vaccinations")


label1 = c()
for (a in categories1[1:length(categories1)]) {
  label1 = append(label1,
                  gsub("And", "&", paste(paste(str_to_title(strsplit(a, "_")[[1]]),collapse = " "),"Change (%)",sep=" ")))
  
}

################
### Analysis ###
################
#source('../func.R')
source('func.R')

### function 1 - mobility across boroughs of 1 category


covid_comp_plot <- function(boro1,boro2,cstat){
  
  cstat_title = paste(str_to_title(strsplit(cstat, "_")[[1]]),collapse = " ")
  
  cstat = cstat_to_colnames(cstat)
  if(cstat == "vx_fully_perc"){
    cstat_title_y = "Fully Vaccined Percentage (%)"
  } else {
    cstat_title_y = "Count"
  }

  
  covid_stat = sym(cstat)
  
  nyc %>%
    select(date,borough,(!!covid_stat)) %>%
    filter(borough %in% c(boro1,boro2)) %>%
    ggplot(na.rm=T) +
    geom_line(aes(x = date, y = (!!covid_stat), color = borough)) +
    labs(title = cstat_title, x="Date", y=cstat_title_y,color="Borough")+
    scale_x_date(date_breaks = "2 months")
}


  
boro_comp_plot <- function(boro1,boro2,cat){
  

  cat_title = paste(strsplit(cat, "_")[[1]],collapse = " ")
  
  cat = gsub(" ","_",tolower(cat))
  
  cat = sym(cat)
  
  nyc %>%
    select(date,borough,(!!cat)) %>%
    filter(borough %in% c(boro1,boro2)) %>%
    ggplot(na.rm=T) +
    geom_line(aes(x = date, y = (!!cat), color = borough)) +
    labs(title = cat_title, x="Date", y="Mobility Change from Baseline (%)",color="Borough")+
    #scale_y_continuous(labels = percent)+
    scale_y_continuous(labels = function(x) paste0(x, "%"))+
    scale_x_date(date_breaks = "2 months")
  
}

#boro_comp_plot("Manhattan","Queens","parks")

#covid_stat = sym(covid_stat)