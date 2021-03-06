# ./build.r
# This is the master file for publishing the SEO/Web performance dashboard
# Blake Abbenante
# 7/7/20

if (!require(tidyverse)) {
  install.packages('tidyverse') # load the base tidyverse libraries (dplyr, tidyr, etc.)
  require(tidyverse)
}
if (!require(janitor)) {
  install.packages('janitor') # functions for augmenting dataframes
  require(janitor)
}
if (!require(readr)) {
  install.packages('readr') # enhanced functions for loading data
  require(readr)
}
if (!require(scales)) {
  install.packages('scales') # pretty labels
  require(scales)
}
if (!require(lubridate)) {
  install.packages('lubridate') # advanced date manipulation
  require(tidyverse)
}
if (!require(flextable)) {
  install.packages('flextable') # pretty table formatting
  require(flextable)
}
if (!require(here)) {
  install.packages('here') # file referencing
  require(here)
}
if (!require(rsconnect)) {
  install.packages('rsconnect') # publishing shiny dashboards
  require(rsconnect)
}
if (!require(httr)) {
  install.packages('httr') # http posts
  require(httr)
}
if (!require(config)) {
  install.packages('config') # read a config file
  require(config)
}
if (!require(RiHana)) {
  install.packages('RiHana') # Hana stuff
  require(RiHana)
}


## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

hana_dates<-RiHana::get_relevant_date()

config<-config::get(file="~/OneDrive - CBRE, Inc/data/config/r_config.yaml")




#calculate how many days in this time interval
n_days <- interval(hana_dates$yoy_date ,hana_dates$yesterdays_date)/days(1)
reporting_date_range<- enframe(hana_dates$yoy_date + days(0:n_days)) %>% 
  mutate(start_week=floor_date(value,unit="week",week_start=1)) %>% 
  group_by(start_week) %>% 
  summarise(days=n()) %>% 
  rename(report_week=start_week)



#load the data extracts
#load("~/OneDrive - CBRE, Inc/data/raw_data/ELT_raw_data.RData")
#load("~/OneDrive - CBRE, Inc/data/raw_data/transform_data.RData")


###Build the reports ####

#put any updates 
updates_this_week<-""



#check if our cached data is less than a day old, otherwise run the ETL script
file_date<-file.info('inc/web_seo_data.RData')$mtime

if(difftime(now(),file_date,units="hours")>24)
{
  source("load_web_seo_dashboard.R")
}

web_seo_pub<-rsconnect::deployDoc("web_seo_dashboard.rmd",forceUpdate=TRUE,launch.browser=FALSE)

if (web_seo_pub){
  my.res<-POST(url=config$insights_webhook,body=get_slack_payload("Web and SEO Dashboard","https://blake-abbenante-hana.shinyapps.io/web_seo_dashboard/",updates_this_week),encode="json")
}

