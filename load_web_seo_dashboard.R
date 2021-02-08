# ./load_marketing_dash.r
# This file builds additional dataframes necessary for the marketing dashboard
# Blake Abbenante
# 1/27/20

#########Build the reporting dfs############################

#filter for leads/contacts that are specific to marketing

library(tidyverse)
library(lubridate)
library(DBI)

#load the data extracts
load("~/OneDrive - CBRE, Inc/data/raw_data/transform_data.RData")


ft_con <- dbConnect(odbc::odbc(), "fivetran", timeout = 10,bigint = c("numeric"))


ahrefs_referring_domains<- dbGetQuery(ft_con,'select date,
referring_domains 
from
(select *,
  rank() over (partition by date order by _fivetran_synced desc) as pos
  from "Ahrefs"."ReferringDomains" rd 
) rd
where rd.pos=1')

ahrefs_domain_rank<- dbGetQuery(ft_con,'select week,
domain_rank 
from
(select *,
  rank() over (partition by week order by _fivetran_synced desc) as pos
  from "Ahrefs"."DomainRank" dr  
) dr
where dr.pos=1')

ahrefs_backlinks<-dbGetQuery(ft_con,'SELECT "date", backlinks
FROM "Ahrefs".backlinks;')

search_console_keyword_performance<- dbGetQuery(ft_con,"select date,
country,
search_type,
keyword,
device,
impressions ,
clicks ,
position 
from google_search_console.keyword_site_report_by_site ksrbs 
where country in ('usa','gbr')")

ahrefs_domain_rank_edited<-ahrefs_domain_rank %>% 
  drop_na()

ahrefs_referring_domains_edited<-ahrefs_referring_domains %>% 
  drop_na()

ahrefs_backlinks_edited<-ahrefs_backlinks %>% 
  mutate_all(list(~na_if(.,""))) %>% 
  drop_na() %>% 
  mutate(backlinks=as.numeric(str_remove_all(backlinks,",")),
         date=mdy(date))
  
search_console_keyword_performance_edited<-search_console_keyword_performance %>% 
  mutate(branded=case_when(
    grepl('(hana|cbre)',keyword,ignore.case = TRUE)~'Branded',
    grepl('(st martins|park place|park district|245 hammersmith|1818 market|2121.*pearl|windmill green|3 world trade)',keyword,ignore.case = TRUE)~'Location',    
    TRUE~'Non-branded'),
         agg_rank=impressions*position,
    search_week=lubridate::floor_date(date,unit="week",week_start = 1),
    search_month=lubridate::floor_date(date,unit="month"),
    search_quarter=lubridate::floor_date(date,unit="quarter"))

digital_leads<-hs_contacts_edited %>% 
  filter(!is.na(digital_channel) & digital_channel!='N/A') %>% 
  mutate(first_page_edited=str_remove(first_page,"^http(s)?://"),
         landing_content=case_when(
           grepl('^blog',first_page_edited,ignore.case = TRUE)~'Blog',
           grepl('^find|/lp/',first_page_edited,ignore.case=TRUE)~'Landing Page',
           grepl('^www.(facebook|linkedin)',first_page_edited)~'Other',
           grepl('^www',first_page_edited,ignore.case = TRUE)~'WWW',
           grepl('member',first_page_edited,ignore.case = TRUE)~'Member Site',
           TRUE~'Other')) %>% 
  select(contact_id,create_date,create_week,create_month,create_quarter,hana_product,acquisition_source,digital_channel,tracking_code,first_page_edited,landing_content)

web_form_conversions<-segment_form_conversions %>% 
  group_by(dau_date,anonymous_id) %>% 
  count(event) %>% 
  pivot_wider(names_from=event,values_from=n,values_fill=list(n=0)) %>% 
  mutate(lead_form_submitted=ifelse(lead_form_submitted>0,1,0),
         lead_form_viewed=case_when(
           lead_form_viewed>1~1,
           lead_form_submitted==1~1,
           TRUE~0)
  ) %>% 
  ungroup(.)

web_content_all_edited<-segment_pageviews_edited %>% 
  mutate(#channel=if_else(grepl('paid',campaign_medium),'Paid',campaign_medium),
    channel=case_when(
      campaign_medium=='paid'~'Paid',
      campaign_medium=='organic'~'Organic',
      campaign_medium=='direct'~'Direct',
      TRUE~'Other'
    )) %>% 
  ungroup(.)

web_data_all_edited<-web_dau_all %>% 
  mutate(#channel=if_else(grepl('paid',campaign_medium),'Paid',campaign_medium),
         channel=case_when(
           campaign_medium=='paid'~'Paid',
           campaign_medium=='organic'~'Organic',
           campaign_medium=='direct'~'Direct',
           TRUE~'Other'
         ),
         landing_content=case_when(
           grepl('^blog',landing_content,ignore.case = TRUE)~'Blog',
           grepl('^find|/lp/',landing_content,ignore.case=TRUE)~'Landing Page',
           grepl('^www.(facebook|linkedin)',landing_content)~'Other',
           grepl('^www',landing_content,ignore.case = TRUE)~'WWW',
           grepl('member',landing_content,ignore.case = TRUE)~'Member Site',
           TRUE~'Other')) %>% 
  left_join(web_form_conversions) %>% 
  replace_na(list(lead_form_submitted=0,lead_form_viewed=0)) %>% 
  group_by(dau_date,week_start,month_start,quarter_start,channel,hana_city,landing_content) %>% 
  summarise(active_users=n_distinct(anonymous_id),
            lead_forms_viewed=sum(lead_form_viewed),
            lead_forms_submitted=sum(lead_form_submitted)) %>% 
  replace_na(list(hana_city='Not Specified')) %>% 
  ungroup(.)

web_converters<-segment_form_conversions %>% 
  filter(event=='lead_form_submitted') %>% 
  select(anonymous_id) %>% 
  unique(.) %>% 
  mutate(converted_id=anonymous_id)

save(web_data_all_edited,
     web_content_all_edited,
     web_converters,
     digital_leads,
     search_console_keyword_performance_edited,
     ahrefs_domain_rank_edited,
     ahrefs_referring_domains_edited,
     ahrefs_backlinks_edited,
     file="inc/web_seo_data.RData")
