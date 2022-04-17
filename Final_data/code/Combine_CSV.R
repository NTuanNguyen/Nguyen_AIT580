
#Set file path that contain all csv
filepath = 'D:/Work/GMU -US/Spring 2022/AIT 580/Final/data'


library(dplyr)
library(readr)

#filter by state name
eastcoast = c('MAINE','NEW HAMPSHIRE','MASSACHUSETTS','RHODE ISLAND',
              'CONNECTICUT','NEW YORK','NEW JERSEY','DELAWARE',
              'MARYLAND', 'VIRGINIA','NORTH CAROLINA','SOUTH CAROLINA',
              'GEORGIA','FLORIDA'
)

#loop bind_row for all csv in folder

df <- list.files(path=filepath, full.names = TRUE) %>% 
  lapply(read_csv) %>% #read each csv
  lapply(\(x) subset(x,x$STATE %in% eastcoast)) %>% #apply state filter for each csv
  lapply(\(x) x = x[, !(colnames(x) %in% c('BEGIN_DAY','BEGIN_TIME','END_YEARMONTH','END_DAY',
                                                       'END_TIME',"CZ_FIPS","CZ_NAME",'WFO','BEGIN_DATE_TIME',
                                                       'CZ_TIMEZONE','END_DATE_TIME','FLOOD_CAUSE','CATEGORY',
                                                       'TOR_LENGTH','TOR_WIDTH','TOR_OTHER_WFO','TOR_OTHER_CZ_STATE',
                                                       "TOR_OTHER_CZ_FIPS","TOR_OTHER_CZ_NAME",'BEGIN_RANGE',
                                                       'BEGIN_AZIMUTH','BEGIN_LOCATION','END_RANGE','END_AZIMUTH',
                                                       'END_LOCATION','BEGIN_LAT','BEGIN_LON',"END_LAT","END_LON",
                                                       "EPISODE_NARRATIVE",'EVENT_NARRATIVE','DATA_SOURCE'))]) %>%
                                                      #apply column filter
  lapply(\(x) mutate(x, across(DAMAGE_CROPS, as.character))) %>% #convert column into character
  bind_rows 

#write output csv
write.csv(df,paste(filepath,'/full.csv',sep = ''), row.names = FALSE)
