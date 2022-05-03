df = read.csv('D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580/Final_data/data/full.csv')

#check ratio of missing value
colSums(is.na(df))/nrow(df)

library("reshape2")
library(tidyverse)
library(data.table)
library(plotly)

df$MONTH_NAME = factor(df$MONTH_NAME, ordered = TRUE,
                          levels = c('January','February','March','April',
                                     'May','June','July','August','September',
                                     'October','November','December'))

df_sum_total = df %>% group_by(YEAR,STATE) %>% count(EVENT_TYPE) %>%
  group_by(YEAR,STATE) %>% summarise(Events = sum(n))

#### LM model ####

lm1 = lm(Events~., data = df_sum_total)
summary(lm1)

ggplot(data = df_sum_total, aes(x = YEAR, y = Events))+
  geom_bar(stat = 'identity', color = 'black',fill = 'cyan') +
  geom_smooth(color = 'red', alpha = 0.5, size = 0.75)+
  labs(x = 'Year',
       y = 'Number of Events',
       title = "Number of Storm Events Based on NOAA's Record (1980 - 2021) \nFor Each State")+
  theme_light()+
  facet_wrap(~STATE)

#### K-mean clustering ####

df_k2 = df %>% group_by(YEAR,STATE) %>% count(EVENT_TYPE) %>%
  group_by(YEAR) %>% summarise(Events = sum(n))

k2 = kmeans(df_k2,2)
df_k2$cluster = as.character(k2$cluster)

colMeans(df_k2[df_k2$cluster == '2', 'Events'])

t.test(df_k2[df_k2$cluster == '1', 'Events'],df_k2[df_k2$cluster == '2', 'Events'],
       paired = FALSE
       )

km_plot = ggplot()+
  geom_point(data = df_k2, 
             mapping = aes(x = YEAR, 
                           y = Events, 
                           colour = cluster),
             size = 3) +
  geom_point(mapping = aes_string(x = k2$centers[, "YEAR"], 
                                  y = k2$centers[, "Events"]),
             color = "#D61D1D", size = 5)+ 
  labs(x = 'Year',
       y = 'Number of Events',
       title = 'Scatter plot of Year versus Total number of events per year \nwith their cluster tendency')+
  theme_light()

#### Source analysis #### Not included

#df_episode = df %>% group_by(YEAR,EPISODE_ID) %>% count(EVENT_TYPE) %>% na.omit(df_episode)

df_source = df %>% group_by(YEAR,EVENT_TYPE) %>% count(SOURCE)

df_source$SOURCE[is.na(df_source$SOURCE)] = 'UNKNOWN'

#Remove some duplicates
df_source$SOURCE[df_source$SOURCE == 'UNKNOWN'] = 'Unknown'
df_source$SOURCE[df_source$SOURCE == 'AIRPLANE PILOT'] = 'Airplane Pilot'
df_source$SOURCE[df_source$SOURCE == 'NEWSPAPER'] = 'Newspaper'
df_source$SOURCE[df_source$SOURCE == 'BROADCAST MEDIA'] = 'Broadcast Media'
df_source$SOURCE[df_source$SOURCE == 'AMATEUR RADIO'] = 'Amateur Radio'
df_source$SOURCE[df_source$SOURCE == 'EMERGENCY MANAGER'] = 'Emergency Manager'
df_source$SOURCE[df_source$SOURCE == 'FIRE DEPT/RESCUE SQUAD'] = 'Fire Department/Rescue'
df_source$SOURCE[df_source$SOURCE == 'POST OFFICE'] = 'Post Office'
df_source$SOURCE[df_source$SOURCE == 'STORM CHASER'] = 'Storm Chaser'
df_source$SOURCE[df_source$SOURCE == 'TRAINED SPOTTER'] = 'Trained Spotter'
df_source$SOURCE[df_source$SOURCE == 'MARINER'] = 'Mariner'
df_source$SOURCE[df_source$SOURCE == 'LAW ENFORCEMENT'] = 'Law Enforcement'

# Assign affilication to each source
df_source = df_source %>% 
  mutate(Affiliation = case_when(
    SOURCE %in% c('AWOS,ASOS,MESONET,ETC','Emergency Manager',
                  'OFFICIAL NWS OBS.','GOVT OFFICIAL','DEPT OF HIGHWAYS',
                  'Fire Department/Rescue','Law Enforcemen','NWS STORM SURVEY',
                  'OTHER FEDERAL AGENCY','PARK/FOREST SERVICE','ARPT EQUIP(AWOS,ASOS)',
                  'Post Office','UTILITY COMPANY','COASTAL OBSERVING STATIO','BUOY',
                  'C-MAN Station','County Official','Other Federal Agency','ASOS',
                  'AWOS','NWS Employee','Mesonet','Department of Highways','COOP Observer',
                  'Official NWS Observations','COOP STATION','NWS Storm Survey','Park/Forest Service',
                  'Coast Guard','Buoy','River/Stream Gage','SHAVE Project','State Official',
                  'AWSS','RAWS','WLON','Drought Monitor','NPOP','Lifeguard','Local Official',
                  'Tribal Official'
    ) ~ "Officials",
    SOURCE == 'Unknown' ~ "Unknown",
    TRUE ~ "Public"))

table(df_source$Affiliation)

df_sauce = df_source %>% group_by(YEAR,Affiliation) %>%  summarise(Counts = sum(n))

ggplot(data = df_sauce, aes(x = YEAR, y = Counts, fill = Affiliation))+
  geom_bar(stat = 'identity', color  = 'black')

#### Event frequency per season #####

df_month = df %>% group_by(YEAR,MONTH_NAME) %>% count(EVENT_TYPE) %>%
  group_by(YEAR,MONTH_NAME) %>% summarise(Events = sum(n))

df_month_only = df %>% group_by(YEAR,MONTH_NAME) %>% count(EVENT_TYPE) %>%
  group_by(MONTH_NAME) %>% summarise(Events = sum(n))

df_month = df_month %>%
  ungroup() %>% add_row(YEAR = 1981, MONTH_NAME = 'January',
                              Events = 0, .before = 13)
df_month$MONTH_NAME = factor(df_month$MONTH_NAME, ordered = TRUE,
                       levels = c('January','February','March','April',
                                  'May','June','July','August','September',
                                  'October','November','December'))
colnames(df_month)[2] = 'Month'

fig_month = plot_ly(data = df_month,x =~Month,
                                y =~Events,
                                frame =~YEAR,
                                text = ~Events,
                                type = 'bar'
                                ) %>% layout(title = 'Number of Storm Events of each month\n(1980-2021) ',
                                             showlegend = FALSE)

library(htmlwidgets)
htmlwidgets::saveWidget(fig_month, 
                        file = "D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580/Final_data/event_per_month.html")


