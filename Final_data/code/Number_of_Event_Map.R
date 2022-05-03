df = read.csv('D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580/Final_data/data/full.csv')

#check ratio of missing value
colSums(is.na(df))/nrow(df)

unique(df$EVENT_TYPE)

library("reshape2")
library(tidyverse)
library(data.table)

df_map = df %>% group_by(YEAR,STATE) %>% count(EVENT_TYPE) %>%
  group_by(YEAR,STATE) %>% summarise(Events = sum(n))

df_map = df_map %>%
  mutate(ISO2 = case_when(
    endsWith(STATE, "CONNECTICUT") ~ "CT",
    endsWith(STATE, "DELAWARE") ~ "DE",
    endsWith(STATE, "FLORIDA") ~ "FL",
    endsWith(STATE, "GEORGIA") ~ "GA",
    endsWith(STATE, "MAINE") ~ "ME",
    endsWith(STATE, "MARYLAND") ~ "MD",
    endsWith(STATE, "MASSACHUSETTS") ~ "MA",
    endsWith(STATE, "NEW HAMPSHIRE") ~ "NH",
    endsWith(STATE, "NEW JERSEY") ~ "NJ",
    endsWith(STATE, "NEW YORK") ~ "NY",
    endsWith(STATE, "NORTH CAROLINA") ~ "NC",
    endsWith(STATE, "RHODE ISLAND") ~ "RI",
    endsWith(STATE, "SOUTH CAROLINA") ~ "SC",
    endsWith(STATE, "VIRGINIA") ~ "VA"
  ))

library(plotly)
library(htmlwidgets)

#export as html file

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)

p = df_map %>% plot_ly() %>%
  add_trace(type = "choropleth", locationmode = 'USA-states',
            locations = df_map$ISO2,
            z = ~Events, text = df_map$STATE, frame=~YEAR,
            color = ~Events, zmin = 0, zmax = 6200, colorscale = 'Blues', reversescale = T)%>%
  add_trace(type = "scattergeo", locationmode = 'USA-states',
            locations = df_map$ISO2, frame=~YEAR,
            text = paste0(df_map$ISO2, "\n", df_map$Events),
            mode = "text",
            textfont = list(color = rgb(0,0,0), size = 12)) %>%
  layout(geo = g,
         title = 'Storm Related Weather Event Occurence Map of the US East Coast \n For Each State'
  ) 

#export map
htmlwidgets::saveWidget(p, file = "D:/Work/GMU -US/Spring 2022/AIT 580/Nguyen_AIT580/Final_data/map.html")
