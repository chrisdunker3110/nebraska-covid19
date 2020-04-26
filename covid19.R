### CREATED BY CHRIS DUNKER, APRIL 5, 2020
### DATA FROM NEBRASKA DHHS AND LOCAL PUBLIC HEALTH DEPARTMENTS

### LOAD IN PACKAGES
library(tidyverse)
library(lubridate)
library(gganimate)
library(plotly)
library(tidyquant)
library(wesanderson)

### LOAD IN COUNTY-LEVEL DATA
coronavirus = read.csv("coronavirus-counties.csv", stringsAsFactors = FALSE)
coronavirus$date = as.Date(coronavirus$date, "%m/%d/%Y")

### CREATE LINEAR SCALE FOR INDIVIDUAL COUNTIES WITH MORE THAN 50 CASES
### USE scale_color_manual TO SET COLORS FOR SPECIFIC COUNTIES
covid_linear = ggplot(data = coronavirus, aes(x = date)) +
  geom_line(aes(y = Adams, color = "Adams"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Adams, color = "Adams")) +
  geom_line(aes(y = Buffalo, color = "Buffalo"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Buffalo, color = "Buffalo")) +
  geom_line(aes(y = Douglas, color = "Douglas"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Douglas, color = "Douglas")) +
  geom_line(aes(y = Dakota, color = "Dakota"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Dakota, color = "Dakota")) +
  geom_line(aes(y = Hall, color = "Hall"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Hall, color = "Hall")) +
  geom_line(aes(y = Lancaster, color = "Lancaster"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Lancaster, color = "Lancaster")) +
  geom_line(aes(y = Sarpy, color = "Sarpy"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Sarpy, color = "Sarpy")) +
  geom_line(aes(y = Dawson, color = "Dawson"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Dawson, color = "Dawson")) +
  geom_line(aes(y = Madison, color = "Madison"), size = 1.2, alpha = .8) +
  geom_point(aes(y = Madison, color = "Madison")) +
  labs(title = "Spread of COVID-19 in Nebraska counties",
       subtitle = "Counties with 50 or more lab-confirmed cases",
       y = "Total cases (lin)",
       caption = "Data from Nebraska Dept. of Health and Human Services and local health departments") +
  theme_minimal() +
  scale_x_date(date_minor_breaks = "1 day") +
  scale_colour_manual(name = "County",
                      values=c(Adams = "blue", Buffalo = "red", Douglas = "gold", Hall = "green", Madison = "palevioletred3",
                               Lancaster = "orange", Sarpy = "purple", Dawson = "cadetblue", Dakota = "yellowgreen")) +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 16, face = 'bold')) 

covid_linear

### LOOK AT CURVE (LOGARITHMIC AND LINEAR) OF TOTAL CASES IN THE STATE
covid_total_log = ggplot(data = coronavirus, aes(x = date)) +
  geom_line(aes(y = Total), size = 1.2, alpha = .8, color = "red") +
  scale_y_log10() +
  labs(title = "COVID-19 in Nebraska",
       subtitle = "Logarithmic scale of total lab-confirmed coronavirus cases since March 6",
       y = "Total cases (log)",
       x = "Date",
       caption = "Data from Nebraska Dept. of Health and Human Services and local health departments") +
  scale_x_date(date_minor_breaks = "1 day") +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 20, face = 'bold')) 

covid_total_log

### TO ANIMATE SCALE FOR PRESENTATION
animate(covid_total_log, fps = 5, end_pause = 16)

### TOTAL LINEAR SCALE
covid_total_linear = ggplot(data = coronavirus, aes(x = date)) +
  geom_line(aes(y = Total), size = 1.2, alpha = .8, color = "red") +
  labs(title = "COVID-19 in Nebraska",
       subtitle = "Linear scale of total lab-confirmed coronavirus cases since March 6",
       y = "Total cases",
       x = "Date",
       caption = "Data from Nebraska Dept. of Health and Human Services and local health departments") +
  scale_x_date(date_minor_breaks = "1 day") +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 20, face = 'bold')) 

covid_total_linear

### USE GGPLOTLY TO CREATE INTERACTIVE GRAPH
ggplotly(covid_total_linear)

### LOAD IN TESTING DATA FROM COVID TRACKING PROJECT AND NEBRASKA DHHS 
### https://covidtracking.com/data/state/nebraska#historical

testing = read.csv("testing.csv", stringsAsFactors = FALSE)
testing$date = as.Date(testing$date, "%m/%d/%Y")

daily_testing = testing %>%
  ggplot(aes(x = date, y = new_tests)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  geom_ma(ma_fun = SMA, n = 7, color = "red", size = 1, linetype = "dotdash") +
  scale_x_date(date_minor_breaks = "1 day") +
  theme_minimal() +
  labs(title = "COVID-19 Testing in Nebraska",
       subtitle = "Daily testing numbers + 7-day rolling average",
       y = "New tests",
       x = "Date",
       caption = "Data from the COVID-19 Tracking Project/Nebraska DHHS") +
  theme(text = element_text(family = 'Helvetica Neue', color = '#444444'),
        plot.title = element_text(size = 16, face = 'bold'))

daily_testing
ggplotly(daily_testing)