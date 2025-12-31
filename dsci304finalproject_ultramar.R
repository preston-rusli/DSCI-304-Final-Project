# SETUP ####
#Clear Workspace 
cat("\014")
rm(list=ls())
set.seed(18552)

# Libraries
library(tidyverse)
library(dplyr)
library(zoo)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(sp)
library(cowplot)
library(ggpubr)
library(maps)
library(sf)
library(stringr)
library(gganimate)
library(ggiraph)
library(patchwork)
library(forcats)
library(leaflet)
library(ggiraph)
library(gganimate)
library(gifski)
library(htmlwidgets)
library(showtext)
library(extrafont)
library(ggrepel)
library(countrycode)
library(tidytext)
library(plotly)
library(ragg)
library(tidyr)
library(countrycode)

setwd("~/Documents/R/DSCI 304/final project")

#https://www.kaggle.com/datasets/aiaiaidavid/the-big-dataset-of-ultra-marathon-running?select=TWO_CENTURIES_OF_UM_RACES.csv
um_race<-read.csv("archive/TWO_CENTURIES_OF_UM_RACES.csv")
View(um_race)
names(um_race)

# DATA CLEANING ######

unique(um_race$Event.distance.length)
um_race %>%
  count(Event.distance.length, sort = TRUE)


#Filter data for 50km and 100km race events
um_race_subset <- um_race %>%
  filter(Event.distance.length %in% c("50km", "100km"))
#Filter data for 1990-2022
um_race_subset <- um_race_subset %>%
  filter(Year.of.event >= 1990 & Year.of.event <=2022)

unique(um_race_subset$Event.distance.length)
summary(um_race_subset$Year.of.event)
View(um_race_subset)

##Filter data for US 
um_race_US_subset <- um_race_subset %>%
  filter(Athlete.country %in% c("USA"))
View(um_race_US_subset)

#Create new data for Athlete's age at event
um_race_US_subset <- um_race_US_subset %>%
  mutate(age_at_event = Year.of.event - Athlete.year.of.birth)


#Remove any data where Year.of.event is less than Athlete.year.of.birth
um_race_US_subset <- um_race_US_subset %>%
  filter(Athlete.year.of.birth <= Year.of.event)
#Remove cases where data seems incorrect age_at_event < 10 and > 90 yrs
um_race_US_subset <- um_race_US_subset %>%
  filter(Year.of.event-Athlete.year.of.birth >=18)
um_race_US_subset <- um_race_US_subset %>%
  filter(Year.of.event-Athlete.year.of.birth <=90)

##Clean gender
um_race_US_subset <- um_race_US_subset %>%
  filter(Athlete.gender != "X")

summary(um_race_US_subset$Year.of.event)

#Create new data for Athlete's age at event and Athlete's age group
um_race_US_subset <- um_race_US_subset %>%
  mutate(age_at_event = Year.of.event - Athlete.year.of.birth,
         age_group = cut(age_at_event,
                         breaks = c(seq(18, 58, by = 8), Inf),
                         labels = c("18-25", "26-33", "34-41", "42-49", "50-57", "58+"),                        
                         right = FALSE))

View(um_race_US_subset)

um_race_US_subset <- um_race_US_subset %>%
  mutate(
    Athlete.time.hours = sapply(Athlete.performance, function(x) {
      x <- gsub(" h", "", x)
      # Split into hours, minutes, seconds
      parts <- as.numeric(strsplit(x, ":")[[1]])
      # Convert to hours
      parts[1] + parts[2]/60 + parts[3]/3600
    })
  )

um_race_US_subset$Athlete.speed <- ifelse(um_race_US_subset$Event.distance.length=="50km", 50/um_race_US_subset$Athlete.time.hours, 
                                          ifelse(um_race_US_subset$Event.distance.length=="100km", 100/um_race_US_subset$Athlete.time.hours, NA))
 
um_race_US_subset_50km <- um_race_US_subset %>% 
  filter(Event.distance.length == "50km")  

um_race_US_subset_100km <- um_race_US_subset %>% 
  filter(Event.distance.length == "100km") 
um_race_US_combined <- bind_rows(um_race_US_subset_50km, um_race_US_subset_100km) 
um_race_US_combined

write.csv(um_race_US_combined, file = "um_race_US.csv", row.names = FALSE)

#PLOT 1 ####
#Number of participants by gender
#Dip in number of participant in the year of 2020 due to COVID on both races
#Increase in number of female participant over the years

# data prep
year_totals <- um_race_US_combined %>%
  group_by(Year.of.event, Event.distance.length) %>%
  summarise(total = n(), .groups = "drop")

um_race_US_combined <- um_race_US_combined %>%
  left_join(year_totals, by = c("Year.of.event", "Event.distance.length"))

um_race_US_combined$tooltip_text <- paste0(
  "Year: ", um_race_US_combined$Year.of.event, "\n",
  "Total participants: ", um_race_US_combined$total
)

total_participants_gender <- ggplot(
  um_race_US_combined,
  aes(x = Year.of.event, fill = Athlete.gender,tooltip = tooltip_text, data_id = paste(Year.of.event, Event.distance.length))
  ) +
  geom_bar_interactive(position = "stack") +
  facet_wrap(~ Event.distance.length, ncol = 2) +
  scale_fill_manual(
    values = c("M" = "#26C6DA","F" = "#FFCA28"),
    labels = c("M" = "Male", "F" = "Female")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), labels = comma,
                     breaks = pretty_breaks(n = 5)) +
  coord_flip() +
  labs(
    title = "Total Participants per Year (50km vs 100km)",
    x = "Year",
    y = "Number of Participants",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "black", color = NA),
    panel.background  = element_rect(fill = "black", color = NA),
    strip.background  = element_rect(fill = "black", color = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray30"),
    axis.text          = element_text(color = "white"),
    axis.title         = element_text(color = "white"),
    strip.text         = element_text(color = "white", face = "bold"),
    legend.background  = element_rect(fill = "black", color = NA),
    legend.key         = element_rect(fill = "black", color = NA),
    legend.title       = element_text(color = "white"),
    legend.text        = element_text(color = "white"),
    plot.title         = element_text(color = "white", face = "bold"),
    plot.margin        = margin(20, 10, 20, 10),
  )
total_participants_gender

ggsave(
  "TotalParticipants_Gender.png",
  plot = total_participants_gender,
  device = ragg::agg_png,
  width = 14, height = 7, units = "in", res = 300,
  background = "black"
)

#PLOT 2 ####
#Age group distribution per year for 50km and 100km Race Event
um_age <- um_race_US_subset %>%
  mutate(
    age_group_cut = case_when(
      age_at_event < 30 ~ "<30",
      age_at_event >= 30 & age_at_event < 40 ~ "30-40",
      age_at_event >= 40 & age_at_event < 50 ~ "40-50",
      age_at_event >= 50 & age_at_event < 60 ~ "50-60",
      age_at_event >= 60 ~ "60+"),
    age_group_cut =factor(age_group_cut,levels=c("60+","50-60","40-50","30-40","<30"))
  )

age_summary_pct <- um_age %>%
  group_by(Year.of.event, Event.distance.length, age_group_cut) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year.of.event, Event.distance.length) %>%
  mutate(pct = n / sum(n))

ggplot(age_summary_pct,
                     aes(x = Year.of.event,
                         y = pct,
                         fill = age_group_cut)) +
  geom_bar(stat = "identity", color = "white") +
  facet_wrap(~ Event.distance.length, ncol = 2) +
  labs(
    title = "Age Distribution of Runner Participants Over Time",
    subtitle = "Percentage of Participants by Age Group",
    x = "Year", y = "Percentage", fill = "Age Group"
  ) +
  scale_fill_manual(
    values = c(
      "<30"   = "skyblue",
      "30-40" = "seagreen",
      "40-50" = "orange",
      "50-60" = "tomato",  
      "60+"   = "darkgrey"   
    )
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )


# PERFORMANCE VS AGE
## 100km Distance
age_speed_by_year_100km <- um_race_US_subset_100km %>%
  filter(Year.of.event >= 1990, Year.of.event <= 2020) %>%
  mutate(age_group = cut(age_at_event,
                         breaks = c(0, 30, 40, 50, 60, Inf),
                         labels = c("<30", "30-40", "40-50", "50-60", "60+"),
                         right = FALSE)) %>%
  group_by(Year.of.event, age_group) %>%
  summarise(
    avg_speed = mean(Athlete.speed, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

label_100km <- age_speed_by_year_100km %>%
  filter(Year.of.event %in% c(1990, 2020))

ggplot(age_speed_by_year_100km, aes(x = Year.of.event, y = avg_speed, color = age_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  geom_text(
    data = label_100km,
    aes(label = round(avg_speed, 1)),
    fontface = "bold",
    vjust = -0.5,
    color = "black"
  ) +    
  labs(
    title = "Average Ultra Marathon Speed For 100km by Age Group (1990–2020)",
    subtitle = "U.S. runners",
    x = "Year of Event",
    y = "Average Speed (km/hr)",
    color = "Age Group"
  ) +
  theme_minimal(base_size = 14)


##50km Distance
age_speed_by_year_50km <- um_race_US_subset_50km %>%
  filter(Year.of.event >= 1990, Year.of.event <= 2020) %>%
  mutate(age_group = cut(age_at_event,
                         breaks = c(0, 30, 40, 50, 60, Inf),
                         labels = c("<30", "30-40", "40-50", "50-60", "60+"),
                         right = FALSE)) %>%
  group_by(Year.of.event, age_group) %>%
  summarise(
    avg_speed = mean(Athlete.speed, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

label_50km <- age_speed_by_year_50km %>%
  filter(Year.of.event %in% c(1990, 2020))

ggplot(age_speed_by_year_50km, aes(x = Year.of.event, y = avg_speed, color = age_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  geom_text(
    data = label_50km,
    aes(label = round(avg_speed, 1)),
    fontface = "bold",
    vjust = -0.5,
    color = "black"
  ) +  
  labs(
    title = "Average Ultra Marathon Speed For 50km by Age Group (1990–2020)",
    subtitle = "U.S. runners",
    x = "Year of Event",
    y = "Average Speed (km/hr)",
    color = "Age Group"
  ) +
  theme_minimal(base_size = 14)


#PLOT 3 ####
# Speed trends top 10 runners vs All Runners

#Remove incorrect data shows impossible time record 
#For Race "50km" , speed between 5km/h and 17km/h
#For Race "100km", speed between 5km/h and 19km/h
um_race_US_subset <- um_race_US_subset %>%
  filter((Event.distance.length == "50km"  & Athlete.speed > 5 & Athlete.speed < 17) |
           (Event.distance.length == "100km" & Athlete.speed > 5 & Athlete.speed < 19))


# 50km Top 10 
top10_50km <- um_race_US_subset %>%
  filter(Event.distance.length == "50km") %>%
  group_by(Year.of.event) %>%
  arrange(desc(Athlete.speed)) %>%
  slice(1:10) %>%
  summarise(
    avg_speed = mean(Athlete.speed),
    group = "Top 10",
    Event.distance.length = "50km"
  )

View(top10_50km)
# 50km All runners 
all_50km <- um_race_US_subset %>%
  filter(Event.distance.length == "50km") %>%
  group_by(Year.of.event) %>%
  summarise(
    avg_speed = mean(Athlete.speed),
    group = "All Runners",
    Event.distance.length = "50km"
  )

speed_50km <- bind_rows(top10_50km, all_50km)

# 100km Top 10
top10_100km <- um_race_US_subset %>%
  filter(Event.distance.length == "100km") %>%
  group_by(Year.of.event) %>%
  arrange(desc(Athlete.speed)) %>%
  slice(1:10) %>%
  summarise(
    avg_speed = mean(Athlete.speed),
    group = "Top 10",
    Event.distance.length = "100km"
  )

# 100km All runners
all_100km <- um_race_US_subset %>%
  filter(Event.distance.length == "100km") %>%
  group_by(Year.of.event) %>%
  summarise(
    avg_speed = mean(Athlete.speed),
    group = "All Runners",
    Event.distance.length = "100km"
  )

speed_100km <- bind_rows(top10_100km, all_100km)

# Combine both distances
speed_both <- bind_rows(speed_50km, speed_100km)

top10_speed_trends <- ggplot(speed_both,
                             aes(x = Year.of.event,
                                 y = avg_speed,
                                 color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Event.distance.length, ncol = 2) +
  labs(
    title = "Average Speed Trends Over Time Top 10 vs All Runners",
    subtitle = "For 50km and 100km Race",
    x = "Year",
    y = "Average Speed (km/h)",
    color = "Group"
  ) +
  scale_color_manual(
    values = c(
      "Top 10"      = "#4FC3F7",
      "All Runners" = "#B0BEC5"
    )
  ) +
  scale_y_continuous(breaks = seq(4, 16, by = 2), limits = c(6, NA))+
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "black", color = NA),
    panel.background  = element_rect(fill = "black", color = NA),
    strip.background  = element_rect(fill = "black", color = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_line(color = "gray30"),
    panel.grid.major.y = element_line(color = "gray30"),
    axis.text         = element_text(color = "white"),
    axis.title        = element_text(color = "white"),
    strip.text        = element_text(size = 14, face = "bold", color = "white"),
    legend.position   = "bottom",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key        = element_rect(fill = "black", color = NA),
    legend.title      = element_text(color = "white"),
    legend.text       = element_text(color = "white"),
    plot.title        = element_text(color = "white", face = "bold"),
    plot.subtitle     = element_text(color = "white")
  )

top10_speed_trends

animated_speed <- top10_speed_trends +
  transition_reveal(Year.of.event) +
  labs(subtitle = "Year: {frame_along}")

animate(
  animated_speed,
  nframes = 150,
  fps = 15,
  end_pause = 45,
  width = 800,
  height = 450,
  renderer = gifski_renderer()
)

anim_save("top10_speed_trends.gif", animation = last_animation())

#PLOT 4 ####
# Average speed trends Males vs Females

# 50km All runners by gender
all_50km_gender <- um_race_US_subset %>%
  filter(Event.distance.length == "50km") %>%
  group_by(Year.of.event, Athlete.gender) %>%
  summarise(
    avg_speed = mean(Athlete.speed),
    group = "All Runners",
    Event.distance.length = "50km",
    .groups = "drop"
  )
# 100km All runners by gender
all_100km_gender <- um_race_US_subset %>%
  filter(Event.distance.length == "100km") %>%
  group_by(Year.of.event, Athlete.gender) %>%
  summarise(
    avg_speed = mean(Athlete.speed),
    group = "All Runners",
    Event.distance.length = "100km",
    .groups = "drop"
  )

# Combine both distances 
all_both_gender <- bind_rows(all_50km_gender, all_100km_gender)


speed_trends_gender <- ggplot(all_both_gender,
                       aes(x = Year.of.event,
                           y = avg_speed,
                           color = Athlete.gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Event.distance.length, ncol = 2) +
  labs(
    title = "Average Speed Trends for All Runners (Male vs Female)",
    x = "Year", y = "Average Speed (km/h)", color = "Gender"
  ) +
  scale_color_manual(values = c(
    "F" = "#FFC857",
    "M" = "#4FC3F7"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "black", color = NA),
    panel.background  = element_rect(fill = "black", color = NA),
    strip.background  = element_rect(fill = "black", color = "white"),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_line(color = "gray30"),
    panel.grid.major.y = element_line(color = "gray30"),
    axis.text         = element_text(color = "white"),
    axis.title        = element_text(color = "white"),
    strip.text        = element_text(size = 14, face = "bold", color = "white"),
    legend.position   = "bottom",
    legend.title      = element_text(color = "white"),
    legend.text       = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key        = element_rect(fill = "black", color = NA),
    plot.title        = element_text(color = "white", face = "bold"),
    plot.subtitle     = element_text(color = "white")
  )

speed_trends_gender

speed_trends_anim <- speed_trends_gender+transition_reveal(Year.of.event)

speed_trends_anim <- animate(
  speed_trends_anim,
  nframes = 150,
  fps = 15,
  end_pause = 45,
  width = 800,
  height = 450,
  renderer = gifski_renderer()
)

anim_save("speed_trends_gender.gif", animation=speed_trends_anim, renderer=gifski_renderer())


#PLOT 5 ####
#Map plot of participants
# US State shapefile is downloaded from website below for year of 2021
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

us.map<-st_read("tl_2021_us_state")
View(us.map)

# Filter map not to include state of Hawaii and Alaska
us.map <- us.map %>%
  filter(!STUSPS %in% c("AK", "HI"))
us.map$state <- us.map$STUSPS

ggplot(us.map) +
  geom_sf() +
  coord_sf(
    xlim = c(-130, -60),   # longitude range
    ylim = c(15, 50),      # latitude range
    expand = FALSE
  )

#Create State variable from the Athlete club data
um_race_US_subset <- um_race_US_subset %>%
  mutate(state = str_extract(Athlete.club, "(?<=,\\s)\\w{2}")) %>%
  filter(!is.na(state) & state != "")

#Group by state with the total from each state
state_summary <- um_race_US_subset %>%
  group_by(state) %>%
  summarise(
    state_total = n(),
  )

View(state_summary)

#Merge with us.map
um_race_US_subset.map <- merge(us.map,state_summary, by="state")  
View(um_race_US_subset.map)

map.theme<-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 panel.background=element_blank(),panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),plot.background=element_blank())

state_labels <- um_race_US_subset.map %>%
  st_centroid() %>%                        
  st_coordinates() %>%                     
  as.data.frame() %>%
  bind_cols(um_race_US_subset.map %>% select(state))

participant.plot <- ggplot() +
  geom_sf_interactive(
    data   = um_race_US_subset.map,
    size   = 0.125,
    aes(
      fill    = state_total,
      data_id = state,
      tooltip = paste0(
        "State: ", state, "\n",
        "Total Participants: ", state_total
      )
    )
  ) +
  scale_fill_distiller(
    palette   = "PuBu",
    direction = 1,
    name      = "Participants per state"
  ) +
  geom_text_repel(
    data   = state_labels,
    aes(X, Y, label = state),
    size   = 2.3,
    color  = "black",
    box.padding      = 0.15,
    point.padding    = 0.1,
    segment.color    = "grey50",
    min.segment.length = 0,
    max.overlaps     = 50
  ) +
  map.theme +
  ggtitle("Race Participation Across the United States") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank(),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    plot.title       = element_text(color = "black", face = "bold"),
    legend.position      = "bottom",
    legend.direction     = "vertical",
    legend.key.height    = unit(0.4, "cm"),
    legend.key.width     = unit(0.8, "cm"),
    legend.background    = element_rect(fill = "white", color = NA),
    legend.key           = element_rect(fill = "white", color = NA),
    legend.title         = element_text(color = "black"),
    legend.text          = element_text(color = "black"),
    plot.margin      = margin(5, 0, 0, 0),
    panel.spacing    = unit(0, "mm")
  ) +
  coord_sf(
    xlim   = c(-130, -60),
    ylim   = c(20, 50),
    expand = FALSE
  )

participant.plot

participant_state_interactive <- girafe(
  ggobj     = participant.plot,
  width_svg = 12,
  height_svg = 6,
  bg        = "white"
) %>%
  girafe_options(
    opts_hover(css = "stroke:#4DD0E1;stroke-width:2;cursor:pointer;")
  ) +
  map.theme +
  ggtitle("Race Participation Across the United States") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank(),
    axis.text        = element_text(color = "black"),
    axis.title       = element_text(color = "black"),
    plot.title       = element_text(color = "black", face = "bold"),
    legend.position      = "bottom",
    legend.direction     = "vertical",
    legend.key.height    = unit(0.4, "cm"),
    legend.key.width     = unit(0.8, "cm"),
    legend.background    = element_rect(fill = "white", color = NA),
    legend.key           = element_rect(fill = "white", color = NA),
    legend.title         = element_text(color = "black"),
    legend.text          = element_text(color = "black"),
    plot.margin      = margin(5, 0, 0, 0),
    panel.spacing    = unit(0, "mm")
  ) +
  coord_sf(xlim   = c(-130, -60), ylim   = c(20, 50), expand = FALSE)

participant_state_interactive <- girafe(
  ggobj     = participant.plot,
  width_svg = 12,
  height_svg = 8,
  bg        = "white" 
) %>%
  girafe_options(
    opts_hover(css = "stroke:#4DD0E1;stroke-width:2;cursor:pointer;")
  )

participant_state_interactive

htmlwidgets::saveWidget(participant_state_interactive, "Participants_state.html", selfcontained = TRUE)


# PLOT 6 ####

topk <- 10
start_year <- 2000

plot_df <- um_race %>%
  transmute(
    year = as.integer(Year.of.event),
    code = as.character(Athlete.country)
  ) %>%
  filter(!is.na(year), !is.na(code), year >= start_year) %>%
  count(year, code, name = "n_year") %>%
  group_by(code) %>%
  complete(year = seq(min(year), max(year)), fill = list(n_year = 0)) %>%
  arrange(year) %>%
  mutate(value = cumsum(n_year)) %>%
  ungroup() %>%
  mutate(
    country = countrycode(code, "ioc", "country.name"),
    country = if_else(is.na(country), code, country)
  ) %>%
  group_by(year) %>%
  mutate(
    rank = dense_rank(desc(value)),
    label_val = comma(value)
  ) %>%
  filter(rank <= topk) %>%
  ungroup()

p <- ggplot(plot_df, aes(rank, value, group = country)) +
  geom_col(width = 0.9, fill = "#4FC3F7") +
  geom_text(aes(y = 0, label = country), hjust = 1, color = "white") +
  geom_text(aes(label = label_val), hjust = 0, color = "white") +
  coord_flip(clip = "off") +
  scale_x_reverse(limits = c(topk + 0.5, 0.5), breaks = 1:topk)+
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  theme_void(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#0B0F14", color = NA),
    plot.background  = element_rect(fill = "#0B0F14", color = NA),
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(color = "grey70", hjust = 0.5),
    plot.caption = element_text(color = "grey70", hjust = 0.5),
    plot.margin = margin(10, 110, 10, 110)
  ) +
  labs(
    title = "Cumulative Ultramarathon Finishers: {closest_state}",
    subtitle = "Top 10 Countries (50km + 100km)",
    caption = "Counts are cumulative finishers"
  )

top_ten_countries <- p +
  transition_states(
    year,
    transition_length = 0,
    state_length = 1
  )

top10countries <- animate(top_ten_countries, nframes = 200, fps = 10, width = 500, height = 400,
        end_pause = 50, renderer = gifski_renderer())

anim_save("ultra_top10countries.gif", animation = top10countries)


# Prep for PLOT 7 ####

library(tidygeocoder)  

top_events_for_sheets <- um_race |>
  group_by(Event.name) |>
  summarise(
    total_finishers = n(),
    distance        = first(Event.distance.length),
    .groups = "drop"
  ) |>
  arrange(desc(total_finishers)) |>
  slice_head(n = 30) %>%
  mutate(
    city    = "",                 
    country = "",
    lat     = "",
    lon     = ""
  )

write.csv(
  top_events_for_sheets,
  "top_ultra_events_for_sheets.csv",
  row.names = FALSE
)

# PLOT 7#### 
top_ultras_geo <- read_excel("top_ultra_events_for_sheets.xlsx") %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )

top_ultras_geo <- top_ultras_geo %>%
  mutate(
    distance_km = case_when(
      str_detect(distance, "mi") ~ as.numeric(str_remove(distance, "mi")) * 1.60934,
      str_detect(distance, "km") ~ as.numeric(str_remove(distance, "km")),
      TRUE ~ NA_real_
    ),
    distance_cat = case_when(
      distance_km <= 60  ~ "Short ultra (≤60 km)",
      distance_km <= 100 ~ "Mid ultra (61–100 km)",
      distance_km <= 160 ~ "Long ultra (101–160 km)",
      distance_km > 160  ~ "Extreme (160+ km)",
      TRUE               ~ "Other"
    ),
    distance_cat = factor(
      distance_cat,
      levels = c(
        "Short ultra (≤60 km)",
        "Mid ultra (61–100 km)",
        "Long ultra (101–160 km)",
        "Extreme (160+ km)",
        "Other"
      )
    )
  )

primary_col <- "#2B1B0F"  
bg_col      <- "#F1E0B2"  
border_col  <- "#C8914B"
accent_col  <- "#8A4B1E"

top_ultras_geo <- top_ultras_geo %>%
  mutate(
    popup_text = paste0(
      "<div style='",
      "font-family:Georgia, \"Times New Roman\", serif;",
      "font-size:12px;",
      "color:", primary_col, ";",
      "background-color:", bg_col, ";",
      "padding:6px 9px;",
      "border-radius:6px;",
      "border:2px solid ", border_col, ";",
      "box-shadow:0 2px 5px rgba(0,0,0,0.2);",
      "'>",
      "<div style='font-size:14px;font-weight:bold;margin-bottom:4px;",
      "color:", primary_col, ";'>",
      event.name,
      "</div>",
      "<div style='margin-bottom:3px;'>",
      city, ", ", country,
      "</div>",
      "<div style='color:", accent_col, ";margin-bottom:2px;'>",
      "<b>Distance:</b> ", distance,
      "</div>",
      "<div style='color:", accent_col, ";'>",
      "<b>Total finishers:</b> ", comma(total_finishers),
      "</div>",
      "</div>"
    )
  )

race_icon <- makeIcon(
  iconUrl    = "running-shoe-with-wings.png",  
  iconWidth  = 30,
  iconHeight = 30,
  iconAnchorX = 15,
  iconAnchorY = 30
)

ultra_map <- leaflet(top_ultras_geo) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(
    lng   = ~lon,
    lat   = ~lat,
    icon  = race_icon,
    popup = ~popup_text
  )

ultra_map <- leaflet(top_ultras_geo) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addMarkers(
    lng = ~lon,
    lat = ~lat,
    icon = race_icon,
    popup = ~popup_text
  )

ultra_map
saveWidget(ultra_map, "top_ultras_world_map.html", selfcontained = TRUE)
