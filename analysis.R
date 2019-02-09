# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

install.packages("lubridate")
install.packages("ggmap")

# Load in your data

evictions <- read.csv("data/Eviction_Notices.csv", stringsAsFactors = FALSE)
View(evictions)

# Compute some values of interest and store them in variables for the report

# How many evictions were there?

num_evictions <- nrow(evictions)
num_features <- ncol(evictions)

# Create a table (data frame) of evictions by zip code (sort descending)

sort_evictions <- evictions %>%
  group_by(Eviction.Notice.Source.Zipcode) %>%
  count() %>%
  arrange(-n)

# Create a plot of the number of evictions each month in the dataset

by_month <- evictions %>%
  mutate(date = as.Date(File.Date, format = "%m/%d/%y")) %>%
  mutate(month = floor_date(date, unit="month")) %>%
  group_by(month) %>%
  count()

View(by_month)

# Store plot in a variable

ggplot(data = by_month) +
  geom_line(mapping = aes(x = month, y = n)) + 
  labs(x = "Time", y = "# of Evictions", title = "Evictions in San Francisco")

# Map evictions in 2017 

# Format the lat/long variables, filter to 2017

# from lecture, i don't understand this too well
evictions_2017 <- evictions %>%
  mutate(date = as.Date(File.Date, format="%m/%d/%y")) %>%
  filter(format(date, "%Y") == "2017") %>%
  separate(Location, c("lat", "long"), ", ") %>%
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)),
    long = as.numeric(gsub("\\)", "", long))
  )

# Create a maptile background

base_plot <- qmplot(
  data = evictions_2017,
  x = long,
  y = lat,
  geom = "blank",
  maptype = "toner-background",
  darken = 0.7,
  legend = "topleft"
)

base_plot

# Add a layer of points on top of the map tiles

evictions_plot <- base_plot +
  geom_point(mapping = aes(x = long, y = lat), color = "red", alpha = 0.3) +
  labs(title = "SF Evictions 2017") +
  theme(plot.margin = margin(0.3, 0, 0, 0, "cm"))

evictions_plot
