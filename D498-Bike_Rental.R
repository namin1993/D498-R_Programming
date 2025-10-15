# ---------------------------
# Install packages
# ---------------------------

# options(repos = "https://cran.r-project.org/")
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('ggpubr')
# install.packages('tidyr')
# install.packages('lubridate')
# install.packages('stringr')
# install.packages('rmarkdown')
# install.packages('knitr')
# install.packages('markdown')

#unlink("/Library/Frameworks/R.framework/Versions/4.2/Resources/library/00LOCK-tidyr", recursive = TRUE)

# ---------------------------
# Import packages
# ---------------------------
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(lubridate)
library(stringr)
library(rmarkdown)
library(knitr)
library(markdown)

# ---------------------------
# Import datasets
# ---------------------------
chicago <- read.csv('chicago.csv')
new_york <- read.csv('new-york-city.csv')
washington <- read.csv('washington.csv')

# ---------------------------
# View datasets and data types
# ---------------------------
View(chicago)
View(new_york)
View(washington)

sapply(chicago, typeof)

# ---------------------------
# Place city dataframes into a list object
# ---------------------------
city_dfs <- list(chicago = chicago, new_york = new_york, washington = washington)


# ---------------------------
# Helper function: normalize & enrich each city's dataframe
# ---------------------------
prepare_city_df <- function(df) {
  df %>%
    # ensure column names exist; be robust to factors/characters
    mutate(
      Trip.Duration = as.numeric(Trip.Duration),
      Start.Station = as.character(Start.Station),
      End.Station   = as.character(End.Station),
      User.Type     = as.character(User.Type),
      Gender        = if ("Gender" %in% names(.)) as.character(Gender) else NA_character_
    ) %>%
    # parse datetimes (try ymd_hms first then fallback to parse_date_time with common orders)
    mutate(
      Start.Time = dplyr::coalesce(
        lubridate::ymd_hms(Start.Time, tz = "UTC"),
        lubridate::parse_date_time(Start.Time, orders = c("ymd HMS","mdy HMS","dmy HMS","ymd HM","mdy HM"), tz = "UTC")
      ),
      End.Time = dplyr::coalesce(
        lubridate::ymd_hms(End.Time, tz = "UTC"),
        lubridate::parse_date_time(End.Time, orders = c("ymd HMS","mdy HMS","dmy HMS","ymd HM","mdy HM"), tz = "UTC")
      )
    ) %>%
    # add derived columns
    mutate(
      Month = ifelse(is.na(Start.Time), NA, month(Start.Time)),            # numeric month 1-12
      Month_label = ifelse(is.na(Month), NA, month(Start.Time, label = TRUE, abbr = FALSE)), # full month name
      Hour = ifelse(is.na(Start.Time), NA, hour(Start.Time))
    )
}

# ---------------------------
# Question 1: most common month for each city
# ---------------------------
# function: most_common_month_per_city
# parameters: list 
# return: data frame summarizing city name, the most common month, 
# the number of trips in that month, and the percentage of total 
# trips it represents

most_common_month_per_city <- bind_rows(
  # loop through each city in city_dfs an run function
  lapply(names(city_dfs), function(city) {
    # call prepare_city_df() to clean city dataframe
    df <- prepare_city_df(city_dfs[[city]])
    # count how many rows actually have a non-missing month
    total_trips <- sum(!is.na(df$Month))
    # create a case where all rows are missing a month 
    if (total_trips == 0) {
      tibble(city = city, Month = NA_character_, Month_label = NA_character_, trips = 0L, pct = NA_real_)
    } else {
      df %>%
        # keep only rows with valid month labels
        filter(!is.na(Month_label)) %>%
        # count how many trips occurred in each month and 
        # sort from most to least frequent
        count(Month_label, name = "trips", sort = TRUE) %>%
        # take the month(s) with the highest trip count
        # if multiple months are tied, keep them all
        slice_max(trips, n = 1, with_ties = TRUE) %>%    # keep ties if any
        # add the city name and calculate the percentage that month represents out of all trips
        mutate(city = city, pct = trips / total_trips) %>%
        # keep only the columns we care about (renames Month_label → Month)
        transmute(city, Month = as.character(Month_label), trips, pct)
    }
  }),
  .id = NULL
)

# ---------------------------
# Q1:Show results as tables
# ---------------------------
cat("Most common month per city:\n")
print(most_common_month_per_city)

# ---------------------------
# Q1: Show results as visuals
# ---------------------------
# ---------------------------
# Question 1: most common month per city
#
# Answer: For each city, the most common month in which there were the most bike trips
# were all in June. 
# In Chicago, 32.7% of all trips taken on Citibike were in June.
# In New York, 25.3% of all trips taken on Citibike were in June.
# In Washington, 22.8% of all trips taken on Citibike were in June.
# For each city, there were over 62,500 trips taken in June. Therefore, this data 
# can be used to prepare for maintenance costs for each bike and allotting enough 
# bike stations for the month of June.
# ---------------------------
ggplot(most_common_month_per_city, aes(x = city, y = trips, fill = Month)) +
  # Position graph vertically by default
  # Adjust the bars to prevent overlap
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(pct * 100, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  # Label axes
  labs(
    title = "Most Common Month for Bike Trips per City",
    x = "City",
    y = "Number of Trips",
    fill = "Month"
  ) +
  # Minimalistic Theme with base font size 13
  theme_minimal(base_size = 13) +
  # Define plot title and legend
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# ---------------------------
# Question 2: average travel time for users in different cities
# (we show avg & median in seconds and minutes, and number of trips)
# ---------------------------
avg_travel_by_city_user <- bind_rows(
  # Loop through each city and apply function
  lapply(names(city_dfs), function(city) {
    # call prepare_city_df() to clean city dataframe
    df <- prepare_city_df(city_dfs[[city]])
    df %>%
      # Removes rows where the User.Type is missing (NA),
      # Removes rows where trip duration is missing,
      # Removes trips that have a duration ≤ 0 (bad or corrupted data)
      filter(!is.na(User.Type), !is.na(Trip.Duration), Trip.Duration > 0) %>%
      # Group the data into separate user types
      group_by(User.Type) %>%
      # Summarize key statistics
      summarise(
        trips = n(),
        avg_seconds = mean(Trip.Duration, na.rm = TRUE),
        median_seconds = median(Trip.Duration, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Convert seconds to minutes
      # Add 'city' column in the stat summary table
      mutate(
        avg_minutes = avg_seconds / 60,
        median_minutes = median_seconds / 60,
        city = city
      ) %>%
      select(city, User.Type, trips, avg_seconds, avg_minutes, median_seconds, median_minutes)
  }),
  .id = NULL
)

# ---------------------------
# Q2: Show results as tables
# ---------------------------
cat("\nAverage travel time by user type (per city):\n")
print(avg_travel_by_city_user)

# ---------------------------
# Q2: Show results as visuals
# ---------------------------
# ---------------------------
# Question 2: average travel time for users in different cities
#
# Answer: The average travel time in minutes for each city depending on the customer type
# are as follows:
# Chicago:
#         Subscriber: 11.7 minutes
#         Dependent: 5.18 minutes
#         Customer: 31 minutes
# New York:
#         Subscriber: 12.8 minutes
#         Customer: 34.3 minutes
#         Unidentified Customer Type: 24.5 minutes
# Washington:
#         Subscriber: 12.3 minutes
#         Customer: 43.9 minutes
# ---------------------------
ggplot(avg_travel_by_city_user, aes(x = reorder(city, avg_minutes), y = avg_minutes, fill = User.Type)) +
  # Adjust the bars to prevent overlap
  geom_col(position = "dodge") +
  # Position graph horizontally
  coord_flip() +
  # Label axes
  labs(
    title = "Average Trip Duration by City and User Type",
    x = "City",
    y = "Average Duration (minutes)",
    fill = "User Type"
  ) +
  # Minimalistic Publication-Ready Theme with base font size 13 and no grid lines
  theme_pubr(base_size = 13) +
  # Define plot title and legend
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# ---------------------------
# Question 3: most common trip (start -> end) per city
# ---------------------------
most_common_trip_per_city <- bind_rows(
  # Loop through each city and apply function
  lapply(names(city_dfs), function(city) {
    # call prepare_city_df() to clean city dataframe
    df <- prepare_city_df(city_dfs[[city]])
    df %>%
      # Filter out only trips with a start and end station recorded
      filter(!is.na(Start.Station), !is.na(End.Station)) %>%
      # Combine the start and end stations into a single string describing the trip
      mutate(trip = str_c(Start.Station, " -> ", End.Station)) %>%
      # Group the data by the 'trips' column and count the number of rides for each unique trip
      # Sort the results from most to least frequent trips
      count(trip, name = "trips", sort = TRUE) %>%
      slice_max(trips, n = 1, with_ties = TRUE) %>%   # keep ties if there are multiple equally-frequent trips
      # Add a new column with the city’s name
      mutate(city = city) %>%
      select(city, trip, trips)
  }),
  .id = NULL
)

# ---------------------------
# Q3: Show results as tables
# ---------------------------
cat("\nMost common trip (start -> end) per city:\n")
print(most_common_trip_per_city)


# ---------------------------
# Q3: Show results as visuals
# ---------------------------
# ---------------------------
# Question 3: most common trip (start -> end) per city
#
# Answer: The most common bike trips per city are as follows:
# Chicago: Lake Shore Dr & Monroe St -> Streeter Dr & Grand Ave at 854 trips
# New York: E 7 St & Avenue A -> Cooper Square & E 7 St at 168 trips
# Washington: Jefferson Dr & 14th St SW -> Jefferson Dr & 14th St SW at 673 trips
# ---------------------------
ggplot(most_common_trip_per_city, aes(x = reorder(city, trips), y = trips, fill = city)) +
  # Don't require legend
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = trip), hjust = 1.05, size = 3, color = "white") +
  # Position graph horizontally
  coord_flip() +
  # Label axes
  labs(
    title = "Most Common Bike Trip per City",
    x = "City",
    y = "Number of Trips"
  ) +
  # Minimalistic Theme with base font size 13
  theme_minimal(base_size = 13) +
  # Define plot title
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# Convert R-Script to Html page
knitr::spin("D498-Bike_Rental.R", knit = FALSE)
rmarkdown::render("D498-Bike_Rental.Rmd", output_format = "html_document")

# DIAGNOSTIC COMMAND
# rmarkdown::render("D498-Bike_Rental.Rmd", output_format = "html_document", clean = FALSE)

