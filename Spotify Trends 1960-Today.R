# Load Libraries and Data --------------------------------------------------
library(tidyverse)
library(ggforce)

Songs <- read_csv("~/GitHub/Spotify Trends 1960-Today/Top 10000 Spotify Songs 1960-Today.csv")
glimpse(Songs)

# Convert album release date to the correct data type
Songs$`Album Release Date` = as.Date(Songs$`Album Release Date`)

# Convert Track Duration column from ms to minutes
Songs$`Track Duration (ms)` = Songs$`Track Duration (ms)` / 60000
Songs <- Songs %>%
  rename("Track Duration" = "Track Duration (ms)")

# Analyzing Track Duration ------------------------------------------------
# Remove outliers with track durations longer than 10 minutes
Duration_Max_Ten_Minutes <- Songs %>%
  filter(`Track Duration` <= 10)

# Scatter plot with loess showing how track duration has changed over the years
Song_Duration <- ggplot(Duration_Max_Ten_Minutes, aes(`Album Release Date`, `Track Duration`)) +
                 geom_point(
                   shape = "diamond",
                   alpha = 0.3,
                   color = "limegreen",
                   size = 1,
                 ) +
                 geom_smooth(method = 'loess',
                             span = 0.1,
                             color = "black",
                             linewidth = 1) +
                 labs(x = "Release Date",
                      y = "Track Duration (minutes)") +
                 ggtitle("Track Duration Over Time") +
                 theme(plot.title = element_text(hjust = 0.5))

# Filter top 10 longest songs
Longest_Songs <- Songs %>%
  select(`Track Name`, `Artist Name(s)`, `Track Duration`, `Album Release Date`) %>%
  arrange(desc(`Track Duration`)) %>%
  slice(1:10)

# Analyzing Music Trends --------------------------------------------------
# Create a data frame with the average of each music trend for each year 
Music_Trends <- Songs %>%
  select(`Album Release Date`, `Danceability`, `Energy`, `Speechiness`, `Acousticness`, 
         `Instrumentalness`, `Liveness`, `Valence`,`Loudness`) %>%
  mutate(`Album Release Date` = as.numeric(format(as.Date(`Album Release Date`, "%Y, %m, %d"), "%Y"))) %>% # Keep just the release year for each track
  mutate(`Speechiness` = `Speechiness` * 10) %>% # Speechiness was on scale from 0.0 to 0.1 while the other categories are on a scale from 0 to 1
  rename(`Release Year` = `Album Release Date`) %>%
  filter(!is.na(`Release Year`) & `Release Year` >= 1960) %>% # Limit range to 1960 to today
  group_by(`Release Year`) %>%
  summarize_at(vars(`Danceability`, `Energy`, `Speechiness`, `Acousticness`, `Instrumentalness`,
                    `Liveness`, `Valence`, `Loudness`), mean) %>% # Calculate the average values of each trend for each year
  arrange(`Release Year`)

# Line plot of loudness over the years
Loudness_Over_Time <- ggplot(Music_Trends, aes(`Release Year`, `Loudness`)) +
                      geom_line(color = "limegreen") +
                      labs(x = "Release Year",
                           y = "Average Loudness (dB)") +
                      ggtitle("Track Loudness Over the Years") +
                      theme(plot.title = element_text(hjust = 0.5))

# Line plot of speechiness and acousticness over the years
Speechiness_Acousticness_Line <- ggplot(Music_Trends, aes(`Release Year`)) +
                                 geom_line(aes(y = `Speechiness`, color = "Speechiness")) +
                                 geom_line(aes(y = `Acousticness`, color = "Acousticness")) +
                                 labs(x = "Release Year",
                                      y = "Average Loudness (dB)") +
                                 ggtitle("Acousticness vs. Speechiness Over Time") +
                                 scale_color_manual(name = "",
                                                    values = c("Speechiness" = "limegreen", "Acousticness" = "black")) +
                                 theme(plot.title = element_text(hjust = 0.5),
                                       legend.position = "top")

# Scatter plot of speechiness vs acousticness
Speechiness_Acousticness_Scatter <- ggplot(Music_Trends, aes(`Speechiness`, `Acousticness`)) +
                                    geom_point(shape = "diamond",
                                               color = "limegreen",
                                               size = 4) +
                                    ggtitle("Acousticness vs. Speechiness") +
                                    theme(plot.title = element_text(hjust = 0.5))

# Facet matrix for scatter plot of every trend vs every trend
Trend_Matrix <- ggplot(Music_Trends) +
                geom_point(aes(x = .panel_x, y = .panel_y),
                           shape = "diamond",
                           color = "limegreen",
                           size = 2) +
                geom_smooth(method = "lm",
                            aes(x = .panel_x, y = .panel_y),
                            color = "black") +
                facet_matrix(vars(`Danceability`, `Energy`, `Speechiness`, `Acousticness`,
                                  `Instrumentalness`, `Liveness`, `Valence`),
                             layer.diag = 0)

# Display Results ---------------------------------------------------------
Song_Duration
Loudness_Over_Time
Speechiness_Acousticness_Line
Speechiness_Acousticness_Scatter
Trend_Matrix