# Next steps
  # Examine reviews by genre (users vs critics)
  # Move to RMarkdown



# Add csv with metacritic videogame reviews to our environment
game.reviews <- read.csv("./metacritic_VGreviews.csv") 

# load some libraries for Exploratory Data Analysis
library(skimr)
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)

# Review some summary information about the game reviews dataframe
skim(game.reviews) 
  # 17K observations with 20 variables/features
  # quick skim of the dataframe shows that there are no missing values in the 11 character columns
  # numeric columns have missing values, but those seem to be bc reviews are stored in different fields

sapply(game.reviews.df, class) # check variable classes

#### Pre-Processing ####
# Convert "release date" column from character to date
game.reviews.df <- game.reviews %>% 
  mutate(., release_date = mdy(release_date)) # Dates in format: "Month - Day - Year"

# Create new column that extracts the year from release_date
game.reviews.df <- game.reviews.df %>% 
  mutate(., release_year = year(release_date))
  
# The critic and user review scores are stored in different columns based on
# whether they are positive, mixed, or negative. 
  
  # Create ONE column to store all the critic reviews
game.reviews.df <-game.reviews.df %>% 
  mutate(., critic_score = case_when(
    is.na(critic_score_pos) & is.na(critic_score_mix) ~ critic_score_neg,
    is.na(critic_score_neg) & is.na(critic_score_pos) ~ critic_score_mix,
    is.na(critic_score_mix) & is.na(critic_score_neg) ~ critic_score_pos,
    TRUE ~ critic_score_pos
  ))

# Two games do not have critic reviews (Kalypso and Eggtooth Team). We will filter them out
game.reviews.df <-  game.reviews.df %>% 
  filter(., !is.na(critic_score))

# Next, repeat the same process for the user review columns
game.reviews.df <- game.reviews.df %>% 
  mutate(., user_score = case_when(
    is.na(user_score_pos) & is.na(user_score_mix) ~ user_score_neg,
    is.na(user_score_neg) & is.na(user_score_pos) ~ user_score_mix,
    is.na(user_score_mix) & is.na(user_score_neg) ~ user_score_pos,
    TRUE ~ user_score_pos
  ))

# Scale the critic-score by 0.1 so that it matches the user score format
game.reviews.df <- game.reviews.df %>%
  mutate(critic_score_scaled = critic_score/10)


# We can now create some charts to explore the review data
  # User score frequency plot
user.reviews.distr <-game.reviews.df %>% 
  ggplot(data = ., aes(x = user_score)) +
  geom_bar(position = "dodge")

user.reviews.distr

  # Critic score frequency plot
critic.reviews.distr <- game.reviews.df %>% 
  ggplot(., aes(x = critic_score_scaled)) +
  geom_bar(position = "dodge")
critic.reviews.distr

  # Overlap critic and user scores on one chart

# Let's observe density plots for the user and critic reviews
game.reviews.distr <- game.reviews.df %>% 
  pivot_longer(., cols = c(user_score, critic_score_scaled), names_to = "review_type", values_to = "review_score") %>% 
  ggplot(., aes(x = review_score, fill = review_type))+
  geom_density(alpha = 0.6) +
  labs(title = "Distribution of Review Scores\n Users vs Critics",
       x = "Review Score")
game.reviews.distr

# Correlation between user and critic scores ####
scatter.user.critic.reviews <- game.reviews.df %>% 
  ggplot(., aes(x = critic_score_scaled, y = user_score))+
  labs(title = "Video Game Reviews: Critic Scores vs User Scores", x= "Critic Review Score", y = "User Review Score") +
  geom_point(size = 0.6, color = "red")+
  geom_smooth(method = "lm") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
scatter.user.critic.reviews




# Check correlation between critic score and user score
cor.test(game.reviews.df$critic_score_scaled, game.reviews.df$user_score, method = "pearson", conf.level = 0.95)
# Pearson's correlation test indicates that  correlation = 0.53, and is statistically siginificant
# Takeaway User and Critic Scores are moderately correlated, with scores generally following the same pattern


# How have number of user provided reviews proceeded over time? 
user.reviews.freq <- game.reviews.df %>%
  group_by(., release_year) %>% 
  summarise(., total_num_user_reviews = sum(num_user_reviews)) %>% 
  ggplot(., aes(x = release_year, y = total_num_user_reviews))+
  #geom_histogram(stat = "identity")
  geom_col()
user.reviews.freq

# Over time, the number of user reviews has risen sharply (to be expected)
# User reviews also dwarf critic reviews (400K user reviews in 2020 compared to 20K critic reviews)

critic.reviews.freq <- game.reviews.df %>%
  group_by(., release_year) %>% 
  summarise(., total_num_critic_reviews = sum(num_critic_reviews)) %>% 
  ggplot(., aes(x = release_year, y = total_num_critic_reviews))+
  #geom_histogram(stat = "identity")
  geom_col()
critic.reviews.freq


# Looking at frequency of user and critic score by platform
  # need to do some grouping, check out example below  from "DataViz from Scratch

game.reviews.df %>% 
  group_by(., platform) %>% 
  summarise(avg_critic_score = mean(critic_score_scaled)) %>% 
  ggplot(., aes(x = platform, y= avg_critic_score, fill = platform)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = mean(game.reviews.df$critic_score_scaled), linetype = "dashed", color = "purple", size = 1.1)

game.reviews.df %>% 
  group_by(., platform) %>% 
  summarise(avg_user_score = mean(user_score)) %>%
  ggplot(., aes(x = platform ,y= avg_user_score, fill = platform)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = mean(game.reviews.df$user_score), linetype = "dashed", color = "purple", size = 1.1)



# Filter platforms: select the platforms with the top 10 highest number of games for easier analysis

  # Table showing number of games in each platform
num.reviews.platform <- game.reviews.df %>% 
  group_by(., platform) %>% 
  summarise(num_reviews = n()) %>% 
  arrange(., desc(num_reviews))
print(num.reviews.platform)

# Slice the Top 10 platforms that have the most reviews
platforms.most.reviews <- num.reviews.platform %>% 
  slice_max(order_by = num_reviews, n = 10)
platforms.most.reviews

# Examine average critic and user scores for platforms with the most game reviews ####
game.reviews.df %>% 
  group_by(., platform) %>% 
  filter(., platform %in% platforms.most.reviews$platform) %>% 
  summarise(avg_user_score = mean(user_score)) %>%
  ggplot(., aes(x = platform ,y= avg_user_score, fill = platform)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = mean(game.reviews.df$user_score), linetype = "dashed", color = "purple", size = 1.1)

# Platforms with above average user scores: PS2, Nintendo DS,Switch, Wii, Xbox

game.reviews.df %>% 
  group_by(., platform) %>% 
  filter(., platform %in% platforms.most.reviews$platform) %>% 
  summarise(avg_critic_score = mean(critic_score_scaled)) %>% 
  ggplot(., aes(x = platform, y= avg_critic_score, fill = platform)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = mean(game.reviews.df$critic_score_scaled), linetype = "dashed", color = "purple", size = 1.1)

# Platforms with above average critic scores: PC, PS4, Nintendo Switch, Xbox, Xbox One


  
  
# cuisine_closures_byBoro <- inspections_top.cuisines %>%
#   group_by(., cuisine, boro) %>%
#   summarise(
#     num_of_restaurants = n(),
#     closures = sum(action == "closed" | action == "reclosed"),
#     closure_ratio = closures / num_of_restaurants
#   )
# 
# ggplot(data = cuisine_closures_byBoro, aes(x = cuisine, y = closure_ratio))+
#   geom_bar(stat = "identity", aes(fill = boro)) +
#   facet_grid(.~boro) +
#   labs(title = 'Inspection closure ratio by top 20 cuisine and borough', x =
#          'Top 20 cuisine', y = 'Inspection closure ratio') +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() +
#   theme(legend.key = element_blank(), legend.position = "bottom") +
#   coord_flip()



game.reviews.df %>% 
  ggplot(data = ., aes(x = critic_score_scaled)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ platform)
  
genres.long %>% 
  count(., genre.value == "Fighting")




# Check the maximum no. of genres that a single game in the dataset has
max(lengths(strsplit(game.reviews.df$genres, ','))) # 12 genres

# Use 'str_split_fixed' function to split the characters in the genres column into individual columns
  # NOTE: since there are duplicate genres, let's plan to remove dupes after making the df longer
game.reviews.genres <- str_split_fixed(game.reviews.df$genres, ",", 12)

genres.cols <- cbind(game.reviews.df, game.reviews.genres)

# gather the dataframe and make it long
genres.long <- pivot_longer(genres.cols, 
                            cols = "1":"12", names_to = "genre.ID", values_to = "genre.value")

# Each title's genres are captured in a separate row. 
# Remove any rows that have an empty genre.value and/or a duplicated value
genres.long <- genres.long %>% 
  filter(., !genre.value == "")


reviews.genres.long <- genres.long %>% 
  distinct(., title, platform, genre.value, .keep_all = TRUE)

# Create a dataframe showing the number of genres that each title is associated with
count.genres.per.game <- genres.long %>% 
  filter(., !genre.value == " ") %>% 
  group_by(., platform, title) %>%
  summarize(num_genres = n())

count.genres.per.game


# Table showing genre reviews
all.genre.reviews <- genres.long %>% 
  group_by(., genre.value) %>% 
  summarise(., genre.frequency = n(),
            genre.pct = ( genre.frequency/ nrow(genres.long) *100),
            avg.critic.review = mean(critic_score_scaled),
            avg.user.review = mean(user_score))

all.genre.reviews

# Table showing Genres with the highest Critic Reviews (minimum = 1000 games)
all.genre.reviews %>% 
  filter(., genre.frequency > 1000) %>% 
  arrange(., desc(avg.critic.review))

# Table showing Genres with the highest Critic Reviews (minimum = 100 games)
all.genre.reviews %>% 
  filter(., genre.frequency > 100) %>% 
  arrange(., desc(avg.critic.review))

# Table showing Genres with the highest User Reviews (minimum = 1000 games)
all.genre.reviews %>% 
  filter(., genre.frequency > 1000) %>% 
  arrange(., desc(avg.user.review))

# Table showing Genres with the highest User Reviews (minimum = 100 games)
all.genre.reviews %>% 
  filter(., genre.frequency > 100) %>% 
  arrange(., desc(avg.user.review))


user.critic.diff <- reviews.genres.long


# The genres columns lists multiple genres. 
# Stackoverflow tips: https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns

# These genres are not as comprehensive -- will stick with original values
# main.genres <- c("Arcade", "Action", "Action Adventure", "Adventure", "Fighting Games", "Shooter",
#                  "Flight/Flying", "Party", "Platformer", "Puzzle", "Racing", "Sci-Fi", "Fantasy",
#                  "Real-Time Strategy", "Role-Playing", "Simulation", "Sports", "Strategy", 
#                  "Turn-Based Strategy", "Wargames", "Wrestling")


# # Table showing reviews for "main" genres
# main.genre.reviews <- genres.long %>% 
#   filter(., genre.value %in% main.genres) %>% 
#   group_by(., genre.value) %>% 
#   summarise(., genre.frequency = n(),
#             genre.pct = ( genre.frequency/nrow(genres.long) *100),
#             avg.critic.review = mean(critic_score_scaled),
#             avg.user.review = mean(user_score)) # %>% 
# 
# # Table showing Main Genres with the highest Critic Reviews
# main.genre.reviews%>% 
#   filter(., genre.frequency > 1000) %>% 
#   arrange(., desc(avg.critic.review))
# 
# # Table showing Main Genres with the highest User Reviews
# main.genre.reviews%>% 
#   filter(., genre.frequency > 1000) %>% 
#   arrange(., desc(avg.user.review))

# Platform Reviews ####
  # Critic Reviews
platform.reviews <- game.reviews.df %>% 
  group_by(., platform) %>% 
  summarise(., avg.annual.critic.score = mean(critic_score_scaled),
            avg.annual.user.score = mean(user_score)) %>% 
  pivot_longer(., cols = !platform, names_to = "review_type", values_to = "review_score")

platform.critic.reviews.chart <- platform.reviews %>%
  filter(., review_type == "avg.annual.critic.score") %>% 
  ggplot(., aes(x = platform, y =review_score, fill = platform))+
  geom_bar(stat = "identity") +
  labs(title = "Game Review Scores by Platform,\n Critic Scores: 1995 - 2020",
       x= "Platform", y = "Average Critic Review Score") +
  scale_color_hue(labels = "Platform") +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  geom_hline(yintercept = mean(game.reviews.df$critic_score_scaled, 
                                linetype = "dashed", color = "purple", size = 1.1))

platform.critic.reviews.chart

  # User Reviews
platform.user.reviews.chart <- platform.reviews %>%
  filter(., review_type == "avg.annual.user.score") %>% 
  ggplot(., aes(x = platform, y =review_score, fill = platform))+
  geom_bar(stat = "identity") +
  labs(title = "Game Review Scores by Platform,\n User Scores: 1995 - 2020",
       x= "Platform", y = "Average User Review Score") +
  scale_color_hue(labels = "Platform") +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")) +
  geom_hline(yintercept = mean(game.reviews.df$user_score, 
                               linetype = "dashed", color = "purple", size = 1.1))

platform.user.reviews.chart

# Title Reviews by Year ####
title.reviews.annual <- game.reviews.df %>% 
  group_by(.,release_year) %>% 
  summarise(., avg.annual.critic.score = mean(critic_score_scaled),
            avg.annual.user.score = mean(user_score))

title.reviews.annual.chart <- title.reviews.annual %>%
  pivot_longer(., cols = !release_year, names_to = 'review_type', values_to = "review_score") %>% 
  ggplot(., aes(x = release_year, y = review_score, color = review_type))+
  geom_line() +
  labs(title = "Metacritic Videogame Review Scores by Release Year\n Users vs Critics: 1995 - 2020",
       x= "Release Year", y = "Review Score",
       color = "Review Type") +
  scale_color_hue(labels = c("Average Critic Score", "Average User Score")) +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

title.reviews.annual.chart



# Title Reviews by Decade ####
title.reviews.decade <- game.reviews.df %>% 
  mutate(., release_decade = case_when(
    release_year %in% c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999) ~ "1990s",
    release_year %in% c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009) ~ "2000s",
    release_year %in% c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019) ~ "2010s",
    TRUE ~ "2020s")) %>% 
  group_by(., release_decade) %>% 
  summarise(., avg.annual.critic.score = mean(critic_score_scaled),
            avg.annual.user.score = mean(user_score)) %>% 
  pivot_longer(., cols = !release_decade, names_to = 'review_type', values_to = "review_score") 


title.reviews.decade.chart <- title.reviews.decade %>%
  ggplot(., aes(x = release_decade, y = review_score, color = review_type, group = review_type))+
  geom_line() +
  labs(title = "Metacritic Videogame Review Scores by Release Decade\n Users vs Critics",
       x= "Decade", y = "Average Review Score",
       color = "Review Type") +
  scale_color_hue(labels = c("Average Critic Score", "Average User Score")) +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

title.reviews.decade.chart

# On average, user review scores have gone down over time.
# The 90s have the highest reviews by users and critics. 
# The average review scores dropped by almost 1- 1.5pts in the 2000s.
# By the 2010s, we start seeing more divergence between critics and users, with users being more critical.


# Next Steps #### 
# Chart avg user & critic reviews by 1) decade, 2) by platform, 3) genres
# correlation of user/critic reviews
# Top Games

# Top Reviewed Games ####

  # Which console had the most games in the Top 100?
highest.reviewed.games  <- game.reviews.df %>%
  slice_min(., n=100, order_by = rank) %>% 
  select(., c(rank, title, critic_score_scaled, platform, release_year))
highest.reviewed.games   

# Chart showing top games
ggplot(data = worst.reviewed.games, aes(x = platform, fill = platform)) +
  geom_bar(position = "dodge") +
  labs(title = " Number of Games Ranked in the Top 100, by platform") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))


  # Which console had the most games in the Bottom 100?
worst.reviewed.games  <- game.reviews.df %>%
  slice_max(., n=100, order_by = rank) %>% 
  select(., c(rank, title, critic_score_scaled, platform, release_year))
worst.reviewed.games  

# chart showing bottom games
ggplot(data = highest.reviewed.games, aes(x = platform, fill = platform)) +
  geom_bar(position = "dodge") +
  labs(title = " Number of Games Ranked in the Bottom 100, by platform") +
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1))

