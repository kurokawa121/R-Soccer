# Link to dataset:https://www.kaggle.com/datasets/stefanoleone992/fifa-20-complete-player-dataset?select=players_20.csv
# Note: Project only uses player_20.csv data

library(tidyverse)
df <- read.csv("/Users/michael/Documents/R projects/players_20.csv")
View(df)

#Creating subsets of df

#Section 1: Visualization of Height vs. Weight among all players

#Physical attributes
player_attributes <- df[, c(1,3,5,7,8)]
View(player_attributes)

#summary stats of physical attributes
summary(player_attributes)

#Visualization of height vs weight
ggplot(player_attributes, aes(weight_kg, height_cm)) + 
  geom_point(color = "blue") + 
  geom_smooth(color = "black") + 
  theme_bw() +
  labs(title = "Relationship between Height vs. Weight",
       subtitle = "Relationship Among Soccer Players",
       x = "Weight in Kilograms",
       y = "Height in Centimeters"
  )
# ------------------------------------------------------------------------------

# Section 2: Height vs Weight of 10 different clubs

#Selecting certain columns from original set 
player_clubs <- df[, c(1,3,5,7,8,10)]
View(player_clubs)

#Creating a dataframe for the frequency of players in each club 
club_counts <- data.frame(table(player_clubs$club))
View(club_counts)

#Filtering data set based on 10 random clubs
subset_player_clubs <- player_clubs %>% 
  select(c(2,4,5,6)) %>% 
  filter(club == "Aalborg BK" |
           club == "AD Alcorcón" |
           club == "Carlisle United" |
           club == "CD Lugo" |
           club == "DC United" | 
           club == "Defensa y Justicia" |
           club == "Feyenoord" | 
           club == "Gillingham" |
           club == "Livorno" |
           club == "LOSC Lille")

view(subset_player_clubs)

#Visualization of Height vs. Weight of the subset data
ggplot(subset_player_clubs) + 
  geom_point(aes(weight_kg, height_cm, color = club)) +
  facet_wrap(~club, nrow = 2) +
  geom_smooth(aes(weight_kg, height_cm),method = "lm", color = "black", alpha = 0.3) +
  theme_bw() +
  labs(title = "Relationship between Height vs. Weight",
       subtitle = "Among Ten Different Clubs",
       x = "Weight in Kilograms",
       y = "Height in Centimeters")


#Summary Statistics on weight for each club
group_by(subset_player_clubs, club) %>% 
  summarize(
    mean = mean(weight_kg),
    sd = sd(weight_kg),
    median = median(weight_kg),
    IQR = IQR(weight_kg)
  )
# ------------------------------------------------------------------------------

# Section 3: Visualization of Weight from subset data

#Visualization with box and whisker plot

ggplot(subset_player_clubs, aes(weight_kg, club)) + 
  geom_boxplot(aes(color = club, fill = club), alpha = 0.6) +
  labs(title = "Weight Difference",
       subtitle = "Among Ten Different Clubs",
       x = "Weight in Kilograms",
       y = "Clubs") +
  theme_bw()

#Statistical Testing with Krusal Wallis test

kruskal.test(weight_kg~club, subset_player_clubs) 
#Based off test results, there is a significant difference in weight among the 10 clubs

# ------------------------------------------------------------------------------

#Section 4: Highest average speed and acceleration among clubs

#Create subset from df set
average_speed_accel <- df[c(3,10,55,56)]

#Finding highest average speed and acceleration for each club

average_per_club <- average_speed_accel %>%
  group_by(club) %>% 
  summarise(
    average_speed = mean(movement_sprint_speed),
    average_acceleration = mean(movement_acceleration)
  ) %>% 
  arrange(desc(average_speed), desc(average_acceleration))
print(average_per_club)

#Subset containing top 10 highest averages 
top_10 <- top_n(average_per_club,10)
print(top_10)
View(top_10)

#Subset containing bottom 10 lowest average
bottom_10 <- average_per_club[689:698,]
print(bottom_10)
View(bottom_10)                  



