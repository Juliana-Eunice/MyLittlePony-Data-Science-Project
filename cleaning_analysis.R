library(readr)
library(tidyverse)

# Importing the dataset
mlp_franchise_market_survey <- read_csv("mlp_franchise_market_survey.csv")
View(mlp_franchise_market_survey)

# Assign to DATA where DATA is the cleaned data frame
DATA <- mlp_franchise_market_survey
attach(DATA)

# Get a glimpse of the data
glimpse(DATA)
summary(DATA)
unique(DATA$Gender)
unique(DATA$Favorite_Pony)
unique(DATA$Fan_Segment)
unique(DATA$Country)

# Fix data types and levels
DATA$Age <- as.integer(DATA$Age)
class(DATA$Age)

DATA$Gender <- as.factor(DATA$Gender)
DATA <- DATA %>% 
  mutate(Gender = fct_recode(Gender,
                             "Male" = "M",
                             "Male" = "male",
                             "Female" = "F",
                             "Female" = "female",
                             "Other" = "NB",
                             "Other" = "Non-binary",
                             "Other" = "Prefer not to say",
                             "Other" = "Unknown"))

DATA$Favorite_Pony <- as.factor(DATA$Favorite_Pony)
DATA <- DATA %>% 
  mutate(Favorite_Pony = fct_recode(Favorite_Pony,
                                    "Derpy Hooves" = "Derpy",
                                    "Derpy Hooves" = "Muffin Pony",
                                    "Pinkie Pie" = "pinkie pie",
                                    "Pinkie Pie" = "Pinky Pie",
                                    "Sunset Shimmer" = "Sunset",
                                    "Fluttershy" = "Flutershy",
                                    "Fluttershy" = "fluttershy",
                                    "Rarity" = "Rarityy",
                                    "Rarity" = "rarity",
                                    "Twilight Sparkle" = "Twilight",
                                    "Twilight Sparkle" = "Twiligh Sparkle",
                                    "Starlight Glimmer" = "Starlight",
                                    "Applejack" = "Apple Jack",
                                    "Applejack" = "applejack",
                                    "Rainbow Dash" = "rainbow dash",
                                    "Rainbow Dash" = "Rainbow",
                                    "Rainbow Dash" = "Rainbox Dash"))

DATA$Fan_Segment <- as.factor(DATA$Fan_Segment)
DATA$Fan_Segment <- factor((DATA$Fan_Segment),
                           levels = c("General Public",
                                      "Casual Fan",
                                      "Child/Parent Collection",
                                      "Brony"),
                           ordered = TRUE)
levels(DATA$Fan_Segment)

DATA$Country <- as.factor(DATA$Country)
DATA <- DATA %>% 
  mutate(Country = fct_recode(Country,
                              "United States of America" = "USA",
                              "United States of America" = "United States",
                              "United Kingdom" = "UK"))
levels(DATA$Country) <- c(levels(DATA$Country), "Other")
summary(DATA)

# Fix invalid and missing values
DATA %>% filter(!complete.cases(DATA))

DATA <- DATA %>% 
  mutate(Gender = replace_na(Gender, "Other")) %>%
  mutate(Country = replace_na(Country, "Other")) %>% 
  filter(Age > 0 & Age < 100) %>% 
  drop_na(Favorite_Pony, Respondent_ID)

DATA <- DATA %>% 
  group_by(Fan_Segment) %>% 
  mutate(Annual_Spend_USD = coalesce(
    Annual_Spend_USD,
    mean(Annual_Spend_USD, na.rm = TRUE)
  )) %>% 
  ungroup()

# Find duplicates
anyDuplicated(DATA)
DATA[duplicated(DATA),] 

# Remove duplicates
DATA <- DATA %>% distinct()

# View the cleaned data frame
summary(DATA)
View(DATA)
