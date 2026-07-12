library(readr)
library(tidyverse)
library(scales)
library(vcd)

# Importing the dataset
mlp_franchise_market_survey <- read_csv("mlp_franchise_market_survey.csv")
View(mlp_franchise_market_survey)


#---- DATA CLEANING ----

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

# Find duplicates
anyDuplicated(DATA)
DATA[duplicated(DATA),] 

# Remove duplicates
DATA <- DATA %>% distinct()

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

# View the cleaned data frame
summary(DATA)
View(DATA)

# Save the cleaned data as a new csv file
write.csv(DATA, "cleaned_mlp_franchise_market_survey.csv", row.names = FALSE)


#---- DATA ANALYSIS AND COMPUTATIONS ----

# New data frame for gender count and frequency
Gender_Data <- DATA %>% 
  count(Gender) %>% 
  rename(Count = n) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  arrange(desc(Percentage))
glimpse(Gender_Data)

# New data frame for favorite pony count and percentage
Pony_Data <- DATA %>% 
  count(Favorite_Pony) %>% 
  rename(Count = n) %>% 
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  arrange(desc(Percentage))
glimpse(Pony_Data)

# New data frame for having an age category column
Age_Cat <- DATA %>% 
  mutate(Age_Category = case_when(
    Age <= 12 ~ "Child",
    Age >= 13 & Age <= 19 ~ "Teenager",
    Age >= 20 & Age <= 30 ~ "Young Adult",
    Age > 30 ~ "Adult"
  ))

# Make the values to factor
Age_Cat$Age_Category <- factor(Age_Cat$Age_Category,
                               levels = c("Child", "Teenager", "Young Adult", "Adult"),
                               ordered = TRUE)
levels(Age_Cat$Age_Category)

# Check if pony preference is independent of gender, age, and country
# Gender
chisq_gender <- chisq.test(Age_Cat$Favorite_Pony, Age_Cat$Gender)
chisq_gender
assocstats(table(Age_Cat$Favorite_Pony, Age_Cat$Gender))

# Age Category
chisq_age <- chisq.test(Age_Cat$Favorite_Pony, Age_Cat$Age_Category)
chisq_age
assocstats(table(Age_Cat$Favorite_Pony, Age_Cat$Age_Category))

# Country
chisq_country <- chisq.test(Age_Cat$Favorite_Pony, Age_Cat$Country)
chisq_country
assocstats(table(Age_Cat$Favorite_Pony, Age_Cat$Country))

# New data frame for financial info by fan segment
Financial_Summary <- DATA %>% 
  group_by(Fan_Segment) %>% 
  summarize(
    Total_Count = n(),
    Average_Spend = mean(Annual_Spend_USD),
    Median_Spend = median(Annual_Spend_USD),
    Standard_Dev = sd(Annual_Spend_USD)
  )

# ANOVA for financial info across segments
Spending_ANOVA <- aov(Annual_Spend_USD ~ Fan_Segment, data = DATA)
summary(Spending_ANOVA)

# Testing difference using Tukey's Honest Significant Difference
TukeyHSD(Spending_ANOVA)


#---- DATA VISUALIZATION ----