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
Age_Cat

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
Financial_Summary

# ANOVA for financial info across segments
Spending_ANOVA <- aov(Annual_Spend_USD ~ Fan_Segment, data = DATA)
summary(Spending_ANOVA)

# Testing difference using Tukey's Honest Significant Difference
TukeyHSD(Spending_ANOVA)

# Creating a demographic/market profile per fan segment (typical fanbase)
Market_Profiles <- Age_Cat %>% 
  group_by(Fan_Segment) %>% 
  summarize(
    Total_Respondent = n(),
    Average_Age = mean(Age),
    Median_Age = median(Age),
    Average_Spending = mean(Annual_Spend_USD),
    Dominant_Gender = names(which.max(table(Gender))),
    Top_Pony_Choice = names(which.max(table(Favorite_Pony)))
  )
Market_Profiles


#---- DATA VISUALIZATION ----

# Detach unclean csv from "DATA"
detach(DATA)

# Read cleaned csv and attach to DATA
DATA <- read_csv("cleaned_mlp_franchise_market_survey.csv")

# Bring back factor levels to fan segment
DATA$Fan_Segment <- factor((DATA$Fan_Segment),
                           levels = c("General Public",
                                      "Casual Fan",
                                      "Child/Parent Collection",
                                      "Brony"),
                           ordered = TRUE)

# Simple bar graph for gender distribution
ggplot(Gender_Data, aes(x = Gender, y = Percentage, fill = Gender)) +
  geom_col(width = 0.6, alpha = 0.85) +
  scale_fill_brewer(palette = "PuRd") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            vjust = -0.5, 
            fontface = "bold", 
            size = 4) +
  theme_minimal() +
  labs(
    title = "Gender Distribution of Survey Respondents",
    x = "Gender Identity",
    y = "Percentage of Total Base (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
    axis.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none" 
  )

# Simple bar graph for age category distribution
ggplot(Age_Cat, aes(x = Age_Category, fill = Age_Category)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100), 
           width = 0.6, alpha = 0.85) +
  scale_fill_brewer(palette = "RdPu") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  stat_count(geom = "text", 
             aes(y = after_stat(count) / sum(after_stat(count)) * 100,
                 label = sprintf("%.1f%%", after_stat(count) / sum(after_stat(count)) * 100)),
             vjust = -0.5, fontface = "bold", size = 4) +
  theme_minimal() +
  labs(
    title = "Age Distribution of Survey Respondents",
    x = "Age Category Group",
    y = "Percentage of Total Base (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
    axis.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    legend.position = "none" 
  )

# Simple horizontal bar graph for country distribution
ggplot(DATA, aes(x = reorder(Country, Country, function(x) length(x)), fill = after_stat(count))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100), 
           width = 0.7, alpha = 0.85) +
  coord_flip() +
  scale_fill_viridis_c(option = "magma", direction = -1, begin = 0.3, end = 0.8) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  stat_count(geom = "text", 
             aes(y = after_stat(count) / sum(after_stat(count)) * 100,
                 label = sprintf("%.1f%%", after_stat(count) / sum(after_stat(count)) * 100)),
             hjust = -0.15, fontface = "bold", size = 4) +
  theme_minimal() +
  labs(
    title = "Geographic Distribution of Survey Respondents",
    x = "Country / Region",
    y = "Percentage of Total Base (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none" 
  )

# Horizontal bar chart for overall character popularity
ggplot(Pony_Data, aes(x = reorder(Favorite_Pony, Percentage), y = Percentage, fill = Percentage)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.85) +
  coord_flip() +
  scale_fill_viridis_c(option = "magma", direction = -1, begin = 0.3, end = 0.8) +
  theme_minimal() +
  labs(
    title = "Character Popularity Distribution Across Collective Fanbase",
    x = "Character",
    y = "Percentage of Total Responses (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(face = "bold"), 
    legend.position = "none" 
  )

# Tiny summary table of your calculated Cramér's V values
Association_Data <- tibble(
  Demographic = c("Age Category", "Gender", "Country"),
  Cramers_V = c(
    assocstats(table(Age_Cat$Favorite_Pony, Age_Cat$Age_Category))$cramer,
    assocstats(table(Age_Cat$Favorite_Pony, Age_Cat$Gender))$cramer,
    assocstats(table(Age_Cat$Favorite_Pony, Age_Cat$Country))$cramer
  )
)

# Cramér's V values against the standard threshold
ggplot(Association_Data, aes(x = Demographic, y = Cramers_V, fill = Demographic)) +
  geom_col(width = 0.4, alpha = 0.85) +
  geom_hline(yintercept = 0.10, linetype = "dashed", color = "firebrick", size = 0.8) +
  annotate("text", x = 1.5, y = 0.115, 
           label = "Threshold for Weak Association (0.10)", 
           color = "firebrick", fontface = "italic", size = 3.5) +
  scale_fill_brewer(palette = "PuRd") +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_minimal() +
  labs(
    title = "Demographic Association Strengths for Character Favorability",
    x = "Demographic Variable",
    y = "Cramér's V (Effect Size)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# Box and whisker plot for merchandise spending per fan segment
ggplot(DATA, aes(x = Fan_Segment, y = Annual_Spend_USD, fill = Fan_Segment)) +
  geom_boxplot(alpha = 0.7, outlier.color = "grey40", outlier.size = 1) +
  scale_fill_brewer(palette = "RdPu") + 
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  labs(
    title = "Annual Merchandise Expenditure Across Fan Segments",
    x = "Fan Segment",
    y = "Annual Spend (USD)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )
