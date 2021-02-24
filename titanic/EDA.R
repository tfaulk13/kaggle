# Libraries and Data -----
library(tidyverse)

train <- read_csv('titanic/data/train.csv')
#test <- read_csv('titanic/data/test.csv')

# Cleaning -----

# Looking at nulls
lapply(train, function (x) sum(is.na(x))) # Age, Cabin, Embarked

# Age

# First thing I noticed is people missing age are disproportionately Pclass = 3
# How do Pclass = 3 Age compare to Pclass = 1 or 2?

train %>%
  filter(!is.na(Age)) %>%
  group_by(Pclass) %>%
  summarize(count = n(),
            mean_surv = mean(Survived),
            mean_age = mean(Age),
            sd_age = sd(Age))

# Means look pretty different but also have decent variance
# Run ANOVA to check for statistical significance
fit <- aov(Age ~ Pclass, data = train)
summary(fit)

# Very significant. As such, going to provide mean by group to fill in missing values
train <- train %>%
  group_by(Pclass) %>%
  mutate(Age = if_else(is.na(Age), mean(Age, na.rm = T), Age)) %>%
  ungroup()

# Cabin
train %>% count(Cabin) # Maybe the level means something or perhaps is just a proxy for class

# Looking at cabin levels by class
train$cabin_level <- str_extract(train$Cabin, '[A-Z]')

train %>% 
  group_by(Pclass, cabin_level) %>% 
  summarize(count = n(),
            mean_surv = mean(Survived),
            mean_age = mean(Age),
            sd_age = sd(Age))

# A few thing to note: 
# Not many missing but most missing Pclass 2 and 3
# Having a cabin number appears to have little impact for Pclass 1
# It does for Pclass 2 and 3 but v small sample size

# Correlation: cabin vs non-cabin
train$has_cabin <- if_else(is.na(train$cabin_level), 0, 1)
cor(train$has_cabin, train$Survived)

# Not strongly correlated at all...going to leave null for now but may revisit 
# during feature engineering

# Embarked -- only two people staying in same cabin...not going to worry
# about them for now

# Feature Engineering -----

