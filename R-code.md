# R code for module activity

## 1- Install and load the main packages

#### Install packages

If any of the below packages is not installed on your computer, please install it. Remember to delete the **#** symbol before running the code.

#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggrepel")
#install.packages("patchwork")
#install.packages("gridExtra")

#### Load packages

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)
library(gridExtra)
library(haven)

## 2- Load the dataset and make a copy

publicmicrodatateachingsample <- read_sav("data/publicmicrodatateachingsample.sav")

census2021teaching <- publicmicrodatateachingsample


Important: Remember to change to pathway to your dataset accordingly. 

If the **read_sav** code does not work, you can use the **Import Dataset** button in the Environment pane to load the dataset in R.

## 3- Drop unnecessary variables

census2021teaching <- census2021teaching[,c("health_in_general",
                                            "hours_per_week_worked",
                                            "resident_age_7d","sex", 
                                            "ethnic_group_tb_6a",
                                            "approx_social_grade")]






## 4- Exploratory analysis

### 4.1- Univariate analysis 

#### 4.1.1 Univariate analysis for health in general and hours worked

##### Frequencies for health in general and hours worked

table(census2021teaching$health_in_general)

table(census2021teaching$hours_per_week_worked)

##### Percentages for health in general and hours worked

prop.table(table(census2021teaching$health_in_general)) * 100

prop.table(table(census2021teaching$hours_per_week_worked)) * 100

You can also calculate these frequencies and percentages with appropriate labels.

##### Frequencies and percentages with labels for health in general and hours worked

census2021teaching %>%
  mutate(
    health_code = as.integer(as.character(health_in_general)),
    health_label = factor(
      health_code,
      levels = c(1, 2, 3, 4, 5, -8),
      labels = c("Very good", "Good", "Fair", "Bad", "Very bad", "Does not apply")
    )
  ) %>%
  count(health_label) %>%
  mutate(percent = round(100 * n / sum(n), 1))


census2021teaching %>%
  mutate(
    hours_code = as.integer(as.character(hours_per_week_worked)),
    hours_label = factor(
      hours_code,
      levels = c(1, 2, 3, 4, -8),
      labels = c("[0 – 15]", "[16 – 30]", "[31 – 48]", "[49 and +]", "Does not apply")
    )
  ) %>%
  count(hours_label) %>%
  mutate(percent = round(100 * n / sum(n), 1))

You can also generate pie charts for better visualization

### a) Pie chart for Health in general

##### a.1) Create a labelled health variable

health <- census2021teaching %>%
  mutate(
    health_code = as.integer(as.character(health_in_general)),
    health_label = factor(
      health_code,
      levels = c(1,2,3,4,5,-8),
      labels = c("Very good", "Good", "Fair", "Bad", "Very bad", "Not applicable")
    )
  ) %>%
  filter(!is.na(health_label)) %>%
  count(health_label, name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 1),
    label = paste0(health_label, " (", pct, "%)")
  )

##### a.2) Plot pie chart

p1 <- ggplot(health, aes(x = "", y = n, fill = health_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label_repel(aes(label = label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,
                   size = 3) +
  labs(title = "Distribution of general health", fill = "Health status") +
  theme_void()+
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(size = 16, hjust = 1))

p1

### b) Pie chart for Hours worked

##### b.1) Create a labelled hours worked variable

hours <- census2021teaching %>%
  mutate(
    hours_code = as.integer(as.character(hours_per_week_worked)),
    hours_label = factor(
      hours_code,
      levels = c(1, 2, 3, 4, -8),
      labels = c("[0 – 15]", "[16 – 30]", "[31 – 48]", "[49 and +]", "Not applicable")
    )
  ) %>%
  filter(!is.na(hours_label)) %>%
  count(hours_label, name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 1),
    label = paste0(hours_label, " (", pct, "%)")
  )

##### b.2) Plot pie chart

p2 <- ggplot(hours, aes(x = "", y = n, fill = hours_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label_repel(aes(label = label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,
                   size = 3) +
  labs(title = "Distribution of worked hours", fill = "Hours worked") +
  theme_void()+
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(size = 16, hjust = 1))

p2

### 4.1.1.1 Transformation of variable Health in general

Create a new variable called **health_binary** by regrouping health in general in two broader categories: **Good or very good health** and **Poor health**

census2021teaching$health_binary <- ifelse(
  census2021teaching$health_in_general %in% c(1, 2), "Good or very good health",
  ifelse(census2021teaching$health_in_general %in% c(3, 4, 5), "Poor health", NA)
)

##### Drop rows where health_in_general = -8 (Does not apply)

census2021teaching <- subset(census2021teaching, health_in_general != -8)

##### Check the new variable's distribution

table(census2021teaching$health_binary)

#####  Create variable **Poor_health** and check its distribution

Code a binary variable that takes 1 for poor health and 0 otherwise. Call this new variable **Poor_health**. This variable will be the explained variable of our regression model.

census2021teaching$Poor_health <- ifelse(
  census2021teaching$health_binary == "Poor health", 1,
  ifelse(census2021teaching$health_binary == "Good or very good health", 0, NA)
)

table(census2021teaching$Poor_health)

### 4.1.2 Univariate analysis for age, sex, ethnicity, social category 

#### Frequencies and percentages for age, sex, ethnicity, social category 

table(census2021teaching$resident_age_7d)
table(census2021teaching$sex)
table(census2021teaching$ethnic_group_tb_6a)
table(census2021teaching$approx_social_grade)

prop.table(table(census2021teaching$resident_age_7d)) * 100
prop.table(table(census2021teaching$sex)) * 100
prop.table(table(census2021teaching$ethnic_group_tb_6a)) * 100
prop.table(table(census2021teaching$approx_social_grade)) * 100

#### Plot pie chart Age variable

##### a) Create a labelled Age variable

Age <- census2021teaching %>%
  mutate(
    Age_code = as.integer(as.character(resident_age_7d)),
    Age_label = factor(
      Age_code,
      levels = c(1, 2, 3, 4,5,6,7, -8),
      labels = c("]0 – 15]", "[16 – 24]", "[25 – 34]", "[35 - 44]", "[45 - 54]", "[55 - 64]", "[65 and +]","Not applicable")
    )
  ) %>%
  filter(!is.na(Age_label)) %>%
  count(Age_label, name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 1),
    label = paste0(Age_label, " (", pct, "%)")
  )

##### b) Plot pie chart

ggplot(Age, aes(x = "", y = n, fill = Age_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label_repel(aes(label = label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,
                   size = 3) +
  labs(title = "Distribution of respondents by Age", fill = "Resident age") +
  theme_void()+
  scale_fill_brewer(palette = "Pastel1") +
  theme(plot.title = element_text(size = 16, hjust = 1))

#### Histogram for Sex variable

##### a) Recode sex variable with labels

sex <- census2021teaching %>%
  mutate(sex_label = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))) %>%
  count(sex_label, name = "n") %>%
  mutate(prop = n / sum(n),
         label = scales::percent(prop, accuracy = 0.1))

##### b) Plot histogram

ggplot(sex, aes(x = sex_label, y = prop)) +
  geom_col(fill = "skyblue", color = "black", width = 0.3) +
  geom_text(aes(label = label), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(expand = expansion(mult = c(0.5, 0.7))) +
  labs(title = "Distribution of respondents by sex", x = "", y = "Percentage") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 12))+
  theme(plot.title = element_text(size = 16, hjust = 0.5))

#### Histogram for Ethnicity variable

##### a) Recode Ethnicity variable with labels

Ethnicity <- census2021teaching %>%
  mutate(Ethnicity_label = factor(ethnic_group_tb_6a, levels = c(1, 2, 3, 4, 5, -8), 
                                  labels = c("Asian", "Black", "Mixed", "White", "Other", "Does not apply"))) %>%
  count(Ethnicity_label, name = "n") %>%
  mutate(prop = n / sum(n),
         label = scales::percent(prop, accuracy = 0.1))

##### b) Plot histogram

ggplot(Ethnicity, aes(x = Ethnicity_label, y = prop)) +
  geom_col(fill = "chocolate", color = "black", width = 0.3) +
  geom_text(aes(label = label), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution of respondents by Ethnicity", x = "", y = "Percentage") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 12))+
  theme(plot.title = element_text(size = 16, hjust = 0.5))

#### Pie chart for variable social class

##### a) Create a labelled social class variable

Social_class <- census2021teaching %>%
  mutate(
    Social_class_code = as.integer(as.character(approx_social_grade)),
    Social_class_label = factor(
      Social_class_code,
      levels = c(1, 2, 3, 4, -8),
      labels = c("Higher, intermediate managers", "Supervisory, junior managers", 
                 "Skilled manual", "Semi-skilled, unskilled manual","Not applicable")
    )
  ) %>%
  filter(!is.na(Social_class_label)) %>%
  count(Social_class_label, name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 1),
    label = paste0(Social_class_label, " (", pct, "%)")
  )

##### b) Plot pie chart

ggplot(Social_class, aes(x = "", y = n, fill = Social_class_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label_repel(aes(label = label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE,
                   size = 3) +
  labs(title = "Distribution of respondents by Social class", fill = "Social class") +
  theme_void()+
  scale_fill_brewer(palette = "Pastel2") +
  theme(plot.title = element_text(size = 16, hjust = 1))

##### Drop under 16 from the table

census2021teaching <- subset(census2021teaching, resident_age_7d != 1)

### 4.2- Bivariate analysis 

##### Cross tabulation of Poor_health by hours worked

tabulation <- table(census2021teaching$Poor_health, census2021teaching$hours_per_week_worked)
tabulation

##### Row percentages

prop.table(tabulation, margin = 1) * 100

##### Column percentages

prop.table(tabulation, margin = 2) * 100

#### Chi square test

##### a) Creating cross tabulations to be tested

tabulation.health_hours <- table(census2021teaching$Poor_health, census2021teaching$hours_per_week_worked)
tabulation.health_age <- table(census2021teaching$Poor_health, census2021teaching$resident_age_7d)
tabulation.health_sex <- table(census2021teaching$Poor_health, census2021teaching$sex)
tabulation.health_ethnicity <- table(census2021teaching$Poor_health, census2021teaching$ethnic_group_tb_6a)
tabulation.health_classes <- table(census2021teaching$Poor_health, census2021teaching$approx_social_grade)

##### b) chi square test on health and our 5 other variables

chisq.test(tabulation.health_hours)
chisq.test(tabulation.health_age)
chisq.test(tabulation.health_sex)
chisq.test(tabulation.health_ethnicity)
chisq.test(tabulation.health_classes)

## 5 Regression model

### 5.1 Recode variables as factors with labels

census2021teaching$hours_per_week_worked <- factor(
  census2021teaching$hours_per_week_worked,
  levels = c(1, 2, 3, 4, -8),
  labels = c("0-15", "16-30", "31-48", "49+", "Does not apply")
)

census2021teaching$resident_age_7d <- factor(
  census2021teaching$resident_age_7d,
  levels = c(1, 2, 3, 4, 5, 6, 7, -8),
  labels = c("0-15", "16-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not applicable")
)

census2021teaching$ethnic_group_tb_6a <- factor(
  census2021teaching$ethnic_group_tb_6a,
  levels = c(1, 2, 3, 4, 5, -8),
  labels = c("Asian", "Black", "Mixed", "White", "Other", "Does not apply")
)

census2021teaching$approx_social_grade <- factor(
  census2021teaching$approx_social_grade,
  levels = c(1, 2, 3, 4, -8),
  labels = c("Higher, intermediate managers",
             "Supervisory, junior managers",
             "Skilled manual",
             "Semi-skilled, unskilled manual",
             "Not applicable")
)

census2021teaching$sex <- factor(
  census2021teaching$sex,
  levels = c(1, 2),
  labels = c("Male", "Female")
)

### 5.2 Pick references that are central or policy relevant

census2021teaching$hours_per_week_worked <- relevel(census2021teaching$hours_per_week_worked, ref = "31-48")
census2021teaching$resident_age_7d         <- relevel(census2021teaching$resident_age_7d,         ref = "45-54")
census2021teaching$sex                  <- relevel(census2021teaching$sex,                  ref = "Male")
census2021teaching$ethnic_group_tb_6a            <- relevel(census2021teaching$ethnic_group_tb_6a,            ref = "White")
census2021teaching$approx_social_grade         <- relevel(census2021teaching$approx_social_grade,         ref = "Higher, intermediate managers")


### 5.3 Fit logistic regression 

model_health <- glm(
  Poor_health ~ hours_per_week_worked + resident_age_7d + sex + ethnic_group_tb_6a + approx_social_grade,
  data = census2021teaching,
  family = binomial(link = "logit")
)

#### Obtain summary results of the logistic regression

summary(model_health)




