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



