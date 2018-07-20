## Mckinsey analytics hackathon
## Date: 20 July 2018
## Author: Vikas Jha

##-------Setting up working directory and list files---------------

setwd("D:/hackathons/mckinsey/")
all_files <- list.files("../mckinsey/input/")

##-------Loading packages------------------------------------------

# general data manipulation
library(data.table) ## data manipulation
library(dplyr) ## data manipulation

# general visualisation
library(ggplot2) # visualisation
library(scales) # visualisation
library(scales) # visualisation
library(corrplot) # visualisation


##-------Load data---------------------------------------------------

d_train <- fread("D:/hackathons/mckinsey/input/train.csv", stringsAsFactors = F, na.strings = c(""," ", "NA"))
d_test <- fread("D:/hackathons/mckinsey/input/test.csv", stringsAsFactors = F, na.strings = c("", " ", "NA"))
sample_sub <- fread("D:/hackathons/mckinsey/input/sample.csv", stringsAsFactors = F)

##-------Overview: File structure and content-------------------------

summary(d_train)
glimpse(d_test)

summary(d_test)
glimpse(d_test)

setdiff(names(d_train), names(d_test))





p1 <- d_train %>% group_by(Count_3_6_months_late) %>% 
  summarise(renewed = sum(renewal), total = n()) %>% 
  mutate(Count_3_6_months_late = as.factor(Count_3_6_months_late), percent_renew = 100*renewed/total) %>%
  melt(value.name = "Count_3_6_months_late") %>%
  ggplot(aes(Count_3_6_months_late, value)) + geom_bar(aes(fill = variable),stat = "identity") + theme(legend.position = "none") +
  labs(x = "Count_3_6_months_late", y = "Total numbers")