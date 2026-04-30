#Andre King
#Data 306
#Exercise 6: Preparation for the Final, Part 1

# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)


getwd()
setwd("/Users/senorking/Desktop/Data306/data/")#my directory

trees <- read.csv("/Users/senorking/Desktop/Data306/data/2015_tree_census_with_neighb_indicators.csv")

str(trees)
# The data contains	683788 observation of  53 variables
head(trees)


## In this Assignment my goal is to explore the data more deeply finding what impacts the health of trees in nyc and if there is even an impact at all. First i will take a look at how the number of stewardship affects the health of trees in nyc then add more factors in the mix such as crime rate or problems noticed around the trees. 

## My hypothesis is that the more stewardship a tree receives the better the health of the tree will be. 

## After running a few codes and ploting Proportion of Tree Health by Stewardship Level it seems there is a slight decrease in trees with poor health and a slight increase in trees with good health. However, the level of stewardship does not seem to have any real impact on the health of trees at all. This can be seen in the way majority of trees with no stewardship have good health similar to the way trees with 1, 2, 3, 4 or more stewardships.   



# View the levels of categorical variables
addmargins(table(trees$health))
trees$health[trees$health == ""] <- "dead" #replace empty values with dead, rerun previous

addmargins(table(trees$steward))

summary(trees$crime_all_rt)
unique(trees$problems)# this shows all the different unique problems trees have. I am i will try to represent this as numbers since some trees have more than one problems

table(trees$sidewalk)

###
trees$steward[trees$steward == ""] <- "dead"	
trees$steward.factor <- factor(trees$steward,
levels = c("dead", "None", "1or2", "3or4", "4orMore"))

table(trees$steward)

stew.health <- table(trees$steward, trees$health)
addmargins(stew.health)

##first diagram to show Tree Health by Stewardship Level, need futher modification for the y axis as numbers
ggplot(trees, aes(x = steward, fill = health)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(
    title = "Tree Health by Stewardship Level",
    x = "Stewardship Level",
    y = "Number of Trees",
    fill = "Health"
  )
  
trees_clean <- trees[trees$status == "Alive", c("tree_dbh", "health", "steward")]
trees_clean <- na.omit(trees_clean)


# Filter out dead or stump trees
trees_filtered <- trees %>%
  filter(!health %in% c("Dead", "Stump")) %>%
  filter(!is.na(health) & !is.na(steward))
  
 ## This is the proportions bar chart. it show the tree heath as a percent out of 100. 
ggplot(trees_filtered, aes(x = steward, fill = health)) +
  geom_bar(position = "fill") +  # stacked bar showing proportions
  labs(
    title = "Proportion of Tree Health by Stewardship Level",
    x = "Stewardship Level",
    y = "Proportion",
    fill = "Health"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() 



###  
table_data <- table(trees_filtered$health, trees_filtered$steward)
chisq.test(table_data)
#Since the p-value is much smaller than 0.05, you can reject the null hypothesis. This means that there is a statistically significant association between health and stewardship. further work must be done to measure the association.

# Excluding "Dead" or "Stumps" from health variable
trees_filtered <- trees[!trees$health %in% c("Dead", "Stump"), ]

# Convert the health variable to a numeric variable
trees$health_numeric <- as.numeric(factor(trees$health, levels = c("Dead", "Poor", "Fair", "Good")))

# Filter out "None" from the steward variable
trees_filtered <- trees[trees$steward != "dead", ]

#The model was created but need work
model <- lm(tree_dbh ~ health + steward , data = trees_clean)
summary(model)

par(mfrow = c(2))
plot(model)











##Presentation

addmargins(prop.table(table(trees$health, trees$steward), margin = 1))


# Barplot of Tree Health by Borough with readable y-axis
ggplot(trees, aes(x = borough, fill = health)) +
  geom_bar(position = "stack") +
  scale_y_continuous(labels = comma) +  # Format y-axis labels
  labs(title = "Tree Health by Borough",
       x = "Borough",
       y = "Count of Trees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Barplot of Tree Health by Steward with readable y-axis
ggplot(trees_filtered, aes(x = steward, fill = health)) +
  geom_bar(position = "stack") +
  scale_y_continuous(labels = comma) +  # Format y-axis labels for readability
  labs(title = "Tree Health by Stewardship",
       x = "Stewardship Level",
       y = "Count of Trees") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## chisq.test
table_data <- table(trees_filtered$health, trees_filtered$steward)
chisq.test(table_data)
#Since the p-value is much smaller than 0.05, you can reject the null hypothesis. This means that there is a statistically significant association between health and stewardship


plot(model)#multiple regression model

##Presentation end

