#Andre King
##Exercise 3: Tables and Probability Estimates
## DATA 306
## 3/10/2025

getwd()

#Part 1: Read, Recode, and Tabulate the 2015 Tree Census Data
#A
setwd("~/Desktop/DATA306/") # modify as needed
## My directory
#setwd("/Users/senorking/Desktop/Data306/data/")

tree15 <- read.csv("data/2015_tree_census_with_neighb_indicators.csv")
#tree15 <- read.csv("/Users/senorking/Desktop/Data306/data/2015_tree_census_with_neighb_indicators.csv")
str(tree15)
dim(tree15)
#Dimension : 683788 rows     53 column

(treetab <- table(tree15$borough)) #Putting the whole statement in parentheses will assign the object name and print the object.

proportions(treetab)

round(proportions(treetab), 3)#round() is 

treepcts <- round(proportions(treetab), 3)*100 # (1) calculates the percentages
treepcts <- as.data.frame(treepcts) # (2) converts the table to a dataframe object
colnames(treepcts) <- c("Borough", "% of NYC trees") # (3) converts the column names to something more accurate and meaningful.
treepcts
marginSums(treetab)#683788

#What would the sum be if we used treepcts?  The sum would be 100 if we use treepcts since treepct calculate in terms of percentage. 
	
#B: Table of counts for the steward variable.
stewardct <- table(tree15$steward) ## Count occurrences of each steward category
stewardct
 
## Table of percentages for the steward variable.
stewardpcts <- round(proportions(stewardct), 4) * 100  # Convert to percentages (4 decimal places)
stewardpcts
sum(stewardpcts)  # Should return 100

##The table of count and the table of percentages shows the same value of stewardship of trees. The tables show that 48782 or 71% of tree has no sign of stewardship. 143557 or 21% of trees shows 1 or 2 signs. Only a small percentage 20000 or 3% have 3or more visible signs of care, this indicates that most trees in NYC do not receive direct care or frequent maintenance. 

#3: Why does steward have a blank, or unlabeled, level?
##The unlabeled section may be because stumps or dead trees were not officially recorded. 

##4
tree15$steward[1] == " "
sum(tree15$steward[1] == " ")
sum(tree15$steward[1] == "")

#b
tree15$steward[tree15$steward == ""] <- "dead" #This makes the col name of missing value dead.
table(tree15$steward)

##5
s.b.tab <- table(tree15$steward, tree15$borough)
addmargins(s.b.tab)

tree15$stew.fac <- factor(tree15$steward,
levels = c("dead", "None", "1or2", "3or4", "4orMore"))
s.b.tab <- table(tree15$stew.fac, tree15$borough)
addmargins(s.b.tab)



##Part 2: Analysis and Odds Ratios
##1
#a: What is the probability a tree will have four or more signs of stewardship?
round(1610/683788, 4)
## 0.0024

#b: Pr(dead) = 0.0462
round(31615/683788, 4)

#c: Pr(dead | Bronx) = 0.0542
round(4618/85203, 4)

#d: Pr(Manhattan | 4orMore) = 0.3441
round(554/1610, 4)

#e: What is the probability a tree will have four or more signs of stewardship if it is growing in Queens? 
## 0.0014
round(340/250551, 4)

#f: What is the probability a tree will have any signs of stewardship, given it is growing in Staten Island? 
## 0.1931
round((18829+1396+112)/105318, 4)

#g: What is the probability a tree recorded in this census is not dead?
## 0.9538
round((683788-31615)/683788, 4)

##2
#a
or.tab <- table(tree15$steward == "4orMore", tree15$borough == "Queens")
## matrix is more convenient than a dataframe here
or.tab <- as.matrix(or.tab)
colnames(or.tab) <- c("Outside Queens", "Inside Queens")
rownames(or.tab) <- c("Less than 4", "4 or more")
or.tab

#b 
inVout_wellTend <- 340/1270
inVout_poorTend <- 250211/431967
OR <- round(inVout_wellTend/inVout_poorTend, 4) #odd ratio

paste("The odds of a well tended tree in Queens is", round(inVout_wellTend, 4), sep = " ")
paste("The odds of a poorly tended tree in Queens is", round(inVout_poorTend, 4), sep = " ")
paste("The odds ratio is", OR, sep = " ")

##3

# Create a table for Manhattan vs. outside Manhattan
od.tab <- table(tree15$steward == "dead", tree15$borough == "Manhattan")
od.tab <- as.matrix(od.tab)# matrix
colnames(od.tab) <- c("Alive", "Dead")
rownames(od.tab) <- c("Outside Manhattan", "Inside Manhattan")
od.tab

# Compute odds
inVout_dead <- 2996/62427  #(65423 - 2996)  # Odds of being dead in Manhattan
inVout_alive <- 28619/589746  #(618365 - 28619)  # Odds of being dead outside Manhattan
OR <- round(inVout_dead / inVout_alive, 4)  # Odds ratio

# Print results
paste("The odds of a tree being dead in Manhattan is", round(inVout_dead, 4), sep = " ")
paste("The odds of a tree being dead outside Manhattan is", round(inVout_alive, 4), sep = " ")
paste("The odds ratio is", OR, sep = " ")

## The odds a tree dying if it is growing in Mahnattan is only 0.048 while the odds a tree dies anywhere else in NYC is 0.0485. This means only 4.80% is dead in Mahnattan while outside of Mahnattan is around the same at 4.85% just 0.5% lower than mahnattan. Or, the odds ratio is 0.989

##4
#The quality of urban tree stewardship is not evenly distributed across NYC boroughs. Manhattan has the highest proportion of stewarded trees of 49.74% and is many times more likely to have highly tended trees compared to other boroughs. while, the Bronx and Queens have the lowest stewardship rates of 18%. This means there is less care for tree. These disparities indicate that certain areas, especially Manhattan, receive significantly more tree care than others.

##5
Since these conditional probabilities differ significantly from the overall probability P(Any Stewardship)= 0.241, stewardship is not independent of borough. The borough where a tree is located affects the likelihood of it receiving stewardship, with Manhattan showing the highest stewardship rates and the Bronx and Queens showing the lowest.