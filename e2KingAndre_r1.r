#Andre King
##Exercise 2: Explore data surfaces in R
## DATA 306
## 3/5/2025

getwd()

## My directory
#school <- read.csv("/Users/senorking/Desktop/Data306/data/NYCschools_r1.csv")
school <- read.csv("./data/NYCschools_r1.csv")
str(school)

sch <- subset(school, select = -c(X, year))

colnames(sch)

ncol(school) - ncol(sch)

##summary(sch[ , -c(1:2)])
summary(sch[ , 3:23]) # equivalent to above

##4
## Write a few sentences comparing two distributions (columns, variables). Cite specific numbers in your interpretation.
## The black student has a median of 20.5% and a mean of 28.11%, ranging from 0% to 93.1%, indicating that some schools have very few Black students while others have a majority. In contrast, the white student proportion has a median of just 3.6% and a mean of 12.96%, ranging from 0% to 83.3%, showing that most schools have very few White students. The low median compared to the mean for White students shows that a small number of schools have a relatively large White population, pulling up the average. Black students appear to be more represented across schools.

##5
##Demographic categories like gender, race, ethnicity, and disability are normally under-
#stood as categorical variables.Yet they presented as continuous here, and it is not a bug. How does that make sense?
##This makes sense because these demographic categories are expressed as proportions not categories. Instead of categorizing individuals, the dataset represents the percentage of each group within a school which makes  analysis and comparisons across different schools easier.

##Conceptually (not in R), how could these continuous variables be converted into categorical (ordinal or nominal) variables?
# One way of converted continuous variables into categorical variables is by creating ordered categories such as "Low" (0–25%), "Moderate" (26–50%), "High" (51–75%), and "Very High" (76–100%) to describe the proportion of a each demographic group.

##6
cor.sch <- cor(sch[ , 3:22], use = "complete.obs")
cor.sch

cor.sch.lt <- cor.sch[lower.tri(cor.sch) == TRUE] #lower.tri() returns what is called a "logical."
stem(cor.sch.lt)
cor.sch.lt

##7
#install.packages("corrplot") # uncomment to install the package, then re-comment
library(corrplot) # load the package
corrplot(cor.sch, type = "lower", order = "hclust")

#What does the plot suggest about NYC schools?
##There is a strong positive correlation between Hispanic and ESl
##There is no correlation between White and poverty
##There is strong positive correlation between Asian and total enrollment
##There is no correlation between elementary and middle. 
##There is a strong negative correlation between disability and total enrollment

##8
install.packages("car")
library(car)
scatterplotMatrix(sch[, c("outsiders_student", "outsiders_teacher",
"outsiders_parent", "total_enrollment")])

##What kinds of relations do you see here? In particular, do you see any evidence of a relation between outsiderness and total enrollment?

##There seems to be a correlation between outsiders student and teacher because in the scatterplot the majority of the data is in the center and the line is pointing to the left showing skewness. 

cor.sch[c(1,17, 19, 20), c(1, 17, 19, 20)]
#dim(cor.sch)

## The outsiderness does not seem to have any serious correlation to total enrollment. this is because the correlation coefficients of outsiders student and total enrollment is -0.07197410, which is very close to zero. The outsiders parent and total enrollment correlation coefficients is 0.04758371 which is also very close to zero. This shows there is no evidence to show relationship of outersiderness and total enrollment. 

##9
hdens <- density(sch$high) # calculate kernel densities

plot(hdens, # ?plot to see the meaning of these arguments
xlim = c(-.2, 1.23),
main = "red = elementary, blue = middle, black = high",
xlab = "")

lines(density(sch$elementary), col = "red")## lines() plots another set of lines over an existing plot.
lines(density(sch$middle), col = "blue")### densities are not saved as a named object for concision

library(DiagrammeR)
grViz("digraph {
node [shape = plaintext]
'+' [shape = doublecircle]
'School size' -> '+' -> 'Outsiders'
'Student age' -> '+'
{rank = same; 'School size' 'Outsiders' '+'}
}")

##10
sch$agemix <- NA
## defines a new column within sch, where each row has the value NA

sch$agemix[sch$elementary > 0.5] <- "Elementary"
sch$agemix[sch$middle > 0.7] <- "Middle"
sch$agemix[sch$high > 0.5] <- "High"
sch$agemix[is.na(sch$agemix) == TRUE] <- "Mixed"
table(sch$agemix)

sum(sch$total_enrollment[sch$agemix == "High"|
sch$agemix == "Elementary"],
na.rm = TRUE)/
sum(sch$total_enrollment,
na.rm = TRUE)

scatterplotMatrix(~ outsiders_parent + outsiders_teacher + outsiders_student + total_enrollment | agemix, data = sch,
smooth = FALSE, # make TRUE to see LOESS
legend = list(coords = "bottomright"))

scatterplotMatrix(~ asian + black + hispanic + white | agemix, data = sch,
smooth = TRUE, # make TRUE to see LOESS
legend = list(coords = "bottomright"))

## Another association that exist within the data is race of students. Different schools seem to have different concentration of races. The plot of asian and white students seems to be similar with skewness to the left while hispanic and black seem to be more similar to each other. The loess curve shows that the relationship between black and hispanic is not linear while all other relationship are linear. 



















