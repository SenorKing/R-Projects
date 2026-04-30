##Andre King
##Exercise 1: Diagram and test hypotheses in R
##DATA 306
##02/16/2025
##Part 1: Following along
		
##3
getwd() #this returns the present working directory

setwd("~/Desktop/DATA306") # working directory already set to "/Users/senorking/Desktop/Spring2025/Data306"

school <- read.csv("data/NYCschools_r1.csv")  #this assigns the data as a value in an object named school

##4
str(school) #give the structure of the data in school 

head(school) #look at the first 6 rows 
head(school, 10) #look at first 10 rows instead of 6

##5
hist(school$mean_class_size) # the $ sign is how we select a select a single element of a list

par(mfrow = c(1, 3)) # This command sets three plots on one row
hist(school[["mean_class_size"]])
hist(school[ , "mean_class_size"])
hist(school$mean_class_size)

##6
par(mfrow = c(1, 3)) # This command sets three plots on one row
hist(school[["mean_class_size"]])
boxplot(school[ , "mean_class_size"], horizantal = TRUE) 
# horizontal = TRUE puts the boxplot on its side 
# The default (horizontal = FALSE) is vertical
# TRUE is a special word in R indicating a boo
## does not need quotes around it.
stripchart(school$mean_class_size, method = "jitter")
# Jitter randomly assigns points to locations so you can see them more easily.
#Remove the "method" argument (and the comma), what happens!
# The method argument can take several preset
## so requires quotes.

## The distribution of the mean class size is skewed to the left with majority between 20 and 25. The stripchart using the jitter options shows the distribution of the mean class size and it shows the majority of the classes of a mean of below 30 with only two outliers.

##7		
summary(school)	# produces summaries of all columns in a data.frame
		
## The summary shows that the mean class cize is about 22. The median is also about 22. This confirms that the distribution is mainly between 20 and 25.	

##8
summary(school$mean_class_size)
#a. 	What is the "center" of the data?
## The center of the data is the mean class size meaning most class size is similar to the mean size
#b. 	How does the data vary about this center? 
## The data changes from the middle getting smaller as it goes to the left and larger as it goes right. the minimum of the data is much different from the mean because there is not a lot of data much less than the mean and similarly the right maximum is much different.

##9
# The average class size of schools in NYC is 22 students but it can be as low as 11 students or as high as 52 students. 

##10
#install.packages("DiagrammeR")
library(DiagrammeR)
grViz("digraph {'School size' -> 'Class size'}") #output will appear in your web browser as an HTML file 
grViz("digraph {'School size' -> 'Class size' [label = '+']}") #replace the + sign with a - sign if you think class size declines as school size increases

grViz( "digraph {
	rankdir = 'LR' ## uncomment this to make the diagram flow from left to right
	'School size' -> 'Class size' [Label = ' -']
}")

##11
plot(school$total_enrollment, school$mean_class_size) #scatterplot
#The plot suggest that there is a correlation between school size and the mean class size but one doess not cause the other. The data show that the mean class size is usually about 20 students per class regardless of the school size. 

##12
school_lm <- lm(mean_class_size ~ total_enrollment, data = school)

summary(school_lm)

##13 
plot(school$mean_class_size ~ school$total_enrollment) 
abline(school_lm, col = "blue", lwd = 2) # col means color, lwd means line width
#the model indicates that the the data does indicate a slight positive relationship that can account for only some of the data  

scatter.smooth(school$mean_class_size ~ school$total_enrollment,
span = 2/3, degree = 3, # these are defaults, play with them to see what changes
lpars = list(col = "red", lwd = 3))
# lpars means line parameters
abline(school_lm, col = "blue", lwd = 3)


## Part 2: An analysis of student belonging and [covariate]

###6
par(mfrow = c(1, 3)) # This command sets three plots on one row
hist(school[["outsiders_student"]])
boxplot(school[ , "outsiders_student"], horizantal = TRUE) 
stripchart(school$outsiders_student, method = "jitter")



###8
summary(school$outsiders_student)
summary(school$poverty)

plot(school$outsiders_student ~ school$disability) 
abline(school_lm, col = "blue", lwd = 2) # col means color, lwd means line width
# The data of outsider student and student with disability seem to increase in the middle of the dissability rate of 0.1 to 0.4 and gets more spread out the further away from the center you go

###10
#install.packages("DiagrammeR") # uncomment this first to install the package, then re-comment it
library(DiagrammeR)

grViz("digraph { rankdir = 'LR' ' Outside Student' -> ' Disability ' [label = ' + ']}")

###12
outsider_lm <- lm(mean_class_size ~ disability, data = school)
summary(outsider_lm)


###13
plot(school$outsiders_student ~ school$disability)
abline(outsider_lm, col = "blue", lwd = 2) # col means color, lwd means line width


scatter.smooth(school$outsiders_student ~ school$disability,
span = 2/3, degree = 2, # these are defaults
lpars = list(col = "red", lwd = 2)) # lpars means line parameters
abline(school_lm, col = "blue", lwd = 2)
## The data indicates that there is a positive increase between the disability and outsider student






 

