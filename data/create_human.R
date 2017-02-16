# Mirva Turkia 6.2.2017
# This is my R script for the exercise 4

library(dplyr)

getwd()
setwd("/Users/mirva/IODS-project/data")

# Reading the Human development data into R
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

# Structure, dimensions and the summary of the hd
str(hd)
dim(hd)
summary(hd)

# Changing the column names of the hd data
colnames(hd)
colnames(hd)[1] <- "HDIrank"
colnames(hd)[2] <- "Country"
colnames(hd)[3] <- "HDI"
colnames(hd)[4] <- "LifeExpect"
colnames(hd)[5] <- "EduExpect"
colnames(hd)[6] <- "EduMean"
colnames(hd)[7] <- "GNI"
colnames(hd)[8] <- "GNIexHDI"
colnames(hd)

# Reading the Gender inequality data into R
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# Structure, dimensions and the summary of the gii
str(gii)
dim(gii)
summary(gii)

# Changing the column names of the gii data
colnames(gii)
colnames(gii)[1] <- "GIIrank"
colnames(gii)[2] <- "Country"
colnames(gii)[3] <- "GII"
colnames(gii)[4] <- "MatMRatio"
colnames(gii)[5] <- "AdoBRate"
colnames(gii)[6] <- "Parliament"
colnames(gii)[7] <- "F2Edu"
colnames(gii)[8] <- "M2Edu"
colnames(gii)[9] <- "FLabourRate"
colnames(gii)[10] <- "MLabourRate"
colnames(gii)

# Creating two new variables to gii data

# The ratio of Female and Male populations with secondary education in each country
gii <- mutate(gii, EduRatio = (F2Edu / M2Edu))
              
# The ratio of labour force participation of females and males in each country
gii <- mutate(gii, LabourRatio = (FLabourRate / MLabourRate))

#Join together the two datasets using the variable Country as the identifier.
# Joining the datasets
human <- inner_join(hd, gii, by = "Country", suffix = c(".hd", ".gii"))
colnames(human)

# Saving the joined data set to the ‘data’ folder
write.csv(human, file = "human")