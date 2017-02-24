# Mirva Turkia 22.2.2017
# This is my data wrangling exercise, week 5

# This data is provided by UN: http://hdr.undp.org/en/content/human-development-index-hdi

# I will upload the data from the link provided to get variable names right

library(stringr); library(dplyr)

# So, at first I will read the data into R
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep=",", header=TRUE)

# Mutating the Gross National Income (GNI) variable to numeric using string manipulation
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric

# Excluding the unneeded variables
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select(human, one_of(keep))

# Removing all rows with missing values
complete.cases(human)
data.frame(human[-1], comp = complete.cases(human))
human_ <- filter(human, complete.cases(human))

# Removing the observations which relate to regions instead of countries
human_$Country
human_ <- human_[1:155, ]
human_$Country

# Defining the row names of the data by the country names
rownames(human_) <- human_$Country

# Removing the country name column from the data
human_ <- select(human_, -Country)
str(human_)

# Saving the human data in my data folder including the row names
getwd()
setwd("/Users/mirva/IODS-project/data")
write.csv(human_, file = "human", row.names = TRUE)
