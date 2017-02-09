# Mirva Turkia 6.2.2017
# This is my R script for the exercise 3

getwd()
setwd("/Users/mirva/IODS-project/data")
por <- read.csv("student-por.csv", sep = ";" , header=TRUE)
math <- read.csv("student-mat.csv", , sep = ";" , header=TRUE)

# The structure and dimensions of the data
str(por)
str(math)
dim(por)
dim(math)
# por: 649 observations & 33 variables, mat: 395 observations, 33 variables

# Joining the two data sets
library(dplyr)
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

# The structure and dimensions of the new data
str(math_por)
dim(math_por)
# 382 observations and 53 variables (13 joined and 2x20 non-joined)

# Combining the 'duplicated' answers in the joined data
colnames(math_por)

alc <- select(math_por, one_of(join_by))

notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

notjoined_columns

for(column_name in notjoined_columns) {
  two_columns <- select(math_por, starts_with(column_name))
  first_column <- select(two_columns, 1)[[1]]
  if(is.numeric(first_column)) {
    alc[column_name] <- round(rowMeans(two_columns))
  } else { 
    alc[column_name] <- first_column
  }
}

colnames(alc)

# The average of the answers related to weekday and weekend alcohol consumption to a new column 
# 'alc_use'
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Creating a new logical column 'high_use' which is TRUE for students for which 'alc_use' is greater than 2
alc <- mutate(alc, high_use = alc_use > 2)

# Glimpse at the joined and modified data
glimpse(alc)

# Saving the joined and modified data set to the ‘data’ folder
write.csv(alc, file = "alcohol")