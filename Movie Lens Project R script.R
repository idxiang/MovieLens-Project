
##Making sure required packages are available##
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")

##Setting libraries##
library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(Metrics)
library(tidyr)

##Movie Lens 10M Dataset##
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl,"ml-10M100K/ratings.dat")))
                      , col.names = c("userId","movieId","rating","timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::",3)

colnames(movies) <- c("movieId","title","genres")

##Since I'm using R 4.0.5##
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),title = 
                                             as.character(title), genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


str(edx) ## Evaluate Edx Set
str(validation) ## Evaluate Validation Set
head(edx) ## See top Rows


edx <-  edx %>% mutate (timestamp = as_datetime(timestamp)) ## changes date into a readable format

head(edx)


edx <-  edx %>% mutate (year = substring(title, nchar(title) - 6 )) ## Extracting Year from title name

head(edx)

edx$year <- gsub("[()]", "", edx$year) ## Removing parenthesis



edx$year <- as.numeric(edx$year ) ## Coerce string into number
str(edx)



edx <- edx %>% separate_rows(genres, sep = "\\|") 
## use separate_rows function split genres into single values.
##Movies with multiple genres will be duplicated in the dataset
head(edx)


validation <-  validation %>% mutate (timestamp = as_datetime(timestamp))
## Repeat all steps for validation set
validation <-  validation %>% mutate (year = substring(title, nchar(title) - 6 ))
## Extract Year  
validation$year <- gsub("[()]", "", validation$year) ## Remove Parens
validation$year <- as.numeric(validation$year ) ## coerce Numeric
validation <- validation %>% separate_rows(genres, sep = "\\|") 
## use separate_rows function split genres into single values. Movies with multiple
## genres will be duplicated in the dataset

str(validation)


sapply(edx,class) ## Validate class of variables

df <- edx %>% group_by(genres) %>% summarise(n = n()) ## See top N genre distribution



head(df[order(-df$n),]) ## Sort by tops


head(df[order(-df$n),]) %>% ## Feed reduced table for graphing
  ggplot(aes(x = reorder(genres, -n), n)) +
  geom_bar(stat="identity") + scale_y_continuous(limits=c(0,10000000)) ## Create plot to see top genre 

edxavg_rating <- edx %>%  group_by(year) %>% 
  summarize(avg_ratings = mean(rating,trim = 0,na.rm = FALSE)) 
## create DataFrame with average ratings by year
edxavg_rating %>% ggplot(aes(x = year,y = avg_ratings)) + 
geom_point() + scale_y_continuous(limits=c(0,5)) ## plot average ratings per year

avg <- mean(edx$rating)  # Mean movie rating in EDX set
RMSECalc <- rmse(avg,validation$rating) # RMSE calculations
RMSECalc #Display RMSE 

year <- edx %>%
  group_by(year) %>% # Group Data by year
  summarize(diff = mean(rating - avg)) # calculate difference between average value and actual value


ratingsbyyear <- validation %>%  left_join(year, by = "year") %>% # Join on Year
  mutate(ratingprediction =  diff + avg) %>% # Generate new predictions
  pull(ratingprediction)  # retrieve 

rmse_Year <- rmse(ratingsbyyear,validation$rating)  # calculate RMSE for model

rmse_Year


user <- edx %>%
  group_by(userId) %>% # Group Data by UserId
  summarize(diff = mean(rating - avg)) # calculate difference between average value and actual value


ratingsbyUser <- validation %>%  left_join(user, by = "userId") %>% # Join on UserId
  mutate(ratingprediction =  diff + avg) %>% # Generate new predictions
  pull(ratingprediction)  # retrieve 

rmse_User <- rmse(ratingsbyUser,validation$rating)  # calculate RMSE for model

rmse_User


movieId <- edx %>%
  group_by(movieId) %>% # Group Data by movieId
  summarize(diff = mean(rating - avg)) # calculate difference between average value and actual value


ratingsbymovie <- validation %>%  left_join(movieId, by = "movieId") %>% # Join on movieId
  mutate(ratingprediction =  diff + avg) %>% # Generate new predictions
  pull(ratingprediction)  # retrieve 

rmse_movie <- rmse(ratingsbymovie,validation$rating)  # calculate RMSE for model

rmse_movie


movie <- edx %>% group_by(movieId) %>% summarize(diff1 = mean (rating - avg)) ## Calculate movie effect

user <-  edx %>% left_join(movie, by = "movieId") %>% group_by(userId) %>% 
  summarize(diff2 = mean(rating - avg - diff1)) #calculate user effect, given movie effect

pred <- validation %>% left_join(movie, by = "movieId") %>% 
  left_join(user, by = "userId") %>% mutate(prediction = avg + diff1 + diff2) %>% pull(prediction)
# compare predicted values to validation dataset

rmse_user_movie <- rmse(validation$rating, pred) ## calculate RMSE 

rmse_user_movie # View RMSE 

