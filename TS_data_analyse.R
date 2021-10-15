#-----------------------------------------------------------------------------------------
# Load libraries
#-----------------------------------------------------------------------------------------
library(stargazer)
library(ggplot2)
library(reshape2)
library(plyr)
library(plm)
library(zoo)

# Clean up working environment
remove(list=ls())
cat("\f")

#-----------------------------------------------------------------------------------------
# Set up work space
#-----------------------------------------------------------------------------------------

# WD Jasper
dir <- "C:/Users/jaspe/OneDrive - Erasmus University Rotterdam/MScBA BAM/Advanced Statistics and Programming/ASP Group Assignment/R-Bestanden/R_project/"

# WD Mathijs
dir <- "/Users/Mathijs/OneDrive - Erasmus University Rotterdam/ASP Group Assignment/R-Bestanden/R_project/"

# WD Freek
dir <- "/Users/Freek/OneDrive - Erasmus University Rotterdam/ASP Group Assignment/R-Bestanden/R_project/"

# Mapping
dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Results/")

# Read CSV
df <- read.csv(file=paste0(dirData,"dfComplete_Sentiment.csv"), sep =',')

#-----------------------------------------------------------------------------------------
# Data Preparation
#-----------------------------------------------------------------------------------------

# Change column names
colnames(df)[colnames(df) == "New.Cases"] <- "new_cases"
colnames(df)[colnames(df) == "New.COVID.19.Hospital.Admissions"] <- "new_hospital_cases"
colnames(df)[colnames(df) == "Week"] <- "week"

# Check types and convert to numeric
str(df)
df$new_hospital_cases  <- as.numeric(as.character(df$new_hospital_cases))

# Check NaN values
#sapply(df5, function(x) sum(is.na(x)))

# Drop weeks of 2020
df <- df[!(df$week > 45),]

# Drop columns
df2 <- subset(df, select = -c(X.1,Date,Year,president,daily_vaccinations_raw,State,X7.Day.Moving.Avg,X,yearweek))

# Add column day number
df2$day <- seq.int(nrow(df2))
df2 <- df2[ , c("day", names(df2)[names(df2) != "day"])]

# Add daily sentiment scores
dfDaily <- read.csv2(file=paste0(dirData,"SentimentScoresDaily.csv"), sep =',')

# Merge dataframes
df3 <- merge(df2, dfDaily, by = "day")

# Remove unnecessary columns
df3 <- subset(df3, select = -c(X, created_at, tweet_sentiment.x))

# Change column name
df3$tweet_sentiment <- df3$tweet_sentiment
df3$tweet_sentiment.y <- NULL

# Fill NaN values based on average
sapply(df3, function(x) sum(is.na(x)))

# approve_estimate
df3$approve_estimate_avg <- df3$approve_estimate
for(i in df3$week) { 
  df3$approve_estimate_avg[df3$week == i] <- mean(df3$approve_estimate_avg[df3$week == i], na.rm = TRUE)
}
for(i in df3$day) {
  df3$approve_estimate[df3$day == i] <- ifelse(is.na(df3$approve_estimate[df3$day == i]), df3$approve_estimate_avg[df3$day == i], df3$approve_estimate[df3$day == i])
}

# disapprove_estimate
df3$disapprove_estimate_avg <- df3$disapprove_estimate
for(i in df3$week) { 
  df3$disapprove_estimate_avg[df3$week == i] <- mean(df3$disapprove_estimate_avg[df3$week == i], na.rm = TRUE)
}
for(i in df3$day) {
  df3$disapprove_estimate[df3$day == i] <- ifelse(is.na(df3$disapprove_estimate[df3$day == i]), df3$disapprove_estimate_avg[df3$day == i], df3$disapprove_estimate[df3$day == i])
}

# total_vaccinations
library(zoo)
df3$total_vaccinations <- na.approx(df3$total_vaccinations)

# people_vaccinated
df3$people_vaccinated <- na.approx(df3$people_vaccinated)

# people_fully_vaccinated
df3$people_fully_vaccinated <- na.locf(df3$people_fully_vaccinated, fromLast = TRUE)

# people_vaccinated_per_hundred
df3$people_vaccinated_per_hundred <- na.approx(df3$people_vaccinated_per_hundred)

# total_vaccinations_per_hundred
df3$total_vaccinations_per_hundred <- na.approx(df3$total_vaccinations_per_hundred)

# people_fully_vaccinated_per_hundred
df3$people_fully_vaccinated_per_hundred <- na.locf(df3$people_fully_vaccinated_per_hundred, fromLast = TRUE)

# new_cases
df3$new_cases <- na.locf(df3$new_cases, fromLast = FALSE)

# new_hospital_cases
df3$new_hospital_cases <- na.locf(df3$new_hospital_cases, fromLast = FALSE)

df4 <- df3

# Drop NaN rows of TweetSentiment
df5 <- na.omit(df4)

# Create final dataframe
df_final <- df5

write.csv(df_final, file.path(dirData, "final_df_day.csv"))

#-----------------------------------------------------------------------------------------
# Data Visualizations
#-----------------------------------------------------------------------------------------

# Statistics of dataframe
library(stargazer)
stargazer(df_final, type='text')

# Plot all variables to check distribution
library(ggplot2)
plot_approve_estimate <- ggplot(df_final, aes(x=approve_estimate)) + geom_histogram(binwidth=.9)
plot_disapprove_estimate <- ggplot(df_final, aes(x=disapprove_estimate)) + geom_histogram(binwidth=.9)
plot_total_vaccinations <- ggplot(df_final, aes(x=total_vaccinations)) + geom_histogram()
plot_people_vaccinated <- ggplot(df_final, aes(x=people_vaccinated)) + geom_histogram()
plot_people_fully_vaccinated <- ggplot(df_final, aes(x=people_fully_vaccinated)) + geom_histogram()
plot_daily_vaccinations <- ggplot(df_final, aes(x=daily_vaccinations)) + geom_histogram()
plot_total_vaccinations_per_hundred <- ggplot(df_final, aes(x=total_vaccinations_per_hundred)) + geom_histogram()
plot_people_vaccinated_per_hundred <- ggplot(df_final, aes(x=people_vaccinated_per_hundred)) + geom_histogram()
plot_people_fully_vaccinated_per_hundred <- ggplot(df_final, aes(x=people_fully_vaccinated_per_hundred)) + geom_histogram()
plot_daily_vaccinations_per_million <- ggplot(df_final, aes(x=daily_vaccinations_per_million)) + geom_histogram()
plot_new_cases <- ggplot(df_final, aes(x=new_cases )) + geom_histogram()
plot_new_hospital_cases <- ggplot(df_final, aes(x=new_hospital_cases )) + geom_histogram()
plot_tweetsentiment <- ggplot(df_final, aes(x=tweet_sentiment)) + geom_histogram(binwidth=.12)

# 12 plots in 1 frame
require(gridExtra)
grid.arrange(plot_approve_estimate, plot_disapprove_estimate, plot_total_vaccinations,plot_people_vaccinated,plot_people_fully_vaccinated,plot_daily_vaccinations,
             plot_total_vaccinations_per_hundred,plot_people_vaccinated_per_hundred,plot_people_fully_vaccinated_per_hundred,plot_daily_vaccinations_per_million,
             plot_new_cases,plot_tweetsentiment,plot_new_hospital_cases,ncol=3)

# Rearrange dataframe
df_final = subset(df_final, select = -c(approve_estimate_avg,disapprove_estimate_avg) )
df_final <- df_final[ , c("daily_vaccinations", names(df_final)[names(df_final) != "daily_vaccinations"])]

# CHeck correlation matrix
res <- cor(df_final[-1], df_final$daily_vaccinations)
round(res, 2)

shapiro.test(df_final$tweet_sentiment)

#-----------------------------------------------------------------------------------------
# Data Regression
#-----------------------------------------------------------------------------------------

# Regressie
mdlA  <- daily_vaccinations ~ tweet_sentiment + approve_estimate + new_hospital_cases
rsltA <- lm(mdlA, data=df_final)
summary(rsltA)

stargazer(rsltA, type='text')










