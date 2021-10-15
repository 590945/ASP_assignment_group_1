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
df                    <- read.csv(file=paste0(dirData,"dfComplete_Sentiment.csv"), sep =',')
df_importstate        <- read.csv(file=paste0(dirData,"us_state_vaccinations.csv"), sep =',')# source : https://covid.ourworldindata.org/data/vaccinations/us_state_vaccinations.csv
df_importdemographics <- read.csv(file=paste0(dirData,"state_demographics.csv"), sep =';')
dfDaily               <- read.csv(file=paste0(dirData,"SentimentScoresDaily.csv"), sep =',')
df_population         <- read.csv(file=paste0(dirData,"nst-est2019-popchg2010_2019.csv"), sep =',')#https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_1873399417

#-----------------------------------------------------------------------------------------
# Data Preparation
#-----------------------------------------------------------------------------------------

# Change column names
colnames(df)[colnames(df) == "New.Cases"] <- "new_cases"
colnames(df)[colnames(df) == "New.COVID.19.Hospital.Admissions"] <- "new_hospital_cases"
colnames(df)[colnames(df) == "Week"] <- "week"
colnames(df)[colnames(df) == "Date"] <- "date"

# Check types and convert to numeric
str(df)
df$new_hospital_cases  <- as.numeric(as.character(df$new_hospital_cases))

# Check NaN values
#sapply(df5, function(x) sum(is.na(x)))

# Drop weeks of 2020
df <- df[!(df$week > 45),]

# Drop columns
df2 <- subset(df, select = -c(X.1,Year,president,daily_vaccinations_raw,State,X7.Day.Moving.Avg,X,yearweek))

# Add column day number
df2$day <- seq.int(nrow(df2))
df2 <- df2[ , c("day", names(df2)[names(df2) != "day"])]

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

# Rearrange dataframe
df_final = subset(df_final, select = -c(approve_estimate_avg,disapprove_estimate_avg) )
df_final <- df_final[ , c("daily_vaccinations", names(df_final)[names(df_final) != "daily_vaccinations"])]

#-----------------------------------------------------------------------------------------
# Panel data adjustments
#-----------------------------------------------------------------------------------------
df_final2 <- subset(df_final, select = -c(total_vaccinations,people_vaccinated,people_fully_vaccinated_per_hundred,
                                    total_vaccinations_per_hundred,people_fully_vaccinated,people_vaccinated_per_hundred,
                                    daily_vaccinations,daily_vaccinations_per_million))

str(df_final)
str(df_importstate)
str(df_importdemographics)
str(df_population)

# Change to correct variable name
colnames(df_importdemographics)[colnames(df_importdemographics) == "Label"] <- "location"
colnames(df_importdemographics)[colnames(df_importdemographics) == "High.school.graduate.or.higher"] <- "high_school_percentage"
colnames(df_importdemographics)[colnames(df_importdemographics) == "Bachelor.s.degree.or.higher"] <- "bachelor_degree_percentage"
colnames(df_importdemographics)[colnames(df_importdemographics) == "Median.Age"] <- "age_median"
colnames(df_population)[colnames(df_population) == "NAME"] <- "location"
colnames(df_population)[colnames(df_population) == "POPESTIMATE2019"] <- "population"

total_df0 <- merge(df_final2,df_importstate, by="date")
total_df0 <- merge(total_df0, df_importdemographics, by="location", all=TRUE)
total_df <- merge(total_df0, df_population, by="location", all=TRUE)


# Find non state levels
unique(total_df[c("location")])

# Drop non state levels
total_df2 <- total_df[!(total_df$location=="United States" | 
                          total_df$location=="Veterans Health"| 
                          total_df$location=="Northern Mariana Islands"| 
                          total_df$location=="Indian Health Svc"|
                          total_df$location=="American Samoa"|
                          total_df$location=="Long Term Care"|
                          total_df$location=="Puerto Rico"|
                          total_df$location=="Federated States of Micronesia"|
                          total_df$location=="Guam"|
                          total_df$location=="Dept of Defense"|
                          total_df$location=="Bureau of Prisons"|
                          total_df$location=="District of Columbia"|
                          total_df$location=="Virgin Islands"|
                          total_df$location=="Republic of Palau"|
                          total_df$location=="Marshall Islands"),]

total_df3 <- subset(total_df2, select = c(location, date, daily_vaccinations, tweet_sentiment, approve_estimate, 
                                           disapprove_estimate, new_cases, new_hospital_cases, people_vaccinated,
                                           high_school_percentage,bachelor_degree_percentage,age_median,population))

# Check for MCAR (missing completely at random)
mcar_test(total_df3)

# Delete rows that contain NA values
completeDF <- na.omit(total_df3)

# Normalize data
completeDF$daily_vaccinations_n <- completeDF$daily_vaccinations / completeDF$population
completeDF$new_cases_n          <- completeDF$new_cases / completeDF$population
completeDF$new_hospital_cases_n <- completeDF$new_hospital_cases / completeDF$population
completeDF$people_vaccinated_n  <- completeDF$people_vaccinated / completeDF$population

# Drop rows daily vaccinations < 0
completeDF <- completeDF[!(completeDF$daily_vaccinations < 0),]

# Summary statistics of dataframe
completeDF.sub <- subset(completeDF, select = c(location, date, daily_vaccinations, tweet_sentiment, approve_estimate, 
                                               disapprove_estimate, new_cases, new_hospital_cases, people_vaccinated,
                                               high_school_percentage,bachelor_degree_percentage,age_median,
                                               daily_vaccinations_n,new_cases_n,new_hospital_cases_n,people_vaccinated_n))

str(completeDF.sub)
stargazer(completeDF.sub, type='text')
stargazer(completeDF.sub)

# Determine state averages of the included variables, as well
# as the number of non missing observations during the 
# selected observation period
completeDF.avg <- 
  ddply(completeDF.sub, .(location), summarise,
        avg.daily_vaccinations     = mean(daily_vaccinations, na.rm=TRUE),
        avg.tweet_sentiment = mean(tweet_sentiment, na.rm=TRUE),
        avg.approve_estimate  = mean(approve_estimate, na.rm=TRUE),
        avg.disapprove_estimate  = mean(disapprove_estimate, na.rm=TRUE),
        avg.new_cases = mean(new_cases, na.rm=TRUE),
        avg.new_hospital_cases = mean(new_hospital_cases, na.rm=TRUE),
        avg.people_vaccinated = mean(people_vaccinated, na.rm=TRUE),
        avg.high_school_percentage = mean(high_school_percentage, na.rm=TRUE),
        avg.bachelor_degree_percentage = mean(bachelor_degree_percentage, na.rm=TRUE),
        avg.age_median = mean(age_median, na.rm=TRUE),
        avg.daily_vaccinations_n = mean(daily_vaccinations_n, na.rm=TRUE),
        avg.new_cases_n = mean(new_cases_n, na.rm=TRUE),
        avg.new_hospital_cases_n = mean(new_hospital_cases_n, na.rm=TRUE),
        avg.people_vaccinated_n = mean(people_vaccinated_n, na.rm=TRUE),
        numValid          = length(location))

# Merge averages in completeDF.avg with completeDF.sub
completeDF.sub <- merge(completeDF.sub, completeDF.avg, by="location")

attach(completeDF.sub)
completeDF.sub$diff.daily_vaccinations         <- daily_vaccinations         - avg.daily_vaccinations
completeDF.sub$diff.tweet_sentiment            <- tweet_sentiment            - avg.tweet_sentiment
completeDF.sub$diff.approve_estimate           <- approve_estimate           - avg.approve_estimate
completeDF.sub$diff.disapprove_estimate        <- disapprove_estimate        - avg.disapprove_estimate
completeDF.sub$diff.new_cases                  <- new_cases                  - avg.new_cases
completeDF.sub$diff.new_hospital_cases         <- new_hospital_cases         - avg.new_hospital_cases
completeDF.sub$diff.people_vaccinated          <- people_vaccinated          - avg.people_vaccinated

completeDF.sub$diff.daily_vaccinations_n       <- daily_vaccinations_n       - avg.daily_vaccinations_n
completeDF.sub$diff.new_cases_n                <- new_cases_n                - avg.new_cases_n
completeDF.sub$diff.new_hospital_cases_n       <- new_hospital_cases_n       - avg.new_hospital_cases_n
completeDF.sub$diff.people_vaccinated_n        <- people_vaccinated_n        - avg.people_vaccinated_n

completeDF.sub$diff.high_school_percentage     <- 0
completeDF.sub$diff.bachelor_degree_percentage <- 0
completeDF.sub$diff.age_median                 <- 0
detach(completeDF.sub)

# Make data balanced
#completeDF.sub.tmp <- completeDF.sub[completeDF.sub$numValid == 185,]
#completeDF.avg <- completeDF.avg[completeDF.avg$numValid == 185,]

# Save dataframes
write.csv(completeDF, file.path(dirData, "completeDF.csv"))
write.csv(completeDF.sub, file.path(dirData, "completeDFsub.csv"))
write.csv(completeDF.avg, file.path(dirData, "completeDFavg.csv"))

