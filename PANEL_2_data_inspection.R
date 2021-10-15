#-----------------------------------------------------------------------------------------
# Load libraries
#-----------------------------------------------------------------------------------------
library(stargazer)
library(ggplot2)
library(reshape2)
library(plyr)
library(plm)
library(zoo)
require(gridExtra)

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
completeDF    <- read.csv(file=paste0(dirData,"completeDF.csv"), sep =',')
completeDFavg <- read.csv(file=paste0(dirData,"completeDFavg.csv"), sep =',')
completeDFsub <- read.csv(file=paste0(dirData,"completeDFsub.csv"), sep =',')

#-----------------------------------------------------------------------------------------
# Data inspection
#-----------------------------------------------------------------------------------------
# Statistics of dataframe
str(completeDF)
stargazer(completeDF)

# Plot all variables to check distribution
plot_tweet_sentiment            <- ggplot(completeDF, aes(x=tweet_sentiment))            + geom_histogram(binwidth=.2)
plot_approve_estimate           <- ggplot(completeDF, aes(x=approve_estimate))           + geom_histogram()
# plot_disapprove_estimate        <- ggplot(completeDF, aes(x=disapprove_estimate))        + geom_histogram()
# plot_new_cases                  <- ggplot(completeDF, aes(x=new_cases))                  + geom_histogram()
plot_new_hospital_cases         <- ggplot(completeDF, aes(x=new_hospital_cases))         + geom_histogram()
# plot_people_vaccinated          <- ggplot(completeDF, aes(x=people_vaccinated))          + geom_histogram()
plot_high_school_percentage     <- ggplot(completeDF, aes(x=high_school_percentage))     + geom_histogram()
plot_bachelor_degree_percentage <- ggplot(completeDF, aes(x=bachelor_degree_percentage)) + geom_histogram()
plot_age_median                 <- ggplot(completeDF, aes(x=age_median))                 + geom_histogram()
plot_age_population             <- ggplot(completeDF, aes(x=population))                 + geom_histogram()
plot_age_daily_vaccinations            <- ggplot(completeDF, aes(x=daily_vaccinations))          + geom_histogram()

# 12 plots in 1 frame
# grid.arrange(plot_tweet_sentiment,plot_approve_estimate,plot_disapprove_estimate,plot_new_cases,plot_new_hospital_cases,plot_people_vaccinated,
#              plot_high_school_percentage,plot_bachelor_degree_percentage,plot_age_median,ncol=3)
grid.arrange(plot_tweet_sentiment,plot_approve_estimate,plot_new_cases,plot_new_hospital_cases,
             plot_high_school_percentage,plot_bachelor_degree_percentage,plot_age_median,plot_age_population,plot_age_daily_vaccinations,ncol=3)


# multi <- arrangeGrob(plot_tweet_sentiment,plot_approve_estimate,plot_disapprove_estimate,plot_new_cases,plot_new_hospital_cases,plot_people_vaccinated,
#              plot_high_school_percentage,plot_bachelor_degree_percentage,plot_age_median,ncol=3)
multi <- arrangeGrob(plot_tweet_sentiment,plot_approve_estimate,plot_new_cases,plot_new_hospital_cases,
                     plot_high_school_percentage,plot_bachelor_degree_percentage,plot_age_median,plot_age_population,plot_age_daily_vaccinations,ncol=3)

ggsave(paste0(dirRslt,"independent_variables_dist.pdf"), multi)

# Plot all normalized variables
plot_daily_vaccinations_n       <- ggplot(completeDF, aes(x=daily_vaccinations_n))       + geom_histogram(binwidth=.001)
plot_new_cases_n                <- ggplot(completeDF, aes(x=new_cases_n))                + geom_histogram()
plot_new_cases_n                <- ggplot(completeDF, aes(x=new_hospital_cases_n))       + geom_histogram()
# plot_people_vaccinated_n        <- ggplot(completeDF, aes(x=people_vaccinated_n ))       + geom_histogram()

# 4 plots in 1 frame
# grid.arrange(plot_daily_vaccinations_n,plot_new_cases_n,plot_new_cases_n,plot_people_vaccinated_n,ncol=2)
grid.arrange(plot_daily_vaccinations_n,plot_new_cases_n,plot_new_cases_n,ncol=2)

# multi_n <- arrangeGrob(plot_daily_vaccinations_n,plot_new_cases_n,plot_new_cases_n,plot_people_vaccinated_n,ncol=2)
multi_n <- arrangeGrob(plot_daily_vaccinations_n,plot_new_cases_n,plot_new_cases_n,ncol=2)
ggsave(paste0(dirRslt,"normalized_independent_variables_dist.pdf"), multi_n)


# Create temporary smaller dataframe of max 5000 obs for test function
temp <- head(completeDF, 5000)

# Check assumptions by performing tests
shapiro.test(temp$tweet_sentiment)
shapiro.test(temp$approve_estimate)
# shapiro.test(temp$disapprove_estimate)
shapiro.test(temp$new_cases)
shapiro.test(temp$new_hospital_cases)
# shapiro.test(temp$people_vaccinated)
shapiro.test(temp$high_school_percentage)
shapiro.test(temp$bachelor_degree_percentage)
shapiro.test(temp$age_median)

shapiro.test(temp$daily_vaccinations_n)
shapiro.test(temp$new_cases_n)
shapiro.test(temp$new_hospital_cases_n)
# shapiro.test(temp$people_vaccinated_n)

