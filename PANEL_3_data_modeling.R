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
completeDF.avg <- read.csv(file=paste0(dirData,"completeDFavg.csv"), sep =',')
completeDF.sub <- read.csv(file=paste0(dirData,"completeDFsub.csv"), sep =',')

#-----------------------------------------------------------------------------------------
# Model formulation
#-----------------------------------------------------------------------------------------

# Model with interaction term and NO normalized variables
mdlA <- daily_vaccinations ~ tweet_sentiment + approve_estimate + new_hospital_cases + tweet_sentiment:high_school_percentage + tweet_sentiment:bachelor_degree_percentage + tweet_sentiment:age_median

# Model without interaction term and NO normalized variables
mdlB <- daily_vaccinations ~ tweet_sentiment + approve_estimate + new_hospital_cases

# Model with interaction term and normalized variables
mdlC <- daily_vaccinations_n ~ tweet_sentiment + approve_estimate + new_hospital_cases_n + tweet_sentiment:high_school_percentage + tweet_sentiment:bachelor_degree_percentage + tweet_sentiment:age_median

# Model without interaction term and normalized variables
mdlD <- daily_vaccinations_n ~ tweet_sentiment + approve_estimate + new_hospital_cases_n

#-----------------------------------------------------------------------------------------
# SET MODEL
#-----------------------------------------------------------------------------------------

mdl <- mdlD

#-----------------------------------------------------------------------------------------
# Preparation
#-----------------------------------------------------------------------------------------

# create variance data
mdlVars      <- all.vars(mdl)
mdlVars.avg  <- paste0("avg.", mdlVars)
mdlVars.diff <- paste0("diff.", mdlVars)

# create between and within frames
completeDF.between <- completeDF.avg[mdlVars.avg]
completeDF.within  <- completeDF.sub[mdlVars.diff]

# rename column names in order to make use of the same model specification mdlA, and to 
# conveniently merge the regresssion objects in stargazer
colnames(completeDF.within) <- 
  gsub("diff\\.", "", colnames(completeDF.within))
colnames(completeDF.between) <- 
  gsub("avg\\.", "", colnames(completeDF.between))

#-----------------------------------------------------------------------------------------
# Pooled regression model
#-----------------------------------------------------------------------------------------

# estimation of the pooled model
rsltPool <- lm (mdl, data=completeDF.sub)

# results
#summary(rsltPool)
#stargazer(rsltPool,align=TRUE, no.space=TRUE, intercept.bottom = FALSE,type='text')

#-----------------------------------------------------------------------------------------
# Between regression model
#-----------------------------------------------------------------------------------------

# estimation of the between model
rsltBetween <- lm (mdl, data=completeDF.between)

# results
#summary(rsltBetween)
#stargazer(rsltBetween,align=TRUE, no.space=TRUE, intercept.bottom = FALSE,type='text')

#-----------------------------------------------------------------------------------------
# Fixed effect regression model
#-----------------------------------------------------------------------------------------

# estimation fixed effect model ('within'). 
rsltFE.State <- plm(mdl, data = completeDF.sub, index=c("location", "date"), model = "within")

# results
#summary(rsltFE.State)
#stargazer(rsltFE.State,align=TRUE, no.space=TRUE, intercept.bottom = FALSE,type='text')

#-----------------------------------------------------------------------------------------
# Random effect regression model
#-----------------------------------------------------------------------------------------

# estimation random effect model ('random')
rsltRE.State <- plm(mdl, data = completeDF.sub, index=c("location", "date"), model = "random")

#-----------------------------------------------------------------------------------------
# Results
#-----------------------------------------------------------------------------------------

# Find fixed variable coefficients
coefficients(rsltFE.State)

round(cbind(VCFE.avg =sapply(coefficients(rsltFE.State), mean, na.rm = TRUE),
            VCFE.std =sapply(coefficients(rsltFE.State), sd, na.rm = TRUE)),3)

# Find random variable coefficients
coefficients(rsltRE.State)

# Tabulate the results
stargazer(rsltPool, rsltBetween, rsltFE.State, rsltRE.State,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

# Tabulate the results for OVERLEAF
stargazer(rsltPool, rsltBetween, rsltFE.State, rsltRE.State,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)

#-----------------------------------------------------------------------------------------
# RSME Calculation
#-----------------------------------------------------------------------------------------

df.ehat  <- rsltFE.State$df.residual
rmse     <- sqrt(sum(residuals(rsltFE.State)^2)/df.ehat)
round(cbind(df.ehat, rmse), 3)

#-----------------------------------------------------------------------------------------
# Test
#-----------------------------------------------------------------------------------------

# Hausman test: compare random and fixed effects models
phtest(rsltFE.State, rsltRE.State)

# Evaluate the fixed effects model versus the pooled regression model
pFtest(rsltFE.State, rsltPool)

#-----------------------------------------------------------------------------------------
# Plot 1.1
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot <- subset(completeDF, !is.na(daily_vaccinations_n) & !is.na(bachelor_degree_percentage),
                           select = c(location, date, daily_vaccinations_n, bachelor_degree_percentage))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot <- completeDF.sub.plot[completeDF.sub.plot$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot, aes(y = daily_vaccinations_n, x = bachelor_degree_percentage)) +
  #add annual outcomes coloured by country
  geom_point(aes(color = location), size = 2) +  #add regrssion line for countries
  #label the axes
  xlab("bachelor degree percentage") +
  ylab("daily vaccinations over time") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSbachelor.pdf"))

#-----------------------------------------------------------------------------------------
# Plot 2.1
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot <- subset(completeDF, !is.na(daily_vaccinations_n) & !is.na(approve_estimate),
                              select = c(location, date, daily_vaccinations_n, approve_estimate))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot <- completeDF.sub.plot[completeDF.sub.plot$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot, aes(y = daily_vaccinations_n, x = approve_estimate)) +
  #add annual outcomes coloured by country
  geom_point(aes(color = location), size = 2) +  #add regerssion line for states
  #label the axes
  #ylim(0, 500000) + #weird sign for axisx
  xlab("approve estimate") +
  ylab("daily vaccinations over time") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSapprove_estimate.pdf"))

#-----------------------------------------------------------------------------------------
# Plot 3.1
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot <- subset(completeDF, !is.na(daily_vaccinations_n) & !is.na(tweet_sentiment),
                              select = c(location, date, daily_vaccinations_n, tweet_sentiment))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot <- completeDF.sub.plot[completeDF.sub.plot$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot, aes(y = daily_vaccinations_n, x = tweet_sentiment)) +
  #add annual outcomes coloured by country
  geom_point(aes(color = location), size = 2) +  #add regerssion line for states
  #label the axes
  #ylim(0, 500000) + #weird sign for axisx
  xlab("tweet sentiment") +
  ylab("daily vaccinations over time") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVStweet_sentiment.pdf"))


#-----------------------------------------------------------------------------------------
# Plot 4.1
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot <- subset(completeDF, !is.na(daily_vaccinations_n) & !is.na(new_hospital_cases_n),
                              select = c(location, date, daily_vaccinations_n, new_hospital_cases_n))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot <- completeDF.sub.plot[completeDF.sub.plot$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot, aes(y = daily_vaccinations_n, x = new_hospital_cases_n)) +
  #add annual outcomes coloured by country
  geom_point(aes(color = location), size = 2) +  #add regerssion line for states
  #label the axes
  #ylim(0, 500000) + #weird sign for axisx
  xlab("new hospital cases") +
  ylab("daily vaccinations over time") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSnew_hospital_cases.pdf"))

#-----------------------------------------------------------------------------------------
# Plot 5.1
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot <- subset(completeDF, !is.na(daily_vaccinations_n) & !is.na(high_school_percentage),
                              select = c(location, date, daily_vaccinations_n, high_school_percentage))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot <- completeDF.sub.plot[completeDF.sub.plot$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot, aes(y = daily_vaccinations_n, x = high_school_percentage)) +
  #add annual outcomes coloured by country
  geom_point(aes(color = location), size = 2) +  #add regerssion line for states
  #label the axes
  #ylim(0, 500000) + #weird sign for axisx
  xlab("high school percentage") +
  ylab("daily vaccinations over time") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVShigh_school_percentage.pdf"))


#-----------------------------------------------------------------------------------------
# Plot 6.1
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot <- subset(completeDF, !is.na(daily_vaccinations_n) & !is.na(age_median),
                              select = c(location, date, daily_vaccinations_n, age_median))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot <- completeDF.sub.plot[completeDF.sub.plot$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot, aes(y = daily_vaccinations_n, x = age_median)) +
  #add annual outcomes coloured by country
  geom_point(aes(color = location), size = 2) +  #add regerssion line for states
  #label the axes
  #ylim(0, 500000) + #weird sign for axisx
  xlab("age median") +
  ylab("daily vaccinations over time") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSage_median2.pdf"))

#-----------------------------------------------------------------------------------------
# Plot 1.2
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot.avg <- subset(completeDF.sub, !is.na(avg.daily_vaccinations_n) & !is.na(avg.bachelor_degree_percentage),
      select = c(location, date, avg.daily_vaccinations_n, avg.bachelor_degree_percentage))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot.avg <- completeDF.sub.plot.avg[completeDF.sub.plot.avg$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot.avg, aes(y = avg.daily_vaccinations_n, x = avg.bachelor_degree_percentage)) +
  #add annual outcomes coloured by state
  geom_point(aes(color = location), size = 4) +  #add regression line for states
  geom_smooth(method="lm", se = FALSE, color = "black") +
  #label the axes
  xlab("avg bachelor degree percentage") +
  ylab("daily vaccinations") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSbachelor2.pdf"))


#-----------------------------------------------------------------------------------------
# Plot 2.2
#-----------------------------------------------------------------------------------------
#selecting observations with no missing values (balanced set)
completeDF.sub.plot.avg <- subset(completeDF.sub, !is.na(avg.daily_vaccinations_n) & !is.na(avg.approve_estimate),
                                  select = c(location, date, avg.daily_vaccinations_n, avg.approve_estimate))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot.avg <- completeDF.sub.plot.avg[completeDF.sub.plot.avg$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot.avg, aes(y = avg.daily_vaccinations_n, x = avg.approve_estimate)) +
  #add annual outcomes coloured by state
  geom_point(aes(color = location), size = 4) +  #add regression line for states
  geom_smooth(method="lm", se = FALSE, color = "black") +
  #label the axes
  xlab("avg approve estimate") +
  ylab("daily vaccinations") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSapprove_estimate2.pdf"))

#-----------------------------------------------------------------------------------------
# Plot 3.2
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot.avg <- subset(completeDF.sub, !is.na(avg.daily_vaccinations_n) & !is.na(avg.tweet_sentiment),
                                  select = c(location, date, avg.daily_vaccinations_n, avg.tweet_sentiment))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot.avg <- completeDF.sub.plot.avg[completeDF.sub.plot.avg$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot.avg, aes(y = avg.daily_vaccinations_n, x = avg.tweet_sentiment)) +
  #add annual outcomes coloured by state
  geom_point(aes(color = location), size = 4) +  #add regression line for states
  geom_smooth(method="lm", se = FALSE, color = "black") +
  #label the axes
  xlab("avg tweet sentiment") +
  ylab("daily vaccinations") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVStweet_sentiment2.pdf"))


#-----------------------------------------------------------------------------------------
# Plot 4.2
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot.avg <- subset(completeDF.sub, !is.na(avg.daily_vaccinations_n) & !is.na(avg.new_hospital_cases_n),
                                  select = c(location, date, avg.daily_vaccinations_n, avg.new_hospital_cases_n))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot.avg <- completeDF.sub.plot.avg[completeDF.sub.plot.avg$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot.avg, aes(y = avg.daily_vaccinations_n, x = avg.new_hospital_cases_n)) +
  #add annual outcomes coloured by state
  geom_point(aes(color = location), size = 4) +  #add regression line for states
  geom_smooth(method="lm", se = FALSE, color = "black") +
  #label the axes
  xlab("avg new hospital cases") +
  ylab("daily vaccinations") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSnew_hospital_cases2.pdf"))

#-----------------------------------------------------------------------------------------
# Plot 5.2
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot.avg <- subset(completeDF.sub, !is.na(avg.daily_vaccinations_n) & !is.na(avg.high_school_percentage),
                                  select = c(location, date, avg.daily_vaccinations_n, avg.high_school_percentage))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot.avg <- completeDF.sub.plot.avg[completeDF.sub.plot.avg$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot.avg, aes(y = avg.daily_vaccinations_n, x = avg.high_school_percentage)) +
  #add annual outcomes coloured by state
  geom_point(aes(color = location), size = 4) +  #add regression line for states
  geom_smooth(method="lm", se = FALSE, color = "black") +
  #label the axes
  xlab("avg high_school_percentage") +
  ylab("daily vaccinations") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVShigh_school_percentage2.pdf"))


#-----------------------------------------------------------------------------------------
# Plot 6.2
#-----------------------------------------------------------------------------------------

#selecting observations with no missing values (balanced set)
completeDF.sub.plot.avg <- subset(completeDF.sub, !is.na(avg.daily_vaccinations_n) & !is.na(avg.age_median),
                                  select = c(location, date, avg.daily_vaccinations_n, avg.age_median))

#select limited number of states for plotting
sublocation <- c("California", "Florida","Texas","Mississippi","Pennsylvania","Iowa","Alabama","Arizona","Massachusetts","West Virginia","Louisiana")
completeDF.sub.plot.avg <- completeDF.sub.plot.avg[completeDF.sub.plot.avg$location %in% sublocation,]

# Scatter Plot
ggplot(completeDF.sub.plot.avg, aes(y = avg.daily_vaccinations_n, x = avg.age_median)) +
  #add annual outcomes coloured by state
  geom_point(aes(color = location), size = 4) +  #add regression line for states
  geom_smooth(method="lm", se = FALSE, color = "black") +
  #label the axes
  xlab("avg age median") +
  ylab("daily vaccinations") + 
  theme(axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2))) +
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(paste0(dirRslt, "daily_vacVSage_median2.pdf"))











