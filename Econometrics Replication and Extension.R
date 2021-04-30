rm(list=ls())

library(tidyverse)
library(readr)
library(readxl)
library(plm)
library(data.table)
library(naniar)
library(tidyr)
library(dplyr)

options(scipen = 100)
options(digits=5)





#Load txt file with population opioid rate and clean the columns
opioid_table <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\popopioids.txt")
opioid_table <- head(opioid_table, 816)
opioid_table$Year.Code <- NULL
opioid_table$State.Code <- NULL
opioid_table$Notes <- NULL
opioid_table$Crude.Rate <- NULL
opioid_table$opioid_deaths <- as.numeric(opioid_table$Deaths)
opioid_table$Deaths <- NULL
opioid_table$pop_100k <- opioid_table$Population/100000
opioid_table$opioid_rate <- opioid_table$opioid_deaths/opioid_table$pop_100k

#Load txt file with population drug rate and add to opioid table
drug_table <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\popdrugs.txt")
drug_table <- head(drug_table, 816)
opioid_table$drug_deaths <- drug_table$Deaths
opioid_table$drug_rate <- as.numeric(opioid_table$drug_deaths/opioid_table$pop_100k)

#Load txt file with white opioid rate and add to opioid table
whiteopioids <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\WhiteOpioids.txt")
whiteopioids <- head(whiteopioids, 816)

opioid_table$whitepop <- whiteopioids$Population
opioid_table$whitepop_100k <- opioid_table$whitepop/100000
opioid_table$white_op_death <- as.numeric(whiteopioids$Deaths)
opioid_table$whiteop_rate <- opioid_table$white_op_death/opioid_table$whitepop_100k

#Load txt file with white drug rate and add to opioid table
whitedrugs <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\WhiteDrugs.txt")
whitedrugs <- head(whitedrugs, 816)

opioid_table$white_drug_death <- as.numeric(whitedrugs$Deaths)
opioid_table$white_drug_rate <- opioid_table$white_drug_death/opioid_table$pop_100k

#Load txt file with black opioid rate and add to opioid table
blackopioids <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\BlackOpioids.txt")
blackopioids <- head(blackopioids, 816)

opioid_table$blackpop <- blackopioids$Population
opioid_table$blackpop_100k <- opioid_table$blackpop/100000
opioid_table$blackop_death <- as.numeric(blackopioids$Deaths)
opioid_table$blackop_rate <- opioid_table$blackop_death/opioid_table$blackpop_100k

#Load txt file with black drug rate and add to opioid table
blackdrugs <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\BlackDrugs.txt")
blackdrugs <- head(blackdrugs, 816)
opioid_table$black_drug_death <- as.numeric(blackdrugs$Deaths)
opioid_table$black_drug_rate <- opioid_table$black_drug_death/opioid_table$blackpop_100k
  
#Load txt file with hispanic opioid rate and add to opioid table
hispanicopioids <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\HispanicOpioids.txt")
hispanicopioids <- head(hispanicopioids, 816)
opioid_table$hispanicpop <- hispanicopioids$Population
opioid_table$hispanicpop_100k <- opioid_table$hispanicpop/100000
opioid_table$hispanicop_death <- as.numeric(hispanicopioids$Deaths)
opioid_table$hispanicop_rate <- opioid_table$hispanicop_death/opioid_table$hispanicpop_100k
  
#Load txt file with hispanic drug rate and add to opioid table
hispanicdrugs <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\HispanicDrugs.txt")
hispanicdrugs <- head(hispanicdrugs, 816)
opioid_table$hispanic_drug_death <- as.numeric(hispanicdrugs$Deaths)
opioid_table$hispanic_drug_rate <- opioid_table$hispanic_drug_death/opioid_table$hispanicpop_100k

  
#Load Excel file with 50 states unemployment
#Source: Iowa State University 

unemployment <-read_xls("C:\\Users\\tyler\\Desktop\\Econometrics Project\\emp-unemployment.xls",
                        sheet = 2)

#Clean unemployment table to prep for merge with opioid table

unemployment <- unemployment[, -c(3:21)]
unemployment <- unemployment[, -c(19:22)]
unemployment$`Average Annual Unemployment Rates by State (see previous tab for Iowa's counties)` <- NULL
colnames(unemployment) <- c("State", "1999", "2000", "2001", "2002", "2003", 
                            "2004", "2005", "2006", "2007", "2008", "2009", 
                            "2010", "2011", "2012", "2013", "2014")
unemployment <- unemployment[-c(1:6),]
unemployment <- unemployment[-c(52:60),]

#Add unemployment column to opioid table
opioid_table$unemployment <- data.frame(a = c(unemployment$`1999`, unemployment$`2000`, 
                                              unemployment$`2001`, unemployment$`2002`, 
                                              unemployment$`2003`, unemployment$`2004`, 
                                              unemployment$`2005`, unemployment$'2006', 
                                              unemployment$'2007', unemployment$'2008', 
                                              unemployment$'2009', unemployment$'2010', 
                                              unemployment$'2011', unemployment$'2012', 
                                              unemployment$'2013', unemployment$'2014'))
opioid_table$unemployment <- unlist(opioid_table$unemployment)


#Convert Suppressed and Unreliable values to NA
#opioid_table[opioid_table=="Suppressed"] <- NA
#opioid_table[opioid_table=="Unreliable"] <- NA

#Remove all rows containing NA values *Note: This significantly shrinks the dataset
#opioid_table <- na.omit(opioid_table)

#Regression for Total Population Opioid Deaths
pop_opioid <- plm(as.numeric(opioid_table$opioid_rate) ~ opioid_table$unemployment,
                     data = opioid_table,
                     index = c('State', 'Year'),
                     model = "within")

summary(pop_opioid)

#Regression for Total Population Opioid Deaths + State Fixed Effects
pop_opioid_tt <- plm(as.numeric(opioid_table$opioid_rate) ~ opioid_table$unemployment + opioid_table$State: as.integer(opioid_table$Year),
                     index = c('State', 'Year'),
                     data = opioid_table,
                     model = "within")

summary(pop_opioid_tt)

#Regression for Total Population All Drug Deaths
pop_alldrugs <- plm(opioid_table$drug_rate ~ opioid_table$unemployment,
                                   data = opioid_table,
                                  index = c('State', 'Year'),
                                   model = "within")

summary(pop_alldrugs)

#Regression for Total Population All Drug Deaths + State Fixed Effects
pop_alldrugs_tt <- plm(opioid_table$drug_rate ~ opioid_table$unemployment + opioid_table$State: as.integer(opioid_table$Year),
                    index = c('State', 'Year'), 
                    data = opioid_table,
                     model = "within")

summary(pop_alldrugs_tt)

#Regression for White Opioids 
white_opioid <- plm(opioid_table$whiteop_rate ~ opioid_table$unemployment,
                                   data = opioid_table,
                                   index = c('State', 'Year'),
                                   model = "within")

summary(white_opioid)

#Regression for White Opioid Deaths + State Time Trends
white_opioid_tt <- plm(opioid_table$whiteop_rate ~ opioid_table$unemployment + opioid_table$State: as.integer(opioid_table$Year),
                       index = c('State', 'Year'), 
                       data = opioid_table,
                       model = "within")

summary(pop_white_opioid_tt)

#Regression for White Drugs
white_drugs<- plm(opioid_table$white_drug_rate ~ opioid_table$unemployment,
                    data = opioid_table,
                    index = c('State', 'Year'),
                    model = "within")

summary(white_drugs)

#Regression for White Drugs + TT
white_drugs_tt<- plm(opioid_table$white_drug_rate ~ opioid_table$unemployment +opioid_table$State: as.integer(opioid_table$Year),
                  data = opioid_table,
                  index = c('State', 'Year'),
                  model = "within")

summary(white_drugs_tt)

#Regression for Black Opioids
black_opioids <- plm(opioid_table$blackop_rate ~ opioid_table$unemployment,
                  data = opioid_table,
                  index = c('State', 'Year'),
                  model = "within")

summary(black_opioids)

#Regression for Black Opioids + TT
black_opioids_tt <- plm(opioid_table$blackop_rate ~ opioid_table$unemployment +opioid_table$State: as.integer(opioid_table$Year),
                     data = opioid_table,
                     index = c('State', 'Year'),
                     model = "within")

summary(black_opioids_tt)

#Regression for Black Drugs
black_drugs <- plm(opioid_table$black_drug_rate ~ opioid_table$unemployment,
                  data = opioid_table,
                  index = c('State', 'Year'),
                  model = "within")

summary(black_drugs)

#Regression for Black Drugs + TT
black_drugs_tt <- plm(opioid_table$black_drug_rate ~ opioid_table$unemployment +opioid_table$State: as.integer(opioid_table$Year),
                   data = opioid_table,
                   index = c('State', 'Year'),
                   model = "within")

summary(black_drugs_tt)

#Regression for Hispanic Opioids
hispanic_opioids <- plm(opioid_table$hispanicop_rate ~ opioid_table$unemployment,
                     data = opioid_table,
                     index = c('State', 'Year'),
                     model = "within")

summary(hispanic_opioids)

#Regression for Hispanic Opioids + TT
hispanic_opioids_tt <- plm(opioid_table$hispanicop_rate ~ opioid_table$unemployment +opioid_table$State: as.integer(opioid_table$Year),
                        data = opioid_table,
                        index = c('State', 'Year'),
                        model = "within")

summary(hispanic_opioids_tt)

#Regression for Hispanic Drugs
hispanic_drugs <- plm(opioid_table$hispanic_drug_rate ~ opioid_table$unemployment,
                                 data = opioid_table,
                                 index = c('State', 'Year'),
                                 model = "within")

summary(hispanic_drugs)

#Regression for Hispanic Drugs + TT
hispanic_drugs_tt <- plm(opioid_table$hispanic_drug_rate ~ opioid_table$unemployment +opioid_table$State: as.integer(opioid_table$Year),
                      data = opioid_table,
                      index = c('State', 'Year'),
                      model = "within")

summary(hispanic_drugs_tt)



opioid_mean <- mean(na.omit(opioid_table$opioid_rate))
white_opioid_mean<- mean(na.omit(opioid_table$whiteop_rate))
black_opioid_mean<- mean(na.omit(opioid_table$blackop_rate))
hispanic_opioid_mean<- mean(na.omit(opioid_table$hispanicop_rate))

drugs_mean<- mean(na.omit(opioid_table$drug_rate))
white_drugs_mean<- mean(na.omit(opioid_table$white_drug_rate))
black_drugs_mean<- mean(na.omit(opioid_table$black_drug_rate))
hispanic_drugs_mean<- mean(na.omit(opioid_table$hispanic_drug_rate))

#Create table for opioid deaths with coefficients, means, and observations
#This is the replication of the top half of Table 5 from the author's paper

opioiddeath_table <- data.frame(
  All= c(pop_opioid$coefficients, coef(summary(pop_opioid))[, 2], opioid_mean, 816),
  AllTimeTrends = c(pop_opioid_tt$coefficients[1], coef(summary(pop_opioid_tt))[1, 2], opioid_mean, 816),
  White = c(white_opioid$coefficients, coef(summary(white_opioid))[, 2], white_opioid_mean, 816),
  WhiteTimeTrends = c(white_opioid_tt$coefficients[1], coef(summary(white_opioid_tt))[1, 2], white_opioid_mean, 816),
  Black = c(black_opioids$coefficients, coef(summary(black_opioids))[, 2], black_opioid_mean, 816),
  BlackTimeTrends = c(black_opioids_tt$coefficients[1], coef(summary(black_opioids_tt))[1, 2], black_opioid_mean, 816),
  Hispanic = c(hispanic_opioids$coefficients, coef(summary(hispanic_opioids))[, 2], hispanic_opioid_mean, 816),
  HispanicTimeTrends = c(hispanic_opioids_tt$coefficients[1],coef(summary(hispanic_opioids_tt))[1, 2], hispanic_opioid_mean, 816)
  
)
opioiddeath_table <- format(round(opioiddeath_table, 2), nsmall = 1)
row.names(opioiddeath_table) <- c("Unemployment rate [0-100]", "Standard Error", "Mean of dependent variable", "Observations")

#Create table for drug deaths with coefficients, means, and observations
drugdeath_table <- data.frame(
  All = c(pop_alldrugs$coefficients, coef(summary(pop_alldrugs))[, 2], drugs_mean, 816),
  AllTimeTrends = c(pop_alldrugs_tt$coefficients[1],coef(summary(pop_alldrugs_tt))[1, 2], drugs_mean, 816),
  White = c(white_drugs$coefficients, coef(summary(white_drugs))[, 2], white_drugs_mean, 816),
  WhiteTimeTrends = c(white_drugs_tt$coefficients[1],coef(summary(white_drugs_tt))[1, 2], white_drugs_mean, 816),
  Black = c(black_drugs$coefficients, coef(summary(black_drugs))[, 2],black_drugs_mean, 816),
  BlackTimeTrends = c(black_drugs_tt$coefficients[1], coef(summary(black_drugs_tt))[1, 2], black_drugs_mean, 816),
  Hispanic = c(hispanic_drugs$coefficients, coef(summary(hispanic_drugs))[, 2], hispanic_drugs_mean, 816),
  HispanicTimeTrends = c(hispanic_drugs_tt$coefficients[1],coef(summary(hispanic_drugs_tt))[1, 2], hispanic_drugs_mean, 816)
  
)
drugdeath_table <- format(round(drugdeath_table, 2), nsmall = 1)
row.names(drugdeath_table) <- c("Unemployment rate [0-100]", "Standard Error", "Mean of dependent variable", "Observations")

#Save Dataframe to be used in Extension
library("writexl")
write_xlsx(opioid_table,"C:\\Users\\tyler\\Desktop\\Econometrics Project\\opioid_table.xlsx")


#===============================================================================
#===============================================================================
#Extension Project
#===============================================================================
#===============================================================================

#Load Excel files with health insurance data. All data obtained from US Census Bureau.

hi_99 <-read_xls("C:\\Users\\tyler\\Desktop\\Econometrics Project\\hi 1999-2009.xls") 
hi_2019 <-read_xlsx("C:\\Users\\tyler\\Desktop\\Econometrics Project\\health insurance to 2019.xlsx")


#Clean first dataset to only contain 1999-2009 US Health Insurance Coverage percent
hi_99 <- head(hi_99, 17)
hi_99 <- tail(hi_99, -4)
hi_99 <- hi_99[,c(1, 9)]
hi_99 <- tail(hi_99, -2)
colnames(hi_99) <- c('Year', 'hi_coverage')

#Clean second data set to only contain 2010-2014 US Health Insurance Coverage percent
hi_2019 <- head(hi_2019, 15)
hi_2019 <- tail(hi_2019, -2)
hi_2019 <- hi_2019[4, c(25, 29, 33, 37,41)]
colnames(hi_2019) <- c(2014, 2013, 2012, 2011, 2010)
hi_2019 <-as.data.frame(t(hi_2019))
hi_2019$Year <- as.character(c(2014, 2013, 2012, 2011, 2010))
hi_2019 <- hi_2019[, c(2, 1)]
colnames(hi_2019) <- c('Year', 'hi_coverage')

#Combine two datasets
health_insurance <- bind_rows(hi_99, hi_2019)
health_insurance <- health_insurance[order(health_insurance$Year),]


#===============================================================================
#===============================================================================
#Produce Charts
#===============================================================================
#===============================================================================

#Load Excel Sheet with nationwide unemployment data
US_death_unemployment <- read_excel("C:\\Users\\tyler\\Desktop\\Econometrics Project\\USunemployment.xlsx")
#Run cleaning commands on dataframe
US_death_unemployment <- US_death_unemployment[12:27,]
colnames(US_death_unemployment) = c("Year", "Unemployment Rate")
US_death_unemployment$`Unemployment Rate` <- as.numeric(US_death_unemployment$`Unemployment Rate`)
#Load txt file with US Opioid death rate and combine with Unemployment Table
US_op <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\nationalopioids.txt")
op_rate <- US_op[1:16, 6]
US_death_unemployment$OpioidRate <- op_rate

#Load txt file with US death rate from all drugs and add to Unemployment Table
US_drug <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\nationaldrugs.txt")
drug_rate <- US_drug[1:16, 6]
US_death_unemployment$DrugRate <- drug_rate

#Create space for margin text
par(mar=c(5, 4, 4, 6) + 0.1)

#Plot Drug Deaths
plot(US_death_unemployment$Year, US_death_unemployment$DrugRate, type="l", 
     lty = 5, col = "blue", 
     ylim=c(0,15), xlim=c(1999, 2014), 
     xlab = "Year", 
     ylab = "Unemployment Rate, [0-100]; Death Rates are Deaths per 100k",
     main = "Drug Death, Opioid Death, and Unemployment Rates 1999-2014")


#Plot unemployment
lines(US_death_unemployment$Year, US_death_unemployment$`Unemployment Rate`, type = "b",lty = 5, col = "black")

#Plot opioid deaths
lines(US_death_unemployment$Year, US_death_unemployment$OpioidRate, type = "l", col = "forestgreen")


#Expand labels for each year
axis(side = 1, 
     at = US_death_unemployment$Year, 
     labels = US_death_unemployment$Year,
     tck=-.05)

#horizontal lines
abline(h=0, col="grey", lty = "longdash")
abline(h=5, col="grey", lty = "longdash")
abline(h=10, col="grey", lty = "longdash")
abline(h=15, col="grey", lty = "longdash")

#Legend
legend("topleft",
       c("All Drugs","Unemployment Rate", "Opioids"),
       fill=c("blue","black", "forestgreen"))
#===============================================================================
#===============================================================================
#Extension Chart
#===============================================================================
#===============================================================================

#Load Excel Sheet with nationwide unemployment data
US_death_unemployment <- read_excel("C:\\Users\\tyler\\Desktop\\Econometrics Project\\USunemployment.xlsx")
#Run cleaning commands on dataframe
US_death_unemployment <- US_death_unemployment[12:27,]
colnames(US_death_unemployment) = c("Year", "Unemployment Rate")
US_death_unemployment$`Unemployment Rate` <- as.numeric(US_death_unemployment$`Unemployment Rate`)
#Load txt file with US Opioid death rate and combine with Unemployment Table
US_op <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\nationalopioids.txt")
op_rate <- US_op[1:16, 6]
US_death_unemployment$OpioidRate <- op_rate

#Load txt file with US death rate from all drugs and add to Unemployment Table
US_drug <- read.delim("C:\\Users\\tyler\\Desktop\\Econometrics Project\\nationaldrugs.txt")
drug_rate <- US_drug[1:16, 6]
US_death_unemployment$DrugRate <- drug_rate

#Create space for margin text
par(mar=c(5, 4, 4, 6) + 0.1)

#Plot Drug Deaths
plot(US_death_unemployment$Year, US_death_unemployment$DrugRate, type="l", 
     lty = 5, col = "blue", 
     ylim=c(0,15), xlim=c(1999, 2014), 
     xlab = "Year", 
     ylab = "Unemployment Rate, [0-100]; Death Rates are Deaths per 100k",
     main = "US Health Insurance Coverage in Relation to Opioid Abuse and Unemployment Rates")


#Plot unemployment
lines(US_death_unemployment$Year, US_death_unemployment$`Unemployment Rate`, type = "b",lty = 5, col = "black")

#Plot opioid deaths
lines(US_death_unemployment$Year, US_death_unemployment$OpioidRate, type = "l", col = "forestgreen")


#Expand labels for each year
axis(side = 1, 
     at = US_death_unemployment$Year, 
     labels = US_death_unemployment$Year,
     tck=-.05)

#horizontal lines
abline(h=0, col="grey", lty = "longdash")
abline(h=5, col="grey", lty = "longdash")
abline(h=10, col="grey", lty = "longdash")
abline(h=15, col="grey", lty = "longdash")

#Legend
legend("topleft",
       c("All Drugs","Unemployment Rate", "Opioids", 'Health Insurance Coverage'),
       fill=c("blue","black", "forestgreen", "red"))

#Add Health Insurance coverage data to table
US_death_unemployment$HealthCoverage <- as.numeric(health_insurance$hi_coverage)

par(new=TRUE)
plot(US_death_unemployment$Year, US_death_unemployment$HealthCoverage, axes=FALSE, type = "l", col = "red", xlab="", ylab="")
mtext("Health Coverage, [0-100]",side=4,col="red",line=2) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)


#===============================================================================
#===============================================================================
# Extension Regression and Table


unemployment_opioid_only <- lm(US_death_unemployment$OpioidRate~US_death_unemployment$`Unemployment Rate`)
summary(unemployment_opioid_only)

unemployment_drugs_only <- lm(US_death_unemployment$DrugRate~US_death_unemployment$`Unemployment Rate`)
summary(unemployment_drugs_only)

unemployment_insurance_opioid <- lm(US_death_unemployment$OpioidRate~US_death_unemployment$`Unemployment Rate`+ US_death_unemployment$HealthCoverage)
summary(unemployment_insurance_opioid)

unemployment_insurance_drugs <- lm(US_death_unemployment$DrugRate~US_death_unemployment$`Unemployment Rate`+ US_death_unemployment$HealthCoverage)
summary(unemployment_insurance_drugs)
#Health Coverage shows a negative correlation associated with opioid and drug deaths. 

extension_table <- data.frame(
  'Opioid Deaths on Unemployment' = c(unemployment_opioid_only$coefficients, NA, coef(summary(unemployment_opioid_only))[2, 2]),
  'Opioid Deaths on Unemployment controlling for Health Coverage' = c(unemployment_insurance_opioid$coefficients, coef(summary(unemployment_insurance_opioid))[2, 2]),
  'Drug Deaths on Unemployment' = c(unemployment_drugs_only$coefficients, NA, coef(summary(unemployment_drugs_only))[2, 2]),
  'Drug Deaths on Unemployment controlling for Health Coverage' = c(unemployment_insurance_drugs$coefficients, coef(summary(unemployment_insurance_drugs))[2, 2])

  
)
extension_table <- format(round(extension_table, 2), nsmall = 1)
row.names(extension_table) <- c("Intercept", "Unemployment Effect on Death Rate", "Health Coverage Effect on Death Rate", "Standard Error")
colnames(extension_table) <- c("Opioid Rate", "Opioid Rate controlling for Health Coverage", "Drug Rate", "Drug Rate controlling for Health Coverage")




