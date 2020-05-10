# Data Wrangling in R
# Social Security Disability Case Study

#### Import ####

# Load the tidyverse
library(tidyverse)
library(lubridate)
library(stringr)

# Read in the coal dataset
ssa <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv")

# Take a look at how this was imported
glimpse(ssa) #The table seems fairly reasonable. There are a few NA, but that's because the data was collected until January of FY17.

#### Making the dataset long ####

#Each row contains data from many different observations.

#Convert tibble to long format
ssa_long<-gather(ssa, month, applications, -Fiscal_Year)
print(ssa_long, n = 20) #This looks good, but the totals and internet counts for the same observation are in different rows. To be addressed later.

#### Formatting Dates ####

#What are the unique months?
unique(ssa_long$month) #We see totals and internet counts...

#Separate totals and internet counts
ssa_long<-separate(ssa_long, month, c("month", "application_method"), sep="_")
print(ssa_long, n=20) #Variables are now separate

#What are the unique months?
unique(ssa_long$month) #Looks good, but the months are spelled out (not abbreviated) in the summer

#Abbreviate months
ssa_long$month<-substr(ssa_long$month, 1, 3)
unique(ssa_long$month) #The months are now abbreviated.

#What about the fiscal year
unique(ssa_long$Fiscal_Year)

#Convert year to a more standard format
ssa_long$Fiscal_Year<-str_replace(ssa_long$Fiscal_Year, "FY", "20")
unique(ssa_long$Fiscal_Year)

#Combine the month and year
#We will assume measurements were taken on the first of each month.

#Create date (character)
paste("01", ssa_long$month, ssa_long$Fiscal_Year)

#Convert to date
ssa_long$Date<-dmy(paste("01", ssa_long$month, ssa_long$Fiscal_Year))
unique(ssa_long$Date)

#### Dealing with Fiscal Year ####

#The US Government's fiscal year begins in October of the previous calendar year, not January.
#We thus need to correct our dates.

#Find the dates with years that have to be changed (Oct, Nov, or Dec)
advanced_date<-which(month(ssa_long$Date)>=10)

#Change the year
year(ssa_long$Date[advanced_date])<-year(ssa_long$Date[advanced_date])-1 #Everything is now in calendar years

#### Widening the data set ####

summary(ssa_long) #There are some unneccessary variables (months, fiscal year)

#Remove unneccessary variables
ssa_long$month<-NULL
ssa_long$Fiscal_Year<-NULL

#Change the application_method to a factor
ssa_long$application_method<-as.factor(ssa_long$application_method)
summary(ssa_long)

#The number of applications for the two methods are part of the same observation, and should be in the same rows

#Widen data
ssa<-spread(ssa_long, application_method, applications)
print(ssa, n=20) #The data is tidy

#### Visualization ####

#Have the efforts from the Social Security Administration to move applications online been successful?

#Percentage of online applications
ssa$online_percentage<-ssa$Internet/ssa$Total *100

#Plot the percentage over time
ggplot(ssa, mapping=aes(x=Date, y=online_percentage))+
  geom_point() #The NA warning is not a problem, we need there was some missing data due to upcoming months

#The efforts have been successful
