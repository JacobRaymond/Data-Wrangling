# Data Wrangling in R
# Austin Water Quality Case Study

#### Import the Data ####

# Load in the libraries that we'll need
library(tidyverse)
library(stringr)
library(lubridate)

# Read in the dataset
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')

# Let's take a look at what we have
glimpse(water) #The import seems to have worked fine over all. All of the data has been imported as characters though.

#### Filtering ####

#There is a lot of unnecessary columns in the tible.

#Filtering the columns we want
water <- tibble('siteName'=water$SITE_NAME,
                'siteType'=water$SITE_TYPE,
                'sampleTime'=water$SAMPLE_DATE,
                'parameterType'=water$PARAM_TYPE,
                'parameter'=water$PARAMETER,
                'result'=water$RESULT,
                'unit'=water$UNIT)
glimpse(water) #Can we decrease the number of rows?

#We are only interested in pH and temperature, hence a large number of rows are redundant (these are stored as parameter)

#Examine the parameters
unique(water$parameter) #More than 3000

#Find pH
unique(water[which(str_detect(water$parameter, "PH")),]$parameter) #There are still 369 parameters names containing "pH"

#Is the parameter type useful?
unique(water$parameterType) #"Alkalinity/Hardness/pH" is likely to be associated to pH, "Conventionals" is likely associated with temperature as it is a basic measurement. 

#Filter to only keep rows with these types
filtered_water<-subset(water, (parameterType=="Alkalinity/Hardness/pH")|parameterType=="Conventionals")
glimpse(filtered_water) #A more reasonable 59,000 observations
unique(filtered_water$parameter) #Only 16 parameters remain

#Filter to keep only pH and Temperature
filtered_water<-subset(filtered_water, (parameter=="PH")|parameter=="WATER TEMPERATURE")
glimpse(filtered_water) 

#### Data Type Conversions ####

#All of the variables aside from "result" are stored as character.

#"Site Type" could be a factor. 
filtered_water$siteType<-as.factor(filtered_water$siteType)
summary(filtered_water) #Much better for analysis: we see a count for each site type

#Convert "parameter", "parameter type", and "unit" to factors
filtered_water$parameter<-as.factor(filtered_water$parameter)
filtered_water$parameterType<-as.factor(filtered_water$parameterType)
filtered_water$unit<-as.factor(filtered_water$unit)
summary(filtered_water)

#Sample time should not be a character
filtered_water$sampleTime #mdy format with time hms
filtered_water$sampleTime<-mdy_hms(filtered_water$sampleTime)
summary(filtered_water) #Can see max and mins for the times

#### Correcting data entry errors ####

#There are some errors in the unit

#One record has the unit listed as "feet"
subset(filtered_water, unit=="Feet") #78.9 is a reasonable value for Fahrenheit; likely the data was recorded as 78.9F which was intepreted as feet by the data entry clerk.
convert<-which(filtered_water$unit=="Feet")
filtered_water$unit[convert]<-"Deg. Fahrenheit"
summary(filtered_water) #Problem solved.

#Seven record has the unit listed as "mg/L"
subset(filtered_water, unit=="MG/L") #A mix of temperature and pH values. The parameters seem correct.
convert<-which(filtered_water$unit=="MG/L" & filtered_water$parameter=="PH")
filtered_water$unit[convert]<-"Standard units"
subset(filtered_water, unit=="MG/L") #We've taken care of the pH, but the temperatures seem to be a mix of Fahrenheit and Celsius measurements.
convert<-which(filtered_water$unit=="MG/L" & filtered_water$result>70) #Likely to be in fahrenheit
filtered_water$unit[convert]<-"Deg. Fahrenheit"
convert<-which(filtered_water$unit=="MG/L") #The remaining MG/L are Celsius measurements
filtered_water$unit[convert]<-"Deg. Celsius"
summary(filtered_water) #Problem solved.

#### Outliers ####

#Start with a basic scatterplot
ggplot(filtered_water, mapping=aes(x=sampleTime, y=result))+
  geom_point() #There is one clear outlier. Moreover, there was NA according to an error message

#Outlier
subset(filtered_water, result>1000000) 

#Remove the outlier and the NA
remove<-which(filtered_water$result>1000000 | is.na(filtered_water$result))
filtered_water<-filtered_water[-remove,]
summary(filtered_water) #There are still some pretty high values (max=7104)

#Find the remaining outliers
subset(filtered_water, result>1000) #Only 73 - can be removed wlog
remove<-which(filtered_water$result>1000)
filtered_water<-filtered_water[-remove,]
summary(filtered_water) #The results are reasonable

#Boxplots by unit
ggplot(filtered_water, mapping=aes(x=unit, y=result))+
  geom_boxplot() #There are two vales measured in Celsius that are above 60 - those are probably fahrenheit values

#Convert to fahrenheit
convert<-which(filtered_water$result>60 & filtered_water$unit=="Deg. Celsius")
filtered_water$unit[convert]<-"Deg. Fahrenheit"
ggplot(filtered_water, mapping=aes(x=unit, y=result))+
  geom_boxplot() #Boxplots are now reasonable despite some statistical outliers