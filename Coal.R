# Data Wrangling in R
# Coal Consumption Case Study

#### Import the Data ####
library(tidyverse)

# Read in the coal dataset
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal) #Variable names were not imported. moreover, the first rows are all NA. This is because the first two rows of the csv are a header row and a blank row


coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip = 2) #Skips the first two rows
glimpse(coal) #Row for each region and column for each year. But the first column does not have a name

colnames(coal)[1]<-"region"
summary(coal) #Better, but the data types are wrong.

#### Conversion from wide to long ####

#There is one observation per year in each column, this is not tidy.
coal_long<-gather(coal, "year", "coal_consumption", -region) #We want to gather all the columns except the region
glimpse(coal_long) #Good, but the types are still wrong

#Convert year to an integer
coal_long$year<-as.integer(coal_long$year)
summary(coal_long)

#Convert coal_consumption to a decimal number
coal_long$coal_consumption<-as.numeric(coal_long$coal_consumption) #NAs introduced by coercion. That's fine -  there were missing values, represented by dashes
summary(coal_long)

#### Segmentation ####

unique(coal_long$region) #The region contains countries, continents, and world. We risk double counting.

# Create a vector of "noncountry" values that appear in the region variable
noncountries <- c("North America", "Central & South America", "Antarctica", "Europe", "Eurasia", 
                  "Middle East", "Africa", "Asia & Oceania", "World")

#Find the rows where a non-country appears
match(coal_long$region, noncountries) #Each number represents a value in the "noncountries" vector (e.g. 3=Antartica)
!is.na(match(coal_long$region, noncountries)) #Convert to boolean
matches<-which(!is.na(match(coal_long$region, noncountries)))

#All of the rows in coal_long except those in matches
coal_country<-coal_long[-matches,]

#All of the matches
coal_region<-coal_long[matches,]

unique(coal_country$region) #Everything looks good
unique(coal_region$region) #Everything looks good

#Note that having World in the data violates the principles of tidy data, as a planet is a different observation entity than a continent
coal_region<-coal_region[-which(coal_region$region=="World"),]

#The data is now tidy.

#### Visualization ####

#Scatterplot
ggplot(coal_region, mapping=aes(x=year, y=coal_consumption))+
  geom_point()#A line graph would be better

ggplot(coal_region, mapping=aes(x=year, y=coal_consumption))+
  geom_line() #ggplot plotted one line!

ggplot(coal_region, mapping=aes(x=year, y=coal_consumption))+
  geom_line(mapping = aes(color=region)) 

#We conclude that Asia and Oceania increased their coal consumption between 1980 and 2010, while other regions decreased it during the same period