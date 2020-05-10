# Data Wrangling in R: Data Cleaning

#### 5.1 Detecting Outliers  ####
#

# Load the tidyverse
library(tidyverse)

# Read in the Medicare payments dataset
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

ggplot(data=inpatient)+
  geom_histogram(mapping=aes(x=AverageCharges)) #There is a large right tail

ggplot(data=inpatient)+
  geom_histogram(mapping=aes(x=AverageCharges)) +
  coord_cartesian(ylim=c(0, 25)) #"Zooms in" the y-axis -  there are some outliers

ggplot(data=inpatient)+
  geom_boxplot(mapping=aes("charges", AverageCharges))

ggplot(data=inpatient)+
  geom_boxplot(mapping=aes(State, AverageCharges)) #One boxplot per state. A lot of outliers above 500,000.

#Create a tibble with the outliers
highCharges<-filter(inpatient, AverageCharges>500000)

#Could the diagnostic explain the charges?
unique(highCharges$DRG) #Yes - only four diagnostics out of the one hundred DRG in "inpatient"

ggplot(data=highCharges)+
  geom_point(mapping=aes(DRG, AverageCharges))+
  theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) #Formatting

#### 5.2 Missing and Special Values in R, Part 1  ####
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Look at a summary of the data
summary(inspections) #There are 14 NAs for "License"

nolicense<-which(is.na(inspections$License)) #Row numbers with the NA

unlicensed<-inspections[nolicense,] #The 14 records appear to belong to religious organizations...
licensed<-inspections[-nolicense,]

#### 5.2 Missing and Special Values in R, Part 2  ####
#

badmath<-c(1,2,3, 4/0, 0/0, NA) #One inf, one NaN, and one NA
is.na(badmath) #NaN are NA
is.nan(badmath) #NA are not NA
is.infinite(badmath)
is.finite(badmath) #NaN and NA are neither finite or infinite

#### 5.3 Breaking Apart Columns With Separate  ####
#

#Separate() breaks columns into multiple parts based on a separator (delimiter or character position)

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", 
           "AverageCharges", "AverageTotalPayments", "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, 
                      skip=1, col_types = types)

unique(inpatient$DRG) #The DRG is made up of two pieces of information: a code and a description. The delimiter is a hyphen.

inpatient_separate<-separate(inpatient, DRG, c("DRGCode", "DRGDescription"), "-") #There's a warning message, let's check one of the incorrect obs, 45894
inpatient$DRG[45894] #There are hyphens in the field

#The code is always three digits... could use position

inpatient_separate<-separate(inpatient, DRG, c("DRGCode", "DRGDescription"), 4) #Separate at the 4th character 
glimpse(inpatient_separate) #Still some formatting problems. These will be addressed later.

#### 5.4 Combining Columns with unite() ####
#

#Unite() combines values from different column. A separator is optional

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

glimpse(inspections)

#Suppose we want to analyze data by region (City, State)

regional_inspections<-unite(inspections, region, City, State, sep=", ") #Region is the new field

#The city and state are gone. Can be ovewrote

regional_inspections<-unite(inspections, region, City, State, sep=", ", remove=F) 

unique(regional_inspections$region) #A lot of typos... will be addressed later

#### 5.5 Manipulating Strings in R with stringr, Part 1 ####
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)
library(stringr)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)

# And take a look at the unique regions
unique(regional_inspections$Region) #Inconsistent capitalization, typos...

regional_inspections$Region<-str_to_upper(regional_inspections$Region)
unique(regional_inspections$Region) #The inconsistent case for Chicago are solved. Let's now fixed Chicago

regional_inspections$Region<-str_replace(regional_inspections$Region, "CCHICAGO", "CHICAGO")
regional_inspections$Region<-str_replace(regional_inspections$Region, "CHCICAGO", "CHICAGO")
regional_inspections$Region<-str_replace(regional_inspections$Region, "CHICAGOCHICAGO", "CHICAGO")
regional_inspections$Region<-str_replace(regional_inspections$Region, "CHCHICAGO", "CHICAGO")
regional_inspections$Region<-str_replace(regional_inspections$Region, "CHICAGOI", "CHICAGO")
unique(regional_inspections$Region) #All that's left is to take care of the NA, starting with Chicago, NA

regional_inspections$Region<-str_replace(regional_inspections$Region, "CHICAGO, NA", "CHICAGO, IL")
unique(regional_inspections$Region) #The other NA will be treated as missing data

regional_inspections$Region<-str_replace(regional_inspections$Region, "NA,", NA) #Error: Can't replace something by NA
NA_regions<-which(str_detect(regional_inspections$Region, "NA,")) #145 observations containing "NA, "
Inactive_regions<-which(str_detect(regional_inspections$Region, "INACTIVE, IL")) #8 observations containing "INACTIVE, NA"
regional_inspections$Region[NA_regions]<-NA
regional_inspections$Region[Inactive_regions]<-NA
unique(regional_inspections$Region) #Notice that NA is not between quotes, meaning it represents true missing data

#### 5.5 Manipulating Strings in R with stringr, Part 2 ####
#

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
library(stringr)

names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Separate at the fourth position
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)

# And take a look at the data now
glimpse(inpatient_separate) #DRGcode and DRGdescription contain spaces and hyphens that are not necessary

inpatient_separate$DRGcode<-str_trim(inpatient_separate$DRGcode)
glimpse(inpatient_separate) #The leading white spaces from DRGcode are gone

inpatient_separate$DRGdescription<-str_sub(inpatient_separate$DRGdescription, 3) #We delete the first two characters "- "
glimpse(inpatient_separate) #The leading "- " from DRGdescription are gone
