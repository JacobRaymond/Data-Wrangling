# Data Wrangling in R

# 4.2 Making Wide Datasets Long with gather()
#

# Load the tidyverse
library(tidyverse)

# Read in the Pew dataset
pew <- read_csv("http://594442.youcanlearnit.net/pew.csv")

# Let's take a look at what we have
pew

# This looks to be a gathering problem.  Our dataset is wide and we want it to be long.
# The gather function can take care of that for us
pew.long <- gather(pew, income, freq, -religion) #-religion indicates to take all column as variables except "religion"

# And what did we get?
pew.long

# 4.3 Making Long Datasets Wide with Spread

# Load the tidyverse
library(tidyverse)

# Read the dataset
weather <- read_csv("http://594442.youcanlearnit.net//mexicanweather.csv")

# Let's look at what we have
weather

# And use spread() to make it wider
weather.wide <- spread(weather, element, value) #We want to create a column for every unique value of the variable "element"

# Where are we now?
weather.wide