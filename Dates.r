# Data Wrangling in R
# 4.5 Dates and Times in R

# Load the tidyverse
library(tidyverse)
library(lubridate)

# Read the dataset
weather <- read_csv("http://594442.youcanlearnit.net//mexicanweather.csv")

# Let's look at what we have
weather

# We can use lubridate functions to extract elements of the date
weather$year <- year(weather$date)
weather$month <- month(weather$date)
weather$day <- day(weather$date)
weather

# We can also extract some derived values such as the weekday
wday("2018-04-01")

# or day of the year
yday("2018-04-01")

# We can also use lubridate to create date values out of different strings

mdy("04/01/2018")
mdy("04/01/18")
dmy("04/01/18")
ymd("2018-04-01")


# And we can include times

ymd_hms("2018-04-01 08:00:00")

# Let's force that to eastern time

ymd_hms("2018-04-01 08:00:00", tz='EST')

