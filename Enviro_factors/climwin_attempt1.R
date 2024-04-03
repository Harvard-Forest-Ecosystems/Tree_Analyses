library(tidyverse)
library(here)
library(dplyr)
library(climwin)

#--------------------------
#--------------------------#
#-------Load in Data-------#
#--------------------------#
#--------------------------#

# summer daytime vpd with all data
vpd2 <- read_csv('GitHub/Dendrometer/data/Climate/vpd_summ_day.csv')

# summer daytime vpd summarized for each year
vpd <- read_csv('GitHub/Dendrometer/data/Climate/vpd_summ_ann.csv')

# growth data for red oaks
growth_red <- read_csv('GitHub/Dendrometer/Data/ro_growth_annual.csv')

#--------------------------
#--------------------------#
#---------Prep Data--------#
#--------------------------#
#--------------------------#
# subset to only include year and data of interest
vpd2 <- vpd2 %>% 
  subset(select = c(vpd, date))

# summarize by day
vpd2 <- vpd2 %>% 
  group_by(date) %>%
  summarize(vpd = mean(vpd, na.rm=TRUE))

# change date format to fit climwin function
vpd2$date <- 
  format(as.Date(vpd2$date, format = "%Y-%m-%d"), "%d/%m/%Y")

# subset to variables of interest
growth_red <- growth_red %>% 
  subset(select = c(growth_inc, year.x))

# add date and remove year column
growth_red <- growth_red %>%
  mutate(date = paste0('01/01/', year.x)) %>% 
  subset(select = -c(year.x))

# format date to fit climwin function
growth_red$date <- 
  format(as.Date(growth_red$date, format = "%d/%m/%Y"), "%d/%m/%Y")


#--------------------------
#--------------------------#
#-----Sliding Analysis-----#
#--------------------------#
#--------------------------#
MassWin <- slidingwin(xvar = list(vpdlist = vpd2$vpd),
                      cdate = vpd2$date,
                      bdate = growth_red$date,
                      baseline = lm(anomaly_inc ~ 1, data = growth_red),
                      range = c(0, 50),
                      type = "absolute",
                      refday = c(01,01),
                      stat = "mean",
                      func = "lin")
