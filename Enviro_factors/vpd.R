library(tidyverse)
library(here)
library(dplyr)
library(plantecophys)

# load in climate data
climate <- read_csv("GitHub/Dendrometer/data/Climate/ameriflux.csv", skip=2)

# make year column 
climate <- climate %>%
  mutate(year = as.numeric(str_sub(TIMESTAMP_START, 1, 4)))

# set -9999 to na
climate[climate == -9999] <- NA

# subset to only necessary columns 
climate <- subset(climate, select = c(year,TIMESTAMP_START, 
                                      TIMESTAMP_END,RH_1_1_1,TA_PI_F_1_1_1))

# calculate vpd
climate <- climate %>%
  mutate(vpd = RHtoVPD(RH_1_1_1, TA_PI_F_1_1_1))

# summarize by year
climate_ann <- climate %>%
  group_by(year) %>%
  summarize(rh = mean(RH_1_1_1, na.rm = TRUE),
            airt = mean(TA_PI_F_1_1_1, na.rm = TRUE),
            vpd = mean(vpd, na.rm = TRUE))


#------------------------------------#
#------------------------------------#
# Calculate day time average vpd
#------------------------------------#
#------------------------------------#
# create date column
climate <- climate %>%
  mutate(month = as.numeric(str_sub(TIMESTAMP_START, 5, 6)),
         day = as.numeric(str_sub(TIMESTAMP_START, 7, 8)))

climate$Date<-as.Date(with(climate,paste(year,month,day,sep="-")),"%Y-%m-%d")


climate <- climate %>%
  mutate(starttime = paste(
           substring(TIMESTAMP_START, 9, 10), 
           substring(TIMESTAMP_START, 11, 12), 
           sep = ":"
         ),
         endtime = paste(
           substring(TIMESTAMP_END, 9, 10), 
           substring(TIMESTAMP_END, 11, 12), 
           sep = ":"
         ))

climate <- climate %>%
  mutate(starttime = paste(
           Date,
           starttime, 
           sep = " "
         ),
         endtime = paste(
           Date,
           endtime, 
           sep = " "
         ))

climate <- climate %>%
  mutate(starttime = as.POSIXct(starttime, format = "%Y-%m-%d %H:%M"),
         endtime = as.POSIXct(endtime, format = "%Y-%m-%d %H:%M"))



# calculate sunrise/sunset times and subset
library(suncalc)
# List unique dates
dates <- unique(climate$Date)
climate <- climate %>%
  mutate (getSunlightTimes(
  date = Date,
  lat = 42.54,
  lon =  c(-72.18),
  data = NULL,
  keep = c("sunrise",'sunset'),
  tz = 'America/New_York'))

# label each observation as day or night
climate <- climate %>%
  mutate(daytime = ifelse(starttime >= sunrise & endtime <= sunset, 
                          'day', 'night'))

# subset to only include day time observations
clim_day <- filter(climate, daytime == 'day')

# calculate annual averages using the daytime measurements

# calculate vpd
clim_day <- clim_day %>%
  mutate(vpd = RHtoVPD(RH_1_1_1, TA_PI_F_1_1_1))

# summarize by year
clim_day_ann <- clim_day %>%
  group_by(year) %>%
  summarize(rh = mean(RH_1_1_1, na.rm = TRUE),
            airt = mean(TA_PI_F_1_1_1, na.rm = TRUE),
            vpd = mean(vpd, na.rm = TRUE))

#------------------------------------#
#------------------------------------#
# Calculate summer day time average
#------------------------------------#
#------------------------------------#
# load in start of season dataframe
seasons <- read_csv("GitHub/Dendrometer/data/EMS_soseos.csv")

# subset
seasons <- seasons %>% 
  rename('year' = 'Year') %>%
  subset(select = c(year, midgup, midgdown))

# combine with clim_summ
clim_summ <- left_join(clim_day, seasons, by = 'year')

clim_summ <- clim_summ %>%
  mutate(jday = format(Date, "%j"))

# for now summer is May 1st to August 31st
clim_summ_day <- clim_summ %>%
  filter(daytime == 'day')%>%
  filter(jday >= midgup & jday <= midgdown)

#
clim_summ_ann <- clim_summ_day %>%
  group_by(year) %>%
  summarize(rh = mean(RH_1_1_1, na.rm = TRUE),
            airt = mean(TA_PI_F_1_1_1, na.rm = TRUE),
            vpd = mean(vpd, na.rm = TRUE))

# only include years that ems plots were measured
clim_summ_ann <- clim_summ_ann %>%
  filter(year >= 1998)

# calculate overall mean
summ_vpd_av <- mean(clim_summ_ann$vpd)
summ_rh_av <- mean(clim_summ_ann$rh)
summ_airt_av <- mean(clim_summ_ann$airt)

# calculate vpd anomaly
clim_summ_ann <- clim_summ_ann %>%
  mutate(vpd_mean = summ_vpd_av,
         rh_mean = summ_rh_av,
         airt_mean = summ_airt_av,
         sum_vpd_anomaly = vpd-vpd_mean,
         sum_rh_anomaly = rh-rh_mean,
         sum_airt_anomaly = airt-airt_mean)

write_csv(clim_summ_day, 'GitHub/Dendrometer/data/Climate/vpd_summ_day.csv')
write_csv(clim_summ_ann, 'GitHub/Dendrometer/data/Climate/vpd_summ_ann.csv')

ggplot()+
  geom_point(clim_summ_ann, mapping = aes(x = year, y = sum_vpd_anomaly))+
  geom_line(clim_summ_ann, mapping = aes(x = year, y = sum_vpd_anomaly))+
  theme_minimal()+
  geom_hline(yintercept = 0)


ggplot()+
  geom_point(clim_summ_ann, mapping = aes(x = year, y = vpd))+
  geom_line(clim_summ_ann, mapping = aes(x = year, y = vpd))+
  theme_minimal()


# segmented regression
library(segmented)

# segmented linear regression
out.lm<-lm(vpd~year,data=clim_summ_ann)
o<-segmented(out.lm)
summary(o)
#plot original data
plot(clim_summ_ann$year, clim_summ_ann$vpd, pch=16, col='steelblue')
#add segmented regression model
plot(o, add=T)


# also grab vpd during the summer time and daytime vpd 
# (mean across growing season, mean maximum per day)
# moving window, are there long periods where it is consistently dry scross a few weeks in the summer
# climwin package may be helpful