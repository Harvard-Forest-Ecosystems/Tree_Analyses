library(tidyverse)
library(here)
library(dplyr)

#------------growth data-----------------
growth <- read_csv('GitHub/Dendrometer/Data/ems_growth_data.csv')

# subset to only include first 3 plots and red oaks
growth <- growth %>%
  filter(pl_col <= 3)

growth_g1 <- filter(growth, plot.x == 'G1')
growth_g1_rp <- growth_g1 %>%
  filter(species == 'rp')%>%
  group_by(plot.x, year.x) %>%
  summarize(num_rp = n())

ggplot()+
  geom_point(growth_g1_rp, mapping = aes(x = year.x, y = num_rp))+
  geom_line(growth_g1_rp, mapping = aes(x = year.x, y = num_rp))


# classify plots based on percent evergreen (basal area)
# hemlocks, white pine, and red pine
growth_plot_evrgrn <- growth %>%
  filter(species == 'hem' | species == 'rp' | species == 'wp')%>%
  group_by(plot.x, year.x) %>%
  summarize(evrgrn_ba = sum(basal_area.x, na.rm = TRUE))



# classify plots based on percent hemlock (basal area)
growth_plot_hem <- growth %>%
  filter(species == 'hem')%>%
  group_by(plot.x, year.x) %>%
  summarize(hem_ba = sum(basal_area.x, na.rm = TRUE))

growth_plot <- growth %>%
  group_by(plot.x, year.x) %>%
  summarize(tot_ba = sum(basal_area.x, na.rm = TRUE))

growth_all <- inner_join(growth_plot, growth_plot_hem, by = c('plot.x','year.x'))
growth_all <- inner_join(growth_plot_evrgrn, growth_all, by = c('plot.x','year.x'))

# percent hemlock
growth_all <- growth_all %>%
  mutate(per_hem = (hem_ba/tot_ba)*100,
         per_evrgrn = (evrgrn_ba/tot_ba)*100,)

# plot
ggplot()+
  geom_point(growth_all, mapping = aes(x = year.x, y = per_hem, col = plot.x))+
  geom_line(growth_all, mapping = aes(x = year.x, y = per_hem, col = plot.x))

# plot
ggplot()+
  geom_point(growth_all, mapping = aes(x = year.x, y = per_evrgrn, col = plot.x))+
  geom_line(growth_all, mapping = aes(x = year.x, y = per_evrgrn, col = plot.x))



# find average percent across all years
growth_per_hem <- growth_all %>%
  group_by(plot.x)%>%
  summarize(per_hem_avg = mean(per_hem, na.rm = TRUE),
            per_evrgrn_avg = mean(per_evrgrn, na.rm = TRUE))



#------------data-----------------
lai <- read_csv('GitHub/Dendrometer/Data/lai-plot.csv')

# remove x plots
lai <- lai %>%
  mutate(pl_col = str_sub(plot, 1,1),
         pl_row = str_sub(plot,2,2))%>%
  filter(pl_col != 'X')%>%
  filter(pl_row <=3)

# load in start of season dataframe
seasons <- read_csv("GitHub/Dendrometer/data/EMS_soseos.csv")

# subset
seasons <- seasons %>% 
  rename('year' = 'Year') %>%
  subset(select = c(year, midgup, midgdown))

# combine with lai
clim_summ <- left_join(lai, seasons, by = 'year')

# subset to only include growing season
lai_summ <- clim_summ %>%
  filter(doy >= midgup & doy <= midgdown)

lai_annual_plot <- lai_summ %>%
  group_by(year,plot) %>%
  summarize(lai_ann = mean(lai, na.rm = TRUE),
            lai_med = median(lai, na.rm = TRUE),
            se_ann = mean(se, na.rm = TRUE),
            upper = lai_ann+se_ann,
            lower = lai_ann-se_ann)

# subset to only include nongrowing season
lai_nongr <- clim_summ %>%
  filter(doy <= midgup | doy >= midgdown)

lai_annual_plot_nongr <- lai_nongr %>%
  group_by(year,plot) %>%
  summarize(lai_nongr = mean(lai, na.rm = TRUE),
            lai_med_non = median(lai, na.rm = TRUE),
            se_nongr = mean(se, na.rm = TRUE),
            upper_nongr = lai_nongr+se_nongr,
            lower_nongr = lai_nongr-se_nongr)

# plot

ggplot()+
  geom_point(lai_annual_plot, mapping = aes(x = year, y = lai_ann, col = plot))+
  geom_line(lai_annual_plot, mapping = aes(x = year, y = lai_ann, col = plot))+
  geom_errorbar(lai_annual_plot, mapping = aes(x = year,ymin = lower, ymax = upper, col = plot), width = 0.2)

ggplot()+
  geom_point(lai_annual_plot, mapping = aes(x = year, y = lai_ann, col = plot))+
  geom_line(lai_annual_plot, mapping = aes(x = year, y = lai_ann, col = plot))+
  geom_point(lai_annual_plot_nongr, mapping = aes(x = year, y = lai_nongr, col = plot))+
  geom_line(lai_annual_plot_nongr, mapping = aes(x = year, y = lai_nongr, col = plot))
  #xlim(2005,2023)+
  #geom_errorbar(lai_annual_plot, mapping = aes(x = year,ymin = lower, ymax = upper, col = plot), width = 0.2)


ggplot()+
  geom_point(lai_annual_plot_nongr, mapping = aes(x = year, y = lai_nongr, col = plot))+
  geom_line(lai_annual_plot_nongr, mapping = aes(x = year, y = lai_nongr, col = plot))#+
  #geom_errorbar(lai_annual_plot_nongr, mapping = aes(x = year,ymin = lower_nongr, ymax = upper_nongr, col = plot), width = 0.2)+




lai_annual <- lai_summ %>%
  group_by(year) %>%
  summarize(lai_ann = mean(lai, na.rm = TRUE),
            lai_med = median(lai, na.rm = TRUE),
            se_ann = median(se, na.rm = TRUE),
            upper = lai_med+se_ann,
            lower = lai_med-se_ann)%>%
  mutate(season = 'growing')

# subset to only include nongrowing season
lai_nongr <- clim_summ %>%
  filter(doy <= midgup | doy >= midgdown)

lai_annual_nongr <- lai_nongr %>%
  group_by(year) %>%
  summarize(lai_ann = mean(lai, na.rm = TRUE),
            lai_med = median(lai, na.rm = TRUE),
            se_ann = median(se, na.rm = TRUE),
            upper = lai_med+se_ann,
            lower = lai_med-se_ann)%>%
  mutate(season = 'nongrowing')

# merge
lai_all_season <- rbind(lai_annual_nongr, lai_annual)

write_csv(lai_all_season, "GitHub/Dendrometer/data/lai_seasonal.csv")

# remove before 2005
lai_after_2005 <- filter(lai_all_season, year >= '2005')

ggplot()+
  geom_point(lai_all_season, mapping = aes(x = year, y = lai_med, col = season))+
  geom_line(lai_all_season, mapping = aes(x = year, y = lai_med, col = season))+
  geom_errorbar(lai_all_season, mapping = aes(x = year,ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()+
  ylab('Median LAI')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title = element_text(size=14,face="bold"))

ggplot()+
  geom_point(lai_after_2005, mapping = aes(x = year, y = lai_med, col = season))+
  geom_line(lai_after_2005, mapping = aes(x = year, y = lai_med, col = season))+
  geom_errorbar(lai_after_2005, mapping = aes(x = year,ymin = lower, ymax = upper), width = 0.2)

lai_annual <- lai_summ %>%
  group_by(year) %>%
  summarize(lai_ann = mean(lai, na.rm = TRUE),
            se_ann = mean(se, na.rm = TRUE),
            upper = lai_ann+se_ann,
            lower = lai_ann-se_ann)

ggplot()+
  geom_point(lai_annual, mapping = aes(x = year, y = lai_ann))+
  geom_line(lai_annual, mapping = aes(x = year, y = lai_ann))+
  geom_errorbar(lai_annual, mapping = aes(x = year,ymin = lower, ymax = upper), width = 0.2)

#----------------------------------------------------#
#--------Plot lai with Percent hemlock----------------
#----------------------------------------------------#
# merge data
growth_per_hem <- growth_per_hem %>%
  rename('plot' = 'plot.x')

lai_hem <- left_join(lai_annual_plot, growth_per_hem, by = 'plot')

# fill nas with 0 (indicates no hemlock)
lai_hem[is.na(lai_hem)] <- 0

# create bins
# low = 0-25% hemlock
# med_low = 25-50%
# med_high = 50-75
# high = >75

lai_hem_binned <- lai_hem %>%
  mutate(hem_bin = ifelse(per_hem_avg <= 25, 'low', 'NA'),
         hem_bin = ifelse(per_hem_avg > 25 & per_hem_avg <= 50, 'med_low', hem_bin),
         hem_bin = ifelse(per_hem_avg > 50 & per_hem_avg <= 75, 'med_high', hem_bin),
         hem_bin = ifelse(per_hem_avg > 75, 'high', hem_bin))

lai_hem_binned <- lai_hem_binned %>%
  mutate(evrgrn_bin = ifelse(per_evrgrn_avg <= 25, 'low', 'NA'),
         evrgrn_bin = ifelse(per_evrgrn_avg > 25 & per_evrgrn_avg <= 50, 'med_low', evrgrn_bin),
         evrgrn_bin = ifelse(per_evrgrn_avg > 50 & per_evrgrn_avg <= 75, 'med_high', evrgrn_bin),
         evrgrn_bin = ifelse(per_evrgrn_avg > 75, 'high', evrgrn_bin),)

lai_hem_bin_avg <- lai_hem_binned %>%
  group_by(year, hem_bin)%>%
  summarize(lai = mean(lai_ann))

lai_evrgrn_bin_avg <- lai_hem_binned %>%
  group_by(year, evrgrn_bin)%>%
  summarize(lai = mean(lai_ann))

# most plots are low 
# plot
ggplot()+
  geom_point(lai_hem_bin_avg, mapping = aes(x = year, y = lai, col = hem_bin))+
  geom_line(lai_hem_bin_avg, mapping = aes(x = year, y = lai, col = hem_bin))

# plot
ggplot()+
  geom_point(lai_evrgrn_bin_avg, mapping = aes(x = year, y = lai, col = evrgrn_bin))+
  geom_line(lai_evrgrn_bin_avg, mapping = aes(x = year, y = lai, col = evrgrn_bin))


#----------------------------------------------------#
#--------Plot lai for nongrowing season with Percent hemlock----------------
#----------------------------------------------------#
# merge data
growth_per_hem <- growth_per_hem %>%
  rename('plot' = 'plot.x')

lai_hem_nongr <- left_join(lai_annual_plot_nongr, growth_per_hem, by = 'plot')

# fill nas with 0 (indicates no hemlock)
lai_hem_nongr[is.na(lai_hem_nongr)] <- 0

# create bins
# low = 0-25% hemlock
# med_low = 25-50%
# med_high = 50-75
# high = >75

lai_hem_binned <- lai_hem_nongr %>%
  mutate(hem_bin = ifelse(per_hem_avg <= 25, 'low', 'NA'),
         hem_bin = ifelse(per_hem_avg > 25 & per_hem_avg <= 50, 'med_low', hem_bin),
         hem_bin = ifelse(per_hem_avg > 50 & per_hem_avg <= 75, 'med_high', hem_bin),
         hem_bin = ifelse(per_hem_avg > 75, 'high', hem_bin),)

lai_hem_binned <- lai_hem_binned %>%
  mutate(evrgrn_bin = ifelse(per_evrgrn_avg <= 25, 'low', 'NA'),
         evrgrn_bin = ifelse(per_evrgrn_avg > 25 & per_evrgrn_avg <= 50, 'med_low', evrgrn_bin),
         evrgrn_bin = ifelse(per_evrgrn_avg > 50 & per_evrgrn_avg <= 75, 'med_high', evrgrn_bin),
         evrgrn_bin = ifelse(per_evrgrn_avg > 75, 'high', evrgrn_bin),)


lai_hem_bin_avg <- lai_hem_binned %>%
  group_by(year, hem_bin)%>%
  summarize(lai = mean(lai_nongr))

lai_evrgrn_bin_avg <- lai_hem_binned %>%
  group_by(year, evrgrn_bin)%>%
  summarize(lai = mean(lai_nongr))

# most plots are low 
# plot
ggplot()+
  geom_point(lai_hem_bin_avg, mapping = aes(x = year, y = lai, col = hem_bin))+
  geom_line(lai_hem_bin_avg, mapping = aes(x = year, y = lai, col = hem_bin))


ggplot()+
  geom_point(lai_evrgrn_bin_avg, mapping = aes(x = year, y = lai, col = evrgrn_bin))+
  geom_line(lai_evrgrn_bin_avg, mapping = aes(x = year, y = lai, col = evrgrn_bin))








# segmented regression
library(segmented)

# segmented linear regression
out.lm<-lm(lai_ann~year,data=lai_annual)
o<-segmented(out.lm)
summary(o)
#plot original data
plot(lai_annual$year, lai_annual$lai_ann, pch=16, col='steelblue')
#add segmented regression model
plot(o, add=T)

# remove before 2005 and rerun
lai_annual_post <- filter(lai_annual, year >=2005)

# segmented linear regression
out.lm<-lm(lai_ann~year,data=lai_annual_post)
o<-segmented(out.lm)
summary(o)
#plot original data
plot(lai_annual_post$year, lai_annual_post$lai_ann, pch=16, col='steelblue')
#add segmented regression model
plot(o, add=T)
  