################################################################################
#
# Sex ratios 11-11-2016
# Analysis - US counties. 
# Check the effect of urb/rur of the link between homicide rate and adult sex 
# ratio. Doubts raised by https://dx.doi.org/10.1007/s12110-016-9271-x
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#
################################################################################

# Erase all objects in memory
rm(list = ls(all = TRUE))

# load required packages
# The code is written and tested on a PC-win7-x64
# R version 3.3.1

# load required packages
library(tidyverse) # version 1.0.0

library(ggthemes) # version 3.2.0
library(extrafont) # version 0.17
library(RColorBrewer) # version 1.1-2
library(viridis) # version 0.3.4
library(cowplot) # version 0.7.0

library(choroplethr) # version 3.5.2
library(tmap) # version 1.6-1
library(rgdal) # version 1.1-10
library(rgeos) # version 0.3-21
library(maptools) # version 0.8-39

# set working directory to the one where you have this file
setwd()

# create sub-directories
ifelse(!dir.exists('data'),dir.create('data'),paste("Directory already exists"))
ifelse(!dir.exists('geodata'),dir.create('geodata'),paste("Directory already exists"))
ifelse(!dir.exists('out'),dir.create('out'),paste("Directory already exists"))

################################################################################
# Get US geodata


# source solution (http://stackoverflow.com/a/34453890/4638884)
f <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = f)
unzip(f, exdir = "geodata/.")
US <- read_shape("geodata/gz_2010_us_050_00_20m.shp")

# reproject geodata
US_prj <- spTransform(US, CRS('+init=epsg:2163'))
names(US_prj@data) <- str_to_lower(names(US_prj@data))
US_prj@data <- US_prj@data %>% mutate(id = str_sub(geo_id,10,14))

# Alaska, Hawaii
# https://rpubs.com/technocrat/thematic-alaska-hawaii
alaska <-  US_prj[US_prj$state=="02",]
alaska <- elide(alaska, rotate=-36)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.5)
alaska <-  elide(alaska, shift=c(-2500000, -2200000))
proj4string(alaska) <- proj4string(US_prj)

hawaii <- US_prj[US_prj$state=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5100000, -1300000))
proj4string(hawaii) <- proj4string(US_prj)


plot(US_prj)
plot(alaska, add=T, col='red')
plot(hawaii, add=T, col='red')


US_prj <- US_prj[!US_prj$state %in% c("02","15","72"),]
US_prj <- rbind(US_prj, alaska, hawaii)

# fortify
gd_county <- fortify(US_prj, region = 'id')


# states
f <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_040_00_20m.zip", destfile = f)
unzip(f, exdir = "geodata/.")
US_st <- read_shape("geodata/gz_2010_us_040_00_20m.shp")
# reproject geodata
US_st_prj <- spTransform(US_st, CRS('+init=epsg:2163'))
names(US_st_prj@data) <- str_to_lower(names(US_st_prj@data))

# Alaska, Hawaii
# https://rpubs.com/technocrat/thematic-alaska-hawaii
alaska_st <-  US_st_prj[US_st_prj$state=="02",]
alaska_st <- elide(alaska_st, rotate=-36)
alaska_st <- elide(alaska_st, scale=max(apply(bbox(alaska_st), 1, diff)) / 2.5)
alaska_st <-  elide(alaska_st, shift=c(-2500000, -2200000))
proj4string(alaska_st) <- proj4string(US_st_prj)

hawaii_st <- US_st_prj[US_st_prj$state=="15",]
hawaii_st <- elide(hawaii_st, rotate=-35)
hawaii_st <- elide(hawaii_st, shift=c(5100000, -1300000))
proj4string(hawaii_st) <- proj4string(US_st_prj)


plot(US_st_prj)
plot(alaska_st, add=T, col='red')
plot(hawaii_st, add=T, col='red')


US_st_prj <- US_st_prj[!US_st_prj$state %in% c("02","15","72"),]
US_st_prj <- rbind(US_st_prj, alaska_st, hawaii_st)


# fortify
gd_state <- fortify(US_st_prj, region = 'state')


# get states borders

US_st_borders <- ik_map_identify_borders(US_st_prj)
gd_state_borders <- fortify(US_st_borders)



# major cities
f <- tempfile()
download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Structures/citiesx020_nt00007.tar.gz", destfile = f)
dir.create('geodata/us_cities')
untar(f, exdir = "geodata/us_cities/.")

cities <- readOGR(dsn = 'geodata/us_cities', layer = 'citiesx020')
cities_sub <- cities[cities$FEATURE%in%c("State Capital","State Capital   County Seat") | 
                             cities$POP_RANGE%in%c("1,000,000 - 9,999,999","500,000 - 999,999"),]
cities_sub@data <- cities_sub@data %>% droplevels()

proj4string(cities_sub) <- CRS('+proj=longlat')
cities_prj <- spTransform(cities_sub, CRS('+init=epsg:2163'))


gd_cities <- data.frame(cities_prj) %>%
        transmute(id = CITIESX020, long = coords.x1, lat = coords.x2,
                  name = NAME, fips = FIPS, state = STATE,
                  huge = POP_RANGE%in%c("1,000,000 - 9,999,999","500,000 - 999,999"),
                  capital = FEATURE%in%c("State Capital","State Capital   County Seat"))

# adjust positions for Juneau (AK) and Honolulu (HI)
gd_cities[gd_cities$state=='AK','long'] <- -1270000
gd_cities[gd_cities$state=='AK','lat'] <- -2030000
gd_cities[gd_cities$state=='HI','long'] <- -775000
gd_cities[gd_cities$state=='HI','lat'] <- -1900000

# SAVE GEODATA
save(gd_state_borders, gd_state, gd_county, gd_cities,
     file = 'geodata/us_geodata.RData')



################################################################################
# Create templates for maps

# my favorite palette
brbg <- RColorBrewer::brewer.pal(11,'BrBG')

basemap_cont <- ggplot()+
        geom_polygon(data = gd_county, aes(x=long, y=lat, group=group), fill='grey50')+
        guides(fill = guide_colorbar(barwidth = 1, barheight = 10))+
        coord_equal(xlim = c(-2100000,3300000),ylim = c(-2400000,800000),expand = c(0,0))+
        theme_map()+
        theme(panel.border=element_rect(color = 'black',size=.5,fill = NA),
              panel.background=element_rect(fill='grey15'),
              legend.position = c(1, 0),
              legend.justification = c(1, 0),
              legend.background = element_rect(colour = NA, fill = 'grey95'),
              legend.title = element_text(size=15),
              legend.text = element_text(size=15))+
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        labs(x = NULL, y = NULL)

basemap_disc <- ggplot()+
        geom_polygon(data = gd_county, aes(x=long, y=lat, group=group), fill='grey50')+
        coord_equal(xlim = c(-2100000,3300000),ylim = c(-2400000,800000),expand = c(0,0))+
        theme_map()+
        theme(panel.border=element_rect(color = 'black',size=.5,fill = NA),
              panel.background=element_rect(fill='grey15'),
              legend.position = c(1, 0),
              legend.justification = c(1, 0),
              legend.background = element_rect(colour = NA, fill = 'grey95'),
              legend.title = element_text(size=15),
              legend.text = element_text(size=15))+
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        labs(x = NULL, y = NULL)



################################################################################
#  County Characteristics, 2000-2007 ICPSR 20660 dataset
# http://www.icpsr.umich.edu/icpsrweb/NACJD/studies/20660

df_cc <- haven::read_sav('data/us_crime/ICPSR_20660/DS0001/20660-0001-Data.sav')

# extract and calculate sex ratio
df <- df_cc %>% select(FIPS, State, Division, CBSA_Status, Pop04, Male15_44_05, Fmale15_44_05, 
                       PctBlack05, CrimeRate04, Muder04, PerstPov04, RuralUrban03, 
                       UnempRate05, MedianAge05)
names(df) <- tolower(names(df))

df <- df %>% mutate(asr = male15_44_05 / fmale15_44_05,
                    murder_rate = muder04 / pop04 * 10e5,
                    metro = ifelse(ruralurban03 <= 3, 'Metro', 'Non-metro'), 
                    south = ifelse(division==3, 'South', 'Non-South'))
df[is.na(df$murder_rate),'murder_rate'] <- 0
df$fips <- str_pad(df$fips, 5, 'left', 0)
df$state <- str_pad(paste(df$state),width = 2,side = 'left',pad = '0')




################################################################################
# EXPLORATORY PLOTS


# Adult sex rates
gg_asr_dens <- ggplot(df)+
        geom_density(aes(asr,color=metro)) + 
        scale_color_manual('Type of county', values = brbg[c(2,8)])+
        coord_cartesian(xlim = c(.8,1.4))+
        theme_minimal(base_size = 15) + 
        theme(legend.position = c(.8,.6))+
        ylab('Density')+
        xlab('Adult sex ratio, males to females')

gg_asr_ecdf <- ggplot(df)+
        stat_ecdf(aes(asr,color=metro)) + 
        scale_color_manual('Type of county', values = brbg[c(2,8)])+
        coord_cartesian(xlim = c(.8,1.4))+
        theme_minimal(base_size = 15) + 
        theme(legend.position = c(.8,.6))+
        ylab('Empirical cumulative density')+
        xlab('Adult sex ratio, males to females')

gg_asr <- plot_grid(gg_asr_dens,gg_asr_ecdf)

ggsave('out/gg_asr.png', gg_asr, width = 12, height = 5, type="cairo-png")


# murder rate
gg_mr_dens <- ggplot(df)+
        geom_density(aes(murder_rate,color=metro)) + 
        scale_color_manual('Type of county', values = brbg[c(2,8)])+
        coord_cartesian(xlim = c(0,200))+
        #scale_x_continuous(trans = 'log') +
        theme_minimal(base_size = 15) + 
        theme(legend.position = c(.8,.6))+
        ylab('Density')+
        xlab('Murder rate per 100K population')

gg_mr_ecdf <- ggplot(df)+
        stat_ecdf(aes(murder_rate,color=metro)) + 
        scale_color_manual('Type of county', values = brbg[c(2,8)])+
        coord_cartesian(xlim = c(0,200))+
        #scale_x_continuous(trans = 'log') +
        theme_minimal(base_size = 15) + 
        theme(legend.position = c(.8,.6))+
        ylab('Empirical cumulative density')+
        xlab('Murder rate per 100K population')

gg_mr <- plot_grid(gg_mr_dens,gg_mr_ecdf)

ggsave('out/gg_mr.png', gg_mr, width = 12, height = 5, type="cairo-png")


# meadian age
gg_ma_dens <- ggplot(df)+
        geom_density(aes(medianage05,color=metro)) + 
        scale_color_manual('Type of county', values = brbg[c(2,8)])+
        theme_minimal(base_size = 15) + 
        theme(legend.position = c(.8,.6))+
        ylab('Density')+
        xlab('Median age of the population')

gg_ma_ecdf <-ggplot(df)+
        stat_ecdf(aes(medianage05,color=metro)) + 
        scale_color_manual('Type of county', values = brbg[c(2,8)])+
        theme_minimal(base_size = 15) + 
        theme(legend.position = c(.8,.6))+
        ylab('Empirical cumulative density')+
        xlab('Median age of the population')

gg_ma <- plot_grid(gg_ma_dens,gg_ma_ecdf)

ggsave('out/gg_ma.png', gg_ma, width = 12, height = 5, type="cairo-png")



################################################################################
# MAPS

# summarize by states
df_st <- df %>% 
        filter(!state%in%c('11')) %>%
        group_by(state, metro) %>%
        summarise_all(.funs = mean, na.rm = T) %>%
        ungroup()


# asr metro / non-metro

df_st_asr_rr <- df_st %>% select(state, metro,asr) %>%
        spread(metro,asr) %>%
        mutate(asrrr = Metro/`Non-metro`)


map_asr <- basemap_cont + 
        geom_map(map = gd_state, data = df_st_asr_rr, aes(map_id=state, fill=asrrr))+
        geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
                  color='grey50', size = .5)+
        scale_fill_gradient2('Metro to\nNon-metro\nASR\nrate ratio\n', 
                             low = brbg[1:5], mid = brbg[6], high = brbg[7:11], midpoint = 1)

ggsave('out/map_asr.png', map_asr, width = 12, height = 7, type="cairo-png")


# homicide metro / non-metro

df_st_hr_rr <- df_st %>% select(state, metro,murder_rate) %>%
        spread(metro,murder_rate) %>%
        mutate(hrrr = Metro/`Non-metro`,
               hrrr_int = cut(hrrr, breaks = c(0,.5,.9,1.1,2,10)))

# # continious scale map
# basemap_cont + 
#         geom_map(map = gd_state, data = df_st_hr_rr, aes(map_id=state, fill=hrrr))+
#         geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
#                   color='grey50', size = .5)+
#         scale_fill_gradient2('Metro to\nNon-metro\nhomicide\nrate ratio\n', 
#                              low = brbg[1:5], mid = brbg[6], high = brbg[7:11], midpoint = 1)


map_hr <- basemap_disc + 
        geom_map(map = gd_state, data = df_st_hr_rr, aes(map_id=state, fill=hrrr_int))+
        geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
                  color='grey50', size = .5)+
        scale_fill_manual('Metro to\nNon-metro\nhomicide\nrate ratio\n', 
                          values = (brewer.pal(5,"BrBG")), 
                          labels = c('less than 0.5','from 0.5 to 0.9','from 0.9 to 1.1',
                                     'from 1.1 to 2','more than 2'))

ggsave('out/map_hr.png', map_hr, width = 12, height = 7, type="cairo-png")



# blacks
map_black <- basemap_cont +
        geom_map(map = gd_county, data = df, aes(map_id=fips, fill=pctblack05))+
        geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
                  color='grey50', size = .5)+
        scale_fill_viridis('Percentage\nof black\npopulation\n', option = 'B')

ggsave('out/map_black.png', map_black, width = 12, height = 7, type="cairo-png")


# median age
map_ma <- basemap_cont +
        geom_map(map = gd_county, data = df, aes(map_id=fips, fill=medianage05))+
        geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
                  color='grey50', size = .5)+
        geom_point(data = gd_cities %>% filter(capital==T, huge==F), 
                   aes(x=long, y=lat), color = 'red', size = 3, pch=1)+
        geom_point(data = gd_cities %>% filter(capital==T, huge==T), 
                   aes(x=long, y=lat), color = 'red', size = 5, pch=1)+
        geom_point(data = gd_cities %>% filter(capital==F, huge==T), 
                   aes(x=long, y=lat), color = 'gold', size = 5, pch=1)+
        scale_fill_viridis('Median\nage of the\npopulation,\nyears\n', option = 'D', direction = -1)

ggsave('out/map_ma.png', map_ma, width = 12, height = 7, type="cairo-png")



# unemployment
basemap_cont +
        geom_map(map = gd_county, data = df, aes(map_id=fips, fill=unemprate05))+
        geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
                  color='grey50', size = .5)+
        scale_fill_viridis('Unemployment\nrate, %\n', option = 'B')

map_unemp <- basemap_disc +
        geom_map(map = gd_county, data = df, aes(map_id=fips, fill=cut_number(unemprate05,5)))+
        geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
                  color='grey50', size = .5)+
        scale_fill_viridis('Unemployment\nrate, %\n', option = 'D',discrete = T)

ggsave('out/map_unemp.png', map_unemp, width = 12, height = 7, type="cairo-png")




# metro
map_urb <- basemap_disc +
        geom_map(map = gd_county, data = df, aes(map_id=fips, fill=factor(ruralurban03)))+
        geom_path(data = gd_state_borders, aes(x=long, y=lat, group=group), 
                  color='grey50', size = .5)+
        scale_fill_viridis('Urban\nRural\ncounty\nclassification\n', option = 'B', discrete = T, direction = -1)+
        
        geom_point(data = gd_cities %>% filter(capital==T, huge==F), 
                   aes(x=long, y=lat), color = 'red', size = 3, pch=1)+
        geom_point(data = gd_cities %>% filter(capital==T, huge==T), 
                   aes(x=long, y=lat), color = 'red', size = 5, pch=1)+
        geom_point(data = gd_cities %>% filter(capital==F, huge==T), 
                   aes(x=long, y=lat), color = 'purple4', size = 5, pch=1)

ggsave('out/map_urb.png', map_urb, width = 12, height = 7, type="cairo-png")



################################################################################
# STAT MODELS

mod1 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr)

mod2 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr + perstpov04)

mod3 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr + perstpov04 + pctblack05)

mod4 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr + perstpov04 + pctblack05 + south)

mod5 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr + perstpov04 + pctblack05 + south + metro)

mod6 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr + perstpov04 + pctblack05 + south + ruralurban03)

mod7 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr + perstpov04 + pctblack05 + south + ruralurban03 + unemprate05)

mod8 <- glm(family = 'poisson', data = df, 
            murder_rate ~ asr + perstpov04 + pctblack05 + south + ruralurban03 + unemprate05 + medianage05)

texreg::htmlreg(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), 
                file = 'out/us_cnt_poisson_models.html',
                single.row = T)
