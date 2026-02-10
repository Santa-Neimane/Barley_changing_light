## This script contains code preparing weather station data, 
## maya measurements of spectral irradiance under each filter type
## and appendix temperature, relative humidity graphs.


# Weather station radiation measurements ----------------------------------

# Hourly data read in
Weather <- read.csv("Viikki-1h-all-2017-2022-PRELIMINARY.csv")
Weather <- subset(Weather,  calendar_year_first== "2022")
Weather <- subset(Weather, month_name_first == "May" | month_name_first == "June" | month_name_first == "July" | month_name_first == "August")

# Formating
Weather$timeformat <- ymd_hms(Weather$time,tz="UTC")
Weather$timeEET <- with_tz(Weather$timeformat, tzone = "Europe/Helsinki")
Weather$DOY <- yday(Weather$timeEET)

# Filter and calculating mean
DailyMeanDF  <- Weather %>%
  subset(DOY > 160 & DOY < 224) %>% 
  group_by(DOY) %>% 
  filter(sun_elevation_median > 15) %>% 
  summarise(MeanDF=mean(PAR_diff_fr_mean, na.rm = TRUE))

MeanDF  <- Weather %>%
  subset(DOY > 160 & DOY < 224) %>% 
  filter(sun_elevation_median > 15) %>% 
  summarise(MeanDF=median(PAR_diff_fr_mean, na.rm = TRUE))

as.vector(MeanDF$MeanDF[1])
medianDF <- as.vector(MeanDF$MeanDF[1])


# Diffuse fraction graph

DF <- ggplot(data=DailyMeanDF)+
  geom_line(aes(x=DOY, y=MeanDF),color='#e11ec6')+
  geom_text(x = 229.5, y = 0.6,
            label = paste("Daily mean"),
            color = '#e11ec6')+
  theme_PlainGray()+
  xlim(160, 237)+
  ylim(0, NA)+
  geom_segment(aes(x=160,xend=223,y=medianDF,yend=medianDF), color='grey')+
  geom_text(x = 231.5, y = 0.41,
            label = paste("Overall median"),
            color = 'grey')+
  labs(y=expression(paste('Diffuse fraction')), x=expression(paste('Day of the year')))+
  theme(axis.text.x = element_text(colour = "#696969", size=8))+
  annotate("text", x = 237, y=1, label ="(c)", family = "sans", fontface = "bold", size=4)


# 1 minute average data read in

Weathermin <- read.csv("Viikki-1min-all-2022-PRELIMINARY.csv")
Weathermin <- subset(Weathermin,  calendar_year== "2022")
Weathermin <- subset(Weathermin, month_name == "May" | month_name == "June" | month_name == "July" | month_name == "August")

# Formating
Weathermin$timeformat <- ymd_hms(Weathermin$time,tz="UTC")
Weathermin$timeEET <- with_tz(Weathermin$timeformat, tzone = "Europe/Helsinki")
Weathermin$DOY <- yday(Weathermin$timeEET)

# Calculating cummulative irradiance
Weathermin$UVA_umol_minCumSum <- Weathermin$UVA_umol*60
Weathermin$PAR_umol_minCumSum <- Weathermin$PAR_umol_CS*60
Weathermin$UVB_umol_minCumSum <- Weathermin$UVB_umol*60


DailyMeanUV  <- Weathermin %>%
  subset(DOY > 160 & DOY < 224) %>% 
  group_by(DOY) %>% 
  summarise(SumUV=sum(UVA_umol_minCumSum, na.rm = TRUE),
            SumPAR=sum(PAR_umol_minCumSum, na.rm = TRUE),
            SumUVB=sum(UVB_umol_minCumSum, na.rm = TRUE),
            NumberOfMeasr=sum(!is.na(UVA_umol_minCumSum)),
            NumberOfPARMeasr=sum(!is.na(PAR_umol_minCumSum)),
            MeanUV=mean(UVA_umol_minCumSum, na.rm = TRUE),
            MeanPAR=mean(PAR_umol_minCumSum, na.rm = TRUE))

# Changing units
DailyMeanUV$SumPARmol <- DailyMeanUV$SumPAR*0.000001
DailyMeanUV$SumUVmol <- DailyMeanUV$SumUV*0.000001  

# PAR and UV graphs

PAR <- ggplot(data=DailyMeanUV)+
  geom_line(aes(x=DOY, y=SumPARmol), color="#4fb053")+
  geom_text(x = 226, y = 41,
            label = paste0("PAR"),
            color = '#4fb053')+
  theme_PlainGray()+
  xlim(160, 237)+
  ylim(0, NA)+
  labs(y=expression(atop('Daily cumulative incident solar', paste('PAR, mol ', m^{-2},' ', day^{-1}))), x=expression(paste('Day of the year')))+
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank())+
  annotate("text", x = 237, y=71, label ="(a)", family = "sans", fontface = "bold", size=4)

UV <- ggplot(data=DailyMeanUV)+
  geom_line(aes(x=DOY, y=SumUVmol), color="#34b7cb")+
  geom_text(x = 226.5, y = 3.7,
            label = paste0("UV-A"),
            color = '#34b7cb')+
  theme_PlainGray()+
  xlim(160, 237)+
  ylim(0, NA)+
  labs(y=expression(atop('Daily cumulative incident solar', paste('UV-A radiation, mol ', m^{-2},' ', day^{-1}))), x=expression(paste('Day of the year')))+
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  annotate("text", x = 237, y=6, label ="(b)", family = "sans", fontface = "bold", size=4)


# Spectral treatments graph -----------------------------------------------
Spectra <- read_excel("Spectral.xlsx")

colors <- c("Sunlight (no filter)" = "darkgrey",
            "Clear - Full spectrum" = "gold3", "Clear - UV-A long" = "#7e6b00", "Clear - No UV" = "#ffe86b",
            "Diffuse - Full spectrum"="#6852ec","Diffuse - UV-A long"="#221091","Diffuse - No UV"="#d4cefa")

ggplot(Spectra, aes(x=wavelength))+
  geom_line(aes(y=Spectra$`No Filter`, color='Sunlight (no filter)'),size=0.3)+
  geom_line(aes(y=Spectra$`Clear UV-A long`,  color='Clear - UV-A long'),size=0.3)+
  geom_line(aes(y=Spectra$`Clear UV`, color='Clear - No UV'),size=0.3)+
  geom_line(aes(y=Spectra$`Diffuse UV-A long`, color='Diffuse - UV-A long'),size=0.3)+  
  geom_line(aes(y=Spectra$`Diffuse UV`, color='Diffuse - No UV'),size=0.3)+
  geom_line(aes(y=Spectra$`Diffuse Control`, color='Diffuse - Full spectrum'),size=0.3)+
  geom_line(aes(y=Spectra$`Clear Control`,  color='Clear - Full spectrum'),size=0.3)+
  theme_PlainGray()+
  labs(y=expression(paste('Spectral irradiance, W ', m^{-2},' ', nm^{-1})), x=expression(paste('Wavelength, nm')), color = "Legend") +
  scale_color_manual(values = colors,breaks = names(colors)[c(1,2,3,4,5,6,7)])+
  guides(color = guide_legend(override.aes = list(size = 0.1, linewidth= 2, linetype=1)))+
  theme(legend.title=element_blank())


# Appendix ----------------------------------------------------------------
# Temperature data and graph
DailyMeanTemp  <- Weather %>%
  subset(DOY > 160 & DOY < 224) %>% 
  group_by(DOY) %>% 
  summarise(MeanTemp=mean(air_temp_C_mean, na.rm = TRUE))

Temp <- ggplot(data=DailyMeanTemp)+
  geom_line(aes(x=DOY, y=MeanTemp),color='gray')+
  theme_PlainGray()+
  ylim(0, NA)+
  labs(y=expression(paste('Mean daily air temperature, °C')), x=expression(paste('Day of the year')))+
  theme(axis.text.x = element_text(colour = "#696969", size=8))+
  annotate("text", x = 232, y=30, label ="(a)", family = "sans", fontface = "bold", size=4)+
  theme(axis.line.x = element_blank(),  # remove x axis line
        axis.text.x = element_blank(),  # remove x axis labels
        axis.ticks.x = element_blank(), # remove x axis ticks
        axis.title.x = element_blank())


# Relative humidity data and graph
DailyMeanRH  <- Weather %>%
  subset(DOY > 160 & DOY < 224) %>% 
  group_by(DOY) %>% 
  summarise(MeanRH=mean(air_RH_mean, na.rm = TRUE))

RH <- ggplot(data=(subset(DailyMeanRH, DOY<224)))+
  geom_line(aes(x=DOY, y=MeanRH),color='gray')+
  theme_PlainGray()+
  ylim(0, NA)+
  labs(y=expression(paste('Mean daily air relative humidity, %')), x=expression(paste('Day of the year')))+
  theme(axis.text.x = element_text(colour = "#696969", size=8))+
  annotate("text", x = 232, y=100, label ="(b)", family = "sans", fontface = "bold", size=4)

