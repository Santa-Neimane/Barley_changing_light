##  This file contains code to analyse long-term trends in air temperature changes
##  across filter types. Data collected with Ibutton sensors
##  Methods of the modelling described in the Appendix.


# Differences in air temperature under the filters ------------------------

# Data source
df_filtertemp <- read.csv("IBottTempFINAL.txt", header = TRUE)

df_filtertemp$DateTimeFin <- as_datetime(df_filtertemp$DateTimeFin)
df_filtertemp$DOY <- yday(df_filtertemp$DateTimeFin)
df_temp_sub <- subset(df_filtertemp, DOY > 160 & DOY < 224)

ggplot()+
  geom_line(data=(subset(df_temp_sub, Filename=="OpenTemp")) , aes(x=DateTimeFin,y=ValueFull, color='Outside (no filter)'),size=0.2, alpha=0.5)+
  geom_line(data=(subset(df_temp_sub, Filename=="Transp")) , aes(x=DateTimeFin,y=ValueFull, color='Clear - Full spectrum'),size=0.2, alpha=0.5)+
  geom_line(data=(subset(df_temp_sub, Filename=="DTransp")) , aes(x=DateTimeFin,y=ValueFull, color='Diffuse - Full spectrum'),size=0.2, alpha=0.5)+
  geom_line(data=(subset(df_temp_sub, Filename=="350")) , aes(x=DateTimeFin,y=ValueFull, color='Clear - UV-A long'),size=0.2, alpha=0.5)+
  geom_line(data=(subset(df_temp_sub, Filename=="D350")) , aes(x=DateTimeFin,y=ValueFull, color='Diffuse - UV-A long'),size=0.2, alpha=0.5)+
  geom_line(data=(subset(df_temp_sub, Filename=="UV")) , aes(x=DateTimeFin,y=ValueFull,  color='Clear - No UV'),size=0.2, alpha=0.5)+
  geom_line(data=(subset(df_temp_sub, Filename=="DUV")) , aes(x=DateTimeFin,y=ValueFull,  color='Diffuse - No UV'),size=0.2, alpha=0.5)+
  theme_PlainGray()+
  scale_color_manual(name="Treatment", values = c('gold3',"#ffe86b","#7e6b00","#6852ec","#d4cefa","#221091",'darkgrey'))+
  theme(legend.position = "top")+
  labs(y="Air temperature, °C", x="Date") +
  guides(color = guide_legend(override.aes = list(size = 0.1, linewidth= 2, linetype=1)))+
  theme(legend.title=element_blank())



df_temp_sub$Filename <- as.factor(df_temp_sub$Filename)

res<-anova_test(data=df_temp_sub,dv=ValueFull,wid=Filename,within=DateTimeFin)
sum(is.na(df_temp_sub))
table(df_temp_sub$ValueFull, df_temp_sub$Filename)


df_temp_sub$DateTimeAnova <- format(df_temp_sub$DateTimeFin, "%Y-%m-%d %H")
print(df_temp_sub$DateTimeAnova)

df_temp_sub$DateTimeAnova <- ymd_h(df_temp_sub$DateTimeAnova)

df_temp_sub_summary <- df_temp_sub %>% 
  group_by(Filename,Diffusion, Filter, DateTimeAnova) %>% 
  summarise(MeanTemp= mean(ValueFull))
# Later hourly mean air temperatures compared

df_temp_sub_summary$ScaledDateTime <- scale(df_temp_sub_summary$DateTimeAnova)

df_temp_sub_summary$ScaledDateTime <- scale(df_temp_sub_summary$DateTimeAnova)
df_temp_sub_summary$HourofDay <- hour(df_temp_sub_summary$DateTimeAnova)
df_temp_sub_summary$HourofDay <- as.factor(df_temp_sub_summary$HourofDay)


df_temp_sub_summary<- subset(df_temp_sub_summary, Filename!="OpenTemp")
# "OpenTemp" is measured outside the filers in the barley field thus
#  excluded for the comparison



# Two models are built, one that accounts for the differences in experimental treatments
modelXX <- lme(MeanTemp~ScaledDateTime+HourofDay, random=~1|Filename, 
             data=df_temp_sub_summary,
             correlation=corAR1(), method = "ML")

model2XX <- lme(MeanTemp~ScaledDateTime+HourofDay, random=~ScaledDateTime|Filename, 
             data=df_temp_sub_summary,
             correlation=corAR1(), method = "ML")

# both models accounted for autocorrelation
anova(modelXX,model2XX)
lrtest(modelXX,model2XX)

