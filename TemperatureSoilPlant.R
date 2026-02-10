##  This file contains code to analyse temperature measurements.
##  Surface temperature measured with an infra-red thermometer
##  
##  Soil measurements and plant surface measurements stored in two
##  separate files: TemperatureSoil.xlsx, TemperaturePlant.xlsx


# Soil Temperature --------------------------------------------------------

# Data source
dataSoilT <- read_excel("TemperatureSoil.xlsx")
dataSoilT$Diff <- as.factor(dataSoilT$Diff)
dataSoilT$FilterNr <- as.factor(dataSoilT$FilterNr)


dataSoilT <- dataSoilT[-65,] 
dataSoilT <- dataSoilT[-171,] # removal of model outliers identified in separate runs

m_soilT <- lme(SoilT ~ Spectral+Diff*ShadeSun,random=~1|FilterNr, data = dataSoilT)
resid_panel(m_soilT, smoother = TRUE, qqbands = TRUE)

summary(m_soilT)
anova(m_soilT)
pairwise <- emmeans(m_soilT, pairwise ~ Diff*ShadeSun)
summary(pairwise)


# details for the graph
dataSoilT$NewShadeSun[dataSoilT$ShadeSun=="Sunny"] = "No overstory"
dataSoilT$NewShadeSun[dataSoilT$ShadeSun=="Shade"] = "Plant shade"
g_labels <- data.frame(NewShadeSun=c("Plant shade","No overstory"),Diff=c("Diffuse", "Diffuse"), label=c("***P*** **= 0.14**","***P*** **< 0.01**"))

dataSoilT$Empty <- "empty"
GraphSoilT <- ggplot(dataSoilT, aes(x=Empty,y=SoilT, color=Diff))+
  geom_bar(stat="summary",
           fun="mean",
           width=0.5,
           position=position_dodge(0.35),
           alpha=0.7,
           fill = "transparent",
           size=0.5)+
  facet_wrap(.~NewShadeSun,
             scales="free_x")+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.35),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(strip.text = element_text(face = "italic", color = "black", hjust = 0),
        strip.background = element_rect(fill = "lightgrey", linewidth = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank())+
  labs(y = expression("Soil temperature, °C"))+
  geom_richtext(x=1, y=26, aes(label=label),size=2.5,color="black",family="sans", data=g_labels, fill = NA,label.color = NA)+
  expand_limits(y = 27)


# Plant surface Temperature -----------------------------------------------

# Data source
dataPlantT <- read_xlsx("TemperaturePlant.xlsx")
dataPlantT$FilterNr <- as.factor(dataPlantT$FilterNr)
dataPlantT$CanopyLevel <- as.factor(dataPlantT$CanopyLevel)


dataPlantT <- dataPlantT[-43,]
dataPlantT <- dataPlantT[-208,] # removal of model outliers identified in separate runs


m_plantT <- lme(PlantT ~ Spectral+Diff+CanopyLevel,random=~1|FilterNr, data = dataPlantT)
resid_panel(m_plantT, smoother = TRUE, qqbands = TRUE)

summary(m_plantT)
anova(m_plantT)
pairwise <- emmeans(m_plantT, pairwise ~ CanopyLevel)
summary(pairwise)


ggplot(dataPlantT, aes(Spectral,PlantT, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral),
                   stat="summary",
                   fun="mean",
                   pattern_fill="white",
                   pattern_color=NA,
                   width=1, 
                   pattern_angle=45, 
                   pattern_key_scale_factor=0.8,
                   position=position_dodge(0.6),
                   alpha=0.7)+
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"))+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  facet_grid(~CanopyLevel)


summary(glht(m_plantT, linfct = mcp(CanopyLevel = "Tukey")), test = adjusted("holm")) # letters to compare differences
