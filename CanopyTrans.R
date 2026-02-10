## This file contains code to analyse canopy PAR transmission data
##

# File read in and preparation
dataLAI <- read_excel("LAI.xlsx")
dataLAI <- left_join(dataLAI, filtermapping, by="FilterNr")

# Canopy transmittance ----------------------------------------------------
dataLAI$segmentmean <- (dataLAI$Segment4PAR+dataLAI$Segment5PAR+dataLAI$Segment6PAR)/3
dataLAI$LightTransmittace <- dataLAI$segmentmean / dataLAI$ExternalSensorPAR

LAIperpMean <- dataLAI %>% 
  group_by(FilterNr, Spectral, Diff) %>% 
  summarise(MeanTrans= mean(LightTransmittace))


ggplot(LAIperpMean, aes(Spectral, MeanTrans, fill=Diff))+
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
        axis.title.x = element_blank())

LAIperpMean$Spectral <- as.factor(LAIperpMean$Spectral)
m_beta_Trans <- glmmTMB(MeanTrans ~ Spectral+Diff, data = LAIperpMean, family = beta_family())
simres <- simulateResiduals(m_beta_Trans)
plot(simres)

Anova(m_beta_Trans, type = "III")
pairwise <- emmeans(m_beta_Trans, pairwise ~ Spectral)
summary(pairwise)