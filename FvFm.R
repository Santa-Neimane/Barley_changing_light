## This file contains code to analyse Fv/Fm leaf chlorophyll fluorescence parameter
##

# Fv/Fm parameter data preparation and analysis ---------------------------
dataPAM <- read_excel("FvFm.xlsx")
dataPAM <- left_join(dataPAM, filtermapping, by="FilterNr")
dataPAM$Fv_Fm <- as.numeric(dataPAM$Fv_Fm)
dataPAM <- subset(dataPAM, !is.na(Fv_Fm))

dataPAM$Spectral_f <- factor(dataPAM$Spectral, levels=c("Full spectrum","UV-A long","No UV"))

### Model with a random factor with filter number did not fit assumptions 
### thus the mean is calculated
dataPAM <- dataPAM[-76,] # outlier identified from model assumption fit

dataPAMMean <- dataPAM %>% 
  group_by(FilterNr,Spectral_f, Diff) %>% 
  summarise(MeanFvFm = mean(Fv_Fm))

m_beta_FvFm2 <- glmmTMB(MeanFvFm ~ Spectral_f*Diff, data = dataPAMMean, family = beta_family())
simres <- simulateResiduals(m_beta_FvFm2)
plot(simres) 


summary(m_beta_FvFm2)
Anova(m_beta_FvFm2, type = "III")
pairwise2 <- emmeans(m_beta_FvFm2, pairwise ~ Diff|Spectral_f)
summary(pairwise2)

# Graph
g_FvFm <- ggplot(dataPAMMean, aes(Spectral_f, MeanFvFm, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral_f),
                   stat="summary",
                   fun="mean",
                   pattern_fill="white",
                   pattern_color=NA,
                   width=1, 
                   pattern_angle=45, 
                   pattern_key_scale_factor=3,
                   position=position_dodge(0.6),
                   alpha=0.7)+
  scale_pattern_manual(values = c("Full spectrum" = "none", "UV-A long" = "stripe", "No UV"="stripe"), guide="none")+
  scale_pattern_density_manual(values = c( "Full spectrum"=0, "UV-A long" = 0.05,"No UV"=0.25),guide="none")+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"),guide="none")+
  theme_PlainGray()+
  stat_summary(aes(color=Diff),fun.data = "mean_se", geom = "errorbar", position=position_dodge(0.6),width=0)+
  annotate("text", x = "Full spectrum", y = 0.9,size=2.5,family="sans",
           label="italic(P)~'='~0.17",
           parse=TRUE)+
  annotate("text", x = "No UV", y = 0.9,size=2.5,family="sans",
           label="italic(P)~'='~0.44",
           parse=TRUE)+
  annotate("text", x = "UV-A long",
           y = 0.9,size=2.5,
           family="sans",
           label="bolditalic(P)~bold('=')~bold('0.03')",
           parse=TRUE)+
  annotate("text", x = 3.4, y=1, label ="(d)", family = "sans", fontface = "bold", size=4)+
  labs(y = "Fv/Fm")+
  theme(legend.position = 'bottom',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  guides(pattern = "none", pattern_density = "none")