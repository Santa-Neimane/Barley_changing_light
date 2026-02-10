##  This file contains code that analyses plant traits,
##  when harvested to estimate biomass of different plant parts

# File read in and preparation
BiomassDry <- read_excel("DryBiomass.xlsx")

# Check correlations
pairs.panels(BiomassDry,
             smooth = TRUE,
             scale = FALSE,
             cex=3) # Generally high correlations, whole plant mass very high

Biomass <- left_join(BiomassDry, filtermapping, by="FilterNr")
Biomass$FilterNr <- as.factor(Biomass$FilterNr)
Biomass$Spectral_f <- factor(Biomass$Spectral, levels=c("Full spectrum","UV-A long","No UV"))


# MANOVA ------------------------------------------------------------------

res.man <- manova(cbind(SpikesDryWeight, StemsDryWeight, GrainsDryWeight, LeavesDryWeight, RootDryWeight_g) ~ Diff+Spectral, data = Biomass)
summary(res.man)
resid.man <- residuals(res.man)
qqnorm(resid.man[, "SpikesDryWeight"])
qqnorm(resid.man[, "GrainsDryWeight"])
qqnorm(resid.man[, "StemsDryWeight"])
qqnorm(resid.man[, "LeavesDryWeight"])
qqnorm(resid.man[, "RootDryWeight_g"])



# Whole plant mass
m_plantmass <- lm(WholePlantWeight ~ Spectral*Diff, data = Biomass)

resid_panel(m_plantmass, smoother = TRUE, qqbands = TRUE)

summary(m_plantmass)
anova(m_plantmass)


# Root to above ratio
m_rootratio <- lm(RootToUpRatio ~ Spectral*Diff, data = Biomass)

resid_panel(m_rootratio, smoother = TRUE, qqbands = TRUE)

summary(m_rootratio)
anova(m_rootratio)

pairwise <- emmeans(m_rootratio, pairwise ~ Spectral)
summary(pairwise)

GraphRootRatio <- ggplot(Biomass, aes(Spectral_f, RootToUpRatio, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral_f),
                   stat="summary",
                   fun="mean",
                   pattern_fill="white",
                   pattern_color=NA,
                   pattern_angle=45, 
                   pattern_key_scale_factor=0.5,
                   alpha=0.7,
                   width = 1,
                   position=position_dodge(0.6))+
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"))+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  stat_summary(aes(color=Diff),fun.data = mean_se, geom = "errorbar",width = 0, position=position_dodge(0.6))+
  theme_PlainGray()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(), 
        legend.position="none")+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")+
  geom_bracket(xmin = "Full spectrum",
               xmax = "No UV",
               label="bolditalic(P)~bold('=')~bold('0.02')",
               type="expression",
               y.position = 18.5,
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("UV-A long", "No UV"),
               xmax = c("Full spectrum", "UV-A long"),
               label=c("italic(P)~'='~0.22","italic(P)~'='~0.46"),
               type="expression",
               y.position = c(16, 17),
               family="sans",
               label.size = 2.8,
               size=0.1,
               inherit.aes = FALSE)+
  expand_limits(y = 19)+
  labs(y=expression(paste('Root:shoot ratio, ', g^{1},' ', "g"^{-1})))+
  annotate("text", x = 3.4, y=19, label ="(b)", family = "sans", fontface = "bold", size=4)+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))

# Height
m_height <- lm(Biomass$HeightwithSpike ~ Spectral*Diff, data = Biomass)

resid_panel(m_height, smoother = TRUE, qqbands = TRUE)

summary(m_height)
anova(m_height)
pairwise <- emmeans(m_height, pairwise ~ Diff|Spectral)
summary(pairwise)

g_height <- ggplot(Biomass, aes(Spectral_f, HeightwithSpike, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral_f),
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum"=0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  stat_summary(aes(color=Diff),fun.data = "mean_se", geom = "errorbar", position=position_dodge(0.6),width=0)+
  labs(y = ("Plant height, cm"))+
  annotate("text", x = "UV-A long",
           y = 90,size=2.5,
           family="sans",
           label="bolditalic(P)~bold('<')~bold('0.01')",
           parse=TRUE)+
  annotate("text", x = "Full spectrum", y = 90,size=2.5,family="sans", 
           label="italic(P)~'='~0.19",
           parse=TRUE)+
  annotate("text", x = "No UV", y = 90,size=2.5,family="sans",
           label="italic(P)~'='~0.88",
           parse=TRUE)+
  annotate("text", x = 3.4, y=100, label ="(b)", family = "sans", fontface = "bold", size=4)+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

# Number of stems ---------------------------------------------------------
ggplot(Biomass, aes(Spectral, NumberofStems, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum"=0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()


m_poisson_stems2 <- glm(NumberofStems ~ Spectral+Diff, data = Biomass, family = poisson())

resid_panel(m_poisson_stems2, smoother = TRUE, qqbands = TRUE)

summary(m_poisson_stems2)
Anova(m_poisson_stems2, type = "III")

# Number of Leaves

## Removing outliers to improve model fit 
## since no other models worked
Biomass_outlierleaves <- Biomass %>%
  group_by(Spectral_f, Diff) %>% 
  mutate(newNumberofLeaves = remove_outliers(NumberofLeaves))

Biomass_outlierleaves <- Biomass_outlierleaves[!is.na(Biomass_outlierleaves$newNumberofLeaves), ]

g_nleaves <- ggplot(Biomass_outlierleaves, aes(Spectral_f, newNumberofLeaves, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum"=0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  stat_summary(aes(color=Diff),fun.data = "mean_se", geom = "errorbar", position=position_dodge(0.6),width=0)+
  labs(y = expression("Number of leaves per plant"))+
  annotate("text",
           x = "UV-A long",
           y = 75,size=2.5,
           family="sans",
           label="bolditalic(P)~bold('=')~bold('0.01')",
           parse=TRUE)+
  annotate("text", x = "Full spectrum", y = 75,size=2.5,family="sans", 
           label="italic(P)~'='~0.10",
           parse=TRUE)+
  annotate("text",
           x = "No UV",
           y = 75,size=2.5,
           family="sans",
           label="bolditalic(P)~bold('<')~bold('0.01')",
           parse=TRUE)+
  annotate("text", x = 3.4, y=85, label ="(c)", family = "sans", fontface = "bold", size=4)+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

m_poisson_leaves3 <- glm(newNumberofLeaves ~ Spectral*Diff, data = Biomass_outlierleaves, family = poisson())

resid_panel(m_poisson_leaves3, smoother = TRUE, qqbands = TRUE)

summary(m_poisson_leaves3)
Anova(m_poisson_leaves3, type = "III")
pairwise <- emmeans(m_poisson_leaves3, pairwise ~ Diff|Spectral)
summary(pairwise)


