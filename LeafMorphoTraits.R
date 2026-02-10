##  This file contains code to analyse leaf traits (area, weight, SLA)
##  


#  Data source
LeafTraits <- read_excel("LeafTraits.xlsx")
LeafTraits <- left_join(LeafTraits, filtermapping, by="FilterNr")
LeafTraits$CanopyLevel_f <- factor(LeafTraits$CanopyLevel, levels=c("Top", "Middle", "Bottom"))
LeafTraits$Spectral_f <- factor(LeafTraits$Spectral, levels=c("Full spectrum","UV-A long","No UV"))


# Leaf Area
ggplot(LeafTraits, aes(Spectral_f, Area, fill=Diff))+
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
  theme(legend.position = 'none',
        axis.line.x = element_blank(),  # remove x axis line,
        axis.ticks.x = element_blank(), # remove x axis ticks
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "lightgrey", fill = NA))+
  facet_grid(~CanopyLevel_f)+
  labs(subtitle="Position in the canopy", y = expression("Leaf area, " * cm^2))

m_Area <- lme(Area ~ Spectral+Diff+CanopyLevel,random=~1|FilterNr, data = LeafTraits)
resid_panel(m_Area, smoother = TRUE, qqbands = TRUE)

summary(m_Area)
anova(m_Area)

# Dry Weight
ggplot(LeafTraits, aes(Spectral_f, DryWeightmg, fill=Diff))+
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
  theme(legend.position = 'none',
        axis.line.x = element_blank(),  # remove x axis line,
        axis.ticks.x = element_blank(), # remove x axis ticks
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "lightgrey", fill = NA))+
  facet_grid(~CanopyLevel_f)+
  labs(subtitle="Position in the canopy", y = expression("Dry weight, mg "))

m_DryWeight <- lme(DryWeightmg ~ Spectral+Diff+CanopyLevel,random=~1|FilterNr, data = LeafTraits)
resid_panel(m_DryWeight, smoother = TRUE, qqbands = TRUE)

summary(m_DryWeight)
anova(m_DryWeight)


# SLA
smallSLA <- ggplot(subset(LeafTraits, CanopyLevel=="Middle"), aes(Spectral_f, SLA, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  stat_summary(aes(color=Diff),fun.data = mean_se, geom = "errorbar", position=position_dodge(0.6),width = 0)+
  theme_PlainGray()+
  theme(legend.position = 'none',
        strip.text = element_text(face = "italic", color = "black", hjust = 0),
        strip.background = element_rect(fill = "lightgrey", linewidth = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank())+
  labs(y=expression(paste('Specific leaf area, ', cm^{2},' ', "g"^{-1})))+
  geom_bracket(xmin = "Full spectrum",
               xmax = "UV-A long",
               label="italic(P)~'='~0.25",               
               type="expression",
               y.position = 310,
               family="sans",
               label.size = 2.8,
               size=0.1,
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("Full spectrum", "UV-A long"),
               xmax = c("No UV", "No UV"),
               label = c("bolditalic(P)~bold('<')~bold('0.01')", "bolditalic(P)~bold('<')~bold('0.01')"),
               type = "expression",
               y.position = c(350, 390),
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  expand_limits(y = 400)+
  annotate("text", x = 3.4, y=400, label ="(c)", family = "sans", fontface = "bold", size=4)+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")

m_SLA <- lme(SLA ~ Spectral+Diff+CanopyLevel,random=~1|FilterNr, data = LeafTraits)
resid_panel(m_SLA, smoother = TRUE, qqbands = TRUE)

summary(m_SLA)
anova(m_SLA)

pairwise <- emmeans(m_SLA, pairwise ~ Spectral|CanopyLevel)
summary(pairwise)

