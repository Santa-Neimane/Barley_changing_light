##  This file contains code to analyse leaf pigments measured with
##  Dualex non-destructive device


# Data source
Pigments <- read_excel("Pigments.xlsx")
Pigments <- left_join(Pigments, filtermapping, by="FilterNr")

Pigments$CanopyLevel_f <- factor(Pigments$CanopyLevel, levels=c("Top", "Middle", "Bottom"))
Pigments$FilterNr <- as.factor(Pigments$FilterNr)

Pigments$Spectral_f <- factor(Pigments$Spectral, levels=c("Full spectrum","UV-A long","No UV"))

# Chloophyll
Pigments$Chl_cube <- Pigments$Chl * Pigments$Chl

m_Chl_cube <- lme(Chl_cube ~ Spectral+Diff+CanopyLevel*LeafPart+SunSide+LeafSide,random=~1|FilterNr, data = Pigments)
check_model(m_Chl_cube, )
resid_panel(m_Chl_cube)

summary(m_Chl_cube)
anova(m_Chl_cube)
EMM <- emmeans(m_Chl_cube, specs = pairwise ~ Spectral | LeafPart+CanopyLevel, re_formula=NULL)
EMM
pairwise <- emmeans(m_Chl_cube, pairwise ~ CanopyLevel)
summary(pairwise)


smallChl <- ggplot(subset(Pigments, LeafPart=="Horiz"&CanopyLevel=="Middle"), aes(Spectral_f, Chl, fill=Diff))+
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
  labs(y = expression("Chlorophyll content, Dualex index"))+
  geom_bracket(xmin = "Full spectrum",
               xmax = "No UV",
               label="bolditalic(P)~bold('=')~bold('0.03')", type="expression",
               y.position = 56,
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("UV-A long", "UV-A long"),
               xmax = c("Full spectrum", "No UV"),
               label = c("italic(P)~'='~0.41", "italic(P)~'='~0.36"),
               type="expression",
               y.position = c(50, 48),
               family="sans",
               label.size = 2.8,
               size=0.1,
               inherit.aes = FALSE)+
  expand_limits(y = 60)+
  annotate("text", x = 3.4, y=56, label ="(b)", family = "sans", fontface = "bold", size=4)+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")

# Flavonoids
m_Flav <- lme(Flav ~ Spectral*Diff*LeafSide+SunSide+
                CanopyLevel_f:LeafPart+CanopyLevel_f+
                LeafPart+Spectral:LeafPart+LeafPart:CanopyLevel_f,random=~1|FilterNr, data = Pigments)
anova(m_Flav)
check_model(m_Flav, )

pairwise <- emmeans(m_Flav, pairwise ~ Spectral|CanopyLevel_f+LeafPart+LeafSide)
summary(pairwise)

pairwise <- emmeans(m_Flav, pairwise ~ CanopyLevel_f)
summary(pairwise)


smallFlav <- ggplot(subset(Pigments, LeafPart=="Horiz"&LeafSide=="adaxial"&CanopyLevel=="Middle"), aes(Spectral_f, Flav, fill=Diff))+
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
  stat_summary(aes(color=Diff), fun.data = mean_se, geom = "errorbar", position=position_dodge(0.6),width = 0)+
  theme_PlainGray()+
  theme(legend.position = 'none',
        strip.text = element_text(face = "italic", color = "black", hjust = 0),
        strip.background = element_rect(fill = "lightgrey", linewidth = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank())+
  labs(y = expression("Flavonoid content, Dualex index"))+
  geom_bracket(xmin = "UV-A long",
               xmax = "No UV",
               y.position = 1,
               family="sans",
               label.size = 2.8,
               inherit.aes = FALSE,
               label="italic(P)~'='~0.09",
               type= "expression",
               size=0.1)+
  geom_bracket(xmin = c("UV-A long"),
               xmax = c("Full spectrum"),
               label="bolditalic(P)~bold('<')~bold('0.01')",
               type="expression",
               y.position = c(1.3),
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("Full spectrum"),
               xmax = c("No UV"),
               label="bolditalic(P)~bold('<')~bold('0.01')",
               type="expression",
               y.position = c(1.15),
               family="sans",
               label.size = 2.8,
               inherit.aes = FALSE,
               size=0.43)+
  expand_limits(y = 1.4)+
  annotate("text", x = 3.4, y=1.4, label ="(a)", family = "sans", fontface = "bold", size=4)+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")

# Anthocyanins
PigmentsAnth <- Pigments[!is.na(Pigments$Anth),] # remove empty values to run a model

m_Anth2 <- lme(
  Anth ~ Spectral+Diff+CanopyLevel_f+LeafPart+LeafSide+SunSide+Spectral:Diff+
  Spectral:CanopyLevel_f+
  Diff:CanopyLevel_f+
  Spectral:LeafPart+
  CanopyLevel_f:LeafPart+
  CanopyLevel_f:LeafSide+
  Spectral:Diff:CanopyLevel_f+
  Spectral:CanopyLevel_f:LeafPart+
  CanopyLevel_f:LeafPart:LeafSide,
  random =~1|FilterNr,
  data   = PigmentsAnth)


anova(m_Anth2)
resid_panel(m_Anth, smoother = TRUE, qqbands = TRUE)

pairwise <- emmeans(m_Anth, pairwise ~ Diff|CanopyLevel_f*Spectral*LeafPart)
summary(pairwise)

pairwise <- emmeans(m_Anth, pairwise ~ Spectral|CanopyLevel_f*Diff*LeafPart)
summary(pairwise)


ggplot(subset(PigmentsAnth, LeafPart=="Horiz"&SunSide=="Sun"), aes(Spectral, Anth, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral),
                   stat="summary",
                   fun="mean",
                   pattern_fill="white",
                   pattern_color=NA,
                   width=0.6, 
                   pattern_angle=45, 
                   pattern_key_scale_factor=0.8,
                   position=position_dodge(0.6),
                   alpha=0.7)+
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"))+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  stat_summary(fun.data = mean_se, geom = "errorbar", position=position_dodge(0.6),width = 0)+
  theme_PlainGray()+
  facet_grid(.~CanopyLevel_f)+
  theme(legend.position = 'none',
        strip.text = element_text(face = "italic", color = "black", hjust = 0),
        strip.background = element_rect(fill = "lightgrey", linewidth = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank())+
  labs(y = expression("Anthocyanin content, Dualex index"))
