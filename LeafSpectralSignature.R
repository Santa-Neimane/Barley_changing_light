##  This file contains code to analyse leaf spectral signature
##  of measurements of reflectance, transmittance
##  and calculated absorbance.
##  In the second part data code to analyse PRI is presented.


# Data source
dataspectra <- read.csv("Spectra.csv")
dataspecta1 <- subset(dataspectra, MeasNr=="1", select=w.length:Reflec) 
# MeasNr - measurement number - "2" is a repeated measurement


# Absorbance --------------------------------------------------------------
dataspecta1$Absorbance <- 1 - dataspecta1$Reflec - dataspecta1$Trans

ggplot(dataspecta1,aes(w.length, Absorbance, color=FilterCoating, fill=FilterCoating))+
  stat_summary(geom = "line", fun.y = mean, width=0.1)+
  stat_summary(geom='ribbon', 
               fun.data = mean_cl_normal, 
               fun.args=list(conf.int=0.95),
               alpha = 0.4,
               colour = NA)+
  theme_bw()+
  facet_nested(SpectralT.x + CanopyLevel ~ LeafSide+MeasLocationLeaf)+
  ggtitle("Absorbance")

ggplot(dataspecta1,aes(w.length, Reflec, color=FilterCoating, fill=FilterCoating))+
  stat_summary(geom = "line", fun.y = mean, width=0.1)+
  stat_summary(geom='ribbon', 
               fun.data = mean_cl_normal, 
               fun.args=list(conf.int=0.95),
               alpha = 0.4,
               colour = NA)+
  theme_bw()+
  facet_nested(SpectralT.x + CanopyLevel ~  LeafSide + MeasLocationLeaf)+
  ggtitle("Reflectance")

ggplot(dataspecta1,aes(w.length, Trans, color=FilterCoating, fill=FilterCoating))+
  stat_summary(geom = "line", fun.y = mean, width=0.1)+
  stat_summary(geom='ribbon', 
               fun.data = mean_cl_normal, 
               fun.args=list(conf.int=0.95),
               alpha = 0.4,
               colour = NA)+
  theme_bw()+
  facet_nested(SpectralT.x + CanopyLevel ~  LeafSide + MeasLocationLeaf)+
  ggtitle("Transmittance")


# PRI ---------------------------------------------------------------------

dataspecta1_PRI <- subset(dataspecta1, w.length==531 | w.length==570)
dataspecta1_PRI$w.length <- as.factor(dataspecta1_PRI$w.length)
dataspecta1_PRI <- pivot_wider(dataspecta1_PRI,
                               id_cols = c("FilterNr","FilterCoating", "CanopyLevel", "MeasLocationLeaf", "LeafSide", "SpectralT.y"),
                               names_from = "w.length",
                               values_from = "Reflec")

colnames(dataspecta1_PRI)[7] <- "w531"
colnames(dataspecta1_PRI)[8] <- "w570"


dataspecta1_PRI$PRI=((dataspecta1_PRI$w531 - dataspecta1_PRI$w570)/(dataspecta1_PRI$w531 + dataspecta1_PRI$w570))

dataspecta1_PRI <- dataspecta1_PRI[-c(72,504),] # removed model outliers
m_PRI <- lme(PRI ~ SpectralT.y+
               FilterCoating+
               CanopyLevel+CanopyLevel:MeasLocationLeaf+
               MeasLocationLeaf+CanopyLevel:MeasLocationLeaf+SpectralT.y:MeasLocationLeaf+FilterCoating:MeasLocationLeaf+
               LeafSide+LeafSide:MeasLocationLeaf,random=~1|FilterNr, data = dataspecta1_PRI)

resid_panel(m_PRI, smoother = TRUE, qqbands = TRUE)

summary(m_PRI)
anova(m_PRI)

pairwise <- emmeans(m_PRI, pairwise ~ SpectralT.y|MeasLocationLeaf)
summary(pairwise)
