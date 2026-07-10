##  This files contains code to gather data and calculate one true experimental
##  mean value under one filer. Firstly all data files are read in, then analysed
##  with PCA and cluster analysis.


PerFilterdata <- read_excel("PerFilterMean.xlsx")
filtermapping <- read_excel("FilterMapping.xlsx")


# Biomass -----------------------------------------------------------------
Biomass <- read_excel("DryBiomass.xlsx")

Biomass <- left_join(filtermapping,Biomass, by="FilterNr")

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.15, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

Biomass_outlierleaves <- Biomass %>%
  group_by(Spectral, Diff) %>% 
  mutate(NumberofLeavesOutlier = remove_outliers(NumberofLeaves))

Biomass_outlierleaves <- Biomass_outlierleaves[,-c(15,16,17)]

PerFilterdata <- left_join(PerFilterdata, Biomass_outlierleaves, by="FilterNr")


# Pigments ----------------------------------------------------------------

Pigments <- read_excel("Pigments.xlsx")
Pigments <- left_join(Pigments, filtermapping, by="FilterNr")

Pigments$Flav[187] <- NA

PigmentsMean <- Pigments %>% 
  group_by(FilterNr) %>%
  summarise(MeanChl = mean(Chl, na.rm = TRUE),
            MeanFlav = mean(Flav, na.rm = TRUE),
            MeanAnth = mean(Anth, na.rm = TRUE))


PigmentsMean$FilterNr <- as.numeric(PigmentsMean$FilterNr)
PerFilterdata <- left_join(PerFilterdata, PigmentsMean, by="FilterNr")


# LAI ---------------------------------------------------------------------

dataLAI <- read_excel("LAI.xlsx")
dataLAI <- left_join(dataLAI, filtermapping, by="FilterNr")
dataLAI$segmentmean <-  (dataLAI$Segment4PAR+dataLAI$Segment5PAR+dataLAI$Segment6PAR)/3
dataLAI$LightTransmittace <- dataLAI$segmentmean / dataLAI$ExternalSensorPAR

LAIperpMean <- dataLAI %>% 
  group_by(FilterNr) %>% 
  summarise(MeanTrans= mean(LightTransmittace))

PerFilterdata <- left_join(PerFilterdata, LAIperpMean[c("FilterNr", "MeanTrans")], by="FilterNr")





# Architecture ------------------------------------------------------------

dataarch <- read_excel("Architect.xlsx")

dataarch$InternodeMid <- as.numeric(dataarch$InternodeMid)
dataarch$LeafTiptoStemBot <- as.numeric(dataarch$LeafTiptoStemBot)

OneParameterPerPlant <- dataarch %>% 
  group_by(FilterNr) %>% 
  summarise(MeanHeightMidS= mean(Height),
            MeanWidthMidS= mean(MaxWidth),
            MeanNumbLeavesMidS= mean(NumberOfLeaves),
            MeanNumbStemsMidS=mean(NumberOfStems))

PerFilterdata <- left_join(PerFilterdata,OneParameterPerPlant, by="FilterNr")


dataarchlevel <- read_xlsx("ArchitectCanopyLevels.xlsx")
dataarchlevel$LeafTiptoStem <- as.numeric(dataarchlevel$LeafTiptoStem)
dataarchlevel$Internode <- as.numeric(dataarchlevel$Internode)


dataarchlevelMean <- dataarchlevel %>% 
  group_by(FilterNr) %>%
  summarise(MeanLeafTiptoStem = mean(LeafTiptoStem, na.rm = TRUE),
            MeanInternode = mean(Internode, na.rm = TRUE)) 

PerFilterdata <- left_join(PerFilterdata, dataarchlevelMean, by="FilterNr")




dataarchlevel_outliersangle <- dataarchlevel %>%
  group_by(Spectral, Diff, CanopyLevel) %>% 
  mutate(newAngle = remove_outliers(LeafAngle))


dataarchlevelMeanAngle <- dataarchlevel_outliersangle %>% 
  group_by(FilterNr) %>%
  summarise(MeannewAngle = mean(newAngle, na.rm = TRUE))


PerFilterdata <- left_join(PerFilterdata, dataarchlevelMeanAngle, by="FilterNr")



# Temperature -------------------------------------------------------------

dataPlantT <- read_xlsx("TemperaturePlant.xlsx")
dataPlantT$PlantT[43] <- NA
dataPlantT$PlantT[208] <- NA


dataPlantTPlantT <- dataPlantT %>% 
  group_by(FilterNr) %>%
  summarise(MeanPlantTemp = mean(PlantT, na.rm = TRUE)) 


PerFilterdata <- left_join(PerFilterdata, dataPlantTPlantT, by="FilterNr")

## Soil
dataSoilT <- read_excel("TemperatureSoil.xlsx")
dataSoilT$SoilT[65] <- NA
dataSoilT$SoilT[171] <- NA

## Diffuse and Shade/Sun

dataSoilTMean <- (subset(dataSoilT, dataSoilT$ShadeSun=="Sunny")) %>% 
  group_by(FilterNr) %>%
  summarise(MeanSoilTDirect = mean(SoilT, na.rm = TRUE))

PerFilterdata <- left_join(PerFilterdata, dataSoilTMean, by="FilterNr")


dataSoilTMeanShade <- (subset(dataSoilT, dataSoilT$ShadeSun=="Shade"))%>% 
  group_by(FilterNr) %>%
  summarise(MeanSoilTShade = mean(SoilT, na.rm = TRUE))

PerFilterdata <- left_join(PerFilterdata, dataSoilTMeanShade, by="FilterNr")

# LeafTraits --------------------------------------------------------------

LeafTraits <- read_excel("LeafTraits.xlsx")

LeafTraitsPerFilter <- LeafTraits %>% 
  group_by(FilterNr) %>%
  summarise(MeanArea = mean(Area, na.rm = TRUE),
            MeanSLA = mean(SLA, na.rm = TRUE),
            MeanDryWeight = mean(DryWeightmg, na.rm = TRUE))


PerFilterdata <- left_join(PerFilterdata, LeafTraitsPerFilter, by="FilterNr")


# Leaf Spectra ------------------------------------------------------------

dataspectra <- read.csv("Spectra.csv")
dataspecta1 <- subset(dataspectra, MeasNr=="1", select=w.length:Reflec)

dataspecta1$Absorbance <- 1 - dataspecta1$Reflec - dataspecta1$Trans

dataspecta1_PRI <- subset(dataspecta1, w.length==531 | w.length==570)
dataspecta1_PRI$w.length <- as.factor(dataspecta1_PRI$w.length)
dataspecta1_PRI <- pivot_wider(dataspecta1_PRI,
                               id_cols = c("FilterNr","FilterCoating", "CanopyLevel", "MeasLocationLeaf", "LeafSide", "SpectralT.y"),
                               names_from = "w.length",
                               values_from = "Reflec")

colnames(dataspecta1_PRI)[7] <- "w531"
colnames(dataspecta1_PRI)[8] <- "w570"


dataspecta1_PRI$PRI=((dataspecta1_PRI$w531 - dataspecta1_PRI$w570)/(dataspecta1_PRI$w531 + dataspecta1_PRI$w570))


PRIPerFilter <- dataspecta1_PRI %>% 
  group_by(FilterNr) %>% 
  summarise(Mean_PRI= mean(PRI))

PerFilterdata <- left_join(PerFilterdata,PRIPerFilter, by="FilterNr")



# Gas Exchange ------------------------------------------------------------

dataGas <- read_excel("GasExchange.xlsx")
dataGas <- left_join(dataGas, filtermapping, by="FilterNr")

dataGas$Grouping <- paste0(dataGas$Spectral,"_",dataGas$Diff)


GasExchange2 <- subset(dataGas, PARi<1990)

GasExchange3 <- subset(GasExchange2, FilterNr!=7)
GasExchange3 <- subset(GasExchange3, FilterNr!=19)



sequence <- 1:36
f_sequence <- setdiff(sequence, c(7, 19))
df_modelresult <- data.frame(FilterNr=numeric(),
                             GasExphi=numeric(),
                             GasExPgmax=numeric(),
                             GasExR=numeric())

for (FN in f_sequence) {
  df <- subset(GasExchange3, FilterNr==FN)
  model <- nlsLM(data=df,
                 formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
                 start = c(phiI0 = 0.02, Pgmax = 60, R =3))
  
  phi <- round(summary(model)$coefficients[1,1], 2)
  Pgmax <- round(summary(model)$coefficients[2,1],1)
  R <- round(summary(model)$coefficients[3,1],2)

  df_modelresult <- add_row(df_modelresult, FilterNr=FN, GasExphi=phi, GasExPgmax=Pgmax,GasExR=R)
}


df_modelresult$calcPhoto1500 <- (((df_modelresult$GasExphi * 1500 * df_modelresult$GasExPgmax) / (df_modelresult$GasExphi * 1500 + df_modelresult$GasExPgmax)) - df_modelresult$GasExR)


df_modelresult <- left_join(df_modelresult,filtermapping, by="FilterNr")




m_calcPhoto1500 <- lm(calcPhoto1500 ~ Spectral*Diff, data = df_modelresult)
summary(m_calcPhoto1500)
anova(m_calcPhoto1500)
resid_panel(m_calcPhoto1500, smoother = TRUE, qqbands = TRUE)

m_calcPhoto1500 <- lm(calcPhoto1500 ~ Spectral+Diff, data = df_modelresult)
summary(m_calcPhoto1500)
anova(m_calcPhoto1500)
resid_panel(m_calcPhoto1500, smoother = TRUE, qqbands = TRUE)



m_phi <- lm(GasExphi ~ Spectral*Diff, data = df_modelresult)
summary(m_phi)
anova(m_phi)
resid_panel(m_phi, smoother = TRUE, qqbands = TRUE)

m_phi <- lm(GasExphi ~ Spectral+Diff, data = df_modelresult)
summary(m_phi)
anova(m_phi)
resid_panel(m_phi, smoother = TRUE, qqbands = TRUE)



m_Pgmax <- lm(GasExPgmax ~ Spectral*Diff, data = df_modelresult)
summary(m_Pgmax)
anova(m_Pgmax)
resid_panel(m_Pgmax, smoother = TRUE, qqbands = TRUE)

m_Pgmax <- lm(GasExPgmax ~ Spectral+Diff, data = df_modelresult)
summary(m_Pgmax)
anova(m_Pgmax)
resid_panel(m_Pgmax, smoother = TRUE, qqbands = TRUE)



m_Pgmax <- lm(GasExPgmax ~ Spectral+Diff, data = df_modelresult)
summary(m_Pgmax)
anova(m_Pgmax)
resid_panel(m_Pgmax, smoother = TRUE, qqbands = TRUE)


df_modelresult %>% 
  group_by(Spectral, Diff) %>% 
  summarize(MeanPgmax=mean(GasExPgmax, na.rm=TRUE))




m_R <- lm(GasExR ~ Spectral*Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)


#df_modelresult$resid <- resid(m_R)
#df_modelresult$fitted <- fitted(m_R)


#ggplot(df_modelresult, aes(fitted, resid, label=FilterNr))+
#  geom_point()+
#  geom_text()

###!!!!
df_modelresult[27,]$GasExR <- NA

m_R <- lm(GasExR ~ Spectral+Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)


qq <- qqnorm(df_modelresult$GasExR, plot.it = FALSE)

qqnorm(df_modelresult$GasExR)
qqline(df_modelresult$GasExR)
text(qq$x,qq$y, labels = df_modelresult$FilterNr)


###!!!
df_modelresult[24,]$GasExR <- NA
m_R <- lm(GasExR ~ Spectral*Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)

###!!!
df_modelresult[19,]$GasExR <- NA
m_R <- lm(GasExR ~ Spectral+Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)


PerFilterdata <- left_join(PerFilterdata,df_modelresult[,-c(5:8)], by="FilterNr")


############# Conductance

dataGas_Stomatal <- read_excel("GasExchange.xlsx")
dataGas_Stomatal <- dataGas_Stomatal[-157,]
dataGas_Stomatal <- left_join(dataGas_Stomatal, filtermapping, by="FilterNr")

GasExchange3_Stomatal <- subset(dataGas_Stomatal, FilterNr!=7)
GasExchange3_Stomatal <- subset(GasExchange3_Stomatal, FilterNr!=19)



sequence <- 1:36
f_sequence <- setdiff(sequence, c(7, 19))
df_modelresult_Stomatal <- data.frame(FilterNr=numeric(),
                             GasCondAlpha=numeric(),
                             GasCondBeta=numeric(),
                             GasCondGamma=numeric(),
                             GasCondGmin=numeric())

for (FN in f_sequence) {
  df <- subset(GasExchange3_Stomatal, FilterNr==FN)
  model <- nlsLM(data=df,
                 formula = Cond ~ alpha*((1-beta*PARi)/(1+gamma*PARi))*PARi+gmin,
                 start = c(alpha=0.002, beta=0.0008, gamma=0.01, gmin=0.4))
  
  alpha <- round(summary(model)$coefficients[1,1], 4)
  beta <- round(summary(model)$coefficients[2,1],5)
  gamma <- round(summary(model)$coefficients[3,1],4)
  gmin <- round(summary(model)$coefficients[4,1],4)
  
  df_modelresult_Stomatal <- add_row(df_modelresult_Stomatal, FilterNr=FN,
                                     GasCondAlpha=alpha, GasCondBeta=beta,
                                     GasCondGamma=gamma, GasCondGmin=gmin)
}



PerFilterdata <- left_join(PerFilterdata,df_modelresult_Stomatal, by="FilterNr")


# Fv/Fm -------------------------------------------------------------------

dataPAM <- read_excel("FvFm.xlsx")
dataPAM <- left_join(dataPAM, filtermapping, by="FilterNr")
dataPAM$Fv_Fm <- as.numeric(dataPAM$Fv_Fm)

dataPAM <- subset(dataPAM, !is.na(Fv_Fm))
dataPAM <- dataPAM[-76,]

dataPAMMean <- dataPAM %>% 
  group_by(FilterNr,Spectral, Diff) %>% 
  summarise(MeanFvFm = mean(Fv_Fm))


PerFilterdata <- left_join(PerFilterdata,dataPAMMean[,c(1,4)], by="FilterNr")


# Data prep and correlation -----------------------------------------------
PCAdata <- PerFilterdata[,c(5:41)]
colnames(PCAdata)

new_names <- c("Spike dw", "Stem dw", "Grain dw", "Leaves dw", "Root dw", "Plant dw",
               "Root:shoot", "Height grains", "Height","No. stems",  "No. leaves", "Chlorophyll",
               "Flavonols","Anthocyanins","Canopy trans","Height mid", "Width mid",  "No. leaves mid",
               "No. stems mid","Leaf curv. r", "Internode length","Leaf angle",   "Plant temp","Soil temp direct",
               "Soil temp shade", "Leaf area","SLA", "Leaf dw","PRI","Photo Exphi","Photo Pgmax","Photo R","CondAlpha",
               "CondBeta", "CondGamma","CondGmin","FvFm")

colnames(PCAdata) <- new_names

cor_mat <- cor(PCAdata, use = "pairwise.complete.obs")
testRes = cor.mtest(PCAdata, conf.level = 0.95)


PCAdata <- PCAdata[,!(names(PCAdata) %in% c("Height"))]

cor_mat <- cor(PCAdata, use = "pairwise.complete.obs")
testRes = cor.mtest(PCAdata, conf.level = 0.95)

PCAdata$Spectral <- PerFilterdata$Spectral
PCAdata$Diff <- PerFilterdata$Diff


ggplot(PCAdata, aes(x = Flavonols, y = SLA)) +
  stat_poly_line(color="grey", alpha=0.2) +
  stat_poly_eq(formula = y ~ x,
               use_label(c("R2", "p", "n")),
               label.x = "right") +
  geom_point(aes(, color=Diff, shape=Spectral), alpha=0.7)+
  scale_color_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  scale_shape_manual(name="Spectral treatment", values=c(16,17,15))+
  
  labs(y=expression(paste('Mean specific leaf area, ', cm^{2},' ', "g"^{-1})),
       x="Mean Flavonol content, Dualex units")+
  theme_PlainGray()
ggsave('Ready_Figure7.png', width = 15, height =7, units='cm', dpi=1500)




PCAdataExtra <- PerFilterdata
PCAdataExtra$FlavonolsPerSLA <- PCAdataExtra$MeanFlav/PCAdataExtra$MeanSLA



ggplot(PCAdataExtra, aes(Spectral, FlavonolsPerSLA, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral),
                   stat="summary",
                   fun="mean",
                   pattern_fill="white",
                   pattern_color=NA,
                   width=0.5, 
                   pattern_angle=45, 
                   pattern_key_scale_factor=0.5,
                   alpha=0.7,
                   size=0.5,
                   position = "dodge")+
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"))+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  stat_summary(fun.data = mean_se, geom = "errorbar",width = 0, position = "dodge", aes(color=Diff))+
  theme_PlainGray()



m_Flav_perSLA <- lm(FlavonolsPerSLA ~ Spectral*Diff, data = PCAdataExtra)

m_Flav_perSLA <- lm(FlavonolsPerSLA ~ Spectral+Diff, data = PCAdataExtra)
summary(m_Flav_perSLA)
anova(m_Flav_perSLA)resid_panel(m_Flav_perSLA, smoother = TRUE, qqbands = TRUE)

pairwise <- emmeans(m_Flav_perSLA, pairwise ~ Spectral)
summary(pairwise)


#PCAdataExtra$Spectral_f <- factor(PCAdataExtra$Spectral, levels=c("Full spectrum","UV-A long", "No UV"))




ggplot(PCAdataExtra, aes(Spectral_f, FlavonolsPerSLA, fill=Diff))+
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
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"),guide="none")+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum"=0, "No UV"=0.25),guide="none")+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")+
  theme_PlainGray()+
  labs(y=expression(paste('Mean flavonoid content per SLA, Dualex index  ', cm^{-2},' ', "g"^{-1})),
       x=NULL)+
  stat_summary(aes(color=Diff),fun.data = "mean_se", geom = "errorbar", position=position_dodge(0.6),width=0)+
  geom_bracket(xmin = "UV-A long",
               xmax = "No UV",
               y.position = 0.0040,
               family="sans",
               label.size = 2.8,
               size=0.43,
               label="bolditalic(P)~bold('=')~bold('0.01')",
               type= "expression",
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("UV-A long"),
               xmax = c("Full spectrum"),
               label="bolditalic(P)~bold('<')~bold('0.01')",
               type="expression",
               y.position = 0.0050,
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("Full spectrum"),
               xmax = c("No UV"),
               label="bolditalic(P)~bold('<')~bold('0.01')",
               type="expression",
               y.position = 0.0045,
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  expand_limits(y = 0.005)+
  guides(fill=guide_legend(override.aes=list(pattern="none")))+
  theme(legend.position = 'bottom')

PCAdataExtra$ChloroPerSLA <- PCAdataExtra$MeanChl/PCAdataExtra$MeanSLA
m_Chloro_perSLA <- lm(ChloroPerSLA ~ Spectral*Diff, data = PCAdataExtra)
summary(m_Chloro_perSLA)
anova(m_Chloro_perSLA)
resid_panel(m_Chloro_perSLA, smoother = TRUE, qqbands = TRUE)

pairwise <- emmeans(m_Chloro_perSLA, pairwise ~ Spectral)
summary(pairwise)


ggplot(PCAdataExtra, aes(Spectral_f, ChloroPerSLA, fill=Diff))+
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
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"),guide="none")+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum"=0, "No UV"=0.25),guide="none")+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")+
  theme_PlainGray()+
  labs(y=expression(paste('Mean chlorophyll content per SLA, Dualex index  ', cm^{-2},' ', "g"^{-1})),
       x=NULL)+
  stat_summary(aes(color=Diff),fun.data = "mean_se", geom = "errorbar", position=position_dodge(0.6),width=0)+
  geom_bracket(xmin = "UV-A long",
               xmax = "No UV",
               y.position = 0.21,
               family="sans",
               label.size = 2.8,
               size=0.43,
               label="bolditalic(P)~bold('=')~bold('0.03')",
               type= "expression",
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("UV-A long"),
               xmax = c("Full spectrum"),
               label="italic(P)~'='~'0.14'",
               type="expression",
               y.position = 0.19,
               family="sans",
               label.size = 2.8,
               size=0.1,
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("Full spectrum"),
               xmax = c("No UV"),
               label="bolditalic(P)~bold('<')~bold('0.01')",
               type="expression",
               y.position = 0.23,
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  expand_limits(y = 0.25)+
  guides(fill=guide_legend(override.aes=list(pattern="none")))+
  theme(legend.position = 'bottom')


# PCA ---------------------------------------------------------------------
PCAperfilter <- PCAdata
nb = estim_ncpPCA(PCAperfilter[, 1:37], ncp.max=5)
nb
res.comp = imputePCA(PCAperfilter[, 1:37],ncp=2, scale=TRUE,  method = "Regularized")

df_pca_full <- as.data.frame(res.comp$comp)

df_pca_full$FilterNr <- PerFilterdata$FilterNr
df_pca_full$Spectral <- PerFilterdata$Spectral
df_pca_full$Diff <- PerFilterdata$Diff
df_pca_full$Row <- PerFilterdata$Row
df_pca_full$Group <- paste(PerFilterdata$Spectral,PerFilterdata$Diff)

# Highly correlated
df_pca_redacted <- df_pca_full[,!(names(df_pca_full) %in% c("Spike dw","Stem dw","Leaves dw", "Root dw"))]

# Height taken off
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Height grains"))]

# Height mid season taken off since it correlates with internode length
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Height mid"))]
# take off soil T
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Soil temp direct", "Soil temp shade"))]

cv <- apply(df_pca_redacted[, 1:30], 2, function(x) {
  sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE))
})

print(cv)
### Between each step was itteration of assesing correlation and contributions
# remove Height Mean Chl, MeanAnth, Number of LeavesMids, MeanPlantT, Fv/Fm
#+ No stems, No leaves since it correlates with dry mass highly
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Plant temp"))]
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Leaf area", "Leaf curv. r"))]
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Photo Exphi", "CondGamma", "CondBeta"))]
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Width mid", "PRI", "CondAlpha"))]
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Canopy trans", "Leaf angle"))]
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("No stems", "No leaves"))]
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Leaf dw"))]
df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Grain dw"))]

new_names <- c("Plant dw", "Root:shoot", "Height", "Chlorophyll", "Flavonol",
               "Anthocyanin", "No. leaves", "No. stems", "Internode length",
               "SLA", "Photo. Pgmax","Photo. R","Cond. Gmin","Fv/Fm",
               "FilterNr", "Spectral", "Diff", "Row", "Group")

colnames(df_pca_redacted) <- new_names


df_pca_redacted <- df_pca_redacted[ ,!(names(df_pca_redacted) %in% c("Cond. Gmin"))]

new_names <- c("Plant dw", "Root:shoot", "Height", "Chlorophyll", "Flavonol",
               "Anthocyanin", "No. leaves", "No. stems", "Internode length",
               "SLA", "Photo. Pgmax","Photo. R","Fv/Fm",
               "FilterNr", "Spectral", "Diff", "Row", "Group")

colnames(df_pca_redacted) <- new_names


pca_res <- PCA(df_pca_redacted, scale.unit = TRUE, ncp = 2, quali.sup = 14:18)
summary(pca_res)


sort(pca_res$var$contrib[,1], decreasing=TRUE)
sort(pca_res$var$contrib[,2], decreasing=TRUE)

df_pca_redacted$Group <- paste(df_pca_redacted$Spectral,df_pca_redacted$Diff)

g_PCA <- fviz_pca_biplot(pca_res,
                mean.point = TRUE,
                col.ind = df_pca_redacted$Group,
                repel = TRUE,
                alpha.var = 0.5,
                alpha.ind=0.4,
                addEllipses= FALSE,
                label = "var",
                pointsize = 2,
                labelsize=3,
                arrowsize=0.3)+
  theme_PlainGray()+
  scale_shape_manual(values = c(16, 10, 16,10,16,10))+
  scale_color_manual(values = c("#9997ff","#9997ff","#97ff99","#97ff99","darkgray","darkgray"))+
  theme(legend.position = "none",
        plot.title = element_blank())+
  labs(x="PC 1 (28.4%)", y="PC 2 (21%)")+
  annotate("text", x = 4, y=3, label ="(a)", family = "sans", fontface = "bold", size=4)
