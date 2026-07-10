##  This files contains code to gather data and calculate one true experimental
##  mean value under one filer. Firstly all data files are read in, then analysed
##  with PCA and cluster analysis.

# Experimental setup and main file generation -----------------------------
PerFilterdata <- read_excel("PerFilterMean.xlsx")
filtermapping <- read_excel("FilterMapping.xlsx")

# Biomass harvest ---------------------------------------------------------
Biomass <- read_excel("DryBiomass.xlsx")
Biomass <- left_join(filtermapping,Biomass, by="FilterNr")

Biomass_outlierleaves <- Biomass %>%
  group_by(Spectral, Diff) %>% 
  mutate(NumberofLeavesOutlier = remove_outliers(NumberofLeaves))

Biomass_outlierleaves <- Biomass_outlierleaves[,-c(15,16,17)] # removes duplicated columns


PerFilterdata <- left_join(PerFilterdata, Biomass_outlierleaves, by="FilterNr")

# Pigments ----------------------------------------------------------------
Pigments <- read_excel("Pigments.xlsx")
Pigments <- left_join(Pigments, filtermapping, by="FilterNr")

Pigments$Flav[187] <- NA

PigmentsMean <- Pigments %>% 
  group_by(FilterNr) %>%
  summarise(MeanChl = mean(Chl, na.rm = TRUE),MeanFlav = mean(Flav, na.rm = TRUE), MeanAnth = mean(Anth, na.rm = TRUE))

PigmentsMean$FilterNr <- as.numeric(PigmentsMean$FilterNr)
PerFilterdata <- left_join(PerFilterdata, PigmentsMean, by="FilterNr")

# Canopy transmittance ----------------------------------------------------
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
  summarise(MeanHeightMidS= mean(Height), MeanWidthMidS= mean(MaxWidth),MeanNumbLeavesMidS= mean(NumberOfLeaves), MeanNumbStemsMidS=mean(NumberOfStems))

PerFilterdata <- left_join(PerFilterdata,OneParameterPerPlant, by="FilterNr")

# second file, where there was division across canopy height levels
dataarchlevel <- read_xlsx("ArchitectCanopyLevels.xlsx")
dataarchlevel$LeafTiptoStem <- as.numeric(dataarchlevel$LeafTiptoStem)
dataarchlevel$Internode <- as.numeric(dataarchlevel$Internode)


dataarchlevelMean <- dataarchlevel %>% 
  group_by(FilterNr) %>%
  summarise(MeanLeafTiptoStem = mean(LeafTiptoStem, na.rm = TRUE), MeanInternode = mean(Internode, na.rm = TRUE)) 

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

# Soil temperature
dataSoilT <- read_excel("TemperatureSoil.xlsx")
dataSoilT$SoilT[65] <- NA
dataSoilT$SoilT[171] <- NA

# Diffuse and Shade/Sun

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

GasExchange2 <- dataGas[-157, ]
GasExchange2 <- subset(GasExchange2, PARi<1990)

GasExchange3 <- subset(GasExchange2, FilterNr!=7)
GasExchange3 <- subset(GasExchange3, FilterNr!=29)


sequence <- 1:36
f_sequence <- setdiff(sequence, c(7, 29))
df_modelresult <- data.frame(FilterNr=numeric(), GasExphi=numeric(), GasExPgmax=numeric(), GasExR=numeric())

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

df_modelresult[18,]$GasExPgmax <- NA


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

df_modelresult[31,]$GasExR <- NA

m_R <- lm(GasExR ~ Spectral+Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)

df_modelresult[25,]$GasExR <- NA
m_R <- lm(GasExR ~ Spectral*Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)

df_modelresult[20,]$GasExR <- NA
m_R <- lm(GasExR ~ Spectral+Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)

df_modelresult[18,]$GasExR <- NA
m_R <- lm(GasExR ~ Spectral+Diff, data = df_modelresult)
summary(m_R)
anova(m_R)
resid_panel(m_R, smoother = TRUE, qqbands = TRUE)

PerFilterdata <- left_join(PerFilterdata,df_modelresult[,-c(5:8)], by="FilterNr")

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

write_xlsx(PerFilterdata, "JoinedAllWithNA.xlsx")

# Data prep and correlation -----------------------------------------------

PerFilterdata <- read_xlsx("JoinedAllWithNA.xlsx")

PCAdata <- PerFilterdata[,c(5:37)]
colnames(PCAdata)

new_names <- c("Spike dw", "Stem dw", "Grain dw", "Leaves dw", "Root dw", "Plant dw",
               "Root to above ratio", "Height grains", "Height","No stems",  "No leaves", "Chlorophyll",
               "Flavonols","Anthocyanins","Canopy trans","Height mid", "Width mid",  "No leaves mid",
               "No stems mid","Leaf curv. r", "Internode length","Leaf angle",   "Plant temp","Soil temp direct",
               "Soil temp shade", "Leaf area","SLA", "Leaf dw","PRI","Photo Exphi","Photo Pgmax","Photo R","FvFm")

colnames(PCAdata) <- new_names

cor_mat <- cor(PCAdata, use = "pairwise.complete.obs")
testRes = cor.mtest(PCAdata, conf.level = 0.95)

corrplot(cor_mat, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'lower',
         sig.level = c(0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20',
         tl.col = "black",
         tl.cex = 0.4)

PCAdata$Spectral <- PerFilterdata$Spectral
PCAdata$Diff <- PerFilterdata$Diff

# PCA ---------------------------------------------------------------------
PCAperfilter <- PCAdata

# imputing missing values
nb = estim_ncpPCA(PCAperfilter[, 1:33],ncp.max=5)
nb
res.comp = imputePCA(PCAperfilter[, 1:33],ncp=2, scale=TRUE,  method = "Regularized")

df_pca_full <- as.data.frame(res.comp$comp) 
df_pca_full$FilterNr <- PerFilterdata$FilterNr
df_pca_full$Spectral <- PerFilterdata$Spectral
df_pca_full$Diff <- PerFilterdata$Diff
df_pca_full$Row <- PerFilterdata$Row
df_pca_full$Group <- paste(PerFilterdata$Spectral,PerFilterdata$Diff)

pca_res <- PCA(df_pca_full, scale.unit = TRUE, ncp = 2, quali.sup = 34:38)
summary(pca_res)
fviz_pca_var(pca_res)

fviz_pca_biplot(pca_res,
                col.ind = df_pca_full$Group,
                repel = TRUE)

# biomass taken off
df_pca_redacted <- df_pca_full[,-c(1:5)]


pca_res <- PCA(df_pca_redacted, scale.unit = TRUE, ncp = 2, quali.sup = 29:33)
summary(pca_res)
fviz_pca_var(pca_res)

fviz_pca_biplot(pca_res,
                col.ind = df_pca_full$Group,
                repel = TRUE)



# height taken off
df_pca_redacted <- df_pca_redacted [,-3]

pca_res <- PCA(df_pca_redacted, scale.unit = TRUE, ncp = 2, quali.sup = 28:32)
summary(pca_res)
fviz_pca_var(pca_res)

fviz_pca_biplot(pca_res,
                col.ind = df_pca_redacted$Group,
                repel = TRUE)


# height mid season taken off since it correlates with internode length
df_pca_redacted <- df_pca_redacted [,-10]

pca_res <- PCA(df_pca_redacted, scale.unit = TRUE, ncp = 2, quali.sup = 27:31)
summary(pca_res)
fviz_pca_var(pca_res)

fviz_pca_biplot(pca_res,
                col.ind = df_pca_redacted$Group,
                repel = TRUE)


# removing soil T
df_pca_redacted <- df_pca_redacted [,-c(17,18)]


# removing traits with low coefficient of variation (cv), treshold 0.1
cv <- apply(df_pca_redacted[, 1:24], 2, function(x) {
  sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE))
})

cv_threshold <- 0.1

barplot(sort(cv), las = 2, main = "Trait CVs", col = "skyblue", cex.names = 0.7)
abline(h = cv_threshold, col = "red", lty = 2)


# remove Height Mean Chl, MeanAnth, Number of LeavesMids, MeanPlantT, Fv/Fm
# and No stems, No leaves since it correlates with dry mass highly

df_pca_redacted <- df_pca_redacted [,-c(3,4,5,6,8,11,16,24)]

df_pca_redacted_orig <- df_pca_redacted

pca_res <- PCA(df_pca_redacted[,1:16], scale.unit = TRUE, ncp = 2)
summary(pca_res)
fviz_pca_var(pca_res)

df_pca_redacted$Group <- paste(df_pca_redacted$Spectral,df_pca_redacted$Diff)

g_PCA <- fviz_pca_biplot(pca_res,
                mean.point = FALSE,
                col.ind = df_pca_redacted$Group,
                repel = TRUE,
                alpha.var = 0.5,
                alpha.ind=0.8,
                addEllipses= FALSE,
                label = "var",
                pointsize = 2,
                labelsize=3,
                arrowsize=0.3)+
  theme_PlainGray()+
  scale_shape_manual(values = c(16, 10, 16,10,16,10))+
  scale_color_manual(values = c("#9997ff","#9997ff","#97ff99","#97ff99","#ff9997","#ff9997"))+
  theme(legend.position = "none",
        plot.title = element_blank())+
  labs(x="PC 1 (21%)", y="PC 2 (15.2%)")

# Clustering --------------------------------------------------------------
df_in <- data.frame(df_pca_redacted[, 1:16])

# two clusters
km <- kmeans(df_in,centers=2)
fviz_cluster(km, data = df_in, ellipse.type = "norm")

# background for clusters
pca_res <- PCA(df_in, scale.unit = TRUE, ncp = 2)
df_clustered <- data.frame(df_in)

pca_clust <- prcomp(df_in, scale=TRUE)
df_clustered$PC1 <- pca_clust$x[,1]
df_clustered$PC2 <- pca_clust$x[,2]

df_clustered$cluster <- as.factor(km$cluster)
df_clustered$Group <- df_pca_redacted$Group
df_clustered$Spectral <- df_pca_redacted$Spectral
df_clustered$Diff <- df_pca_redacted$Diff


ggplot(df_clustered, aes(x = PC1, y = PC2)) +
  labs(title = "Cluster analysis",
       x = "PC1",
       y = "PC2",
       color = "Cluster",
       shape = "Group") +
  theme_PlainGray()+
  stat_chull(geom="polygon",aes(fill=cluster), alpha=0.2)+
  geom_point(aes(color=Spectral, shape=Diff),size = 2, alpha = 0.8)+
  scale_shape_manual(name="Filter type",values = c(10, 16))+
  scale_color_manual(name="Spectral treatment",values = c("#9997ff", "#97ff99", "#ff9997"))+
  scale_fill_manual(name="Cluster number", values = c("lightgrey", "#97ff99"))+
  labs(x="PC 1 (21%)", y="PC 2 (15.2%)")+
  theme(plot.title = element_blank())
