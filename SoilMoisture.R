## This file contains code to analyse soil moisture measurements
## to check if moisture content significantly does not differ
## across spectral treatments


# Data source
dataSoilMoisture <- read_excel("SoilMoisture0607_1700.xlsx")

# subset to compare only measurements under treatment filters
dataSoilMoistureSub <- subset(dataSoilMoisture, FilterOrOutside=="Filter")

modelsoilm2 <- lme(MoistureContent~SpectralTreatment*Diffusive, random=~1|FilterNumber, 
                data=dataSoilMoistureSub)

resid_panel(modelsoilm2, smoother = TRUE, qqbands = TRUE)

summary(modelsoilm2)
anova(modelsoilm2)
pairwise <- emmeans(modelsoilm2, ~SpectralTreatment*Diffusive)
summary(pairwise)
cld(pairwise, akoha=0.05, Letters=letters) # comparison with letter symbols