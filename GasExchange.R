## This file contains code to analyse gas exchange data
##



# Licor gas exchange data analysis ----------------------------------------

# Data read in
dataGas <- read_excel("GasExchange.xlsx")
dataGas <- left_join(dataGas, filtermapping, by="FilterNr")
dataGas$Grouping <- paste0(dataGas$Spectral,"_",dataGas$Diff)
GasExchange2 <- dataGas[-157, ]
GasExchange2 <- subset(GasExchange2, PARi<1990)
GasExchange3 <- subset(GasExchange2, FilterNr!=7) # removal of outlier
GasExchange3 <- subset(GasExchange3, FilterNr!=29


# Fit separate models for each group and store predictions
predictions <- GasExchange3 %>%
  group_by(Grouping) %>%
  do({
    model <- nlsLM(data = ., 
                   formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
                   start = c(phiI0 = 0.02, Pgmax = 60, R = 1))
    new.data <- data.frame(PARi = seq(0, 1990, by = 1))
    pred <- predFit(model, newdata = new.data, interval = "confidence", level = 0.95)
    cbind(new.data, as.data.frame(pred))
  }) %>%
  bind_rows()

# Plot all fits and confidence intervals
ggplot(predictions, aes(x = PARi, y = fit, color = Grouping,fill=Grouping, linetype = Grouping)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) +
  geom_point(data = GasExchange3, aes(x = PARi, y = Photo)) +
  theme_minimal()


# No UV spectral treatment group ------------------------------------------
# Calculate parameter values
model <- nlsLM(data = GasExchange3[GasExchange3$Grouping=="No UV_Diffuse",], 
               formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
               start = c(phiI0 = 0.02, Pgmax = 60, R = 1))
summary(model)
UV_diff_phi <- round(summary(model)$coefficients[1,1], 2)
UV_diff_Pgmax <- round(summary(model)$coefficients[2,1],1)
UV_diff_R <- round(summary(model)$coefficients[3,1],2)


model <- nlsLM(data = GasExchange3[GasExchange3$Grouping=="No UV_Clear",], 
               formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
               start = c(phiI0 = 0.02, Pgmax = 60, R = 1))
summary(model)
UV_clear_phi <- round(summary(model)$coefficients[1,1], 2)
UV_clear_Pgmax <- round(summary(model)$coefficients[2,1],1)
UV_clear_R <- round(summary(model)$coefficients[3,1],2)

# Graph
UV <- ggplot((subset(predictions,  Grouping %in% c("No UV_Diffuse", "No UV_Clear"))), aes(x = PARi, y = fit, color = Grouping, fill=Grouping, linetype = Grouping)) +
  geom_line(size=0.8) +
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4,  size=0.2) +
  theme_PlainGray()+
  ylim(-10, 50)+
  labs(x=expression(paste('PAR')))+
  labs(title = "No UV")+
  theme(plot.title = element_text(hjust = 0.5 , colour = "black", size=8, family="sans"))+
  labs(y=expression(paste(x=expression(paste('PAR')))))+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())+
  geom_text(x=260, y=4, label=paste0("φPAR0: ",UV_diff_phi, ", Pmax: ", UV_diff_Pgmax, ", R: ", UV_diff_R),
            show.legend = FALSE, 
            size = 2,
            hjust = 0)+
  geom_text(x=260, y=0, label=paste0("φPAR0: ",UV_clear_phi, ", Pmax: ", UV_clear_Pgmax, ", R: ", UV_clear_R),
            show.legend = FALSE, 
            size = 2,
            hjust = 0,
            color="gold3")+
  guides(color="none", linetype="none",
         fill = guide_legend(override.aes = list(alpha = 1)))

# Full spectrum spectral treatment group ----------------------------------
model <- nlsLM(data = GasExchange3[GasExchange3$Grouping=="Full spectrum_Diffuse",], 
               formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
               start = c(phiI0 = 0.02, Pgmax = 60, R = 1))
summary(model)
Control_diff_phi <- round(summary(model)$coefficients[1,1], 2)
Control_diff_Pgmax <- round(summary(model)$coefficients[2,1],1)
Control_diff_R <- round(summary(model)$coefficients[3,1],2)


model <- nlsLM(data = GasExchange3[GasExchange3$Grouping=="Full spectrum_Clear",], 
               formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
               start = c(phiI0 = 0.02, Pgmax = 60, R = 1))
summary(model)
Control_clear_phi <- round(summary(model)$coefficients[1,1], 2)
Control_clear_Pgmax <- round(summary(model)$coefficients[2,1],1)
Control_clear_R <- round(summary(model)$coefficients[3,1],2)

Control <- ggplot((subset(predictions,  Grouping %in% c("Full spectrum_Diffuse", "Full spectrum_Clear"))), aes(x = PARi, y = fit, color = Grouping,fill=Grouping, linetype = Grouping)) +
  geom_line(size=0.8) +
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, size=0.2) +
  theme_PlainGray()+
  ylim(-10, 50)+
  labs(title = "Full spectrum")+
  theme(plot.title = element_text(hjust = 0.5, colour = "black", size=8, family="sans"))+
  labs(x=expression(paste('PAR')))+
  guides(linetype='none')+
  geom_text(x=260, y=4, label=paste0("φPAR0: ",Control_diff_phi, ", Pmax: ", Control_diff_Pgmax, ", R: ", Control_diff_R),
            show.legend = FALSE, 
            size = 2,
            hjust = 0)+
  geom_text(x=260, y=0, label=paste0("φPAR0: ",Control_clear_phi, ", Pmax: ", Control_clear_Pgmax, ", R: ", Control_clear_R),
            show.legend = FALSE, 
            size = 2,
            hjust = 0,
            color="gold3")+
  theme(plot.title = element_text(hjust = 0.5, colour = "black", size=8, family="sans"))+
  labs(y=expression(atop('Photosynthetic rate,', paste(mu,'mol ', CO[2],' ', m^{-2},' ', s^{-1}))), x=expression(paste('PAR')))+
  theme(legend.position="none")


model <- nlsLM(data = GasExchange3[GasExchange3$Grouping=="UV-A long_Diffuse",], 
               formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
               start = c(phiI0 = 0.02, Pgmax = 60, R = 1))
summary(model)
UVA_diff_phi <- round(summary(model)$coefficients[1,1], 2)
UVA_diff_Pgmax <- round(summary(model)$coefficients[2,1],1)
UVA_diff_R <- round(summary(model)$coefficients[3,1],2)


model <- nlsLM(data = GasExchange3[GasExchange3$Grouping=="UV-A long_Clear",], 
               formula = Photo ~ (((phiI0 * PARi * Pgmax) / (phiI0 * PARi + Pgmax)) - R), 
               start = c(phiI0 = 0.02, Pgmax = 60, R = 1))
summary(model)
UVA_clear_phi <- round(summary(model)$coefficients[1,1], 2)
UVA_clear_Pgmax <- round(summary(model)$coefficients[2,1],1)
UVA_clear_R <- round(summary(model)$coefficients[3,1],2)


# UV-A long spectral treatment group --------------------------------------
UVA <- ggplot((subset(predictions,  Grouping %in% c("UV-A long_Diffuse", "UV-A long_Clear"))), aes(x = PARi, y = fit, color = Grouping,fill=Grouping, linetype = Grouping)) +
  geom_line(size=0.8) +
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, size=0.2) +
  theme_PlainGray()+
  ylim(-10, 50)+
  labs(title = "UV-A long")+
  theme(plot.title = element_text(hjust = 0.5, colour = "black", size=8, family="sans"))+
  geom_text(x=260, y=4, label=paste0("φPAR0: ",UVA_diff_phi, ", Pmax: ", UVA_diff_Pgmax, ", R: ", UVA_diff_R),
            show.legend = FALSE, 
            size = 2,
            hjust = 0)+
  geom_text(x=260, y=0, label=paste0("φPAR0: ",UVA_clear_phi, ", Pmax: ", UVA_clear_Pgmax, ", R: ", UVA_clear_R),
            show.legend = FALSE, 
            size = 2,
            hjust = 0,
            color="gold3")+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())+
  theme(legend.position="none")+
  labs(x=expression(paste('PAR')))


# Details for text --------------------------------------------------------

data_details <- GasExchange3[GasExchange3$Grouping=="No UV_Clear",]
data_details <- subset(data_details, data_details$PARi>1499)
mean <- round(mean(data_details$Photo),1)
se <- round(std.error(data_details$Photo),2)


data_details <- GasExchange3[GasExchange3$Grouping=="No UV_Diffuse",]
data_details <- subset(data_details, data_details$PARi>1499)
mean <- round(mean(data_details$Photo),1)
se <- round(std.error(data_details$Photo),1)
