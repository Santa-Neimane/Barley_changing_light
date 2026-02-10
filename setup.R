# Libraries ---------------------------------------------------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(plotrix)
library(multcomp) 
library(ggpattern)
library(lme4)
library(glmmTMB)
library(ggpubr)
library(nlme)
library(psych)
library(ggResidpanel)
library(mvnormtest)
library(DHARMa)
library(ggh4x)
library(minpack.lm)
library(investr)
library(tidyverse)
library(writexl)
library(ggpmisc)
library(patchwork)
library(reshape2)
library(lsmeans) 
library(ggtext)
library(corrplot)
library(lubridate)
library(performance)
library(rstatix)
library(lmtest)
library(missMDA)
library(ggrepel)
library(FactoMineR)
library(factoextra)


# File with filter number identification ----------------------------------
filtermapping <- read_excel("FilterMapping.xlsx")

# Theme for graphs --------------------------------------------------------
theme_PlainGray <- function(){
  font <- "sans"   #assign font family up front
  theme_classic() %+replace%    #replace elements we want to change
    theme(
      text=element_text(family=font),
      axis.text.y = element_text(colour = "#696969", size=8),
      axis.text.x = element_text(colour = "black", size=8),
      axis.title = element_text(colour = "gray18", size=8),
      axis.ticks = element_line(colour = "#C0C0C0", size= 0.3),
      axis.line = element_line(colour = "#A9A9A9", size = 0.3),
      legend.text = element_text(hjust = 0,size=8, colour="#696969"),
      legend.title = element_text(size=8,colour="#696969"),
      plot.subtitle = element_text(size=8, face="italic", color="black", hjust=0),
    )
}

diffuse_exp <- expression(italic("Diffuse"))
clear_exp <-  expression(italic("Clear"))

# Outlier identification --------------------------------------------------
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

