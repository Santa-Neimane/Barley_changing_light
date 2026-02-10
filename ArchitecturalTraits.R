# Here plant architectural traits are analysed
# Two data files have to be imported since 
# one dataset contains additional factor CanopyLevel (height)

# Data file with no Canopy levels -----------------------------------------
dataarch <- read_excel("Architect.xlsx")

# Since there are missing values, correct format has to be speficied
dataarch$InternodeMid <- as.numeric(dataarch$InternodeMid)
dataarch$LeafTiptoStemBot <- as.numeric(dataarch$LeafTiptoStemBot)
dataarch$FilterNr <- as.factor(dataarch$FilterNr)

# Check correlations within the dataframe to determine if MANOVA needed
arch_numeric <- dataarch[, -c(1,2,3,4,5)]

pairs.panels(arch_numeric,
             smooth = TRUE,
             scale = FALSE,
             cex=3) ## Weak correlations

# Plant Height

ggplot(dataarch, aes(Spectral, Height, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())


m_height <- lme(Height ~ Spectral+Diff,random=~1|FilterNr, data = dataarch)

resid_panel(m_height, smoother = TRUE, qqbands = TRUE)

summary(m_height)
anova(m_height)
pairwise <- emmeans(m_height, pairwise ~ Spectral)
summary(pairwise)


dataarch$Spectral_f <- factor(dataarch$Spectral, levels=c("Full spectrum","UV-A long", "No UV"))

ggplot(dataarch, aes(Spectral_f, Height))+
  geom_bar_pattern(aes(pattern_density=Spectral_f),
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
  stat_summary(fun.data = mean_se, geom = "errorbar",width = 0, position = "dodge")+
  theme_PlainGray()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position="none")+
  geom_bracket(xmin = "UV-A long",
               xmax = "No UV",
               label ="P = 0.08",
               y.position = 73,
               family="sans",
               label.size = 2.8)+
  geom_bracket(xmin = c("UV-A long", "Full spectrum"),
               xmax = c("Full spectrum", "No UV"),
               label = c("P = 1.00", "P = 0.07"),
               y.position = c(60, 65),
               family="sans",
               label.size = 2.8)+
  expand_limits(y = 80)+
  labs(y = expression("Height, cm"))

# Width
ggplot(dataarch, aes(Spectral_f, MaxWidth, fill=Diff))+
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
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  annotate("text", x = 1, y = 42,size=2.5,family="sans", label = expression(bolditalic(P) * bold(" = 0.03")))+
  annotate("text", x = 2, y = 42,size=2.5,family="sans", label = expression(bolditalic(P) * bold(" = 0.03")))+
  annotate("text", x = 3, y = 42,size=2.5,family="sans", label = expression(bolditalic(P) * bold(" = 0.03")))+
  labs(y = expression("Width, cm"))


m_width <- lme(MaxWidth ~ Spectral+Diff,random=~1|FilterNr, data = dataarch)

resid_panel(m_width, smoother = TRUE, qqbands = TRUE)

summary(m_width)
anova(m_width)
pairwise <- emmeans(m_width, pairwise ~Spectral)
pairwise
summary(pairwise)
cld(pairwise, alpha=0.05, Letters=letters, adjust="tukey")
pairs(pairwise, simple = "each")


g_width <- ggplot(dataarch, aes(Spectral_f, MaxWidth, fill=Diff))+
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
  labs(y = ("Plant width, cm"))+
  annotate("text", x = 2, y = 47,size=2.5,family="sans", 
           label="'Main effect of the'~italic('filter type') ~ bolditalic(P)~bold('=')~bold('0.03')",
           parse=TRUE)+
  annotate("text", x = 2, y = 42,size=2.5,family="sans", 
           label="'Main effect of the'~italic('spectral treatment') ~ bolditalic(P)~bold('=')~bold('0.01')",
           parse=TRUE)+
  annotate("text", x = 3.4, y=50, label ="(a)", family = "sans", fontface = "bold", size=4)+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


# Number of leaves

ggplot(dataarch, aes(Spectral, dataarch$NumberOfLeaves, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())



model_poisson_leaves <- glmer(NumberOfLeaves ~ Spectral+Diff+(1|FilterNr), data = dataarch, family = poisson())

resid_panel(model_poisson_leaves, smoother = TRUE, qqbands = TRUE)

summary(model_poisson_leaves)
Anova(model_poisson_leaves, type = "III")



# Number of stems
ggplot(dataarch, aes(Spectral, NumberOfStems, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())



model_poisson_stems <- glmer(NumberOfStems ~ Spectral+Diff+(1|FilterNr), data = dataarch, family = poisson())

resid_panel(model_poisson_stems, smoother = TRUE, qqbands = TRUE)

summary(model_poisson)
Anova(model_poisson_stems, type = "III")
summary(glht(model_poisson_stems, mcp(Spectral="Tukey")))
pairwise <- emmeans(model_poisson_stems, pairwise ~ Spectral)
pairwise
summary(pairwise)


GraphNumbStems <- ggplot(dataarch, aes(Spectral_f, NumberOfStems, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral_f),
                   stat="summary",
                   fun="mean",
                   pattern_fill="white",
                   pattern_color=NA,
                   pattern_angle=45, 
                   pattern_key_scale_factor=0.8,
                   alpha=0.7,
                   width = 1,
                   position=position_dodge(0.6))+
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"))+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  stat_summary(aes(color=Diff),fun.data = mean_se, geom = "errorbar",width = 0, position=position_dodge(0.6))+
  theme_PlainGray()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),,
        axis.text.x=element_blank(),
        legend.position="none")+
  geom_bracket(xmin = "UV-A long",
               xmax = "No UV",
               label="bolditalic(P)~bold('<')~bold('0.01')",
               type="expression",
               y.position = 15.5,
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  geom_bracket(xmin = "UV-A long",
               xmax = "Full spectrum",
               label="italic(P)~'='~0.09",
               type="expression",
               y.position = 14.5,
               family="sans",
               label.size = 2.8,
               size=0.1,
               inherit.aes = FALSE)+
  geom_bracket(xmin = "No UV",
               xmax = "Full spectrum",
               label="italic(P)~'='~0.66",
               type="expression",
               y.position = 13,
               family="sans",
               label.size = 2.8,
               size=0.1,
               inherit.aes = FALSE)+
  expand_limits(y = 16)+
  labs(y = expression("Number of stems per plant"))+
  annotate("text", x = 3.4, y=14, label ="(a)", family = "sans", fontface = "bold", size=4)+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")

# Data file with Canopy levels --------------------------------------------

dataarchlevel <- read_xlsx("ArchitectCanopyLevels.xlsx")
dataarchlevel$LeafTiptoStem <- as.numeric(dataarchlevel$LeafTiptoStem)
dataarchlevel$Internode <- as.numeric(dataarchlevel$Internode)
dataarchlevel$FilterNr <- as.factor(dataarchlevel$FilterNr)

# Leaf curvature radius 

ggplot(dataarchlevel, aes(Spectral, LeafTiptoStem, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  facet_grid(~CanopyLevel)


dataarchlevel_radius <- dataarchlevel[!is.na(dataarchlevel$LeafTiptoStem), ]
m_radius <- lme(LeafTiptoStem ~ Spectral+CanopyLevel+Diff,random=~1|FilterNr, data = dataarchlevel_radius)

resid_panel(m_radius, smoother = TRUE, qqbands = TRUE)

summary(m_radius)
anova(m_radius)
pairwise <- emmeans(m_radius, pairwise ~ Spectral|CanopyLevel)
summary(pairwise)
pairwise <- emmeans(m_radius, pairwise ~ CanopyLevel)
summary(pairwise)


dataarchlevel$Spectral_f <- factor(dataarchlevel$Spectral, levels=c("Full spectrum","UV-A long", "No UV"))

smallleafradius <- ggplot(subset(dataarchlevel, CanopyLevel=="Middle"), aes(Spectral_f, LeafTiptoStem, fill=Diff))+
  geom_bar_pattern(aes(pattern_density=Spectral_f),
                   stat="summary",
                   fun="mean",
                   pattern_fill="white",
                   pattern_color=NA,
                   width=1, 
                   pattern_angle=45, 
                   pattern_key_scale_factor=0.8,
                   alpha=0.7,
                   position=position_dodge(0.6))+
  scale_pattern_manual(values = c("UV-A long" = "stripe", "Full spectrum" = "none", "No UV"="stripe"))+
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  stat_summary(aes(color=Diff),fun.data = mean_se, geom = "errorbar", position=position_dodge(0.6),width = 0)+
  theme_PlainGray()+
  theme(legend.position = 'none',
        strip.text = element_text(face = "italic", color = "black", hjust = 0),
        strip.background = element_rect(fill = "lightgrey", linewidth = NA),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x = element_blank())+
  labs(y = expression("Leaf curvature radius, cm"))+
  geom_bracket(xmin = "Full spectrum",
               xmax = "UV-A long",
               label="bolditalic(P)~bold('=')~bold('0.02')",
               type="expression",
               y.position = 19,
               family="sans",
               label.size = 2.8,
               size=0.43,
               inherit.aes = FALSE)+
  geom_bracket(xmin = c("No UV", "UV-A long"),
               xmax = c("Full spectrum", "No UV"),
               label = c(label="italic(P)~'='~0.51",label="italic(P)~'='~0.20"),
               type="expression",
               y.position = c(17, 15),
               family="sans",
               label.size = 2.8,
               size=0.1,
               inherit.aes = FALSE)+
  expand_limits(y = 18)+
  annotate("text", x = 3.4, y=20, label ="(d)", family = "sans", fontface = "bold", size=4)+
  scale_fill_manual(name="Filter type", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"), guide="none")

ggsave('MiddleSmallLeafradius.png', width = 7, height =5, units='cm', dpi=750)

# Internode
ggplot(dataarchlevel, aes(Spectral, Internode, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  facet_grid(~CanopyLevel)

dataarchlevel_internode <- dataarchlevel[!is.na(dataarchlevel$Internode), ]

resid_panel(m_internode, smoother = TRUE, qqbands = TRUE)

m_internode <- lme(Internode ~ Spectral+Diff+CanopyLevel,random=~1|FilterNr, data = dataarchlevel_internode)
summary(m_internode)
anova(m_internode)

# Leaf angle
ggplot(dataarchlevel, aes(Spectral, LeafAngle, fill=Diff))+
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
  scale_pattern_density_manual(values = c("UV-A long" = 0.05, "Full spectrum" = 0, "No UV"=0.25))+
  scale_fill_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("gold3","#6852ec"))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=Diff), position=position_dodge(0.6),width = 0)+
  scale_color_manual(name="Filter", labels = c(clear_exp,diffuse_exp), values=c("#c2a402","#5846c7"))+
  theme_PlainGray()+
  theme(legend.position = 'none',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  facet_grid(~CanopyLevel)

dataarchlevel_outliersangle <- dataarchlevel %>%
  group_by(Spectral, Diff, CanopyLevel) %>% 
  mutate(newAngle = remove_outliers(LeafAngle))

dataarchlevel_angle <- dataarchlevel_outliersangle[!is.na(dataarchlevel_outliersangle$newAngle), ]

dataarchlevel_angle$newAngleSQRT <- sqrt(dataarchlevel_angle$newAngle)
m_angle <- lme(newAngleSQRT ~ Spectral+Diff+CanopyLevel,random=~1|FilterNr, data = dataarchlevel_angle)

resid_panel(m_angle, smoother = TRUE, qqbands = TRUE)

summary(m_angle)
anova(m_angle)