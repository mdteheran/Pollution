ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("actuar","AICcmodavg","agricolae","apaTables","aod","arm","astsa",
              "boot","DescTools","broom","car","carData","caret","cmprsk","corrr","corrplot",
              "cowplot","correlationfunnel","explore","DataExplorer","datasets","DataEditR","data.table","dplyr",
              "dynlm","ellipse","easystats","esquisse","effects","effectsize","faraway","fable","flexmix","flexsurv","forcats",
              "forecast","foreign","gapminder","GGally","ggcorrplot","ggalt","ggpubr","ggstatsplot",
              "gmodels","ggridges","ggfortify","ggplot2","gplots","gridExtra","haven","HH","Hmisc",
              "hrbrthemes","ISLR","janitor","kableExtra","KMsurv","lubridate","lsr","lmtest","lmSupport","MASS","missForest","modelr",
              "modelsummary","pls","mFilter","MLmetrics","mstate","multcompView","multcomp",
              "nlme","oddsratio","parameters","PairedData","palmerpenguins","PASWR",
              "pwr","pls","PerformanceAnalytics","PASWR","picante","psych",
              "psychometric","QuantPsyc","rattle","ranger","RColorBrewer","readr","rmarkdown","rcompanion",
              "riskRegression","rstatix","rpivotTable","RVAideMemoire","splines","skimr","SMPracticals",
              "stats","sjstats","ssym","summarytools","survMisc","survminer","survival","texreg","tidyverse",
              "timsac","tsdl","tsbox","tsibble","tseries","TSstudio","vars","viridis","wesanderson","xts","xtable","yarrr")
ipak(packages)
library(remotes)
remotes::install_github("arcruz0/paqueteadp")
library(paqueteadp)
library(devtools)
devtools::install_github("FinYang/tsdl")
library(readxl)
pollution <- read_excel("C:/Users/Admin/Dropbox/bioestadística/cuarto semestre/tesis/database.xlsx")
dim(pollution)
names(pollution)
str(pollution)
attach(pollution)
skimr::skim(pollution)
sum(is.na(pollution))
colSums(is.na(pollution))
source("C:/Users/Admin/Dropbox/bioestadística/tercer semestre/estadísticas especiales/macros.txt")


##Objetivo 1 - Tabla 1##
glimpse(pollution)
stby(pollution$Concentration, pollution$`Air pollutant`, FUN=descr)
esquisse::esquisser(pollution)
explore(pollution)

#Normalidad#
ggplot(pollution) +
 aes(x = Concentration, fill = `2020-2019`) +
 geom_density(adjust = 1.2) +
 scale_fill_hue(direction = 1) +
 labs(x = "Concentration (µg/m3)") +
 theme_bw() +
 facet_wrap(vars(`Air pollutant`), scales = "free")

PM10_2019_filt<-filter(pollution, `Air pollutant` == "PM10" & `2020-2019` == "G2_ctr")
shapiro.test(PM10_2019_filt$Concentration)
PM10_2020_filt<-filter(pollution, `Air pollutant` == "PM10" & `2020-2019` == "G1_tto")
shapiro.test(PM10_2020_filt$Concentration)

PM2.5_2019_filt<-filter(pollution, `Air pollutant` == "PM2.5" & `2020-2019` == "G2_ctr")
shapiro.test(PM2.5_2019_filt$Concentration)
PM2.5_2020_filt<-filter(pollution, `Air pollutant` == "PM2.5" & `2020-2019` == "G1_tto")
shapiro.test(PM2.5_2020_filt$Concentration)

O3_2019_filt<-filter(pollution, `Air pollutant` == "O3" & `2020-2019` == "G2_ctr")
shapiro.test(O3_2019_filt$Concentration)
O3_2020_filt<-filter(pollution, `Air pollutant` == "O3" & `2020-2019` == "G1_tto")
shapiro.test(O3_2020_filt$Concentration)

SO2_2019_filt<-filter(pollution, `Air pollutant` == "SO2" & `2020-2019` == "G2_ctr")
shapiro.test(SO2_2019_filt$Concentration)
SO2_2020_filt<-filter(pollution, `Air pollutant` == "SO2" & `2020-2019` == "G1_tto")
shapiro.test(SO2_2020_filt$Concentration)


data_filt_NQ<- pollution %>%
  filter(Quarantine == "NQ")
stby(data_filt_NQ$Concentration, data_filt_NQ$`Air pollutant`, FUN=descr)
data_filt_NQ_0<- data_filt_NQ %>%
  filter(`Monitoring (times)` == 0)
stby(data_filt_NQ_0$Concentration, data_filt_NQ_0$`Air pollutant`, FUN=descr)
data_filt_NQ_24<- data_filt_NQ %>%
  filter(`Monitoring (times)` == 24)
stby(data_filt_NQ_24$Concentration, data_filt_NQ_24$`Air pollutant`, FUN=descr)
data_filt_NQ_48<- data_filt_NQ %>%
  filter(`Monitoring (times)` == 48)
stby(data_filt_NQ_48$Concentration, data_filt_NQ_48$`Air pollutant`, FUN=descr)
data_filt_NQ_72<- data_filt_NQ %>%
  filter(`Monitoring (times)` == 72)
stby(data_filt_NQ_72$Concentration, data_filt_NQ_72$`Air pollutant`, FUN=descr)
data_filt_NQ_96<- data_filt_NQ %>%
  filter(`Monitoring (times)` == 96)
stby(data_filt_NQ_96$Concentration, data_filt_NQ_96$`Air pollutant`, FUN=descr)


data_filt_NQ_control<- pollution %>%
  filter(Quarantine == "NQ_control")
stby(data_filt_NQ_control$Concentration, data_filt_NQ_control$`Air pollutant`, FUN=descr)
data_filt_NQ_control_0<- data_filt_NQ_control %>%
  filter(`Monitoring (times)` == 0)
stby(data_filt_NQ_control_0$Concentration, data_filt_NQ_control_0$`Air pollutant`, FUN=descr)
data_filt_NQ_control_24<- data_filt_NQ_control %>%
  filter(`Monitoring (times)` == 24)
stby(data_filt_NQ_control_24$Concentration, data_filt_NQ_control_24$`Air pollutant`, FUN=descr)
data_filt_NQ_control_48<- data_filt_NQ_control %>%
  filter(`Monitoring (times)` == 48)
stby(data_filt_NQ_control_48$Concentration, data_filt_NQ_control_48$`Air pollutant`, FUN=descr)
data_filt_NQ_control_72<- data_filt_NQ_control %>%
  filter(`Monitoring (times)` == 72)
stby(data_filt_NQ_control_72$Concentration, data_filt_NQ_control_72$`Air pollutant`, FUN=descr)
data_filt_NQ_control_96<- data_filt_NQ_control %>%
  filter(`Monitoring (times)` == 96)
stby(data_filt_NQ_control_96$Concentration, data_filt_NQ_control_96$`Air pollutant`, FUN=descr)


data_filt_SIQ<- pollution %>%
  filter(Quarantine == "SIQ")
stby(data_filt_SIQ$Concentration, data_filt_SIQ$`Air pollutant`, FUN=descr)
data_filt_SIQ_0<- data_filt_SIQ %>%
  filter(`Monitoring (times)` == 0)
stby(data_filt_SIQ_0$Concentration, data_filt_SIQ_0$`Air pollutant`, FUN=descr)
data_filt_SIQ_24<- data_filt_SIQ %>%
  filter(`Monitoring (times)` == 24)
stby(data_filt_SIQ_24$Concentration, data_filt_SIQ_24$`Air pollutant`, FUN=descr)
data_filt_SIQ_48<- data_filt_SIQ %>%
  filter(`Monitoring (times)` == 48)
stby(data_filt_SIQ_48$Concentration, data_filt_SIQ_48$`Air pollutant`, FUN=descr)
data_filt_SIQ_72<- data_filt_SIQ %>%
  filter(`Monitoring (times)` == 72)
stby(data_filt_SIQ_72$Concentration, data_filt_SIQ_72$`Air pollutant`, FUN=descr)
data_filt_SIQ_96<- data_filt_SIQ %>%
  filter(`Monitoring (times)` == 96)
stby(data_filt_SIQ_96$Concentration, data_filt_SIQ_96$`Air pollutant`, FUN=descr)


data_filt_SIQ_control<- pollution %>%
  filter(Quarantine == "SIQ_control")
stby(data_filt_SIQ_control$Concentration, data_filt_SIQ_control$`Air pollutant`, FUN=descr)
data_filt_SIQ_control_0<- data_filt_SIQ_control %>%
  filter(`Monitoring (times)` == 0)
stby(data_filt_SIQ_control_0$Concentration, data_filt_SIQ_control_0$`Air pollutant`, FUN=descr)
data_filt_SIQ_control_24<- data_filt_SIQ_control %>%
  filter(`Monitoring (times)` == 24)
stby(data_filt_SIQ_control_24$Concentration, data_filt_SIQ_control_24$`Air pollutant`, FUN=descr)
data_filt_SIQ_control_48<- data_filt_SIQ_control %>%
  filter(`Monitoring (times)` == 48)
stby(data_filt_SIQ_control_48$Concentration, data_filt_SIQ_control_48$`Air pollutant`, FUN=descr)
data_filt_SIQ_control_72<- data_filt_SIQ_control %>%
  filter(`Monitoring (times)` == 72)
stby(data_filt_SIQ_control_72$Concentration, data_filt_SIQ_control_72$`Air pollutant`, FUN=descr)
data_filt_SIQ_control_96<- data_filt_SIQ_control %>%
  filter(`Monitoring (times)` == 96)
stby(data_filt_SIQ_control_96$Concentration, data_filt_SIQ_control_96$`Air pollutant`, FUN=descr)


data_filt_SubaQ<- pollution %>%
  filter(Quarantine == "SubaQ")
stby(data_filt_SubaQ$Concentration, data_filt_SubaQ$`Air pollutant`, FUN=descr)
data_filt_SubaQ_0<- data_filt_SubaQ %>%
  filter(`Monitoring (times)` == 0)
stby(data_filt_SubaQ_0$Concentration, data_filt_SubaQ_0$`Air pollutant`, FUN=descr)
data_filt_SubaQ_24<- data_filt_SubaQ %>%
  filter(`Monitoring (times)` == 24)
stby(data_filt_SubaQ_24$Concentration, data_filt_SubaQ_24$`Air pollutant`, FUN=descr)
data_filt_SubaQ_48<- data_filt_SubaQ %>%
  filter(`Monitoring (times)` == 48)
stby(data_filt_SubaQ_48$Concentration, data_filt_SubaQ_48$`Air pollutant`, FUN=descr)
data_filt_SubaQ_72<- data_filt_SubaQ %>%
  filter(`Monitoring (times)` == 72)
stby(data_filt_SubaQ_72$Concentration, data_filt_SubaQ_72$`Air pollutant`, FUN=descr)
data_filt_SubaQ_96<- data_filt_SubaQ %>%
  filter(`Monitoring (times)` == 96)
stby(data_filt_SubaQ_96$Concentration, data_filt_SubaQ_96$`Air pollutant`, FUN=descr)


data_filt_SubaQ_control<- pollution %>%
  filter(Quarantine == "SubaQ_control")
stby(data_filt_SubaQ_control$Concentration, data_filt_SubaQ_control$`Air pollutant`, FUN=descr)
data_filt_SubaQ_control_0<- data_filt_SubaQ_control %>%
  filter(`Monitoring (times)` == 0)
stby(data_filt_SubaQ_control_0$Concentration, data_filt_SubaQ_control_0$`Air pollutant`, FUN=descr)
data_filt_SubaQ_control_24<- data_filt_SubaQ_control %>%
  filter(`Monitoring (times)` == 24)
stby(data_filt_SubaQ_control_24$Concentration, data_filt_SubaQ_control_24$`Air pollutant`, FUN=descr)
data_filt_SubaQ_control_48<- data_filt_SubaQ_control %>%
  filter(`Monitoring (times)` == 48)
stby(data_filt_SubaQ_control_48$Concentration, data_filt_SubaQ_control_48$`Air pollutant`, FUN=descr)
data_filt_SubaQ_control_72<- data_filt_SubaQ_control %>%
  filter(`Monitoring (times)` == 72)
stby(data_filt_SubaQ_control_72$Concentration, data_filt_SubaQ_control_72$`Air pollutant`, FUN=descr)
data_filt_SubaQ_control_96<- data_filt_SubaQ_control %>%
  filter(`Monitoring (times)` == 96)
stby(data_filt_SubaQ_control_96$Concentration, data_filt_SubaQ_control_96$`Air pollutant`, FUN=descr)

#Objetivo 1 correlaciones#
#PM10#
PM10_National_filt<-filter(pollution, `Air pollutant` == "PM10" & Spec_Quarant == "National")
shapiro.test(PM10_National_filt$Concentration)
ggcorr(PM10_National_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen",
       label_round = 2, method = c("pairwise", "spearman"),label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, PM10 National") +
  theme(plot.title = element_text(hjust = 0.6))
PM10_Smart_filt<-filter(pollution, `Air pollutant` == "PM10" & Spec_Quarant == "Smart")
shapiro.test(PM10_Smart_filt$Concentration)
ggcorr(PM10_Smart_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"),label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, PM10 Smart") +
  theme(plot.title = element_text(hjust = 0.6))
PM10_Suba_filt<-filter(pollution, `Air pollutant` == "PM10" & Spec_Quarant == "Suba")
shapiro.test(PM10_Suba_filt$Concentration)
ggcorr(PM10_Suba_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"),label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, PM10 Suba") +
  theme(plot.title = element_text(hjust = 0.6))

#PM2.5#
PM2.5_National_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Spec_Quarant == "National")
shapiro.test(PM2.5_National_filt$Concentration)
ggcorr(PM2.5_National_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, PM2.5 National") +
  theme(plot.title = element_text(hjust = 0.6))
PM2.5_Smart_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Spec_Quarant == "Smart")
shapiro.test(PM2.5_Smart_filt$Concentration)
ggcorr(PM2.5_Smart_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, PM2.5 Smart") +
  theme(plot.title = element_text(hjust = 0.6))
PM2.5_Suba_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Spec_Quarant == "Suba")
shapiro.test(PM2.5_Suba_filt$Concentration)
ggcorr(PM2.5_Suba_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, PM2.5 Suba") +
  theme(plot.title = element_text(hjust = 0.6))

#O3#
O3_National_filt<-filter(pollution, `Air pollutant` == "O3" & Spec_Quarant == "National")
shapiro.test(O3_National_filt$Concentration)
ggcorr(O3_National_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, O3 National") +
  theme(plot.title = element_text(hjust = 0.6))
O3_Smart_filt<-filter(pollution, `Air pollutant` == "O3" & Spec_Quarant == "Smart")
shapiro.test(O3_Smart_filt$Concentration)
ggcorr(O3_Smart_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, O3 Smart") +
  theme(plot.title = element_text(hjust = 0.6))
O3_Suba_filt<-filter(pollution, `Air pollutant` == "O3" & Spec_Quarant == "Suba")
shapiro.test(O3_Suba_filt$Concentration)
ggcorr(O3_Suba_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, O3 Suba") +
  theme(plot.title = element_text(hjust = 0.6))

#SO2#
SO2_National_filt<-filter(pollution, `Air pollutant` == "SO2" & Spec_Quarant == "National")
shapiro.test(SO2_National_filt$Concentration)
ggcorr(SO2_National_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, SO2 National") +
  theme(plot.title = element_text(hjust = 0.6))
SO2_Smart_filt<-filter(pollution, `Air pollutant` == "SO2" & Spec_Quarant == "Smart")
shapiro.test(SO2_Smart_filt$Concentration)
ggcorr(SO2_Smart_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, SO2 Smart") +
  theme(plot.title = element_text(hjust = 0.6))
SO2_Suba_filt<-filter(pollution, `Air pollutant` == "SO2" & Spec_Quarant == "Suba")
shapiro.test(SO2_Suba_filt$Concentration)
ggcorr(SO2_Suba_filt[, 1:4], nbreaks = 10, label = T, low = "blue", high = "lightgreen", 
       label_round = 2, method = c("pairwise", "spearman"), label_size = 7, size=6, name = "Spearman correlation", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlogram Pollution Vs Environmental, SO2 Suba") +
  theme(plot.title = element_text(hjust = 0.6))

##Objetivo 1-2 - Figura 1 - Hipótesis - Tamaño del efecto##
#PM10#
data_filt_PM10<-filter(pollution, `Air pollutant` == "PM10")
#Subset PM10 NQ-NQ_control <0#
PM10_National_filt<-filter(pollution, `Air pollutant` == "PM10" & Spec_Quarant == "National")
PM10_National_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
PM10_NQ <- subset(data_filt_PM10,  Quarantine == "NQ", Concentration, drop = TRUE)
descr(PM10_NQ)
PM10_NQ_control <- subset(data_filt_PM10,  Quarantine == "NQ_control", Concentration, drop = TRUE)
descr(PM10_NQ_control)
t.test(PM10_NQ, PM10_NQ_control, alternative = "less", var.equal = F, data= data_filt_PM10)
#Subset PM10 SIQ-SIQ_control <0#
PM10_Smart_filt<-filter(pollution, `Air pollutant` == "PM10" & Spec_Quarant == "Smart")
PM10_Smart_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
PM10_SIQ <- subset(data_filt_PM10,  Quarantine == "SIQ", Concentration, drop = TRUE)
descr(PM10_SIQ)
PM10_SIQ_control <- subset(data_filt_PM10,  Quarantine == "SIQ_control", Concentration, drop = TRUE)
descr(PM10_SIQ_control)
t.test(PM10_SIQ, PM10_SIQ_control, alternative = "less", var.equal = F, data= data_filt_PM10)
#Subset PM10 SubaQ-SubaQ_control <0#
PM10_Suba_filt<-filter(pollution, `Air pollutant` == "PM10" & Spec_Quarant == "Suba")
PM10_Suba_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
PM10_SubaQ <- subset(data_filt_PM10,  Quarantine == "SubaQ", Concentration, drop = TRUE)
descr(PM10_SubaQ)
PM10_SubaQ_control <- subset(data_filt_PM10,  Quarantine == "SubaQ_control", Concentration, drop = TRUE)
descr(PM10_SubaQ_control)
t.test(PM10_SubaQ, PM10_SubaQ_control, alternative = "less", var.equal = F, data= data_filt_PM10)

#PM2.5#
data_filt_PM2.5<-filter(pollution, `Air pollutant` == "PM2.5")
#Subset PM2.5 NQ-NQ_control <0#
PM2.5_National_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Spec_Quarant == "National")
PM2.5_National_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
PM2.5_NQ <- subset(data_filt_PM2.5,  Quarantine == "NQ", Concentration, drop = TRUE)
descr(PM2.5_NQ)
PM2.5_NQ_control <- subset(data_filt_PM2.5,  Quarantine == "NQ_control", Concentration, drop = TRUE)
descr(PM2.5_NQ_control)
t.test(PM2.5_NQ, PM2.5_NQ_control, alternative = "less", var.equal = F, data= data_filt_PM2.5)
#Subset PM2.5 SIQ-SIQ_control <0#
PM2.5_Smart_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Spec_Quarant == "Smart")
PM2.5_Smart_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
PM2.5_SIQ <- subset(data_filt_PM2.5,  Quarantine == "SIQ", Concentration, drop = TRUE)
descr(PM2.5_SIQ)
PM2.5_SIQ_control <- subset(data_filt_PM2.5,  Quarantine == "SIQ_control", Concentration, drop = TRUE)
descr(PM2.5_SIQ_control)
t.test(PM2.5_SIQ, PM2.5_SIQ_control, alternative = "less", var.equal = F, data= data_filt_PM2.5)
#Subset PM2.5 SubaQ-SubaQ_control <0#
PM2.5_Suba_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Spec_Quarant == "Suba")
PM2.5_Suba_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
PM2.5_SubaQ <- subset(data_filt_PM2.5,  Quarantine == "SubaQ", Concentration, drop = TRUE)
descr(PM2.5_SubaQ)
PM2.5_SubaQ_control <- subset(data_filt_PM2.5,  Quarantine == "SubaQ_control", Concentration, drop = TRUE)
descr(PM2.5_SubaQ_control)
t.test(PM2.5_SubaQ, PM2.5_SubaQ_control, alternative = "less", var.equal = F, data= data_filt_PM2.5)

#O3#
data_filt_O3<-filter(pollution, `Air pollutant` == "O3")
#Subset O3 NQ-NQ_control <0#
O3_National_filt<-filter(pollution, `Air pollutant` == "O3" & Spec_Quarant == "National")
O3_National_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
O3_NQ <- subset(data_filt_O3,  Quarantine == "NQ", Concentration, drop = TRUE)
descr(O3_NQ)
O3_NQ_control <- subset(data_filt_O3,  Quarantine == "NQ_control", Concentration, drop = TRUE)
descr(O3_NQ_control)
t.test(O3_NQ, O3_NQ_control, alternative = "less", var.equal = F, data= data_filt_O3)
#Subset O3 SIQ-SIQ_control <0#
O3_Smart_filt<-filter(pollution, `Air pollutant` == "O3" & Spec_Quarant == "Smart")
O3_Smart_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
O3_SIQ <- subset(data_filt_O3,  Quarantine == "SIQ", Concentration, drop = TRUE)
descr(O3_SIQ)
O3_SIQ_control <- subset(data_filt_O3,  Quarantine == "SIQ_control", Concentration, drop = TRUE)
descr(O3_SIQ_control)
t.test(O3_SIQ, O3_SIQ_control, alternative = "less", var.equal = F, data= data_filt_O3)
#Subset O3 SubaQ-SubaQ_control <0#
O3_Suba_filt<-filter(pollution, `Air pollutant` == "O3" & Spec_Quarant == "Suba")
O3_Suba_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
O3_SubaQ <- subset(data_filt_O3,  Quarantine == "SubaQ", Concentration, drop = TRUE)
descr(O3_SubaQ)
O3_SubaQ_control <- subset(data_filt_O3,  Quarantine == "SubaQ_control", Concentration, drop = TRUE)
descr(O3_SubaQ_control)
t.test(O3_SubaQ, O3_SubaQ_control, alternative = "less", var.equal = F, data= data_filt_O3)
#SO2#
data_filt_SO2<-filter(pollution, `Air pollutant` == "SO2")
#Subset SO2 NQ-NQ_control <0#
SO2_National_filt<-filter(pollution, `Air pollutant` == "SO2" & Spec_Quarant == "National")
SO2_National_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
SO2_NQ <- subset(data_filt_SO2,  Quarantine == "NQ", Concentration, drop = TRUE)
descr(SO2_NQ)
SO2_NQ_control <- subset(data_filt_SO2,  Quarantine == "NQ_control", Concentration, drop = TRUE)
descr(SO2_NQ_control)
t.test(SO2_NQ, SO2_NQ_control, alternative = "less", var.equal = F, data= data_filt_SO2)
#Subset SO2 SIQ-SIQ_control <0#
SO2_Smart_filt<-filter(pollution, `Air pollutant` == "SO2" & Spec_Quarant == "Smart")
SO2_Smart_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
SO2_SIQ <- subset(data_filt_SO2,  Quarantine == "SIQ", Concentration, drop = TRUE)
descr(SO2_SIQ)
SO2_SIQ_control <- subset(data_filt_SO2,  Quarantine == "SIQ_control", Concentration, drop = TRUE)
descr(SO2_SIQ_control)
t.test(SO2_SIQ, SO2_SIQ_control, alternative = "less", var.equal = F, data= data_filt_SO2)
#Subset SO2 SubaQ-SubaQ_control <0#
SO2_Suba_filt<-filter(pollution, `Air pollutant` == "SO2" & Spec_Quarant == "Suba")
SO2_Suba_filt  %>% cohens_d(Concentration ~ Quarantine, paired = F, var.equal = F)
SO2_SubaQ <- subset(data_filt_SO2,  Quarantine == "SubaQ", Concentration, drop = TRUE)
descr(SO2_SubaQ)
SO2_SubaQ_control <- subset(data_filt_SO2,  Quarantine == "SubaQ_control", Concentration, drop = TRUE)
descr(SO2_SubaQ_control)
t.test(SO2_SubaQ, SO2_SubaQ_control, alternative = "less", var.equal = F, data= data_filt_SO2)

##Objetivo 1-2 - Año/Momento del día - Hipótesis##
#PM10#
data_filt_PM10<- pollution %>%
  filter(`Air pollutant` == "PM10")
stby(data_filt_PM10$Concentration, data_filt_PM10$`2020-2019`, FUN=descr)
stby(data_filt_PM10$Concentration, data_filt_PM10$Day_moment, FUN=descr)

PM10_NQ <- subset(data_filt_PM10,  Quarantine == "NQ")
stby(PM10_NQ$Concentration, PM10_NQ$Day_moment, FUN=descr)
PM10_NQ_control <- subset(data_filt_PM10,  Quarantine == "NQ_control")
stby(PM10_NQ_control$Concentration, PM10_NQ_control$Day_moment, FUN=descr)

PM10_SIQ <- subset(data_filt_PM10,  Quarantine == "SIQ")
stby(PM10_SIQ$Concentration, PM10_SIQ$Day_moment, FUN=descr)
PM10_SIQ_control <- subset(data_filt_PM10,  Quarantine == "SIQ_control")
stby(PM10_SIQ_control$Concentration, PM10_SIQ_control$Day_moment, FUN=descr)

PM10_SubaQ <- subset(data_filt_PM10,  Quarantine == "SubaQ")
stby(PM10_SubaQ$Concentration, PM10_SubaQ$Day_moment, FUN=descr)
PM10_SubaQ_control <- subset(data_filt_PM10,  Quarantine == "SubaQ_control")
stby(PM10_SubaQ_control$Concentration, PM10_SubaQ_control$Day_moment, FUN=descr)

ggbetweenstats(data  = data_filt_PM10, x= `2020-2019`, y= Concentration, title= "[PM10, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= data_filt_PM10)
ggbetweenstats(data  = data_filt_PM10, x= Day_moment, y= Concentration, title= "[PM10, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")

PM10_National_day<- subset(data_filt_PM10,  Day_moment == "day" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM10_National_day)
PM10_National_night<- subset(data_filt_PM10,  Day_moment == "night" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM10_National_night)

PM10_Smart_day<- subset(data_filt_PM10,  Day_moment == "day" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM10_Smart_day)
PM10_Smart_night<- subset(data_filt_PM10,  Day_moment == "night" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM10_Smart_night)

PM10_Suba_day<- subset(data_filt_PM10,  Day_moment == "day" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM10_Suba_day)
PM10_Suba_night<- subset(data_filt_PM10,  Day_moment == "night" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM10_Smart_night)


#PM2.5#
data_filt_PM2.5<- pollution %>%
  filter(`Air pollutant` == "PM2.5")
stby(data_filt_PM2.5$Concentration, data_filt_PM2.5$`2020-2019`, FUN=descr)
stby(data_filt_PM2.5$Concentration, data_filt_PM2.5$Day_moment, FUN=descr)

PM2.5_NQ <- subset(data_filt_PM2.5,  Quarantine == "NQ")
stby(PM2.5_NQ$Concentration, PM2.5_NQ$Day_moment, FUN=descr)
PM2.5_NQ_control <- subset(data_filt_PM2.5,  Quarantine == "NQ_control")
stby(PM2.5_NQ_control$Concentration, PM2.5_NQ_control$Day_moment, FUN=descr)

PM2.5_SIQ <- subset(data_filt_PM2.5,  Quarantine == "SIQ")
stby(PM2.5_SIQ$Concentration, PM2.5_SIQ$Day_moment, FUN=descr)
PM2.5_SIQ_control <- subset(data_filt_PM2.5,  Quarantine == "SIQ_control")
stby(PM2.5_SIQ_control$Concentration, PM2.5_SIQ_control$Day_moment, FUN=descr)

PM2.5_SubaQ <- subset(data_filt_PM2.5,  Quarantine == "SubaQ")
stby(PM2.5_SubaQ$Concentration, PM2.5_SubaQ$Day_moment, FUN=descr)
PM2.5_SubaQ_control <- subset(data_filt_PM2.5,  Quarantine == "SubaQ_control")
stby(PM2.5_SubaQ_control$Concentration, PM2.5_SubaQ_control$Day_moment, FUN=descr)

ggbetweenstats(data  = data_filt_PM2.5, x= `2020-2019`, y= Concentration, title= "[PM2.5, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= data_filt_PM2.5)
ggbetweenstats(data  = data_filt_PM2.5, x= Day_moment, y= Concentration, title= "[PM10, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")

PM2.5_National_day<- subset(data_filt_PM2.5,  Day_moment == "day" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM2.5_National_day)
PM2.5_National_night<- subset(data_filt_PM2.5,  Day_moment == "night" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM2.5_National_night)

PM2.5_Smart_day<- subset(data_filt_PM2.5,  Day_moment == "day" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM2.5_Smart_day)
PM2.5_Smart_night<- subset(data_filt_PM2.5,  Day_moment == "night" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM2.5_Smart_night)

PM2.5_Suba_day<- subset(data_filt_PM2.5,  Day_moment == "day" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM2.5_Suba_day)
PM2.5_Suba_night<- subset(data_filt_PM2.5,  Day_moment == "night" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= PM2.5_Suba_night)

#O3#
data_filt_O3<- pollution %>%
  filter(`Air pollutant` == "O3")
stby(data_filt_O3$Concentration, data_filt_O3$`2020-2019`, FUN=descr)
stby(data_filt_O3$Concentration, data_filt_O3$Day_moment, FUN=descr)

O3_NQ <- subset(data_filt_O3,  Quarantine == "NQ")
stby(O3_NQ$Concentration, O3_NQ$Day_moment, FUN=descr)
O3_NQ_control <- subset(data_filt_O3,  Quarantine == "NQ_control")
stby(O3_NQ_control$Concentration, O3_NQ_control$Day_moment, FUN=descr)

O3_SIQ <- subset(data_filt_O3,  Quarantine == "SIQ")
stby(O3_SIQ$Concentration, O3_SIQ$Day_moment, FUN=descr)
O3_SIQ_control <- subset(data_filt_O3,  Quarantine == "SIQ_control")
stby(O3_SIQ_control$Concentration, O3_SIQ_control$Day_moment, FUN=descr)

O3_SubaQ <- subset(data_filt_O3,  Quarantine == "SubaQ")
stby(O3_SubaQ$Concentration, O3_SubaQ$Day_moment, FUN=descr)
O3_SubaQ_control <- subset(data_filt_O3,  Quarantine == "SubaQ_control")
stby(O3_SubaQ_control$Concentration, O3_SubaQ_control$Day_moment, FUN=descr)

ggbetweenstats(data  = data_filt_O3, x= `2020-2019`, y= Concentration, title= "[O3, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= data_filt_O3)
ggbetweenstats(data  = data_filt_O3, x= Day_moment, y= Concentration, title= "[PM10, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")

O3_National_day<- subset(data_filt_O3,  Day_moment == "day" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= O3_National_day)
O3_National_night<- subset(data_filt_O3,  Day_moment == "night" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= O3_National_night)

O3_Smart_day<- subset(data_filt_O3,  Day_moment == "day" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= O3_Smart_day)
O3_Smart_night<- subset(data_filt_O3,  Day_moment == "night" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= O3_Smart_night)

O3_Suba_day<- subset(data_filt_O3,  Day_moment == "day" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= O3_Suba_day)
O3_Suba_night<- subset(data_filt_O3,  Day_moment == "night" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= O3_Suba_night)

#SO2#
data_filt_SO2<- pollution %>%
  filter(`Air pollutant` == "SO2")
stby(data_filt_SO2$Concentration, data_filt_SO2$`2020-2019`, FUN=descr)
stby(data_filt_SO2$Concentration, data_filt_SO2$Day_moment, FUN=descr)

SO2_NQ <- subset(data_filt_SO2,  Quarantine == "NQ")
stby(SO2_NQ$Concentration, SO2_NQ$Day_moment, FUN=descr)
SO2_NQ_control <- subset(data_filt_SO2,  Quarantine == "NQ_control")
stby(SO2_NQ_control$Concentration, SO2_NQ_control$Day_moment, FUN=descr)

SO2_SIQ <- subset(data_filt_SO2,  Quarantine == "SIQ")
stby(SO2_SIQ$Concentration, SO2_SIQ$Day_moment, FUN=descr)
SO2_SIQ_control <- subset(data_filt_SO2,  Quarantine == "SIQ_control")
stby(SO2_SIQ_control$Concentration, SO2_SIQ_control$Day_moment, FUN=descr)

SO2_SubaQ <- subset(data_filt_SO2,  Quarantine == "SubaQ")
stby(SO2_SubaQ$Concentration, SO2_SubaQ$Day_moment, FUN=descr)
SO2_SubaQ_control <- subset(data_filt_SO2,  Quarantine == "SubaQ_control")
stby(SO2_SubaQ_control$Concentration, SO2_SubaQ_control$Day_moment, FUN=descr)

ggbetweenstats(data  = data_filt_SO2, x= `2020-2019`, y= Concentration, title= "[SO2, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= data_filt_SO2)
ggbetweenstats(data  = data_filt_SO2, x= Day_moment, y= Concentration, title= "[PM10, ??g/m3] (media) en los años 2020 (G1_tto) y 2019 (G2_ctr)")

SO2_National_day<- subset(data_filt_SO2,  Day_moment == "day" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= SO2_National_day)
SO2_National_night<- subset(data_filt_SO2,  Day_moment == "night" & Spec_Quarant == "National")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= SO2_National_night)

SO2_Smart_day<- subset(data_filt_SO2,  Day_moment == "day" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= SO2_Smart_day)
SO2_Smart_night<- subset(data_filt_SO2,  Day_moment == "night" & Spec_Quarant == "Smart")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= SO2_Smart_night)

SO2_Subat_day<- subset(data_filt_SO2,  Day_moment == "day" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= SO2_Subat_day)
SO2_Suba_night<- subset(data_filt_SO2,  Day_moment == "night" & Spec_Quarant == "Suba")
t.test(Concentration~ `2020-2019`, alternative = "less", var.equal = F, data= SO2_Suba_night)


#Objetivo específico 2 - Agotamiento del efecto durante cuarentena#
#PM10 - National Quarantine#
data_filt_PM10<- pollution %>%
  filter(`Air pollutant` == "PM10")
PM10_NQ <- subset(data_filt_PM10,  Quarantine == "NQ")
plotmeans(PM10_NQ$Concentration ~ PM10_NQ$effect_depletion, 
          main="Depletion effect on National Quarantine",
          ylab=paste("[PM10, mcrg/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
PM10_NQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = PM10_NQ, var.equal = TRUE)
PM10_NQ_AOV
eta_sq(PM10_NQ_AOV)
pairwise.t.test(PM10_NQ$Concentration, PM10_NQ$effect_depletion, p.adj = "bonf")
#PM10 - SMART Quarantine#
data_filt_PM10<- pollution %>%
  filter(`Air pollutant` == "PM10")
PM10_SIQ <- subset(data_filt_PM10,  Quarantine == "SIQ")
plotmeans(PM10_SIQ$Concentration ~ PM10_SIQ$effect_depletion, 
          main="Depletion effect on Smart Quarantine",
          ylab=paste("[PM10 mcgr/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
PM10_SIQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = PM10_SIQ, var.equal = TRUE)
PM10_SIQ_AOV
eta_sq(PM10_SIQ_AOV)
pairwise.t.test(PM10_SIQ$Concentration, PM10_SIQ$effect_depletion, p.adj = "bonf")
#PM10 - Suba Quarantine#
data_filt_PM10<- pollution %>%
  filter(`Air pollutant` == "PM10")
PM10_SubaQ <- subset(data_filt_PM10,  Quarantine == "SubaQ")
plotmeans(PM10_SubaQ$Concentration ~ PM10_SubaQ$effect_depletion, 
          main="Depletion effect on Suba Quarantine",
          ylab=paste("[PM10, mcgr/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
PM10_SubaQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = PM10_SubaQ, var.equal = TRUE)
PM10_SubaQ_AOV
eta_sq(PM10_SubaQ_AOV)
pairwise.t.test(PM10_SubaQ$Concentration, PM10_SubaQ$effect_depletion, p.adj = "bonf")

#PM2.5 - National Quarantine#
data_filt_PM2.5<- pollution %>%
  filter(`Air pollutant` == "PM2.5")
PM2.5_NQ <- subset(data_filt_PM2.5,  Quarantine == "NQ")
plotmeans(PM2.5_NQ$Concentration ~ PM2.5_NQ$effect_depletion, 
          main="Depletion effect on National Quarantine",
          ylab=paste("[PM2.5, mcgr/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
PM2.5_NQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = PM2.5_NQ, var.equal = TRUE)
PM2.5_NQ_AOV
eta_sq(PM2.5_NQ_AOV)
pairwise.t.test(PM2.5_NQ$Concentration, PM2.5_NQ$effect_depletion, p.adj = "bonf")
#PM2.5 - SMART Quarantine#
data_filt_PM2.5<- pollution %>%
  filter(`Air pollutant` == "PM2.5")
PM2.5_SIQ <- subset(data_filt_PM2.5,  Quarantine == "SIQ")
plotmeans(PM2.5_SIQ$Concentration ~ PM2.5_SIQ$effect_depletion, 
          main="Depletion effect on Smart Quarantine",
          ylab=paste("[PM2.5, mcgr/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
PM2.5_SIQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = PM2.5_SIQ, var.equal = TRUE)
PM2.5_SIQ_AOV
eta_sq(PM2.5_SIQ_AOV)
pairwise.t.test(PM2.5_SIQ$Concentration, PM2.5_SIQ$effect_depletion, p.adj = "bonf")
#PM2.5 - Suba Quarantine#
data_filt_PM2.5<- pollution %>%
  filter(`Air pollutant` == "PM2.5")
PM2.5_SubaQ <- subset(data_filt_PM2.5,  Quarantine == "SubaQ")
plotmeans(PM2.5_SubaQ$Concentration ~ PM2.5_SubaQ$effect_depletion, 
          main="Depletion effect on Suba Quarantine",
          ylab=paste("[PM2.5, mcgr/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
PM2.5_SubaQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = PM2.5_SubaQ, var.equal = TRUE)
PM2.5_SubaQ_AOV
eta_sq(PM2.5_SubaQ_AOV)
pairwise.t.test(PM2.5_SubaQ$Concentration, PM2.5_SubaQ$effect_depletion, p.adj = "bonf")

#O3 - National Quarantine#
data_filt_O3<- pollution %>%
  filter(`Air pollutant` == "O3")
O3_NQ <- subset(data_filt_O3,  Quarantine == "NQ")
plotmeans(O3_NQ$Concentration ~ O3_NQ$effect_depletion, 
          main="Depletion effect on National Quarantine",
          ylab=paste("[O3, ??g/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
O3_NQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = O3_NQ, var.equal = TRUE)
O3_NQ_AOV
eta_sq(O3_NQ_AOV)
pairwise.t.test(O3_NQ$Concentration, O3_NQ$effect_depletion, p.adj = "bonf")
#O3 - SMART Quarantine#
data_filt_O3<- pollution %>%
  filter(`Air pollutant` == "O3")
O3_SIQ <- subset(data_filt_O3,  Quarantine == "SIQ")
plotmeans(O3_SIQ$Concentration ~ O3_SIQ$effect_depletion, 
          main="Depletion effect on Smart Quarantine",
          ylab=paste("[O3, ??g/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
O3_SIQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = O3_SIQ, var.equal = TRUE)
O3_SIQ_AOV
eta_sq(O3_SIQ_AOV)
pairwise.t.test(O3_SIQ$Concentration, O3_SIQ$effect_depletion, p.adj = "bonf")
#O3 - Suba Quarantine#
data_filt_O3<- pollution %>%
  filter(`Air pollutant` == "O3")
O3_SubaQ <- subset(data_filt_O3,  Quarantine == "SubaQ")
plotmeans(O3_SubaQ$Concentration ~ O3_SubaQ$effect_depletion, 
          main="Depletion effect on Suba Quarantine",
          ylab=paste("[O3, ??g/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
O3_SubaQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = O3_SubaQ, var.equal = TRUE)
O3_SubaQ_AOV
eta_sq(O3_SubaQ_AOV)
pairwise.t.test(O3_SubaQ$Concentration, O3_SubaQ$effect_depletion, p.adj = "bonf")

#SO2 - National Quarantine#
data_filt_SO2<- pollution %>%
  filter(`Air pollutant` == "SO2")
SO2_NQ <- subset(data_filt_SO2,  Quarantine == "NQ")
plotmeans(SO2_NQ$Concentration ~ SO2_NQ$effect_depletion, 
          main="Depletion effect on National Quarantine",
          ylab=paste("[SO3, ??g/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
SO2_NQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = SO2_NQ, var.equal = TRUE)
SO2_NQ_AOV
eta_sq(SO2_NQ_AOV)
pairwise.t.test(SO2_NQ$Concentration, SO2_NQ$effect_depletion, p.adj = "bonf")
#SO2 - SMART Quarantine#
data_filt_SO2<- pollution %>%
  filter(`Air pollutant` == "SO2")
SO2_SIQ <- subset(data_filt_SO2,  Quarantine == "SIQ")
plotmeans(SO2_SIQ$Concentration ~ SO2_SIQ$effect_depletion, 
          main="Depletion effect on Smart Quarantine",
          ylab=paste("[SO3, ??g/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
SO2_SIQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = SO2_SIQ, var.equal = TRUE)
SO2_SIQ_AOV
eta_sq(SO2_SIQ_AOV)
pairwise.t.test(SO2_SIQ$Concentration, SO2_SIQ$effect_depletion, p.adj = "bonf")
#SO2 - Suba Quarantine#
data_filt_SO2<- pollution %>%
  filter(`Air pollutant` == "SO2")
SO2_SubaQ <- subset(data_filt_SO2,  Quarantine == "SubaQ")
plotmeans(SO2_SubaQ$Concentration ~ SO2_SubaQ$effect_depletion, 
          main="Depletion effect on Suba Quarantine",
          ylab=paste("[SO3, ??g/m3] (mean)"),
          xlab=paste("One week pre-quarantine (t0) Vs During quarantine (t1 - t4)"))
SO2_SubaQ_AOV<-oneway.test(Concentration ~ effect_depletion, data = SO2_SubaQ, var.equal = TRUE)
SO2_SubaQ_AOV
eta_sq(SO2_SubaQ_AOV)
pairwise.t.test(SO2_SubaQ$Concentration, SO2_SubaQ$effect_depletion, p.adj = "bonf")

#Objetivo específico 3 - Modelos Lineales Generalizados de interacción#
#PM10 - National Quarantine#
pollution$Day<-factor(pollution$Day, labels= c("no", "si"))
pollution$Treatment<-factor(pollution$Treatment, labels= c("no", "si"))

data_filt_PM10<- pollution %>%
  filter(`Air pollutant` == "PM10")
PM10_National_filt <- subset(data_filt_PM10,  Spec_Quarant == "National")
summary(MLG_PM10_Nat_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = PM10_National_filt))
confint.default(MLG_PM10_Nat_ide)
allEffects(MLG_PM10_Nat_ide)
glance(MLG_PM10_Nat_ide)
anova(MLG_PM10_Nat_ide)
Cookdis_glm(MLG_PM10_Nat_ide)
vif(MLG_PM10_Nat_ide)
envelope_glm(MLG_PM10_Nat_ide, rep = 1000, conf = 0.95)

summary(MLG_PM10_Nat_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = PM10_National_filt))
confint.default(MLG_PM10_Nat_log)
allEffects(MLG_PM10_Nat_log)
glance(MLG_PM10_Nat_log)
anova(MLG_PM10_Nat_log)
Cookdis_glm(MLG_PM10_Nat_log)
vif(MLG_PM10_Nat_log)
envelope_glm(MLG_PM10_Nat_log, rep = 1000, conf = 0.95)

summary(MLG_PM10_Nat_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = PM10_National_filt))
confint.default(MLG_PM10_Nat_inv)
allEffects(MLG_PM10_Nat_inv)
glance(MLG_PM10_Nat_inv)
anova(MLG_PM10_Nat_inv)
Cookdis_glm(MLG_PM10_Nat_inv)
vif(MLG_PM10_Nat_inv)
envelope_glm(MLG_PM10_Nat_inv, rep = 1000, conf = 0.95)

#PM10 - Smart Quarantine#
data_filt_PM10<- pollution %>%
  filter(`Air pollutant` == "PM10")
PM10_Smart_filt <- subset(data_filt_PM10,  Spec_Quarant == "Smart")
summary(MLG_PM10_Sma_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = PM10_Smart_filt))
confint.default(MLG_PM10_Sma_ide)
allEffects(MLG_PM10_Sma_ide)
glance(MLG_PM10_Sma_ide)
anova(MLG_PM10_Sma_ide)
Cookdis_glm(MLG_PM10_Sma_ide)
vif(MLG_PM10_Sma_ide)
envelope_glm(MLG_PM10_Sma_ide, rep = 1000, conf = 0.95)

summary(MLG_PM10_Sma_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = PM10_Smart_filt))
confint.default(MLG_PM10_Sma_log)
allEffects(MLG_PM10_Sma_log)
glance(MLG_PM10_Sma_log)
anova(MLG_PM10_Sma_log)
Cookdis_glm(MLG_PM10_Sma_log)
vif(MLG_PM10_Sma_log)
envelope_glm(MLG_PM10_Sma_log, rep = 1000, conf = 0.95)

summary(MLG_PM10_Sma_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = PM10_Smart_filt))
confint.default(MLG_PM10_Sma_inv)
allEffects(MLG_PM10_Sma_inv)
glance(MLG_PM10_Sma_inv)
anova(MLG_PM10_Sma_inv)
Cookdis_glm(MLG_PM10_Sma_inv)
vif(MLG_PM10_Sma_inv)
envelope_glm(MLG_PM10_Sma_inv, rep = 1000, conf = 0.95)

#PM10 - Suba Quarantine#
data_filt_PM10<- pollution %>%
  filter(`Air pollutant` == "PM10")
PM10_Suba_filt <- subset(data_filt_PM10,  Spec_Quarant == "Suba")
summary(MLG_PM10_Suba_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = PM10_Suba_filt))
confint.default(MLG_PM10_Suba_ide)
allEffects(MLG_PM10_Suba_ide)
glance(MLG_PM10_Suba_ide)
anova(MLG_PM10_Suba_ide)
Cookdis_glm(MLG_PM10_Suba_ide)
vif(MLG_PM10_Suba_ide)
envelope_glm(MLG_PM10_Suba_ide, rep = 1000, conf = 0.95)

summary(MLG_PM10_Suba_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = PM10_Suba_filt))
confint.default(MLG_PM10_Suba_log)
allEffects(MLG_PM10_Suba_log)
glance(MLG_PM10_Suba_log)
anova(MLG_PM10_Suba_log)
Cookdis_glm(MLG_PM10_Suba_log)
vif(MLG_PM10_Suba_log)
envelope_glm(MLG_PM10_Suba_log, rep = 1000, conf = 0.95)

summary(MLG_PM10_Suba_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = PM10_Suba_filt))
confint.default(MLG_PM10_Suba_inv)
allEffects(MLG_PM10_Suba_inv)
glance(MLG_PM10_Suba_inv)
anova(MLG_PM10_Suba_inv)
Cookdis_glm(MLG_PM10_Suba_inv)
vif(MLG_PM10_Suba_inv)
envelope_glm(MLG_PM10_Suba_inv, rep = 1000, conf = 0.95)

#PM2.5 - National Quarantine#
data_filt_PM2.5<- pollution %>%
  filter(`Air pollutant` == "PM2.5")
PM2.5_National_filt <- subset(data_filt_PM2.5,  Spec_Quarant == "National")
summary(MLG_PM2.5_Nat_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = PM2.5_National_filt))
confint.default(MLG_PM2.5_Nat_ide)
allEffects(MLG_PM2.5_Nat_ide)
glance(MLG_PM2.5_Nat_ide)
anova(MLG_PM2.5_Nat_ide)
Cookdis_glm(MLG_PM2.5_Nat_ide)
vif(MLG_PM2.5_Nat_ide)
envelope_glm(MLG_PM2.5_Nat_ide, rep = 1000, conf = 0.95)

summary(MLG_PM2.5_Nat_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = PM2.5_National_filt))
confint.default(MLG_PM2.5_Nat_log)
allEffects(MLG_PM2.5_Nat_log)
glance(MLG_PM2.5_Nat_log)
anova(MLG_PM2.5_Nat_log)
Cookdis_glm(MLG_PM2.5_Nat_log)
vif(MLG_PM2.5_Nat_log)
envelope_glm(MLG_PM2.5_Nat_log, rep = 1000, conf = 0.95)

summary(MLG_PM2.5_Nat_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = PM2.5_National_filt))
confint.default(MLG_PM2.5_Nat_inv)
allEffects(MLG_PM2.5_Nat_inv)
glance(MLG_PM2.5_Nat_inv)
anova(MLG_PM2.5_Nat_inv)
Cookdis_glm(MLG_PM2.5_Nat_inv)
vif(MLG_PM2.5_Nat_inv)
envelope_glm(MLG_PM2.5_Nat_inv, rep = 1000, conf = 0.95)

#PM2.5 - Smart Quarantine#
data_filt_PM2.5<- pollution %>%
  filter(`Air pollutant` == "PM2.5")
PM2.5_Smart_filt <- subset(data_filt_PM2.5,  Spec_Quarant == "Smart")
summary(MLG_PM2.5_Sma_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = PM2.5_Smart_filt))
confint.default(MLG_PM2.5_Sma_ide)
allEffects(MLG_PM2.5_Sma_ide)
glance(MLG_PM2.5_Sma_ide)
anova(MLG_PM2.5_Sma_ide)
Cookdis_glm(MLG_PM2.5_Sma_ide)
vif(MLG_PM2.5_Sma_ide)
envelope_glm(MLG_PM2.5_Sma_ide, rep = 1000, conf = 0.95)

summary(MLG_PM2.5_Sma_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = PM2.5_Smart_filt))
confint.default(MLG_PM2.5_Sma_log)
allEffects(MLG_PM2.5_Sma_log)
glance(MLG_PM2.5_Sma_log)
anova(MLG_PM2.5_Sma_log)
Cookdis_glm(MLG_PM2.5_Sma_log)
vif(MLG_PM2.5_Sma_log)
envelope_glm(MLG_PM2.5_Sma_log, rep = 1000, conf = 0.95)

summary(MLG_PM2.5_Sma_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = PM2.5_Smart_filt))
confint.default(MLG_PM2.5_Sma_inv)
allEffects(MLG_PM2.5_Sma_inv)
glance(MLG_PM2.5_Sma_inv)
anova(MLG_PM2.5_Sma_inv)
Cookdis_glm(MLG_PM2.5_Sma_inv)
vif(MLG_PM2.5_Sma_inv)
envelope_glm(MLG_PM2.5_Sma_inv, rep = 1000, conf = 0.95)

#PM2.5 - Suba Quarantine#
data_filt_PM2.5<- pollution %>%
  filter(`Air pollutant` == "PM2.5")
PM2.5_Suba_filt <- subset(data_filt_PM2.5,  Spec_Quarant == "Suba")
summary(MLG_PM2.5_Suba_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = PM2.5_Suba_filt))
confint.default(MLG_PM2.5_Suba_ide)
allEffects(MLG_PM2.5_Suba_ide)
glance(MLG_PM2.5_Suba_ide)
anova(MLG_PM2.5_Suba_ide)
Cookdis_glm(MLG_PM2.5_Suba_ide)
vif(MLG_PM2.5_Suba_ide)
envelope_glm(MLG_PM2.5_Suba_ide, rep = 1000, conf = 0.95)

summary(MLG_PM2.5_Suba_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = PM2.5_Suba_filt))
confint.default(MLG_PM2.5_Suba_log)
allEffects(MLG_PM2.5_Suba_log)
glance(MLG_PM2.5_Suba_log)
anova(MLG_PM2.5_Suba_log)
Cookdis_glm(MLG_PM2.5_Suba_log)
vif(MLG_PM2.5_Suba_log)
envelope_glm(MLG_PM2.5_Suba_log, rep = 1000, conf = 0.95)

summary(MLG_PM2.5_Suba_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = PM2.5_Suba_filt))
confint.default(MLG_PM2.5_Suba_inv)
allEffects(MLG_PM2.5_Suba_inv)
glance(MLG_PM2.5_Suba_inv)
anova(MLG_PM2.5_Suba_inv)
Cookdis_glm(MLG_PM2.5_Suba_inv)
vif(MLG_PM2.5_Suba_inv)
envelope_glm(MLG_PM2.5_Suba_inv, rep = 1000, conf = 0.95)

#O3 - National Quarantine#
data_filt_O3<- pollution %>%
  filter(`Air pollutant` == "O3")
O3_National_filt <- subset(data_filt_O3,  Spec_Quarant == "National")
summary(MLG_O3_Nat_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = O3_National_filt))
confint.default(MLG_O3_Nat_ide)
allEffects(MLG_O3_Nat_ide)
glance(MLG_O3_Nat_ide)
anova(MLG_O3_Nat_ide)
Cookdis_glm(MLG_O3_Nat_ide)
vif(MLG_O3_Nat_ide)
envelope_glm(MLG_O3_Nat_ide, rep = 1000, conf = 0.95)

summary(MLG_O3_Nat_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = O3_National_filt))
confint.default(MLG_O3_Nat_log)
allEffects(MLG_O3_Nat_log)
glance(MLG_O3_Nat_log)
anova(MLG_O3_Nat_log)
Cookdis_glm(MLG_O3_Nat_log)
vif(MLG_O3_Nat_log)
envelope_glm(MLG_O3_Nat_log, rep = 1000, conf = 0.95)

summary(MLG_O3_Nat_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = O3_National_filt))
confint.default(MLG_O3_Nat_inv)
allEffects(MLG_O3_Nat_inv)
glance(MLG_O3_Nat_inv)
anova(MLG_O3_Nat_inv)
Cookdis_glm(MLG_O3_Nat_inv)
vif(MLG_O3_Nat_inv)
envelope_glm(MLG_O3_Nat_inv, rep = 1000, conf = 0.95)

#O3 - Smart Quarantine#
data_filt_O3<- pollution %>%
  filter(`Air pollutant` == "O3")
O3_Smart_filt <- subset(data_filt_O3,  Spec_Quarant == "Smart")
summary(MLG_O3_Sma_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = O3_Smart_filt))
confint.default(MLG_O3_Sma_ide)
allEffects(MLG_O3_Sma_ide)
glance(MLG_O3_Sma_ide)
anova(MLG_O3_Sma_ide)
Cookdis_glm(MLG_O3_Sma_ide)
vif(MLG_O3_Sma_ide)
envelope_glm(MLG_O3_Sma_ide, rep = 1000, conf = 0.95)

summary(MLG_O3_Sma_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = O3_Smart_filt))
confint.default(MLG_O3_Sma_log)
allEffects(MLG_O3_Sma_log)
glance(MLG_O3_Sma_log)
anova(MLG_O3_Sma_log)
Cookdis_glm(MLG_O3_Sma_log)
vif(MLG_O3_Sma_log)
envelope_glm(MLG_O3_Sma_log, rep = 1000, conf = 0.95)

summary(MLG_O3_Sma_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = O3_Smart_filt))
confint.default(MLG_O3_Sma_inv)
allEffects(MLG_O3_Sma_inv)
glance(MLG_O3_Sma_inv)
anova(MLG_O3_Sma_inv)
Cookdis_glm(MLG_O3_Sma_inv)
vif(MLG_O3_Sma_inv)
envelope_glm(MLG_O3_Sma_inv, rep = 1000, conf = 0.95)

#O3 - Suba Quarantine#
data_filt_O3<- pollution %>%
  filter(`Air pollutant` == "O3")
O3_Suba_filt <- subset(data_filt_O3,  Spec_Quarant == "Suba")
summary(MLG_O3_Suba_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = O3_Suba_filt))
confint.default(MLG_O3_Suba_ide)
allEffects(MLG_O3_Suba_ide)
glance(MLG_O3_Suba_ide)
anova(MLG_O3_Suba_ide)
Cookdis_glm(MLG_O3_Suba_ide)
vif(MLG_O3_Suba_ide)
envelope_glm(MLG_O3_Suba_ide, rep = 1000, conf = 0.95)

summary(MLG_O3_Suba_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = O3_Suba_filt))
confint.default(MLG_O3_Suba_log)
allEffects(MLG_O3_Suba_log)
glance(MLG_O3_Suba_log)
anova(MLG_O3_Suba_log)
Cookdis_glm(MLG_O3_Suba_log)
vif(MLG_O3_Suba_log)
envelope_glm(MLG_O3_Suba_log, rep = 1000, conf = 0.95)

summary(MLG_O3_Suba_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = O3_Suba_filt))
confint.default(MLG_O3_Suba_inv)
allEffects(MLG_O3_Suba_inv)
glance(MLG_O3_Suba_inv)
anova(MLG_O3_Suba_inv)
Cookdis_glm(MLG_O3_Suba_inv)
vif(MLG_O3_Suba_inv)
envelope_glm(MLG_O3_Suba_inv, rep = 1000, conf = 0.95)

#SO2 - National Quarantine#
data_filt_SO2<- pollution %>%
  filter(`Air pollutant` == "SO2")
SO2_National_filt <- subset(data_filt_SO2,  Spec_Quarant == "National")
summary(MLG_SO2_Nat_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = SO2_National_filt))
confint.default(MLG_SO2_Nat_ide)
allEffects(MLG_SO2_Nat_ide)
glance(MLG_SO2_Nat_ide)
anova(MLG_SO2_Nat_ide)
Cookdis_glm(MLG_SO2_Nat_ide)
vif(MLG_SO2_Nat_ide)
envelope_glm(MLG_SO2_Nat_ide, rep = 1000, conf = 0.95)

summary(MLG_SO2_Nat_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = SO2_National_filt))
confint.default(MLG_SO2_Nat_log)
allEffects(MLG_SO2_Nat_log)
glance(MLG_SO2_Nat_log)
anova(MLG_SO2_Nat_log)
Cookdis_glm(MLG_SO2_Nat_log)
vif(MLG_SO2_Nat_log)
envelope_glm(MLG_SO2_Nat_log, rep = 1000, conf = 0.95)

summary(MLG_SO2_Nat_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = SO2_National_filt))
confint.default(MLG_SO2_Nat_inv)
allEffects(MLG_SO2_Nat_inv)
glance(MLG_SO2_Nat_inv)
anova(MLG_SO2_Nat_inv)
Cookdis_glm(MLG_SO2_Nat_inv)
vif(MLG_SO2_Nat_inv)
envelope_glm(MLG_SO2_Nat_inv, rep = 1000, conf = 0.95)

#SO2 - Smart Quarantine#
data_filt_SO2<- pollution %>%
  filter(`Air pollutant` == "SO2")
SO2_Smart_filt <- subset(data_filt_SO2,  Spec_Quarant == "Smart")
summary(MLG_SO2_Sma_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = SO2_Smart_filt))
confint.default(MLG_SO2_Sma_ide)
allEffects(MLG_SO2_Sma_ide)
glance(MLG_SO2_Sma_ide)
anova(MLG_SO2_Sma_ide)
Cookdis_glm(MLG_SO2_Sma_ide)
vif(MLG_SO2_Sma_ide)
envelope_glm(MLG_SO2_Sma_ide, rep = 1000, conf = 0.95)

summary(MLG_SO2_Sma_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = SO2_Smart_filt))
confint.default(MLG_SO2_Sma_log)
allEffects(MLG_SO2_Sma_log)
glance(MLG_SO2_Sma_log)
anova(MLG_SO2_Sma_log)
Cookdis_glm(MLG_SO2_Sma_log)
vif(MLG_SO2_Sma_log)
envelope_glm(MLG_SO2_Sma_log, rep = 1000, conf = 0.95)

summary(MLG_SO2_Sma_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = SO2_Smart_filt))
confint.default(MLG_SO2_Sma_inv)
allEffects(MLG_SO2_Sma_inv)
glance(MLG_SO2_Sma_inv)
anova(MLG_SO2_Sma_inv)
Cookdis_glm(MLG_SO2_Sma_inv)
vif(MLG_SO2_Sma_inv)
envelope_glm(MLG_SO2_Sma_inv, rep = 1000, conf = 0.95)

#SO2 - Suba Quarantine#
data_filt_SO2<- pollution %>%
  filter(`Air pollutant` == "SO2")
SO2_Suba_filt <- subset(data_filt_SO2,  Spec_Quarant == "Suba")
summary(MLG_SO2_Suba_ide<- glm(Concentration~ Treatment*Day, family = gaussian(link = "identity"), data = SO2_Suba_filt))
confint.default(MLG_SO2_Suba_ide)
allEffects(MLG_SO2_Suba_ide)
glance(MLG_SO2_Suba_ide)
anova(MLG_SO2_Suba_ide)
Cookdis_glm(MLG_SO2_Suba_ide)
vif(MLG_SO2_Suba_ide)
envelope_glm(MLG_SO2_Suba_ide, rep = 1000, conf = 0.95)

summary(MLG_SO2_Suba_log<- glm(Concentration~ Treatment*Day, family = gaussian(link = "log"), data = SO2_Suba_filt))
confint.default(MLG_SO2_Suba_log)
allEffects(MLG_SO2_Suba_log)
glance(MLG_SO2_Suba_log)
anova(MLG_SO2_Suba_log)
Cookdis_glm(MLG_SO2_Suba_log)
vif(MLG_SO2_Suba_log)
envelope_glm(MLG_SO2_Suba_log, rep = 1000, conf = 0.95)

summary(MLG_SO2_Suba_inv<- glm(Concentration~ Treatment*Day, family = Gamma(link = "inverse"), data = SO2_Suba_filt))
confint.default(MLG_SO2_Suba_inv)
allEffects(MLG_SO2_Suba_inv)
glance(MLG_SO2_Suba_inv)
anova(MLG_SO2_Suba_inv)
Cookdis_glm(MLG_SO2_Suba_inv)
vif(MLG_SO2_Suba_inv)
envelope_glm(MLG_SO2_Suba_inv, rep = 1000, conf = 0.95)


#Objetivo específico 3 - Diferencias ambientalmente significativas#
#PM10#
PM10_NQ_filt<-filter(pollution, `Air pollutant` == "PM10" & Quarantine == "NQ")
descr(PM10_NQ_filt$Concentration)
t.test(PM10_NQ_filt$Concentration, mu = 75, alternative = "less")
stat.test <- PM10_NQ_filt %>% t_test(Concentration ~ 1, mu = 75, alternative = "less")
stat.test
PM10_NQ_filt %>% cohens_d(Concentration ~ 1, mu = 75)
gghistostats(data = PM10_NQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 75) + xlab("National Quarantine, [PM10, ??g m3]") +
             geom_vline(xintercept = 75, color = "red", linetype = "dashed") +
             labs(caption = NULL)
PM10_NQ_control_filt<-filter(pollution, `Air pollutant` == "PM10" & Quarantine == "NQ_control")
descr(PM10_NQ_control_filt$Concentration)
t.test(PM10_NQ_control_filt$Concentration, mu = 75, alternative = "less")
stat.test <- PM10_NQ_control_filt %>% t_test(Concentration ~ 1, mu = 75, alternative = "less")
stat.test
PM10_NQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 75)
gghistostats(data = PM10_NQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 75) + xlab("National Quarantine - control, [PM10, ??g m3]") +
             geom_vline(xintercept = 75, color = "red", linetype = "dashed") +
             labs(caption = NULL)

PM10_SIQ_filt<-filter(pollution, `Air pollutant` == "PM10" & Quarantine == "SIQ")
descr(PM10_SIQ_filt$Concentration)
t.test(PM10_SIQ_filt$Concentration, mu = 75, alternative = "less")
stat.test <- PM10_SIQ_filt %>% t_test(Concentration ~ 1, mu = 75, alternative = "less")
stat.test
PM10_SIQ_filt %>% cohens_d(Concentration ~ 1, mu = 75)
gghistostats(data = PM10_SIQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 75) + xlab("Smart Quarantine, [PM10, ??g m3]") +
             geom_vline(xintercept = 75, color = "red", linetype = "dashed") +
             labs(caption = NULL)
PM10_SIQ_control_filt<-filter(pollution, `Air pollutant` == "PM10" & Quarantine == "SIQ_control")
descr(PM10_SIQ_control_filt$Concentration)
t.test(PM10_SIQ_control_filt$Concentration, mu = 75, alternative = "less")
stat.test <- PM10_SIQ_control_filt %>% t_test(Concentration ~ 1, mu = 75, alternative = "less")
stat.test
PM10_SIQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 75)
gghistostats(data = PM10_SIQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 75) + xlab("Smart Quarantine - control, [PM10, ??g m3]") +
             geom_vline(xintercept = 75, color = "red", linetype = "dashed") +
             labs(caption = NULL)

PM10_SubaQ_filt<-filter(pollution, `Air pollutant` == "PM10" & Quarantine == "SubaQ")
descr(PM10_SubaQ_filt$Concentration)
t.test(PM10_SubaQ_filt$Concentration, mu = 75, alternative = "less")
stat.test <- PM10_SubaQ_filt %>% t_test(Concentration ~ 1, mu = 75, alternative = "less")
stat.test
PM10_SubaQ_filt %>% cohens_d(Concentration ~ 1, mu = 75)
gghistostats(data = PM10_SubaQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 75) + xlab("Suba Quarantine, [PM10, ??g m3]") +
             geom_vline(xintercept = 75, color = "red", linetype = "dashed") +
             labs(caption = NULL)
PM10_SubaQ_control_filt<-filter(pollution, `Air pollutant` == "PM10" & Quarantine == "SubaQ_control")
descr(PM10_SubaQ_control_filt$Concentration)
t.test(PM10_SubaQ_control_filt$Concentration, mu = 75, alternative = "less")
stat.test <- PM10_SubaQ_control_filt %>% t_test(Concentration ~ 1, mu = 75, alternative = "less")
stat.test
PM10_SubaQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 75)
gghistostats(data = PM10_SubaQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 75) + xlab("Suba Quarantine - control, [PM10, ??g m3]") +
             geom_vline(xintercept = 75, color = "red", linetype = "dashed") +
             labs(caption = NULL)

#PM2.5#
PM2.5_NQ_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Quarantine == "NQ")
descr(PM2.5_NQ_filt$Concentration)
t.test(PM2.5_NQ_filt$Concentration, mu = 37, alternative = "less")
stat.test <- PM2.5_NQ_filt %>% t_test(Concentration ~ 1, mu = 37, alternative = "less")
stat.test
PM2.5_NQ_filt %>% cohens_d(Concentration ~ 1, mu = 37)
gghistostats(data = PM2.5_NQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 37) + xlab("National Quarantine, [PM2.5, ??g m3]") +
             geom_vline(xintercept = 37, color = "red", linetype = "dashed") +
             labs(caption = NULL)
PM2.5_NQ_control_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Quarantine == "NQ_control")
descr(PM2.5_NQ_control_filt$Concentration)
t.test(PM2.5_NQ_control_filt$Concentration, mu = 37, alternative = "less")
stat.test <- PM2.5_NQ_control_filt %>% t_test(Concentration ~ 1, mu = 37, alternative = "less")
stat.test
PM2.5_NQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 37)
gghistostats(data = PM2.5_NQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 37) + xlab("National Quarantine - control, [PM2.5, ??g m3]") +
             geom_vline(xintercept = 37, color = "red", linetype = "dashed") +
             labs(caption = NULL)

PM2.5_SIQ_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Quarantine == "SIQ")
descr(PM2.5_SIQ_filt$Concentration)
t.test(PM2.5_SIQ_filt$Concentration, mu = 37, alternative = "less")
stat.test <- PM2.5_SIQ_filt %>% t_test(Concentration ~ 1, mu = 37, alternative = "less")
stat.test
PM2.5_SIQ_filt %>% cohens_d(Concentration ~ 1, mu = 37)
gghistostats(data = PM2.5_SIQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 37) + xlab("Smart Quarantine, [PM2.5, ??g m3]") +
             geom_vline(xintercept = 37, color = "red", linetype = "dashed") +
             labs(caption = NULL)
PM2.5_SIQ_control_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Quarantine == "SIQ_control")
descr(PM2.5_SIQ_control_filt$Concentration)
t.test(PM2.5_SIQ_control_filt$Concentration, mu = 37, alternative = "less")
stat.test <- PM2.5_SIQ_control_filt %>% t_test(Concentration ~ 1, mu = 37, alternative = "less")
stat.test
PM2.5_SIQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 37)
gghistostats(data = PM2.5_SIQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 37) + xlab("Smart Quarantine - control, [PM2.5, ??g m3]") +
             geom_vline(xintercept = 37, color = "red", linetype = "dashed") +
             labs(caption = NULL)

PM2.5_SubaQ_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Quarantine == "SubaQ")
descr(PM2.5_SubaQ_filt$Concentration)
t.test(PM2.5_SubaQ_filt$Concentration, mu = 37, alternative = "less")
stat.test <- PM2.5_SubaQ_filt %>% t_test(Concentration ~ 1, mu = 37, alternative = "less")
stat.test
PM2.5_SubaQ_filt %>% cohens_d(Concentration ~ 1, mu = 37)
gghistostats(data = PM2.5_SubaQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 37) + xlab("Suba Quarantine, [PM2.5, ??g m3]") +
             geom_vline(xintercept = 37, color = "red", linetype = "dashed") +
             labs(caption = NULL)
PM2.5_SubaQ_control_filt<-filter(pollution, `Air pollutant` == "PM2.5" & Quarantine == "SubaQ_control")
descr(PM2.5_SubaQ_control_filt$Concentration)
t.test(PM2.5_SubaQ_control_filt$Concentration, mu = 37, alternative = "less")
stat.test <- PM2.5_SubaQ_control_filt %>% t_test(Concentration ~ 1, mu = 37, alternative = "less")
stat.test
PM2.5_SubaQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 37)
gghistostats(data = PM2.5_SubaQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 37) + xlab("Suba Quarantine - control, [PM2.5, ??g m3]") +
             geom_vline(xintercept = 37, color = "red", linetype = "dashed") +
             labs(caption = NULL)

#O3#
O3_NQ_filt<-filter(pollution, `Air pollutant` == "O3" & Quarantine == "NQ")
descr(O3_NQ_filt$Concentration)
t.test(O3_NQ_filt$Concentration, mu = 100, alternative = "less")
stat.test <- O3_NQ_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
O3_NQ_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = O3_NQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("National Quarantine, [O3, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)
O3_NQ_control_filt<-filter(pollution, `Air pollutant` == "O3" & Quarantine == "NQ_control")
descr(O3_NQ_control_filt$Concentration)
t.test(O3_NQ_control_filt$Concentration, mu = 100, alternative = "less")
stat.test <- O3_NQ_control_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
O3_NQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = O3_NQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("National Quarantine - control, [O3, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)

O3_SIQ_filt<-filter(pollution, `Air pollutant` == "O3" & Quarantine == "SIQ")
descr(O3_SIQ_filt$Concentration)
t.test(O3_SIQ_filt$Concentration, mu = 100, alternative = "less")
stat.test <- O3_SIQ_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
O3_SIQ_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = O3_SIQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("Smart Quarantine, [O3, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)
O3_SIQ_control_filt<-filter(pollution, `Air pollutant` == "O3" & Quarantine == "SIQ_control")
descr(O3_SIQ_control_filt$Concentration)
t.test(O3_SIQ_control_filt$Concentration, mu = 100, alternative = "less")
stat.test <- O3_SIQ_control_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
O3_SIQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = O3_SIQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("Smart Quarantine - control, [O3, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)

O3_SubaQ_filt<-filter(pollution, `Air pollutant` == "O3" & Quarantine == "SubaQ")
descr(O3_SubaQ_filt$Concentration)
t.test(O3_SubaQ_filt$Concentration, mu = 100, alternative = "less")
stat.test <- O3_SubaQ_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
O3_SubaQ_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = O3_SubaQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("Suba Quarantine, [O3, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)
O3_SubaQ_control_filt<-filter(pollution, `Air pollutant` == "O3" & Quarantine == "SubaQ_control")
descr(O3_SubaQ_control_filt$Concentration)
t.test(O3_SubaQ_control_filt$Concentration, mu = 100, alternative = "less")
stat.test <- O3_SubaQ_control_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
O3_SubaQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = O3_SubaQ_control_filt, x = Concentration, type = "parametric",
            alternative = "less", test.value = 100) + 
            xlab("Suba Quarantine - control, [O3, ??g m3]") +
            geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
            labs(caption = NULL)
  

#SO2#
SO2_NQ_filt<-filter(pollution, `Air pollutant` == "SO2" & Quarantine == "NQ")
descr(SO2_NQ_filt$Concentration)
t.test(SO2_NQ_filt$Concentration, mu = 100, alternative = "less")
stat.test <- SO2_NQ_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
SO2_NQ_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = SO2_NQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("National Quarantine, [SO2, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)
SO2_NQ_control_filt<-filter(pollution, `Air pollutant` == "SO2" & Quarantine == "NQ_control")
descr(SO2_NQ_control_filt$Concentration)
t.test(SO2_NQ_control_filt$Concentration, mu = 100, alternative = "less")
stat.test <- SO2_NQ_control_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
SO2_NQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = SO2_NQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("National Quarantine - control, [SO2, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)

SO2_SIQ_filt<-filter(pollution, `Air pollutant` == "SO2" & Quarantine == "SIQ")
descr(SO2_SIQ_filt$Concentration)
t.test(SO2_SIQ_filt$Concentration, mu = 100, alternative = "less")
stat.test <- SO2_SIQ_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
SO2_SIQ_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = SO2_SIQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("Smart Quarantine, [SO2, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)
SO2_SIQ_control_filt<-filter(pollution, `Air pollutant` == "SO2" & Quarantine == "SIQ_control")
descr(SO2_SIQ_control_filt$Concentration)
t.test(SO2_SIQ_control_filt$Concentration, mu = 100, alternative = "less")
stat.test <- SO2_SIQ_control_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
SO2_SIQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = SO2_SIQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("Smart Quarantine - control, [SO2, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)

SO2_SubaQ_filt<-filter(pollution, `Air pollutant` == "SO2" & Quarantine == "SubaQ")
descr(SO2_SubaQ_filt$Concentration)
t.test(SO2_SubaQ_filt$Concentration, mu = 100, alternative = "less")
stat.test <- SO2_SubaQ_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
SO2_SubaQ_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = SO2_SubaQ_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("Suba Quarantine, [SO2, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)
SO2_SubaQ_control_filt<-filter(pollution, `Air pollutant` == "SO2" & Quarantine == "SubaQ_control")
descr(SO2_SubaQ_control_filt$Concentration)
t.test(SO2_SubaQ_control_filt$Concentration, mu = 100, alternative = "less")
stat.test <- SO2_SubaQ_control_filt %>% t_test(Concentration ~ 1, mu = 100, alternative = "less")
stat.test
SO2_SubaQ_control_filt %>% cohens_d(Concentration ~ 1, mu = 100)
gghistostats(data = SO2_SubaQ_control_filt, x = Concentration, type = "parametric",
             alternative = "less", test.value = 100) + xlab("Suba Quarantine - control, [SO2, ??g m3]") +
             geom_vline(xintercept = 100, color = "red", linetype = "dashed") +
             labs(caption = NULL)

