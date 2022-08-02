
# Load libraries and remove data in environment ----------------------------------------------------------
library(car)
library(lme4)
library('mgcv') # gams - good for my data where relationships are not linear 
library(purrr)
library(broom)
library(move)
# library(goft)
#library(moveVis)
#library(argosfilter)
#library(sf)
#library(maps)
#library(mapdata)
library(fields)
library(lattice)
library(effects)
#library(trip)
library(Matrix)
library(tidyverse)
#library(plyr)
library(ggplot2)
#library(diveMove)
library(pbapply)
library(wrapr)
library(stringr)
library(lubridate)
library(microbenchmark)
library(data.table)
#library(ggstatsplot)
# library(ncdf4)
#library(raster)
library(magrittr)
library(dplyr)
library(rgl)
#library(lme4)
# install.packages('nlme')
#library(nlme)
# library(lmerTest)
#library('mgcv')
#library(car)
#library(ggpubr)
#library(rstatix)
#library(lsr)
#require(GGally)
#require(reshape2)
#require(compiler)
#require(parallel)
#require(boot)
library(stringi)
library("readxl")
# library("xlsx")
library(AICcmodavg)
library(gridExtra)
library('chron')

# install.packages('extrafont')  
library(extrafont)
# font_import()
loadfonts(device = "postscript")
extrafont::loadfonts(device="win")
windowsFonts("Times"=windowsFont("TT Times New Roman"))

rm(list = ls())
standard_error <- function(x) sd(x) / sqrt(length(x)) # Create own function


# Set working directory ---------------------------------------------------


setwd('C:/Users/Sean Evans/Documents/2021/Cousine Island Conservation/Cousine Island Hatchery Paper/Computing/Data')
# Temp_file = readxl::read_xlsx('Temperature Logs Concise.xlsx')
Temp_file = read_excel('Temperature Logs Concise.xlsx',sheet = "Sheet1")
Temp_metadata_file = read_excel('Temperature Logs Concise.xlsx',sheet = "Sheet2")

Temp_metadata_file<-t(Temp_metadata_file)
colnames(Temp_metadata_file)<-Temp_metadata_file[1,]
Temp_metadata_file<-Temp_metadata_file[-1,]
Temp_metadata_file <- data.frame(Temp_metadata_file)
Temp_metadata_file['ID']<-rownames(Temp_metadata_file)
rownames(Temp_metadata_file)<- seq(1,NROW(Temp_metadata_file))


ht_success = readxl::read_xlsx('HB nest success.xlsx')

# Temp_file = readxl::read_xlsx('Temperature Logs Concise.xlsx')
# readxl_example('Temperature Logs Concise.xlsx')
# read_excel('Temperature Logs Concise.xlsx',sheet = "Sheet2")
# excel_sheets('Temperature Logs Concise.xlsx',sheets='Sheet2')

# Setting up dataframes ---------------------------------------------------

Temp_file$ID <- as.factor(Temp_file$ID)
Temp_file$hr <- strftime(Temp_file$TIME,'%H:%M',tz = 'UTC')

# Temp_file$hr <- strftime(Temp_file$TIME,'%H:%M',tz = 'UTC+4') #Check timezone
ht_success <- ht_success %>% filter(Exclude=='NO')
# ht_success$Duration_of_relocation  <- as.factor(ht_success$Duration_of_relocation)
# ht_success$Duration_of_relocation <- strftime(ht_success$Duration_of_relocation,'%H:%M',tz = 'UTC')
# 
# as.POSIXlt(ht_success$Duration_of_relocation, tz = "UTC", format = c("%H:%M"))
# h <- hist(ht_success$Duration_of_relocation,100)
# h[["counts"]]
# density(ht_success$Duration_of_relocation)
# ht_success$Duration_of_relocation <- chron(times=ht_success$Duration_of_relocation)


ht_success$Duration_of_relocation <- as.numeric(format(ht_success$Duration_of_relocation, "%H")) +  as.numeric(format(ht_success$Duration_of_relocation, '%M'))/60
# h <- hist((ht_success %>% filter(Duration_of_relocation<2))$Duration_of_relocation,1000)
Dur_lim = 0.5
ht_success <- ht_success %>% mutate(Duration_limit = ifelse(Duration_of_relocation>0.5, 1, 0))
summary(ht_success)
###########################################################################

# Emergence and hatching success analysis Excluding In situ relocated ---------------------------------
# ht_success <- ht_success %>% filter(Treatment==c('HATCHERY','IN SITU'))
ht_success <- ht_success %>% filter(Treatment!=c('IN-SITU RELOCATED'))

### Overall Hatching success of mixed management approach for all seasons
mean(ht_success$Hatching_success)
standard_error(ht_success$Hatching_success)
sd(ht_success$Hatching_success)
length(ht_success$Hatching_success)

### Yearly variation
ht_success %>% group_by(Year) %>% summarise(ES =mean(Emergence_success, na.rm = T), HS =mean(Hatching_success, na.rm = T))

# HS and standard error
standard_error <- function(x) sd(x) / sqrt(length(x)) # Create own function
ht_success %>% group_by(Year) %>% summarise(HS =mean(Hatching_success, na.rm = T), SE = standard_error(Hatching_success), N = length(Hatching_success))

## Variation amongst treatments
ht_success %>% group_by(Treatment) %>% summarise(ES =mean(Emergence_success, na.rm = T), SDES = sd(Emergence_success), SEES = standard_error(Emergence_success), HS =mean(Hatching_success, na.rm = T), SDHS = sd(Hatching_success), SEHS = standard_error(Hatching_success), N = n())


## Variation amongst treatments per year
ht_success %>% group_by(Year,Treatment) %>% summarise(ES =mean(Emergence_success, na.rm = T), SDES = sd(Emergence_success), SEES = standard_error(Emergence_success), HS =mean(Hatching_success, na.rm = T), SDHS = sd(Hatching_success), SEHS = standard_error(Hatching_success), N = n())


## Difference in clutch size between treatments per year
ht_success %>% group_by(Year,Treatment) %>% summarise(ES =mean(Clutch_size, na.rm = T))
#% Note: Very consistent, but clutch size was slightly lower in 2019/20 in the hatchery - this was also the year when success was lower overall (in situ and hatchery)


## test for normality in data before t.tests and anovas
ht_success %>% filter(Treatment=='HATCHERY') %>% select(c(Hatching_success,Emergence_success,Clutch_size)) %>% summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, p.value = shapiro.test(.)$p.value))
#% note: significant at p<0.05 meaning the data are  not normal










# Plotting ----------------------------------------------------------------


### Simple Boxplots

p1<-ggplot(ht_success)+
  geom_boxplot(aes(x=Treatment,y=Hatching_success),fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.7)
# scale_fill_manual(values=c("#1D1D1D", "#888888"))
p2<-ggplot(ht_success)+
  geom_boxplot(aes(x=Treatment,y=Emergence_success),fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.7)

p3<-ggplot(ht_success)+
  geom_boxplot(aes(x=Treatment,y=Clutch_size),fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.7)

grid.arrange(p1, p2, p3, nrow=2, ncol=2)


### Function for plotting by Treatment
plot_Treatment <- function(success,ylabel_success){
  success <- enquo(success)
  ggplot(ht_success,aes(x=Treatment,y=!!success))+
    geom_boxplot(fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.70, outlier.shape = NaN)+
    stat_summary(fun.y = mean, geom="point", shape=20, size=5, color="black", fill="black") +
    scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))+
    xlab('TREATMENT')+
    ylab(ylabel_success)+
    theme_bw()+
    theme(strip.text = element_text(size=12),
          axis.text.x = element_text(size=14),
          axis.title.x = element_text(size=14,vjust = -0.5),
          axis.text.y = element_text(size=14),
          axis.title.y = element_text(size=14),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}

plot_Treatment(Hatching_success, 'HATCHING SUCCESS [%]')
plot_Treatment(Emergence_success, 'EMERGENCE SUCCESS [%]')

  















### Function for plotting by Year and Treatment
fontsize = 12

plot_Year.Treatment <- function(success,ylabel_success){
  success <- enquo(success)
  yearly_means_per_Treatment <<- ht_success %>% # <<- returns the object in glob env without printing in console, otherwise it does not save it.
    group_by(Year,Treatment) %>% 
    summarise(mean = mean(!!success), SE = standard_error(!!success)) %>% # calculate mean for plotting as well
    ungroup()
  # yearly_means_per_Treatment <- yearly_means_per_Treatment[-c(8), ] 
  yearly_means_per_Treatment <- yearly_means_per_Treatment %>% mutate(ypos = c(-0.2,0.2,-0.2,0.2,-0.2,0.2,-0.2,0.2))
  yearly_means_per_Treatment <- yearly_means_per_Treatment %>% mutate(ymin = c(-0.355,0.02,-0.355 ,0.02,-0.355,0.02,-0.355,0.02),ymax = c(-0.025,0.35,-0.025,0.35,-0.025,0.35,-0.025,0.35))

  ## Density plots and histograms of distributions per treatment and per year
  # ggplot(ht_success)+
  #   geom_histogram(aes(Clutch_size))

  # with means from dataset defined above ('yearly_means_per_Treatment') * in situ relocated has   been removed
  # ggplot(ht_success %>% group_by(Year,Treatment))+
  #   geom_density(aes(success ,fill=Treatment),alpha=0.70)+
  #   geom_vline(data = yearly_means_per_Treatment ,aes(xintercept = mean,colour = Treatment))+
  #   facet_wrap(~Year)+
  #   scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))
  # 
  # ggplot(ht_success)+
  #   facet_wrap(~Year)+
  #   geom_density(aes(success ,fill=Treatment),alpha=0.70)+
  #   scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))
  # 
  # ggplot(ht_success)+
  #   facet_wrap(~Year)+
  #   geom_density(aes(Clutch_size ,fill=Treatment),alpha=0.70)+
  #   scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))




  # Boxplots
  # ggplot(ht_success %>% filter(Treatment %in% c('HATCHERY','IN SITU')))+
  #   facet_wrap(~Year)+
  #   geom_boxplot(aes(success ,fill=Treatment),alpha=0.70)+
  #   scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))+
  #   theme(axis.title.y=element_blank(),
  #         axis.text.y=element_blank(),
  #         axis.ticks.y=element_blank())
  
  #boxplot with means not medians
  ggplot(ht_success,aes(x=Treatment,y=!!success, fill=Treatment))+
    scale_x_discrete(labels = c('HATCHERY' = 'Hatchery','IN SITU' = 'In situ'))+
    facet_wrap(~Year)+
    # geom_vline(data = yearly_means_per_Treatment ,aes(xintercept = mean,colour = Treatment),linetype="dashed", size=1)+
    # geom_boxplot(aes(success ,fill=Treatment, middle = mean(success)),alpha=0.70)+
    geom_boxplot(alpha=0.70, outlier.shape = NaN)+ #fatten=NULL,
    stat_summary(fun.y = mean, geom="point", shape=20, size=3, color="black", fill="black") +
    # geom_errorbar(data=yearly_means_per_Treatment, aes(x=yearly_means_per_Treatment$mean,ymin = yearly_means_per_Treatment$ymin,ymax = yearly_means_per_Treatment$ymax), width = 0.75, size = 1, linetype = "solid", geom = 'errorbar')+
    scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))+
    xlab('Treatment')+
    ylab(ylabel_success)+
    theme_bw()+
    theme(
          # text=element_text(face = 'plain', family="Times"),
          strip.text = element_text(face = 'plain', size=fontsize,family="Times"),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          axis.text.y = element_text(face = 'plain', size=fontsize,family = "Times"),
          axis.title.y = element_text(vjust = 2, face = 'plain', size=fontsize,family="Times"),
          legend.position = 'none',
          # legend.title = element_text(face="bold",size=16,family="Times"), 
          # legend.text = element_text(size=14,family="Times"),
          # legend.key.size = unit(1, 'cm'), #change legend key size
          # legend.key.height = unit(1.2, 'cm'), #change legend key height
          # legend.key.width = unit(1.2, 'cm'), #change legend key width
          axis.text.x = element_text(face = 'plain', size=fontsize,family="Times"),
          axis.title.x = element_text(face = 'plain', size=fontsize,vjust = -0.5,family="Times"),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}  

plot_Year.Treatment(Emergence_success,'Emergence success [%]')
plot_Year.Treatment(Hatching_success,'Hatching success [%]')

path = 'C:/Users/Sean Evans/Documents/2021/Cousine Island Conservation/Cousine Island Hatchery Paper/Computing/Results'

ggsave(path = path, filename='HS CB format draft.jpeg', units="in", width = 5,height = 3.5, device='jpeg', dpi=300)

dev.off()


### Standard error plots comparing different methods.

mean_HS_Hatchery = (ht_success %>% filter(Treatment=='HATCHERY'))$Hatching_success %>% mean(na.rm=T)
SE_HS_Hatchery = (ht_success %>% filter(Treatment=='HATCHERY'))$Hatching_success %>% standard_error()
n_HS_Hatchery = (ht_success %>% filter(Treatment=='HATCHERY'))$Hatching_success %>% length()

treatment_tibble = tibble('NEST TREATMENT' = c('Netted','Fenced','Control','Hatchery'), 'HATCHING SUCCESS RATE (%)' = c(60.1,54.2,27.6,mean_HS_Hatchery), SE = c(4.76,4.67,4.61,SE_HS_Hatchery))
treatment_tibble = treatment_tibble %>% mutate(ymin = .data[['HATCHING SUCCESS RATE (%)']]-.data[['SE']], ymax = .data[['HATCHING SUCCESS RATE (%)']]+.data[['SE']])
treatment_tibble$`NEST TREATMENT`  = factor(treatment_tibble$`NEST TREATMENT`, levels=c('Netted','Fenced','Control','Hatchery'))

ggplot(treatment_tibble, aes(x = .data[['NEST TREATMENT']],y = .data[['HATCHING SUCCESS RATE (%)']]))+
  geom_point(size=3)+
  ylim(20,100)+
  geom_errorbar(aes(x=`NEST TREATMENT`,ymin = ymin,ymax = ymax), width = 0.05, size = 0.6, linetype = "solid", geom = 'errorbar')+
  theme_bw()+
  theme(strip.text = element_text(size=12),
        axis.text.x = element_text(size=14),axis.title.x = element_text(size=14,vjust = -1),
        axis.text.y = element_text(size=14),axis.title.y = element_text(size=14),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  






















# Set up temporary dataframe for models

#[[[[1]]]]
##t.tests between Hatchery and In situ
tmp <- ht_success %>% filter(Treatment %in% c('HATCHERY','IN SITU'))









#[[[[1]]]]

## test for normality in data before t.tests and anovas
tmp %>% select(c(Hatching_success,Emergence_success,Clutch_size)) %>% summarise_all(.funs = funs(statistic = t.test(.~tmp$Treatment)$statistic, p.value = t.test(.~tmp$Treatment)$p.value))
#% note: significant at p<0.05 meaning the data are significantly different


# Wilcox test
tmp %>% select(c(Hatching_success,Emergence_success,Clutch_size)) %>% summarise_all(.funs = funs(statistic = wilcox.test(.~tmp$Treatment)$statistic, p.value = wilcox.test(.~tmp$Treatment)$p.value))

## Example of single t.test
t.test(tmp$Hatching_success~tmp$Treatment)

kruskal.test(tmp$Emergence_success~tmp$Treatment)

tmp %>% group_by(Treatment) %>%  summarise(meanHS = mean(Hatching_success),
                                           sdHS = sd(Hatching_success),
                                           seHS = standard_error(Hatching_success),
                                           nHS = length(Hatching_success),
                                           meanES = mean(Emergence_success),
                                           sdES = sd(Emergence_success),
                                           seES = standard_error(Emergence_success),
                                           nES = length(Emergence_success))


# Anova -------------------------------------------------------------------

leveneTest(tmp$Hatching_success,tmp$Treatment) # p-value < 0.05 therefore variances not equal

















# Emergence success -------------------------------------------------------

## 1 way anova - Treatment
one.wayT <- aov(Emergence_success ~ Treatment, data=tmp)
summary(one.wayT)
AIC(one.wayT)

## 1 way anova - Year
one.wayY <- aov(Emergence_success ~ Year, data=tmp)
summary(one.wayY)
AIC(one.wayY)

## 1 way anova - Clutch size
one.wayC <- aov(Emergence_success ~ Clutch_size, data=tmp)
summary(one.wayC)
AIC(one.wayC)

## 2 way anova
two.wayTY <- aov(Emergence_success ~ Treatment+Year, data=tmp)
two.wayTYC <- aov(Emergence_success ~ Treatment+Year+Clutch_size, data=tmp)

summary(two.wayTY)
AIC(two.wayTY)
summary(two.wayTYC)
AIC(two.wayTYC)

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
model.set <- list(one.wayT, one.wayY, one.wayC, two.wayTY, two.wayTYC)
model.names <- c('one.wayT', 'one.wayY', 'one.wayC', 'two.wayTY', 'two.wayTYC')

AICc_table<-aictab(model.set, modnames = model.names)
view(AICc_table)
# from these results which model is the best fit 




## two.wayTYC is the best model
augment(two.wayTY)
table_two.wayTY<-tidy(two.wayTY)
glance(two.wayTY)

plot(two.wayTYC)
plot(effect('Year',two.wayTYC))
plot(effect('Treatment',two.wayTYC))
plot(effect('Clutch_size',two.wayTYC))
qqnorm(two.wayTYC$residuals) # sad face = non-constant variance - model is skewed = bad...

view(table_two.wayTYC)
# GLM fits --------------------------------------------------------------


lm_T <- lm(Emergence_success ~ Treatment, data=tmp)
lm_TY <- lm(Emergence_success ~ Treatment+Year, data=tmp)
lm_TYC <- lm(Emergence_success ~ Treatment+Year+Clutch_size, data=tmp)

glm__T <- glm(Emergence_success ~ Treatment, data=tmp)
glm__TY <- glm(Emergence_success ~ Treatment+Year, data=tmp)
glm__TYC <- glm(Emergence_success ~ Treatment+Year+Clutch_size, data=tmp)

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
model.set <- list(lm_T, lm_TY, lm_TYC)
model.names <- c('lm_T', 'lm_TY', 'lm_TYC')

aictab(model.set, modnames = model.names)
# from these results it appears that the one-way model is the best fit 

model.set <- list(glm__T, glm__TY, glm__TYC)
model.names <- c('glm__T', 'glm__TY', 'glm__TYC')

aictab(model.set, modnames = model.names)

## Tidy tables of results
augment(glm__TYC)
tidy(glm__TYC)
glance(glm__TYC)

plot(glm__TYC)

plot(glm__TYC$effects)
effect('Year',glm__TYC)
effect('Treatment',glm__TYC)
effect('Clutch_size',glm__TYC)
     
plot(effect('Year',glm__TYC))
plot(effect('Treatment',glm__TYC))
plot(effect('Clutch_size',glm__TYC))

plot(lm_TYC)
plot(effect('Year',lm_TYC))
plot(effect('Treatment',lm_TYC))
plot(effect('Clutch_size',lm_TYC))

















# Hatching success --------------------------------------------------------

## 1 way anova - Treatment
one.wayT <- aov(Hatching_success ~ Treatment, data=tmp)
summary(one.wayT)
AIC(one.wayT)

## 1 way anova - Year
one.wayY <- aov(Hatching_success ~ Year, data=tmp)
summary(one.wayY)
AIC(one.wayY)

## 1 way anova - Clutch size
one.wayC <- aov(Hatching_success ~ Clutch_size, data=tmp)
summary(one.wayC)
AIC(one.wayC)

## 2 way anova
two.wayTY <- aov(Hatching_success ~ Treatment+Year, data=tmp)
two.wayTYC <- aov(Hatching_success ~ Treatment+Year+Clutch_size, data=tmp)

summary(two.wayTY)
AIC(two.wayTY)
summary(two.wayTYC)
AIC(two.wayTYC)

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
model.set <- list(one.wayT, one.wayY, one.wayC, two.wayTY, two.wayTYC)
model.names <- c('one.wayT', 'one.wayY', 'one.wayC', 'two.wayTY', 'two.wayTYC')

AICc_table<-aictab(model.set, modnames = model.names)
view(AICc_table)

# from these results it appears that the one-way model is the best fit 


## two.wayTY is the best model
augment(two.wayTY)
table_two.wayTY<-tidy(two.wayTY)
glance(two.wayTY)

view(table_two.wayTY)

# GLM fits --------------------------------------------------------------


lm_T <- lm(Hatching_success ~ Treatment, data=tmp)
lm_TY <- lm(Hatching_success ~ Treatment+Year, data=tmp)
lm_TYC <- lm(Hatching_success ~ Treatment+Year+Clutch_size, data=tmp)

glm__T <- glm(Hatching_success ~ Treatment, data=tmp)
glm__TY <- glm(Hatching_success ~ Treatment+Year, data=tmp)
glm__TYC <- glm(Hatching_success ~ Treatment+Year+Clutch_size, data=tmp)

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
model.set <- list(lm_T, lm_TY, lm_TYC)
model.names <- c('lm_T', 'lm_TY', 'lm_TYC')

aictab(model.set, modnames = model.names)
# from these results it appears that the one-way model is the best fit 

model.set <- list(glm__T, glm__TY, glm__TYC)
model.names <- c('glm__T', 'glm__TY', 'glm__TYC')

aictab(model.set, modnames = model.names)

## Tidy tables of results
augment(glm__TY)
tidy(glm__TY)
glance(glm__TY)
## Plots of results
plot(glm__TY)
plot(glm__TYC$effects)
effect('Year',glm__TYC)
effect('Treatment',glm__TYC)
effect('Clutch_size',glm__TYC)

plot(effect('Year',glm__TYC))
plot(effect('Treatment',glm__TYC))
plot(effect('Clutch_size',glm__TYC))




















# Set up temporary dataframe for models
#[[[[2]]]]


#Adding a categorical variable effect - duration of relocation 


##### Duartion_limit = binary variable #####
tmp <- ht_success %>% filter(Treatment %in% c('HATCHERY','IN SITU')) %>% filter(!is.na(Duration_of_relocation))

##### Duartion_limit = categorical variable #####
tmp <- ht_success %>% mutate(Duration_limit = ifelse(Duration_of_relocation>2, 'more than 2 hrs', 'less than or equal to 2 hrs')) %>% filter(Treatment %in% c('HATCHERY','IN SITU')) %>% filter(!is.na(Duration_of_relocation))


hist(tmp$Duration_of_relocation,breaks=100)
abline(v=2)
tmp %>% group_by(Duration_limit) %>% summarise(n = length(Hatching_success),meanHS = mean(Hatching_success),seHS = standard_error(Emergence_success),meanES = mean(Hatching_success),seES = standard_error(Emergence_success))
#remove outliers
tmp <- tmp %>% filter(Duration_of_relocation>0 & Duration_of_relocation<2)

# Emergence success -------------------------------------------------------


## 1 way anova - Treatment
one.wayT <- aov(Emergence_success ~ Treatment, data=tmp)
summary(one.wayT)
AIC(one.wayT)

## 1 way anova - Treatment
one.wayD <- aov(Emergence_success ~ Duration_of_relocation, data=tmp)
summary(one.wayD)
AIC(one.wayD)

## 1 way anova - Year
one.wayY <- aov(Emergence_success ~ Year, data=tmp)
summary(one.wayY)
AIC(one.wayY)

## 1 way anova - Clutch size
one.wayC <- aov(Emergence_success ~ Clutch_size, data=tmp)
summary(one.wayC)
AIC(one.wayC)

## 2 way anova
# two.wayTY <- aov(Emergence_success ~ Treatment+Year, data=tmp)
# two.wayTYC <- aov(Emergence_success ~ Treatment+Year+Clutch_size, data=tmp)
two.wayYD <- aov(Emergence_success ~ Year+Duration_of_relocation, data=tmp)
# two.wayTC <- aov(Emergence_success ~ Treatment+Clutch_size, data=tmp)
two.wayCD <- aov(Emergence_success ~ Clutch_size+Duration_of_relocation, data=tmp)
two.wayYCD <- aov(Emergence_success ~ Year+Clutch_size+Duration_of_relocation, data=tmp)

summary(two.wayTY)
AIC(two.wayTY)
summary(two.wayYD)
AIC(two.wayYD)

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
# model.set <- list(one.wayT, one.wayY, one.wayC, two.wayTY, two.wayTYC)
# model.names <- c('one.wayT', 'one.wayY', 'one.wayC', 'two.wayTY', 'two.wayTYC')
model.set <- list(two.wayYD, one.wayD,two.wayCD,two.wayYCD)
model.names <- c('two.wayYD', 'one.wayD','two.wayCD','two.wayYCD')

AICc_table1<-aictab(model.set, modnames = model.names)
view(AICc_table1)
# from these results it appears that the two-way model is the best fit 


## two.wayTYC is the best model
augment(two.wayTYC)
table_two.wayTYC<-tidy(two.wayTYC)
glance(two.wayTYC)

augment(two.wayYD)
table_two.wayTYC<-tidy(two.wayYD)
glance(two.wayYD)



plot(two.wayYD)
plot(effect('Year',two.wayYD))
plot(effect('Duration_limit',two.wayYD))
plot(effect('Duration_of_relocation',two.wayYD))
plot(effect('Clutch_size',two.wayTYC))
qqnorm(two.wayTYC$residuals) # sad face = non-constant variance - model is skewed = bad...

view(table_two.wayTYC)
# GLM fits --------------------------------------------------------------


lm_T <- lm(Emergence_success ~ Duration_limit, data=tmp)
lm_TY <- lm(Emergence_success ~ Duration_limit+Year, data=tmp)
lm_TYC <- lm(Emergence_success ~ Duration_limit+Year+Clutch_size, data=tmp)

glm__T <- glm(Emergence_success ~ Duration_limit, data=tmp)
glm__TY <- glm(Emergence_success ~ Duration_limit+Year, data=tmp)
glm__TYC <- glm(Emergence_success ~ Duration_limit+Year+Clutch_size, data=tmp)

# summary(lmer(Emergence_success ~ Duration_limit+Year+(1|Clutch_size), data=tmp,REML = FALSE))

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
model.set <- list(lm_T, lm_TY, lm_TYC)
model.names <- c('lm_T', 'lm_TY', 'lm_TYC')

aictab(model.set, modnames = model.names)
# from these results it appears that the one-way model is the best fit 

model.set <- list(glm__T, glm__TY, glm__TYC)
model.names <- c('glm__T', 'glm__TY', 'glm__TYC')

aictab(model.set, modnames = model.names)

## Tidy tables of results
augment(glm__TYC)
tidy(glm__TYC)
glance(glm__TYC)

plot(glm__TYC)

plot(glm__TYC$effects)
effect('Year',glm__TYC)
effect('Treatment',glm__TYC)
effect('Clutch_size',glm__TYC)

plot(effect('Year',glm__TYC))
plot(effect('Duration_limit',glm__TYC))
plot(effect('Clutch_size',glm__TYC))

plot(lm_TYC)
plot(effect('Year',lm_TYC))
plot(effect('Duration_limit',lm_TYC))
plot(effect('Clutch_size',lm_TYC))













# Hatching success --------------------------------------------------------

## 1 way anova - Treatment
one.wayT <- aov(Hatching_success ~ Treatment, data=tmp)
summary(one.wayT)
AIC(one.wayT)

## 1 way anova - Year
one.wayY <- aov(Hatching_success ~ Year, data=tmp)
summary(one.wayY)
AIC(one.wayY)

## 1 way anova - Clutch size
one.wayC <- aov(Hatching_success ~ Clutch_size, data=tmp)
summary(one.wayC)
AIC(one.wayC)

## 2 way anova
two.wayTY <- aov(Hatching_success ~ Treatment+Year, data=tmp)
two.wayTYC <- aov(Hatching_success ~ Treatment+Year+Clutch_size, data=tmp)

summary(two.wayTY)
AIC(two.wayTY)
summary(two.wayTYC)
AIC(two.wayTYC)

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
model.set <- list(one.wayT, one.wayY, one.wayC, two.wayTY, two.wayTYC)
model.names <- c('one.wayT', 'one.wayY', 'one.wayC', 'two.wayTY', 'two.wayTYC')

AICc_table<-aictab(model.set, modnames = model.names)
view(AICc_table)
# from these results it appears that the one-way model is the best fit 


## two.wayTYC is the best model
augment(two.wayTYC)
table_two.wayTYC<-tidy(two.wayTYC)
glance(two.wayTYC)

view(table_two.wayTYC)

# GLM fits --------------------------------------------------------------


lm_T <- lm(Hatching_success ~ Treatment, data=tmp)
lm_TY <- lm(Hatching_success ~ Treatment+Year, data=tmp)
lm_TYC <- lm(Hatching_success ~ Treatment+Year+Clutch_size, data=tmp)

glm__T <- glm(Hatching_success ~ Treatment, data=tmp)
glm__TY <- glm(Hatching_success ~ Treatment+Year, data=tmp)
glm__TYC <- glm(Hatching_success ~ Treatment+Year+Clutch_size, data=tmp)

####### It looks like the additive effects of year and treatment has a significant effect on emergence and hatching success but not clutch size according to t.tests and anova
# Compare AIC for best model fit
model.set <- list(lm_T, lm_TY, lm_TYC)
model.names <- c('lm_T', 'lm_TY', 'lm_TYC')

aictab(model.set, modnames = model.names)
# from these results it appears that the one-way model is the best fit 

model.set <- list(glm__T, glm__TY, glm__TYC)
model.names <- c('glm__T', 'glm__TY', 'glm__TYC')

aictab(model.set, modnames = model.names)

## Tidy tables of results
augment(glm__TYC)
tidy(glm__TYC)
glance(glm__TYC)
## Plots of results
plot(glm__TYC)
plot(glm__TYC$effects)
effect('Year',glm__TYC)
effect('Treatment',glm__TYC)
effect('Clutch_size',glm__TYC)

plot(effect('Year',glm__TYC))
plot(effect('Treatment',glm__TYC))
plot(effect('Clutch_size',glm__TYC))

# Boxplot

p1<-ggplot(tmp)+
  geom_boxplot(aes(x=Treatment,y=Hatching_success),fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.7)
# scale_fill_manual(values=c("#1D1D1D", "#888888"))
p2<-ggplot(tmp)+
  geom_boxplot(aes(x=Treatment,y=Emergence_success),fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.7)

p3<-ggplot(tmp)+
  geom_boxplot(aes(x=Treatment,y=Clutch_size),fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.7)

grid.arrange(p1, p2, p3, nrow=2, ncol=2)

















###########################################################################

# Temperature data analysis -----------------------------------------------


# Excluding once-daily temp data from hatchery ----------------------------

tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In-Situ','Hatchery')) %>% filter(!ID %in% c('HATCH_01','HATCH_02','HATCH_09'))

tmp <- tmp %>% group_by(nest_type,DAY)
tmp<-tmp %>% summarise(ymin=min(TEMP)) %>% ungroup() %>% mutate(tmp %>% summarise(ymax=max(TEMP)) %>% select(ymax)) %>% ungroup() %>% mutate(tmp %>% summarise(x=unique(DAY)) %>% select(x)) %>% ungroup() %>% mutate(tmp %>% summarise(y=mean(TEMP)) %>% select(y))

ggplot(tmp,aes(x=x,y=y,ymin=ymin,ymax=ymax, fill=nest_type, linetype=nest_type))+
  geom_line(size=2)+
  #geom_ribbon(alpha=0.6)+
  scale_fill_manual(values=c("black", "darkgrey"))+
  ylab('Temperature [DegC]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  geom_hline(yintercept = 35, colour = 'purple')+ # Refer to Ganes paper for optimal temperatures (thermal limits/ pivotal temp)
  geom_hline(yintercept = 25, colour = 'purple')+
  geom_hline(yintercept = 29.2, colour = 'blue',linetype=2, size=1)+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), legend.text = element_text(size=14),
        axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))














# Including once-daily data from hatchery ---------------------------------

# Select graph (i.e. nest type) -------------------------------------------

# Line graph
number = 10
ID = (Temp_file$ID %>% unique())[number]
tmp <- Temp_file %>% filter(ID == (Temp_file$ID %>% unique())[number])
tmp = tmp %>% group_by(DAY) %>% summarise('TEMP' = mean(TEMP))
plot(tmp$DAY,tmp$TEMP,'line')


# Boxplot
ggplot() +
  geom_boxplot(aes(group=tmp$DAY,y=tmp$TEMP))+
  xlab(label = tmp$DAY %>% unique())


# Plot all graphs ---------------------------------------------------------

Temp_file$nest_type = rep(1,length(Temp_file$ID))

tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In-Situ','Hatchery'))

ggplot(tmp %>% group_by(nest_type,ID))+
  geom_line(aes(DAY,TEMP,colour=nest_type))
            

ggplot(tmp %>% filter(nest_type=='Hatchery') %>% group_by(ID,DAY) %>% mutate(mean_temp=mean(TEMP)))+
  geom_point(aes(DAY,mean_temp,colour=ID))


# boxplot of hatchery versus in situ --------------------------------------
tmp$DAY <- as.factor(tmp$DAY)
tmp <- tmp %>% group_by(nest_type,DAY)

ggplot(tmp %>% group_by(nest_type)) +
  geom_boxplot(aes(x=DAY,y=TEMP,group=nest_type,colour=nest_type))

tmp <- tmp %>% group_by(DAY)
ggplot(tmp,aes(x=DAY,y=TEMP,fill=nest_type))+
  geom_boxplot(color=("black"))+
  scale_fill_manual(values=c("black", "grey")) +
  scale_alpha_manual(values=c(0.4,1)) +
  #geom_vline(xintercept = 21, colour = 'red')+ # Refer to Sea turtle appendix 
  #geom_vline(xintercept = 43, colour = 'red')+ # Refer to Sea turtle appendix
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  scale_x_discrete(breaks = seq(0,80,10))







##### Plot all graphs by treatment using average of all recordings per day #####

## Ymin and Ymax of the Temperature per day for each treatment
tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In-Situ','Hatchery'))
tmp <- tmp %>% group_by(nest_type,DAY)
tmp<-tmp %>% summarise(ymin=min(TEMP)) %>% ungroup() %>% mutate(tmp %>% summarise(ymax=max(TEMP)) %>% select(ymax)) %>% ungroup() %>% mutate(tmp %>% summarise(x=unique(DAY)) %>% select(x)) %>% ungroup() %>% mutate(tmp %>% summarise(y=mean(TEMP)) %>% select(y))

ggplot(tmp,aes(x=x,y=y,ymin=ymin,ymax=ymax, fill=nest_type, linetype=nest_type))+
  geom_line(size=1.0)+
  # geom_ribbon(alpha=0.6)+
  scale_fill_manual(values=c("black", "darkgrey"))+
  ylab('Temperature [DegC]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  geom_hline(yintercept = 35, colour = 'purple')+ # Refer to Ganes paper for optimal temperatures (thermal limits/ pivotal temp)
  geom_hline(yintercept = 25, colour = 'purple')+
  geom_hline(yintercept = 29.2, colour = 'blue',linetype=2, size=1)+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), 
        legend.text = element_text(size=14),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1.8, 'cm'), #change legend key width
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))









##### Plot all graphs by treatment using midnight recordings only #####
fontsize = 12
tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In situ','Hatchery'))
tmp <- tmp %>% filter(hr=='00:00') %>% group_by(nest_type,DAY)

tmp<-tmp %>% summarise(ymin=min(TEMP)) %>% ungroup() %>% mutate(tmp %>% summarise(ymax=max(TEMP)) %>% select(ymax)) %>% ungroup() %>% mutate(tmp %>% summarise(x=unique(DAY)) %>% select(x)) %>% ungroup() %>% mutate(tmp %>% summarise(y=mean(TEMP)) %>% select(y))

ggplot(tmp,aes(x=x,y=y,ymin=ymin,ymax=ymax, fill=nest_type, linetype=nest_type))+
  geom_line(size=1.1)+
  # geom_ribbon(alpha=0.6)+
  scale_fill_manual(name='Nest type',values=c("black", "darkgrey"))+
  scale_colour_discrete(name='Nest type')+
  labs(fill="Nest type", linetype="Nest type")+
  ylab('Temperature (\u00B0C)')+
  xlab('Days')+
  # geom_vline(xintercept = 20, colour = 'grey', size=1)+ # Refer to Sea turtle appendix 
  # geom_vline(xintercept = 40, colour = 'grey', size=1)+
  geom_hline(aes(yintercept = 35, colour = 'Thermal limits'), size=1)+ # Refer to Ganes paper for optimal temperatures (thermal limits/ pivotal temp)
  geom_hline(yintercept = 25, colour = 'grey', size=1)+
  geom_hline(aes(yintercept = 29.2, colour = 'Pivitol temperature'),linetype=2, size=1)+
  scale_colour_manual(labels=c('Thermal limit','Pivitol temperature'),values=c('grey','grey'))+
  theme_bw()+
  theme(strip.text = element_text(face = 'plain', size=fontsize,family="Times"),
        legend.title = element_blank(),
        legend.text = element_text(face = 'plain', size=fontsize,family="Times"),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1.8, 'cm'), #change legend key width
        axis.text.x = element_text(face = 'plain', size=fontsize,family = "Times"),
        axis.title.x = element_text(face = 'plain', size=fontsize,family = "Times",vjust = -1),
        axis.text.y = element_text(face = 'plain', size=fontsize,family = "Times"),
        axis.title.y = element_text(face = 'plain', size=fontsize,family = "Times",vjust = 2),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

path = 'C:/Users/Sean Evans/Documents/2021/Cousine Island Conservation/Cousine Island Hatchery Paper/Computing/Results/Temperature Results'

ggsave(path = path, filename='Hatchery versus In situ midnight temp CB format draft.jpeg', units="in", width = 7.5,height = 3.5, device='jpeg', dpi=300)
dev.off()

# Correlation
cor((tmp %>% filter(nest_type=='Hatchery' & DAY<60))$y, (tmp %>% filter(nest_type=='In-Situ' & DAY<60))$y)
scatter.smooth((tmp %>% filter(nest_type=='Hatchery' & DAY<60))$y, (tmp %>% filter(nest_type=='In-Situ' & DAY<60))$y)



# boxplot of difference in TSP temperature between treatments 




## 1 standard deviation from mean of the Temperature per day for each treatment
tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In-Situ','Hatchery'))
tmp <- tmp %>% group_by(nest_type,DAY)
tmp<-tmp %>% summarise(ymin=mean(TEMP)-sd(TEMP)) %>% ungroup() %>% mutate(tmp %>% summarise(ymax=mean(TEMP)+sd(TEMP)) %>% select(ymax)) %>% ungroup() %>% mutate(tmp %>% summarise(x=unique(DAY)) %>% select(x)) %>% ungroup() %>% mutate(tmp %>% summarise(y=mean(TEMP)) %>% select(y))

ggplot(tmp,aes(x=x,y=y,ymin=ymin,ymax=ymax, fill=nest_type, linetype=nest_type))+
  geom_line()+
  geom_ribbon(alpha=0.6)+
  scale_fill_manual(values=c("black", "darkgrey"))+
  ylab('TEMPERATURE [\u00B0C]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  geom_hline(yintercept = 35, colour = 'purple')+ # Refer to Ganes paper for optimal temperatures
  geom_hline(yintercept = 25, colour = 'purple')+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), legend.text = element_text(size=14),
        axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))























# ####### Average temperature across cover per day ------------------------

##### In-Situ nests grouped by cover
tmp<-right_join(Temp_metadata_file %>% select(ID,Cover),Temp_file,by='ID')
tmp<-right_join(Temp_metadata_file %>% select(ID,Location),tmp,by='ID')
tmp_in_situ<-tmp %>% filter(Location=='In-Situ') %>% group_by(Cover,DAY)
tmp_max_min_in_situ<-tmp_in_situ %>% summarise(means=mean(TEMP),ymin=min(TEMP),ymax=max(TEMP))

tmp_stdev_in_situ<-tmp_in_situ %>% summarise(means=mean(TEMP),ymin=mean(TEMP)-sd(TEMP),ymax=mean(TEMP)+sd(TEMP))


ggplot(tmp_max_min_in_situ,aes(x=DAY, y=means,ymin=ymin,ymax=ymax,linetype=Cover))+
  geom_line(size=1.5)+
  # geom_ribbon(alpha=0.6)+
  scale_fill_manual(values=c("black", "darkgrey", 'grey'))+
  ylab('TEMPERATURE [\u00B0C]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  geom_hline(yintercept = 35, colour = 'purple')+ # Refer to Ganes paper for optimal temperatures
  geom_hline(yintercept = 25, colour = 'purple')+
  geom_hline(yintercept = 29.2, colour = 'blue',linetype=2, size=1)+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), legend.text = element_text(size=14),
        axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))









##### In-Situ nests grouped by cover with hatchery included for comparison
fontsize = 12
tmp<-right_join(Temp_metadata_file %>% select(ID,Cover),Temp_file,by='ID')
tmp<-right_join(Temp_metadata_file %>% select(ID,Location),tmp,by='ID')
tmp_in_situ<-tmp %>% filter(Location=='In-Situ') %>% group_by(Cover,DAY)
tmp_hatchery<-tmp %>% filter(Location=='Hatchery') %>% group_by(DAY)
tmp_max_min_in_situ<-tmp_in_situ %>% summarise(means=mean(TEMP),ymin=min(TEMP),ymax=max(TEMP))
tmp_max_min_hatchery<-tmp_hatchery %>% summarise(means=mean(TEMP)) %>% mutate(Cover='HATCHERY')
tmp_cover <- full_join(tmp_max_min_in_situ,tmp_max_min_hatchery)

tmp_stdev_in_situ<-tmp_in_situ %>% summarise(means=mean(TEMP),ymin=mean(TEMP)-sd(TEMP),ymax=mean(TEMP)+sd(TEMP))
tmp_stdev_hatchery<-tmp_hatchery %>% summarise(means=mean(TEMP))
#Sample size for each Cover
tmp %>% group_by(ID,Cover) %>% count()

ggplot(tmp_cover,aes(x=DAY, y=means,ymin=ymin,ymax=ymax,linetype=Cover))+
  geom_line(size=1)+
  # geom_ribbon(alpha=0.6)+
  scale_fill_manual(values=c("black", "darkgrey", 'grey'))+
  ylab('Temperature (\u00B0C)')+
  xlab('Days')+
  # geom_vline(xintercept = 20, colour = 'grey', size=1)+ # Refer to Sea turtle appendix 
  # geom_vline(xintercept = 40, colour = 'grey', size=1)+
  geom_hline(aes(yintercept = 35, colour = 'Thermal limit'),linetype='solid', size=1)+ # Refer to Ganes paper for optimal temperatures (thermal limits/ pivotal temp)
  geom_hline(aes(yintercept = 29.2, colour = 'Pivitol temperature'),linetype='dashed', size=1)+
  geom_hline(yintercept = 25, colour = 'grey', size=1)+
  scale_linetype_manual(labels=c('Hatchery','Partial','Shade','Sun'),values=c("solid","twodash","longdash", "dotted"))+
  scale_colour_manual(labels=c('Thermal limit','Pivitol temperature'),values=c('grey','grey'))+
  theme_bw()+
  theme(strip.text = element_text(face = 'plain', size=fontsize,family="Times"),
      legend.title = element_blank(),
      legend.text = element_text(face = 'plain', size=fontsize,family="Times"),
      legend.key.size = unit(1, 'cm'), #change legend key size
      legend.key.height = unit(1, 'cm'), #change legend key height
      legend.key.width = unit(1.8, 'cm'), #change legend key width
      axis.text.x = element_text(face = 'plain', size=fontsize,family = "Times"),
      axis.title.x = element_text(face = 'plain', size=fontsize,family = "Times",vjust = 0),
      axis.text.y = element_text(face = 'plain', size=fontsize,family = "Times"),
      axis.title.y = element_text(face = 'plain', size=fontsize,family = "Times",vjust = 2),
      panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

path = 'C:/Users/Sean Evans/Documents/2021/Cousine Island Conservation/Cousine Island Hatchery Paper/Computing/Results/Temperature Results/Cover'

ggsave(path = path, filename='Average cover temp graphs Hathery comparison CB format draft.jpeg', units="in", width = 7.5,height = 3.5, device='jpeg', dpi=300)
dev.off()
# Diurnal variation -------------------------------------------------------


## In-Situ midday versus midnight observations - searching for diurnal variation per cover
tmp<-right_join(Temp_metadata_file %>% select(ID,Cover),Temp_file,by='ID')
tmp<-right_join(Temp_metadata_file %>% select(ID,Location),tmp,by='ID')
tmp00<-tmp %>% filter(Location=='In-Situ' & hr=='00:00') %>% group_by(Cover,DAY)
tmp12<-tmp %>% filter(Location=='In-Situ' & hr=='12:00') %>% group_by(Cover,DAY)
tmp00$diurnal_var <- tmp00$TEMP-tmp12$TEMP
tmp <- tmp00 %>% summarise(means=mean(diurnal_var))

ggplot(tmp,aes(x=DAY, y=means, colour=Cover))+
  geom_line()+
  ylab('Diurnal variation [DegC]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), legend.text = element_text(size=14),
        axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


## Hatchery/In situ midday versus midnight observations - searching for diurnal variation
Location =  'Hatchery' #'In-Situ' 
tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In-Situ','Hatchery'))
tmp<-right_join(Temp_metadata_file %>% select(ID,Location),tmp,by='ID')
tmp00<-tmp %>% filter(Location==Location & hr=='00:00') %>% group_by(DAY) %>% filter(!ID %in% c('HATCH_01','HATCH_02','HATCH_09'))
tmp12<-tmp %>% filter(Location==Location & hr=='12:00') %>% group_by(DAY)
tmp<-inner_join(tmp12 %>% select(ID,DATE,TEMP),tmp00 %>% select(ID,DATE,TEMP),by=c('DATE','ID'))
tmp$diurnal_var <- tmp$TEMP.x-tmp$TEMP.y #(12:00-00:00)
p1 <- tmp
tmp <- tmp %>% group_by(DAY.x) %>% summarise(means=mean(diurnal_var))
max(tmp$means); min(tmp$means); sd(abs(tmp$means))
# Plotting difference
ggplot(tmp,aes(x=DAY.x, y=means))+
  geom_line()+
  ylab('Diurnal variation [DegC]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), legend.text = element_text(size=14),
        axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))
# Plotting midday and midnight observations
p12 <- p1 %>% group_by(DAY.x) %>% summarise(means=mean(TEMP.x)) %>% mutate(TOD = 'midday')
p00 <- p1 %>% group_by(DAY.x) %>% summarise(means=mean(TEMP.y)) %>% mutate(TOD = 'midnight')
tmp<-rbind(p12 %>% select(DAY.x,means,TOD),p00 %>% select(DAY.x,means,TOD))

ggplot(tmp,aes(x=DAY.x, y=means,linetype=TOD))+
  geom_line()+
  ylab('Diurnal variation [DegC]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), legend.text = element_text(size=14),
        axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))


## Hatchery midnight comparisons between once-daily and hourly 
tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In-Situ','Hatchery'))
tmp<-right_join(Temp_metadata_file %>% select(ID,Location),tmp,by='ID')
tmp00<-tmp %>% filter(Location=='Hatchery' & hr=='00:00') %>% group_by(DAY) %>% filter(ID %in% c('HATCH_01','HATCH_02','HATCH_09')) %>% mutate(TOD = 'Once-daily')

# Only at midnight 
tmp12<-tmp %>% filter(Location=='Hatchery' & hr=='00:00') %>% group_by(DAY) %>% filter(ID %in% c('HATCH_03','HATCH_04','HATCH_05','HATCH_06','HATCH_07','HATCH_08')) %>% mutate(TOD = 'Hourly')
# Daily average
tmp12<-tmp %>% filter(Location=='Hatchery') %>% group_by(DAY) %>% filter(ID %in% c('HATCH_03','HATCH_04','HATCH_05','HATCH_06','HATCH_07','HATCH_08')) %>% mutate(TOD = 'Hourly')

tmp<-rbind(tmp12 %>% select(ID,DATE,TEMP,TOD),tmp00 %>% select(ID,DATE,TEMP,TOD))
tmp <- tmp %>% group_by(DAY,TOD) %>% summarise(means=mean(TEMP))

ggplot(tmp,aes(x=DAY, y=means, linetype=TOD))+
  geom_line(size=1.5)+
  ylab('Diurnal variation [DegC]')+
  xlab('Days')+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  theme(axis.title=element_text(size=17,face="bold"),
        legend.title = element_text(face="bold",size=16), legend.text = element_text(size=14),
        axis.text.x = element_text(size=14),axis.text.y = element_text(size=14))






# TSP ---------------------------------------------------------------------
tmp = Temp_file %>%
  mutate(nest_type = ifelse(grepl("^IN",ID),'In-Situ','Hatchery')) 
# %>% filter(!ID %in% c('HATCH_01','HATCH_02','HATCH_09'))

# Get mean values over TSP for each nest ID
mean_nest_type = tmp %>% group_by(nest_type,DAY) %>% summarise('TEMP' = mean(TEMP))

ggplot(mean_nest_type)+
  geom_point(aes(DAY,TEMP,colour=nest_type),size=3)+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  scale_x_discrete(breaks = seq(0,80,10))

mean_Hatchery_Temp <- mean((mean_nest_type %>% filter(nest_type=='Hatchery'))[20:40,]$TEMP)
mean_IN_SITU_Temp <- mean((mean_nest_type %>% filter(nest_type=='In-Situ'))[20:40,]$TEMP)
mean_Hatchery_Temp-mean_IN_SITU_Temp

# T-test ------------------------------------------------------------------

## Look at mean TSP per nest
# Based on this difference 
tmp %>% ungroup()
mean_TSP_per_ID = tmp %>% group_by(nest_type,ID,DAY) %>% summarise('TEMP' = mean(TEMP)) %>% filter(DAY==c(20:40))
mean_TSP_per_ID %>% group_by(nest_type,ID) %>% summarise(mean = mean(TEMP))

mean_TSP_per_ID = tmp %>% group_by(nest_type,ID,DAY) %>% summarise('TEMP' = mean(TEMP)) %>% ungroup()
mean_TSP_per_ID$DAY <- as.integer(mean_TSP_per_ID$DAY)
mean_TSP_per_ID <- mean_TSP_per_ID %>% filter(DAY>=20 & DAY<=40)
t_test_prep <- mean_TSP_per_ID %>% group_by(nest_type,ID) %>% summarise(mean_Temp = mean(TEMP))

mean_nest_type = t_test_prep %>% group_by(nest_type) %>%  summarise(mtemp = mean(mean_Temp))

mean_Hatchery_Temp <- mean((mean_nest_type %>% filter(nest_type=='Hatchery'))$mtemp)
mean_IN_SITU_Temp <- mean((mean_nest_type %>% filter(nest_type=='In-Situ'))$mtemp)
mean_Hatchery_Temp-mean_IN_SITU_Temp


t_test_prep %>% select(c('nest_type','mean_Temp'))

ggplot(t_test_prep)+
  geom_boxplot(aes(nest_type,mean_Temp,fill=nest_type),alpha=0.60)+
  scale_fill_manual(values=c("black", "darkgrey"))

ggplot(t_test_prep)+
  facet_wrap(~nest_type)+
  geom_histogram(aes(mean_Temp,fill=nest_type),alpha=0.60)+
  scale_fill_manual(values=c("black", "darkgrey"))

#Boxplot
ggplot(t_test_prep %>% group_by(nest_type),aes(nest_type,mean_Temp,fill=nest_type))+
  geom_boxplot(fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.70, outlier.shape = NaN)+
  stat_summary(fun.y = mean, geom="point", shape=20, size=5, color="black", fill="black") +
  scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))+
  xlab('NEST TYPE')+
  ylab('TEMPERATURE [\u00B0C]')+
  geom_hline(yintercept = 35, colour = 'purple', size=1)+ # Refer to Ganes paper for optimal temperatures (thermal limits/ pivotal temp)
  geom_hline(yintercept = 25, colour = 'purple', size=1)+
  geom_hline(yintercept = 29.2, colour = 'blue',linetype=2, size=1)+
  theme_bw()+
  theme(strip.text = element_text(size=12),
        axis.text.x = element_text(size=14),
        axis.title.x = element_text(size=14,vjust = -0.5),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

wilcox.test(mean_Temp~nest_type,t_test_prep %>% group_by(nest_type))
  #adjust_pvalue(method = "bonferroni") %>%
  #add_significance("p.adj")

kruskal.test(mean_Temp~nest_type,t_test_prep %>% group_by(nest_type))


t.test(mean_Temp~nest_type,t_test_prep %>% group_by(nest_type))

t_test_prep %>% group_by(nest_type) %>% summarise(meanTemp = mean(mean_Temp),
                                                  seTemp = standard_error(mean_Temp),
                                                  sdTemp = sd(mean_Temp))







### Look at all values between day 20 and 40 for t.test TSP difference between nest types
#Boxplot
ggplot(mean_TSP_per_ID,aes(nest_type,TEMP,fill=nest_type))+
  geom_boxplot(fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.70, outlier.shape = NaN)+
  stat_summary(fun.y = mean, geom="point", shape=20, size=5, color="black", fill="black") +
  scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))+
  xlab('NEST TYPE')+
  ylab('TSP TEMPERATURE [\u00B0C]')+
  geom_hline(yintercept = 35, colour = 'purple', size=1)+ # Refer to Ganes paper for optimal temperatures (thermal limits/ pivotal temp)
  geom_hline(yintercept = 25, colour = 'purple', size=1)+
  geom_hline(yintercept = 29.2, colour = 'blue',linetype=2, size=1)+
  theme_bw()+
  theme(strip.text = element_text(size=12),
        axis.text.x = element_text(size=14),
        axis.title.x = element_text(size=14,vjust = -0.5),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))











#Histogram
ggplot(mean_TSP_per_ID)+
  facet_wrap(~nest_type)+
  geom_histogram(aes(TEMP,fill=nest_type),alpha=0.60)+
  scale_fill_manual(values=c("black", "darkgrey"))

# test for normality
shapiro.test((mean_TSP_per_ID %>% filter(nest_type=='Hatchery'))$TEMP) # significant at p<0.05 meaning the data are  not normal
shapiro.test((mean_TSP_per_ID %>% filter(nest_type=='In-Situ'))$TEMP) # significant at p<0.05 meaning the data are  not normal
# Test for poisson distribution


# T.test
t.test(TEMP~nest_type,mean_TSP_per_ID %>% group_by(nest_type))
wilcox.test(TEMP~nest_type,mean_TSP_per_ID %>% group_by(nest_type))
kruskal.test(TEMP~nest_type,mean_TSP_per_ID %>% group_by(nest_type))

mean_TSP_per_ID %>% group_by(nest_type) %>% summarise(meanTemp = mean(TEMP),
                                                  seTemp = standard_error(TEMP))

# Yearly ------------------------------------------------------------------

tmp <- tmp %>% mutate(year = year(DATE))

yearly_data <- tmp %>% group_by(year,DAY) %>% summarise('TEMP' = mean(TEMP))

ggplot(yearly_data %>% filter(year==2019))+
  geom_point(aes(DAY,TEMP,colour=year),size=3)+
  geom_vline(xintercept = 20, colour = 'red')+ # Refer to Sea turtle appendix 
  geom_vline(xintercept = 40, colour = 'red')+
  scale_x_discrete(breaks = seq(0,80,10))


# Selected nests ----------------------------------------------------------
tmp <- tmp %>% ungroup()
# tmp %>% filter(ID==-c('IN-SITU_03','IN-SITU_04','IN-SITU_05','IN-SITU_06','IN-SITU_07','HATCH_03','HATCH_04','HATCH_05','HATCH_06','HATCH_07','HATCH_08'))
# tmp %>% filter(ID!=c('IN-SITU_01','IN-SITU_02','HATCH_01','HATCH_02','HATCH_09')) %>% view()









# Crabs -------------------------------------------------------------------


# Inter-annual variation in Crab activity and predation
crab <- ht_success %>% mutate(crabs = ifelse(Crab_activity == "YES",1,0))

lm_ <- lm(Emergence_success ~ Crab_activity, data=ht_success)
glm__ <- glm(crabs ~ Year, data=crab %>% filter(Treatment == 'IN SITU'),family = binomial)#(link = 'logit'))
glance(glm__)
summary(glm__)

plot(glm__)
anova(glm__,test = 'Chisq') #-- Year has a significant effect on crab activity --#
Anova(glm__,type = "II",test = "Wald") #type II ANOVA
exp(coef(glm__)) #odds ratios
confint_tidy(glm__) #confidence intervals



# prop.test()

crab %>% filter(Treatment=='IN SITU') %>% group_by(Crab_activity,Year) %>% summarise(counts = n())
crab %>%  filter(Treatment=='IN SITU') %>% group_by(Year) %>% summarise(counts = n())
Years <- c(9,39,29,32)
Totals <- c(24,54,34,37)
prop.test(Years,Totals)

