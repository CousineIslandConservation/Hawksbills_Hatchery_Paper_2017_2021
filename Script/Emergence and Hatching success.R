# Load libraries  ----------------------------------------------------------
library(car)
library(lme4) 
library(purrr)
library(broom)
library(move)
library(fields)
library(lattice)
library(effects)
library(Matrix)
library(tidyverse)
library(ggplot2)
library(pbapply)
library(wrapr)
library(stringr)
library(lubridate)
library(magrittr)
library(dplyr)
library(rgl)
library(stringi)
library("readxl")
library(AICcmodavg)
library(gridExtra)
library('chron')

# Set working directory
setwd('C:/Users/../Data')

# Load temperature data and metadata
Temp_file = read_excel('Temperature Logs Concise.xlsx',sheet = "Sheet1")
Temp_metadata_file = read_excel('Temperature Logs Concise.xlsx',sheet = "Sheet2")

# Load hawksbill nesting success data
ht_success = readxl::read_xlsx('HB nest success.xlsx')

# Temp_file = readxl::read_xlsx('Temperature Logs Concise.xlsx')
# readxl_example('Temperature Logs Concise.xlsx')
# read_excel('Temperature Logs Concise.xlsx',sheet = "Sheet2")
# excel_sheets('Temperature Logs Concise.xlsx',sheets='Sheet2')

# Setting up dataframes ---------------------------------------------------
Temp_metadata_file<-t(Temp_metadata_file)
colnames(Temp_metadata_file)<-Temp_metadata_file[1,]
Temp_metadata_file<-Temp_metadata_file[-1,]
Temp_metadata_file <- data.frame(Temp_metadata_file)
Temp_metadata_file['ID']<-rownames(Temp_metadata_file)
rownames(Temp_metadata_file)<- seq(1,NROW(Temp_metadata_file))
Temp_file$ID <- as.factor(Temp_file$ID)
Temp_file$hr <- strftime(Temp_file$TIME,'%H:%M',tz = 'UTC')
ht_success <- ht_success %>% filter(Exclude=='NO')
###########################################################################

# Emergence and hatching success analysis ---------------------------------
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

# Plotting ----------------------------------------------------------------
### Simple Boxplots of hatching and emergence success and clutch size per treatment

p1<-ggplot(ht_success)+
  geom_boxplot(aes(x=Treatment,y=Hatching_success),fill=c("#1D1D1D", "#C5C5C5C5"),alpha=0.7)
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
  yearly_means_per_Treatment <- yearly_means_per_Treatment %>% mutate(ypos = c(-0.2,0.2,-0.2,0.2,-0.2,0.2,-0.2,0.2))
  yearly_means_per_Treatment <- yearly_means_per_Treatment %>% mutate(ymin = c(-0.355,0.02,-0.355 ,0.02,-0.355,0.02,-0.355,0.02),ymax = c(-0.025,0.35,-0.025,0.35,-0.025,0.35,-0.025,0.35))
  
  #boxplot with means not medians
  ggplot(ht_success,aes(x=Treatment,y=!!success, fill=Treatment))+
    scale_x_discrete(labels = c('HATCHERY' = 'Hatchery','IN SITU' = 'In situ'))+
    facet_wrap(~Year)+
    geom_boxplot(alpha=0.70, outlier.shape = NaN)+ #fatten=NULL,
    stat_summary(fun.y = mean, geom="point", shape=20, size=3, color="black", fill="black") +
    scale_fill_manual(values=c("#1D1D1D", "#C5C5C5C5", "#888888"))+
    xlab('Treatment')+
    ylab(ylabel_success)+
    theme_bw()+
    theme(
          # text=element_text(face = 'plain', family="Times"),
          strip.text = element_text(face = 'plain', size=fontsize,family="Times"),
          axis.text.y = element_text(face = 'plain', size=fontsize,family = "Times"),
          axis.title.y = element_text(vjust = 2, face = 'plain', size=fontsize,family="Times"),
          legend.position = 'none',
          axis.text.x = element_text(face = 'plain', size=fontsize,family="Times"),
          axis.title.x = element_text(face = 'plain', size=fontsize,vjust = -0.5,family="Times"),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}  

plot_Year.Treatment(Emergence_success,'Emergence success [%]')
plot_Year.Treatment(Hatching_success,'Hatching success [%]')

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
##t.tests between Hatchery and In situ
tmp <- ht_success %>% filter(Treatment %in% c('HATCHERY','IN SITU'))

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

# Emergence success -------------------------------------------------------
# GLM fits --------------------------------------------------------------
glm__T <- glm(Emergence_success ~ Treatment, data=tmp)
glm__TY <- glm(Emergence_success ~ Treatment+Year, data=tmp)
glm__TYC <- glm(Emergence_success ~ Treatment+Year+Clutch_size, data=tmp)
# Compare AIC for best model fit
model.set <- list(glm__T, glm__TY, glm__TYC)
model.names <- c('glm__T', 'glm__TY', 'glm__TYC')
aictab(model.set, modnames = model.names)

## Tidy tables of results
augment(glm__TYC)
tidy(glm__TYC)
glance(glm__TYC)
plot(effect('Year',glm__TYC))
plot(effect('Treatment',glm__TYC))
plot(effect('Clutch_size',glm__TYC))

# Hatching success --------------------------------------------------------
# GLM fits --------------------------------------------------------------
glm__T <- glm(Hatching_success ~ Treatment, data=tmp)
glm__TY <- glm(Hatching_success ~ Treatment+Year, data=tmp)
glm__TYC <- glm(Hatching_success ~ Treatment+Year+Clutch_size, data=tmp)

model.set <- list(glm__T, glm__TY, glm__TYC)
model.names <- c('glm__T', 'glm__TY', 'glm__TYC')

aictab(model.set, modnames = model.names)

## Tidy tables of results
augment(glm__TY)
tidy(glm__TY)
glance(glm__TY)
## Plots of results
plot(effect('Year',glm__TYC))
plot(effect('Treatment',glm__TYC))
plot(effect('Clutch_size',glm__TYC))











###########################################################################

# Temperature data analysis -----------------------------------------------

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
  scale_fill_manual(values=c("black", "darkgrey", 'grey'))+
  ylab('Temperature (\u00B0C)')+
  xlab('Days')+
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

# T.test
t.test(TEMP~nest_type,mean_TSP_per_ID %>% group_by(nest_type))
wilcox.test(TEMP~nest_type,mean_TSP_per_ID %>% group_by(nest_type))
kruskal.test(TEMP~nest_type,mean_TSP_per_ID %>% group_by(nest_type))

mean_TSP_per_ID %>% group_by(nest_type) %>% summarise(meanTemp = mean(TEMP),
                                                  seTemp = standard_error(TEMP))

# Crabs -------------------------------------------------------------------
# Inter-annual variation in Crab activity and predation
crab <- ht_success %>% mutate(crabs = ifelse(Crab_activity == "YES",1,0))
glm__ <- glm(crabs ~ Year, data=crab %>% filter(Treatment == 'IN SITU'),family = binomial)#(link = 'logit'))
glance(glm__)
summary(glm__)

plot(glm__)
anova(glm__,test = 'Chisq') #-- Year has a significant effect on crab activity --#
Anova(glm__,type = "II",test = "Wald") #type II ANOVA
exp(coef(glm__)) #odds ratios
confint_tidy(glm__) #confidence intervals




