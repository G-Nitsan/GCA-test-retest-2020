library(lme4)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(plyr)

################################################################################################
#YOUNG DATA
data.try <- read.csv("C:/Users/galni/Google Drive/Gal_Limudim/CAN lab/Test retest Shai/preperaing the data/young/TRY_cleaned_primary_08-18-2020_forGCA_corrected.csv")
names(data.try)[names(data.try) == 'ï..Subject'] <- 'Subject'
names(data.try)[names(data.try) == 'BIN_START_TIME'] <- 'Time'
names(data.try)[names(data.try) == 'pre1_post2'] <- 'Pre1_Post2'


#Exclude participants:
data.try.24s <-data.try %>% filter(Subject %in% c(2,3,6,8,9,11,12,13,14,15,16,17,18,19,22,23,24,27,28,29,30,31,32,33))
unique(data.try.24s[c("Subject")])
length(unique(data.try.24s[["Subject"]]))


#plot raw data 
ggplot(data.try.ex, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line") + 
  facet_grid(load~condition~Subject, labeller=quick_labeller)

#create polynomials:
adjust_data <- function (.data, .timeInterval = 20, .degree = 3) {
  .data$TimeBin = 1 + (.data$Time - min(.data$Time)) / .timeInterval
  summary(.data$TimeBin)
  t <- poly(unique(.data$TimeBin), .degree)
  summary(t)
  .data[,paste("ot", 1:.degree, sep="")] <- t[.data$TimeBin, 1:.degree]
  
  return(.data)
}


data.gca.try <- adjust_data(data.try.24s, .degree = 3)
#write.csv( data.gca, paste(res_dir,exp_name,".gca.csv",sep='') )
summary(data.gca.try)



quick_labeller <- labeller(
  condition = as_labeller(c("c" = "Onset", "r" = "Offset")),
  load = as_labeller(c(`1` = "Low WM load", `4` = "High WM load")),
  Pre1_Post2  = as_labeller(c(`1` = "Pre test", `2` = "Post test"))
)


#aggragating data
agg.data.gca.try<-aggregate(.~Subject+condition+load+Time+Pre1_Post2, data.gca.try.filterd, mean)
agg.data.gca.try.cohort = agg.data.gca.try %>% filter(condition =='c')
agg.data.gca.try.rhyme = agg.data.gca.try %>% filter(condition =='r')
summary(agg.data.gca.try.cohort)
agg.data.gca.try.cohort$Pre1_Post2 <- as.factor(agg.data.gca.try.cohort$Pre1_Post2)
agg.data.gca.try.cohort$load <- as.factor(agg.data.gca.try.cohort$load)
agg.data.gca.try.cohort$Subject <- as.factor(agg.data.gca.try.cohort$Subject)

agg.data.gca.try.rhyme$Pre1_Post2 <- as.factor(agg.data.gca.try.rhyme$Pre1_Post2)
agg.data.gca.try.rhyme$load <- as.factor(agg.data.gca.try.rhyme$load)
agg.data.gca.try.rhyme$Subject <- as.factor(agg.data.gca.try.rhyme$Subject)

#filter time
agg.data.gca.try = agg.data.gca.try %>% filter(Time > 200 & Time < 1500) 

###########################MODELING COHORT AND RHYME SEPERATELY################
#24subjects COHORT
gca.model1.C24 <- lmer(Target ~ (ot1 + ot2 + ot3)*(Pre1_Post2*load) + 
                         (ot1 + ot2 + ot3| Subject),
                       data=agg.data.gca.try.cohort, REML=F)
summary (gca.model1.C24)
tab_model(gca.model1.C24)

#Plot data + model fit
data.comp <- data.frame(agg.data.gca.try.cohort, GCA_Full=fitted(gca.model1.C24))


#seting the y axis values
ggplot(data.comp, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line")+ coord_cartesian(ylim = c(0, 0.8))+                      # col?
  stat_summary(aes(y=GCA_Full, linetype=factor(Pre1_Post2)), fun.y=mean, geom="line") + # nterpulation line
  facet_grid(load ~ condition, labeller=quick_labeller)


#24subjects RHYME
gca.model1.R24 <- lmer(Target ~ (ot1 + ot2 + ot3)*(Pre1_Post2*load) + 
                         (ot1 + ot2 + ot3| Subject),
                       data=agg.data.gca.try.rhyme, REML=F)
summary (gca.model1.R24)
tab_model(gca.model1.R24)

#Plot data + model fit
data.comp <- data.frame(agg.data.gca.try.rhyme, GCA_Full=fitted(gca.model1.R24))


#seting the y axis values
ggplot(data.comp, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line")+ coord_cartesian(ylim = c(0, 0.8))+                      # col?
  stat_summary(aes(y=GCA_Full, linetype=factor(Pre1_Post2)), fun.y=mean, geom="line") + # nterpulation line
  facet_grid(load ~ condition, labeller=quick_labeller)




################################################################################################
########################################OLD DATA################################################
################################################################################################
data.tro <- read.csv("C:/Users/galni/Google Drive/Gal_Limudim/CAN lab/Test retest Shai/preperaing the data/old/TRO_with_span_cleaned_primary_14_9_2020_forGCA_corrected.csv")
names(data.tro)[names(data.tro) == 'ï..RECORDING_SESSION_LABEL'] <- 'Subject'
names(data.tro)[names(data.tro) == 'BIN_START_TIME'] <- 'Time'
names(data.tro)[names(data.tro) == 'pre1_post2'] <- 'Pre1_Post2'
names(data.tro)[names(data.tro) == 'AVERAGE_IA_1_SAMPLE_COUNT_.'] <- 'Target'



#create polynomials:
adjust_data <- function (.data, .timeInterval = 20, .degree = 3) {
  .data$TimeBin = 1 + (.data$Time - min(.data$Time)) / .timeInterval
  summary(.data$TimeBin)
  t <- poly(unique(.data$TimeBin), .degree)
  summary(t)
  .data[,paste("ot", 1:.degree, sep="")] <- t[.data$TimeBin, 1:.degree]
  
  return(.data)
}


data.gca.tro <- adjust_data(data.tro, .degree = 3)
#write.csv( data.gca, paste(res_dir,exp_name,".gca.csv",sep='') )
summary(data.gca.tro)


quick_labeller <- labeller(
  condition = as_labeller(c("c" = "Onset", "r" = "Offset")),
  load = as_labeller(c(`1` = "Low WM load", `4` = "High WM load")),
  Pre1_Post2  = as_labeller(c(`1` = "Pre test", `2` = "Post test"))
)


#plot raw data
ggplot(data.gca.tro, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line") + 
  facet_grid(load ~ condition, labeller=quick_labeller)



#aggragating data
agg.data.gca.tro<-aggregate(.~Subject+condition+load+Time+Pre1_Post2, data.gca.tro, mean)
agg.data.gca.tro$Pre1_Post2 <- as.factor(agg.data.gca.tro$Pre1_Post2)
agg.data.gca.tro$load <- as.factor(agg.data.gca.tro$load)
agg.data.gca.tro$Subject <- as.factor(agg.data.gca.tro$Subject)
summary(agg.data.gca.tro)
#filter time
agg.data.gca.tro = agg.data.gca.tro %>% filter(Time > 200 & Time < 1500) 

agg.data.gca.tro.cohort = agg.data.gca.tro %>% filter(condition =='c')
agg.data.gca.tro.rhyme = agg.data.gca.tro %>% filter(condition =='r')
summary(agg.data.gca.tro.cohort)
summary(agg.data.gca.tro.rhyme)

###########################MODELING COHORT AND RHYME SEPERATELY################
#COHORT
gca.model2.TRO.C <- lmer(Target ~ (ot1 + ot2 + ot3)*(Pre1_Post2*load) + 
                           (ot1 + ot2 | Subject),
                         data=agg.data.gca.tro.cohort, REML=F)
summary (gca.model2.TRO.C)
tab_model(gca.model2.TRO.C)

#Plot data + model fit
data.comp <- data.frame(agg.data.gca.tro.cohort, GCA_Full=fitted(gca.model2.TRO.C))


#seting the y axis values
ggplot(data.comp, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line")+ coord_cartesian(ylim = c(0, 0.8))+                      # col?
  stat_summary(aes(y=GCA_Full, linetype=factor(Pre1_Post2)), fun.y=mean, geom="line") + # nterpulation line
  facet_grid(load ~ condition, labeller=quick_labeller)

#ggsave("gca.model2.TRO.C.tiff", units="in", width=11, height=9, dpi=300, compression = 'lzw')


#RHYME
gca.model2.TRO.R <- lmer(Target ~ (ot1 + ot2 + ot3)*(Pre1_Post2*load) + 
                           (ot1 + ot2 | Subject),
                         data=agg.data.gca.tro.rhyme, REML=F)
summary (gca.model2.TRO.R)
tab_model(gca.model2.TRO.R)
#Plot data + model fit
data.comp <- data.frame(agg.data.gca.tro.rhyme, GCA_Full=fitted(gca.model2.TRO.R))


#seting the y axis values
ggplot(data.comp, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line")+ coord_cartesian(ylim = c(0, 0.8))+                      # col?
  stat_summary(aes(y=GCA_Full, linetype=factor(Pre1_Post2)), fun.y=mean, geom="line") + # nterpulation line
  facet_grid(load ~ condition, labeller=quick_labeller)

#ggsave("gca.model2.TRO.R.tiff", units="in", width=11, height=9, dpi=300, compression = 'lzw')

