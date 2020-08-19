
data.try <- read.csv("C:/Users/galni/Google Drive/Gal_Limudim/CAN lab/Test retest Shai/preperaing the data/young/TRY_cleaned_primary_08-18-2020_forGCA.csv")
names(data.try)[names(data.try) == 'ï..Subject'] <- 'Subject'
names(data.try)[names(data.try) == 'BIN_START_TIME'] <- 'Time'
names(data.try)[names(data.try) == 'pre1_post2'] <- 'Pre1_Post2'

library(lme4)
library(ggplot2)
library(dplyr)
library(sjPlot)


adjust_data <- function (.data, .timeInterval = 20, .degree = 3) {
  .data$TimeBin = 1 + (.data$Time - min(.data$Time)) / .timeInterval
  summary(.data$TimeBin)
  t <- poly(unique(.data$TimeBin), .degree)
  summary(t)
  .data[,paste("ot", 1:.degree, sep="")] <- t[.data$TimeBin, 1:.degree]
  
  return(.data)
}


data.gca.try <- adjust_data(data.try, .degree = 3)
#write.csv( data.gca, paste(res_dir,exp_name,".gca.csv",sep='') )
summary(data.gca.try)



quick_labeller <- labeller(
  condition = as_labeller(c("c" = "Onset", "r" = "Offset")),
  load = as_labeller(c(`1` = "Low WM load", `4` = "High WM load")),
  Pre1_Post2  = as_labeller(c(`1` = "Pre test", `2` = "Post test"))
)

#filter time
data.gca.try = data.gca.try %>% filter(Time > 200 & Time < 1700) 
#plot raw data
ggplot(data.gca.try, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line") + 
  facet_grid(load ~ condition, labeller=quick_labeller)

p<-ggplot(data.gca.Y, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line") + 
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=Target), alpha=0.3) +
  theme_bw() +                     
  facet_grid(load ~ condition, labeller=quick_labeller)

#with error bars
ggplot(data.gca.try, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line") + 
  stat_summary(fun.data=mean_se, geom="errorbar")+
  facet_grid(load ~ condition, labeller=quick_labeller)

ggsave("raw data pre post.tiff", units="in", width=14, height=9, dpi=300, compression = 'lzw')

#aggragating data
agg.data.gca.try<-aggregate(.~Subject+condition+load+Time, data.gca.try, mean)


#####################FINAL MODEL#########################
gca.model1 <- lmer(Target ~ (ot1 + ot2 + ot3)*(Pre1_Post2*load*condition) + 
                     (ot1 + ot2 + ot3| Subject),
                   data=agg.data.gca.try, REML=F)
summary (gca.model1)
tab_model(gca.model1)
coefs <- data.frame(coef(summary(gca.model1)))
coefs$p <- 2 * (1 - pnorm(abs(coefs$t.value)))
write.table(coefs)


#Plot data + model fit
data.comp <- data.frame(agg.data.gca.try, GCA_Full=fitted(gca.model1))

ggplot(data.comp, aes(x=Time, y=Target, color=factor(Pre1_Post2))) +
  stat_summary(fun.data=mean_se, geom="line") +                       # col?
  stat_summary(aes(y=GCA_Full, linetype=factor(Pre1_Post2)), fun.y=mean, geom="line") + # nterpulation line
  facet_grid(load ~ condition, labeller=quick_labeller)

#ggsave("GCA.tiff", units="in", width=14, height=9, dpi=300, compression = 'lzw')

