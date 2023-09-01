library(tidyverse)
library(binom)
library(lme4)
# setwd("C:/Users/s03an7/Documents/GitHub/precrastination/precrastinationPilot")

fixdat = readRDS(file="scratch/processedFixData.Rda")
dat  = aggregate(data=fixdat, xFixFlip ~ fixNum +targSideRel, FUN="mean")

#fixdat$subj = as.character(fixdat$subj)
cbPalette <- c("#56B4E9", "#E69F00")

# classify every fixation as homo (right), central, or hetro (left)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$fixX=fixdat$xFixFlip

fixdat$side[which(fixdat$fixX <(0-centralWidth/2))] = "hard"
fixdat$side[which(fixdat$fixX >(0+centralWidth/2))] = "easy"
fixdat$side = as.factor(fixdat$side)

aggData = (filter(fixdat, 
  side!="central", 
  fixNum<7, 
  fixNum>1, 
  targSideRel=="absent")
  %>% group_by(condition,subj,fixNum)
    %>% summarise(
     propHard=mean(side=="hard"), 
     nTrials=length(trialNum),
     lower = binom.confint(propHard*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHard*nTrials,nTrials, method='wilson')$upper)) 
 
accdat  = aggregate(data=aggData, propHard ~ subj + condition, FUN="mean")
write.csv(accdat, "ProportionsAggregated.txt", row.names=F)

levels(aggData$subj)<- 1:length(levels(aggData$subj))


(aggData %>% 
  group_by(subj) %>%
  summarise(meanProp = mean(propHard)) %>%
  arrange(desc(meanProp))) -> subj_order

aggData$subj <- fct_relevel(aggData$subj, levels = as.character(subj_order$subj))


plt = ggplot(aggData, aes(x=fixNum, y=propHard, ymin=lower, ymax=upper))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj, nrow=3)
plt = plt + scale_x_continuous(name="fixation number")#, breaks=c(2,3,4,5,6))
plt = plt + scale_y_continuous(name="proportion of fixations to hard side",breaks=c(0.1, 0.5, 1) )
#plt = plt + labs(color="condition")+scale_color_manual(labels = c("icons", "lines"),values = cbPalette ) 
ggsave("proportion.pdf", width=8, height=5)
ggsave("proportion.png", width=8, height=5)



