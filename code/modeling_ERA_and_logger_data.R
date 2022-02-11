###
#  Modeling
#   Soil temperature loggers
#    and remotely
#     sensed data
###
rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(ggpubr)
library(mgcv)
library(gratia)
library(DHARMa)

dat <- read.csv("data/processed/ERA_logger_data_validation.csv") %>% as_tibble() %>%
  drop_na(ERA_temps) %>% mutate(depth = factor(depth))


ggplot(dat,aes(x=temps,y=ERA_temps,color=depth)) + geom_point(size=.75,alpha=.75,pch=21) + geom_smooth(se=FALSE,size=1.5) +
  facet_wrap(~depth)

mod <- bam(temps ~ te(ERA_temps,depth,bs="fs",k=20),select=TRUE,data=dat)

mod_lm <- bam(temps ~ ERA_temps*depth,select=TRUE,data=dat)

# Both AIC and BIC selected for the non-linear model
AIC(mod,mod_lm)
BIC(mod,mod_lm)



summary(mod)
sim_resid <- simulateResiduals(mod)
plot(sim_resid)

b0 <- coef(mod)[1]

test <- gratia::smooth_estimates(mod)

test$adj_est <- test$est + b0

plotted_together <- test %>% filter(smooth == "te(ERA_temps,depth)") %>%
  ggplot(aes(x=ERA_temps,group=depth)) + #geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=depth),alpha=.1) +
  geom_line(aes(y = adj_est, color=depth),size=1.5) + theme_pubr() +
  scale_color_manual(values = c('#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')) +
  scale_fill_manual(values = c('#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')) + 
  xlab("ERA climatic reanalysis temperature (C)") +
  ylab("Modeled soil logger temperature (C)") +
  ylim(0,30) +
  xlim(0,30)

grid <- test %>% filter(smooth == "te(ERA_temps,depth)") %>%
  ggplot(aes(x=ERA_temps,group=depth)) + #geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=depth),alpha=.1) +
  geom_line(aes(y = adj_est, color=depth),size=1.5) + theme_pubr() +
  scale_color_manual(values = c('#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')) +
  scale_fill_manual(values = c('#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')) + 
  xlab("ERA climatic reanalysis temperature (C)") +
  ylab("Modeled soil logger temperature (C)") +
  ylim(0,30) +
  xlim(0,30) +
  facet_wrap(~depth)

ggsave(plotted_together,file="output/lines_together.png",width=10,height=10,units="in",dpi=600)
ggsave(grid,file="output/gridded.png",width=10,height=10,units="in",dpi=600)
