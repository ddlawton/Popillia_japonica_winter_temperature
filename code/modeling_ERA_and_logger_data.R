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

dat <- read.csv("data/processed/ERA_logger_data_validation_site_comparison_Feb232022.csv") %>% as_tibble() %>%
  drop_na(ERA_temps) %>% mutate(depth = factor(depth))


ggplot(dat,aes(x=temp,y=ERA_temps,color=depth)) + geom_point(size=.75,alpha=.75,pch=21) + geom_smooth(se=FALSE,size=1.5) + #theres a weird 200+ point in the data. Lets remove it.
  facet_wrap(~depth)

dat <- dat %>% filter(temp <100)



raw_data_graph <- ggplot(dat,aes(y=temp,x=ERA_temps,color=depth)) + geom_point(size=.75,alpha=.75,pch=21) + geom_smooth(method="gam",se=FALSE,size=1.5) + #much better!
  facet_grid(depth~location) + 
  ylab("Logger Temperature Data (C)") +
  xlab("ERA Temperature Data (C)")


ggsave(raw_data_graph,file="output/raw_data_graph.png",width=20,height=20,units="in",dpi=600)

rural_dat <- dat %>% filter(location == "rural")
urban_dat <- dat %>% filter(location == "urban")


mod_rural <- bam(temp ~ te(ERA_temps,depth,bs="fs",k=20),select=TRUE,data=rural_dat,family=scat())
mod_urban <- bam(temp ~ te(ERA_temps,depth,bs="fs",k=20),select=TRUE,data=urban_dat,family=scat())

mod_rural_lm <- bam(temp ~ ERA_temps*depth,select=TRUE,data=rural_dat)
mod_urban_lm <- bam(temp ~ ERA_temps*depth,select=TRUE,data=urabn_dat)

# Both AIC and BIC selected for the non-linear model
AIC(mod_rural,mod_rural_lm) # GAM
AIC(mod_urban,mod_urban_lm) # GAM
BIC(mod_rural,mod_rural_lm) # GAM
BIC(mod_urban,mod_urban_lm) # GAM


summary(mod_rural)
sim_resid <- simulateResiduals(mod_rural)
plot(sim_resid)


summary(mod_urban)
sim_resid <- simulateResiduals(mod_urban) #its not as pretty as the rural model, but will do.
plot(sim_resid)

# Rural model figures

b0 <- coef(mod_rural)[1]

test <- gratia::smooth_estimates(mod_rural)

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
  ggplot(aes(x=ERA_temps,group=depth)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=depth),alpha=.1) +
  geom_line(aes(y = adj_est, color=depth),size=1.5) + theme_pubr() +
  scale_color_manual(values = c('#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')) +
  scale_fill_manual(values = c('#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')) + 
  xlab("ERA climatic reanalysis temperature (C)") +
  ylab("Modeled soil logger temperature (C)") +
  ylim(0,30) +
  xlim(0,30) +
  facet_wrap(~depth)

ggsave(plotted_together,file="output/lines_together_rural.png",width=10,height=10,units="in",dpi=600)
ggsave(grid,file="output/gridded_rural.png",width=10,height=10,units="in",dpi=600)

# Urban model figures

b0 <- coef(mod_urban)[1]

test <- gratia::smooth_estimates(mod_urban)

test$adj_est <- test$est + b0


plotted_together <- test %>% filter(smooth == "te(ERA_temps,depth)") %>%
  ggplot(aes(x=ERA_temps,group=depth)) + #geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=depth),alpha=.1) +
  geom_line(aes(y = adj_est, color=depth),size=1.5) + theme_pubr() +
  scale_color_manual(values = c('#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04')) +
  scale_fill_manual(values = c('#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04')) + 
  xlab("ERA climatic reanalysis temperature (C)") +
  ylab("Modeled soil logger temperature (C)") +
  ylim(0,30) +
  xlim(0,30)

grid <- test %>% filter(smooth == "te(ERA_temps,depth)") %>%
  ggplot(aes(x=ERA_temps,group=depth)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=depth),alpha=.1) +
  geom_line(aes(y = adj_est, color=depth),size=1.5) + theme_pubr() +
  scale_color_manual(values = c('#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04')) +
  scale_fill_manual(values = c('#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04')) + 
  xlab("ERA climatic reanalysis temperature (C)") +
  ylab("Modeled soil logger temperature (C)") +
  ylim(0,30) +
  xlim(0,30) +
  facet_wrap(~depth)

ggsave(plotted_together,file="output/lines_together_urban.png",width=10,height=10,units="in",dpi=600)
ggsave(grid,file="output/gridded_urban.png",width=10,height=10,units="in",dpi=600)