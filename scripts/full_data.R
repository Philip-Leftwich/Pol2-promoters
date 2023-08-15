# Packages ----

rm(list=ls()) ### clear memory

library(tidyverse)
library(scales)
library(emmeans)
library(grid)
library(png)
library(sjPlot) 
library(ggh4x)

# ________________----

# Source files ----


source("scripts/custom_theme.R")


# _______________----

# Read data----

Aag2 <- read.csv("data/Aag2_Pol2.csv", na.strings="")

Aag2 <- Aag2 %>% mutate(Cell_Line= "Aag2")
U4 <- read.csv("data/U4_Pol2.csv", na.strings="")
U4 <- U4 %>% mutate(Cell_Line="U4")

Full_data <- full_join(Aag2, U4) %>% 
  filter(!is.na(Value))


norsp <- Full_data %>% filter(Promoter %in% c("Hr5", 
                                              "AePUb", 
                                              "400AePub",   
                                              "800AePUb",
                                              "AlbPub",
                                              "Hsp83",      
                                              "shortHsp83", 
                                              "OpIE2", 
                                              "CqPUb")) %>% 
  droplevels() %>% 
  mutate(Context=factor(Context, levels=c("Bom_K10","Kozak_SV40","Lep_P10"))) %>% 
  mutate(Promoter = factor(Promoter, levels = c("Hr5", "OpIE2", "shortHsp83", "Hsp83", "CqPUb", "400AePub", "800AePUb", "AePUb", "AlbPub"))) %>% 
  mutate(Promoter = fct_recode(Promoter, "400AePUb" = "400AePub",
                               "AlbPUb" = "AlbPub"))

#________________________________-----

# Model data ----
mod2<-glm(Value~Promoter*Context*Rep*Cell_Line,family=Gamma(link="log"), 
           data=norsp[-c(530,532, 534, 535,1113,1132),])

#__________________________________-----

# Figures ----

means <- emmeans::emmeans(mod2,specs = pairwise ~ Promoter*Context*Cell_Line, type="response")

means2 <- means$emmeans %>% as_tibble()



options(scipen=999)


pos <- position_dodge(width=0)



supp.labs.2 <-as_labeller(c(Aag2="A) Aag2 cells", 
                            U4= "B) U4.4 cells"))



means3 <- means2 %>% mutate(type = case_when(str_detect(Promoter, "AePUb") ~ "AePUb",
                                             str_detect(Promoter, "Hsp") ~ "AeHsp83",
                                             Promoter == "Hr5" ~ "Hr5IE1",
                                             TRUE ~ as.character(Promoter)), .before = Promoter) %>% 
  mutate(type = factor(type, levels = c("Hr5IE1", "OpIE2", "AeHsp83", "AePUb", "CqPUb", "AlbPUb")))

norsp <- norsp %>% mutate(type = case_when(str_detect(Promoter, "AePUb") ~ "AePUb",
                                          str_detect(Promoter, "Hsp") ~ "AeHsp83",
                                          Promoter == "Hr5" ~ "Hr5IE1",
                                          TRUE ~ as.character(Promoter)), .before = Promoter) %>% 
  mutate(type = factor(type, levels = c("Hr5IE1", "OpIE2", "AeHsp83", "AePUb", "CqPUb", "AlbPUb")))



design <- "
  ABCCDDDEF
  GHIIJJJK#
"

means3 %>%
  group_by(Promoter, Cell_Line) %>% 
  mutate(width=0.2*n()) %>% ### work around to prevent error bars from being too wide for single value AlbPub
  ggplot(aes(x=Promoter, y=response, colour=Context))+
  geom_beeswarm(data=norsp, aes(x=Promoter, 
                                y=Value, 
                                colour=Context, 
                                group=Context, 
                                shape=as.character(Rep)), 
                dodge=0, fill="white", alpha=0.3, size=0.6)+

  geom_line(aes(y = response, group = Context), alpha=0.4)+

  geom_errorbar(aes(min=lower.CL, max=upper.CL),width=0, position=pos,  size=0.6)+
  geom_point(aes(x=Promoter, y=response, colour=Context, fill=Context), position=pos)+
  scale_shape_manual(labels=c("Replicate 1", "Replicate 2", "Replicate 3"), values=c(21,22,25))+
  scale_color_manual(values=c("#3d6721", "#a86826", "#006c89"))+
  facet_manual(~ Cell_Line + type, 
               scales="free_x", 
               labeller=labeller(Cell_Line=supp.labs.2), 
               strip = strip_nested(bleed = TRUE), 
               design = design)+
  scale_y_continuous(trans='log10', 
                     expand = expansion(mult = c(0.1, 0)), 
                     limits=c(0.001,150), 
                     labels=scales::number_format(accuracy = 1))+
  scale_x_discrete(labels = c("Hr5" = "1200bp",
                   "OpIE2" = "540bp",
                   "shortHsp83" = "875bp",
                   "Hsp83" = "1400bp",
                   "CqPUb" = "1500bp",
                   "400AePUb" = "400bp",
                   "800AePUb" = "800bp",
                   "AePUb" = "1500bp",
                   "AlbPUb" = "1500bp"))+
  labs(colour= "TIS 3'UTR",
       fill= "TIS 3'UTR",
       shape="",
       y="Firefly/Renilla Luciferase ratio",
       x="")+
  theme_custom()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        strip.placement = "outside",
        axis.line.x.top = element_line(colour = "black"),
        strip.text = element_text(hjust = 0)
        )+
  guides(shape=guide_legend(override.aes=list(size=1)))+
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, size = 1)


ggsave("Figure2.png", type="cairo", width=7, height=9.5, units="in", dpi=600)

#__________________________________-----

# ANOVA ----



mod3<-glm(Value~Promoter*Context,family=Gamma(link="log"), 
          data=norsp[-c(530,532, 534, 535,1113,1132),])

drop1(mod3, test = "F")

mod4<-glm(Value~Promoter+Context,family=Gamma(link="log"), 
          data=norsp[-c(530,532, 534, 535,1113,1132),])

drop1(mod4, test = "F")