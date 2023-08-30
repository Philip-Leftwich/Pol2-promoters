# Packages ----

rm(list=ls()) ### clear memory

# if (!requireNamespace("devtools", quietly = TRUE)) 
#  install.packages("devtools")
# library(devtools)
# devtools::install_github("NightingaleHealth/ggforestplot")

library(tidyverse)
library(DHARMa)
library(scales)
library(ggbeeswarm)
library(emmeans)
library(grid)
library(png)
library(sjPlot) 





# ________________----

# Source files ----

source("scripts/custom_theme.R")
source("scripts/annotation_script.R")

# _______________----

# Read data----

pilotdata=read_csv("data/comb_Pol2.csv")

str(pilotdata) ### because read_csv characters not converted to factors

pilotdata[sapply(pilotdata, is.character)] <- lapply(pilotdata[sapply(pilotdata, is.character)], as.factor) ### change all characters to factors

pilotdata <- filter(pilotdata, !is.na(Value))

#__________________________-----

# Model data ----

lsmod <- glm(Value~Context*UTR*Cell_Line, data=pilotdata, family=Gamma(link="log"))



# ________________________----

# Figures ----


means <- emmeans::emmeans(lsmod,specs =~ Context*UTR*Cell_Line, type="response")


utr_relevel <- c("K10","SV40","P10")

context_relevel <- c("BmLo","Syn21","Kozak","BmHi","Lep")

cell_line_relevel <- c("Aag2..Ae..aegypti.","C6.36..Ae..albopictus.","U4.4..Ae..albopictus.","Hsu..Cu..quinquefasciatus.","Sf9..S..frugiperda.")

means2 <- means %>% as_tibble() %>% 
  mutate(Context=fct_relevel(Context, context_relevel)) %>% 
  mutate(UTR=fct_relevel(UTR, utr_relevel)) %>% 
  mutate(Cell_Line=fct_relevel(Cell_Line, cell_line_relevel)) 


pilotdataplot <- pilotdata %>% 
  add_row(ID=583, Promoter_ID=583, Promoter=" ", Cell_Line= "Aag2..Ae..aegypti.", Value=NA, Context="Kozak", UTR="P10", Cell_Line_Integer=1, Context_UTR="Kozak_P10") %>% 
  mutate(Context=fct_relevel(Context, context_relevel)) %>% 
  mutate(UTR=fct_relevel(UTR, utr_relevel))%>% 
  mutate(Cell_Line=fct_relevel(Cell_Line, cell_line_relevel)) 


supp.labs <-as_labeller(c(`Aag2..Ae..aegypti.`="A) Aag2 cells", 
                          `C6.36..Ae..albopictus.`= "B) C6.36 cells",
                          `Hsu..Cu..quinquefasciatus.`="D) Hsu cells",
                          `Sf9..S..frugiperda.`="F) Sf9 cells", 
                          `U4.4..Ae..albopictus.`="C) U4.4 cells"))

limits_fun <- function(x) {
  if (max(x) > 75) {
    c(1, 300)
  } else {
    if (max(x) < 10) {
      c(0.1, 10)
    }
    else{
    c(0.1, 30)
  }
  }
}

breaks_fun <- function(x) {
  if (max(x) > 75) {
    c(1, 10, 100, 300)
  } else {
    if (max(x) < 10) {
      c(0.1, 0.3, 1, 3)
    }
    else{
      c(0.1, 1, 3,30)
    }
  }
}


overall_prediction_pilot <- ggplot(means2, aes(x=Context, y=response, colour=UTR))+
  geom_beeswarm(data=pilotdataplot, aes(x=Context, y=Value, colour=UTR,group=UTR),shape=21, dodge.width=0.3, fill="white", alpha=0.3, size=0.6)+
  geom_point(aes(x=Context, y=response, color=UTR),position=position_dodge(width=0.3))+
  geom_linerange(aes(min=lower.CL, max=upper.CL),position=position_dodge(width=0.3))+
  facet_wrap(~ Cell_Line, scales="free", labeller=labeller(Cell_Line=supp.labs))+
  scale_y_log10(breaks = breaks_fun, limits = limits_fun) 

overall_prediction_pilot


a1 = annotation_custom2(rasterGrob(aegypti, interpolate=TRUE), ymin=log10(1), ymax=log10(5), xmin=3.5, xmax=5.5, data=means2[1,])
a2 = annotation_custom2(rasterGrob(albopictus, interpolate=TRUE), ymin=log10(1), ymax=log10(5), xmin=3.5, xmax=5.5,data=means2[16,])
a3 = annotation_custom2(rasterGrob(culex, interpolate=TRUE), ymin=log10(0.05), ymax=log10(0.5), xmin=3.5, xmax=5.5, data=means2[31,])
a4 = annotation_custom2(rasterGrob(frugiperda, interpolate=TRUE), ymin=log10(0.05), ymax=log10(0.5), xmin=3.5, xmax=5.5, data=means2[46,])
a5 = annotation_custom2(rasterGrob(albopictus, interpolate=TRUE), ymin=log10(1), ymax=log10(5), xmin=3.5, xmax=5.5, data=means2[61,])


overall_prediction_pilot+
  scale_color_manual(values=c("#3d6721", "#a86826", "#006c89"))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x="", y = "Firefly/Renilla Luciferase ratio", color="3' UTR")+
  theme_custom()+
  theme(legend.position=c(0.8,0.3))+
  a1+a2+a3+a4+a5


# ggsave("Figure1.png", type="cairo", width=7, height=9.5, units="in", dpi=600)

#_________________________-----


# Full model table---

tab_model(lsmod)

# ANOVA Tables ----


drop1(lsmod, test = "F")

lsmoda <- glm(Value~Context+UTR+Cell_Line+Context:Cell_Line+UTR:Cell_Line+Context:UTR, data=pilotdata, family=Gamma(link="log"))

drop1(lsmoda, test = "F")

lsmodb <- glm(Value~Context+UTR+Cell_Line, data=pilotdata, family=Gamma(link="log"))

drop1(lsmodb, test = "F")



# Contrasts -----



means1 <- emmeans::emmeans(lsmoda,specs = pairwise ~ Context, type="response") %>% 
  confint() 

means1$contrasts %>% 
  as_tibble()%>% 
  select(!c(df,SE)) %>% 
  mutate(lower.CL = round(lower.CL, digits = 2)) %>% 
  mutate(upper.CL = round(upper.CL, digits = 2)) %>% 
  mutate(ratio = round(ratio, digits = 2)) %>% 
  mutate(`±95% CI` = str_c(lower.CL, "-", upper.CL)) %>% 
  select(!c(lower.CL, upper.CL))  %>% 
  rename(`Contrast` = contrast, `Mean Relative Expression` = ratio) %>% 
  flextable() %>% 
  print(., preview = "docx")
  


means2 <- emmeans::emmeans(lsmoda,specs = pairwise ~ UTR, type="response") %>% 
  confint() 

means2$contrasts %>% 
  as_tibble()%>% 
  select(!c(df,SE)) %>% 
  mutate(lower.CL = round(lower.CL, digits = 2)) %>% 
  mutate(upper.CL = round(upper.CL, digits = 2)) %>% 
  mutate(ratio = round(ratio, digits = 2)) %>% 
  mutate(`±95% CI` = str_c(lower.CL, "-", upper.CL)) %>% 
  select(!c(lower.CL, upper.CL))  %>% 
  rename(`Contrast` = contrast, `Mean Relative Expression` = ratio) %>% 
  flextable() %>% 
  print(., preview = "docx")
