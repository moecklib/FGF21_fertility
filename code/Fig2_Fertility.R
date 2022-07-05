#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#*#ANALYSIS FGF21 Fertility Fig2 Analysis of Fertility
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Analysis of histology data of reproductive organs. Quantification
#July 2022, BEAT MOECKLI

#loading necessary packages
lapply(c("tidyverse", "RColorBrewer", "data.table", "colorspace", "readxl", 
         "ggpubr", "ggbeeswarm"), require, character.only = TRUE)

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Raw results import and annotation####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Import of raw result file
FGF21_Fert<-read.csv("data/FGF21_Evaluation.csv")%>%
  mutate(Group=factor(x=Group, levels = c("ND", "HFD", "FGF21")))

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Define colors & source functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colors, used RColorBrewer, display.brewer.pal(12, "Paired")
mycolors_fill<-c("#FFFFFF", RColorBrewer::brewer.pal(8, "Paired")[c(1,5)])
mycolors_stroke<-c("#000000", RColorBrewer::brewer.pal(8, "Paired")[c(2,6)])

#Source the graph functions
source("code/Functions.R")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Plots for figure 2 regarding fertility####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

colnames(FGF21_Fert)

FGF21_Fert %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Nb_pregnancy))+
  boxplot_FGF21(nudge = 0.2)+
  labs(y= "Pregnancies", x=NULL)
save_plot("NbPregnancies")

FGF21_Fert %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Nb_offspring))+
  boxplot_FGF21(nudge = 2)+
  labs(y= "Offspring", x=NULL)
save_plot("NbOffspring")

FGF21_Fert %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Nb_offspring_alive))+
  boxplot_FGF21(nudge = 1.8)+
  labs(y= "Offspring alive", x=NULL)
save_plot("NbOffspringAlive")

#Small script to obtain median per group for the writing of the result section.
FGF21_Fert%>% group_by(Group)%>%
  summarise(median_NbOff=median(Nb_offspring),
            median_NbOffAlive=median(Nb_offspring_alive))

            