#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#*#ANALYSIS FGF21 Fertility Fig2 Analysis of Fertility
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Analysis of histology data of reproductive organs. Quantification
#July 2022, BEAT MOECKLI

#loading necessary packages
lapply(c("tidyverse", "RColorBrewer", "data.table", "colorspace", "readxl", 
         "ggpubr", "ggbeeswarm", "GGally"), require, character.only = TRUE)

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

#Number of pregnancies with Boxplot
FGF21_Fert %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Nb_Preg_perAnimal))+
  boxplot_FGF21(nudge = 0.2)+
  labs(y= "Pregnancies", x=NULL)
save_plot("NbPregnancies_test3")

#Create Bar Plot to illustrate the different proportions
FGF21_Fert%>%filter(!is.na(Nb_Preg_perAnimal))%>%
  mutate(Nb_Preg_perAnimal=factor(x=Nb_Preg_perAnimal, levels = c("2", "1", "0")))%>%
  ggplot(aes(x=Group, fill=Nb_Preg_perAnimal, by=Group))+
  geom_bar(position="fill", color="white")+
  scale_fill_manual(values=mycolors_cycle,
                    name="Pregnancies")+
  theme_Publication(base_size=22)+
  labs(x=NULL, y="Proportion")+
  geom_text(stat = "prop", position = position_fill(.5),
            size=7)
save_plot("NbPregnancies", folder ="Fig2")


#Number of offspring
FGF21_Fert %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Nb_Pups_perAnimal))+
  boxplot_FGF21(nudge = 2)+
  labs(y= "Offspring", x=NULL)
save_plot("NbOffspring", folder ="Fig2")

#Number of offspring alive
FGF21_Fert %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Nb_PupsAlive_perAnimal))+
  boxplot_FGF21(nudge = 1.8)+
  labs(y= "Offspring alive", x=NULL)
save_plot("NbOffspringAlive", folder ="Fig2")

#Small script to obtain median per group for the writing of the result section.
FGF21_Fert%>% group_by(Group)%>%
  summarise(median_NbOff=median(Nb_offspring),
            median_NbOffAlive=median(Nb_offspring_alive))

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Additional code for revisions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Create template dataframe to perform visualisation of ggbeeswarm
test<-data.frame(FGF21=sample(20:30, 80, replace = TRUE), Control=sample(15:25, 80, replace = TRUE))%>%
  pivot_longer(cols=everything() , names_to = "Group", values_to = "Test")

#Visualisation of ggbeeswarm to test different conditions
test %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Test))+
  geom_beeswarm(cex = 6)+
  labs(y= "Offspring", x=NULL)
?geom_beeswarm

#Determine the standard deviation and the mean of the different groups
x<-FGF21_Fert%>%filter(!is.na(Nb_Pups_perAnimal))%>%
  select(Nb_Pups_perAnimal)
sd(x[x$Group=="FGF21",46])
median(x[,1])
mean(x[x$Group=="HFD",46])
mean(x[x$Group=="FGF21",46])

#Perform power calculation
library(pwr)
plot.power.htest
pwr::pwr.t.test(n=9, d=(6.2-0.8)/((2.2+3.4)/2), power=NULL)
