#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#*#ANALYSIS FGF21 Fertility Fig4 Analysis of reproductive organs
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Analysis of histology data of reproductive organs. Quantification
#MAI 2022, BEAT MOECKLI

#loading necessary packages
lapply(c("tidyverse", "RColorBrewer", "data.table", "colorspace", "readxl", 
         "ggpubr", "ggbeeswarm"), require, character.only = TRUE)

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Raw results import and annotation####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Import of raw result file
FGF_21<-read.csv("data/FGF21_Fertility.csv")


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Plot structure & functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colours to be used throughout. Colors chosen with Viz Palette and colorgorical
mycolors_controls<-c("#cccccc","#ffb7ad","#ff695f","#c00000")
mycolors_treatment<-c("#cccccc","#a8d5e6","#4d5f98")
mycolors_time<-c("#cccccc","#cc96eb","#49388e")
mycolors_groups<-wes_palette("GrandBudapest1")

#Theme publication function
theme_Publication <- function(base_size=16, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5, margin = margin(0,0,20,0)),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vetical",
            legend.key.size= unit(0.5, "cm"),
            #legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
}


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Plots for figure S3 (All qPCR results combined####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

FGF_21[1:22, c("group", "MatureCL", "MatureFoll", "EndometrialThickness")] %>% 
  ggplot(aes(x=group, y=MatureCL, fill=group))+
  geom_boxplot()+
  geom_point()+
  scale_fill_manual(values=mycolors_controls)+
  geom_signif(comparisons = list(c("FGF21", "HFD")))+
  geom_signif(comparisons = list(c("FGF21", "ND")), 
              position = position_nudge(y=0.5))+
  theme_Publication()

FGF_21[1:22, c("group", "MatureCL", "MatureFoll", "EndometrialThickness")] %>% 
  ggplot(aes(x=group, y=MatureFoll))+
  geom_boxplot()+
  geom_point()+
  geom_signif(comparisons = list(c("FGF21", "HFD")))+
  geom_signif(comparisons = list(c("FGF21", "ND")), 
              position = position_nudge(y=0.3))

FGF_21[1:22, c("group", "MatureCL", "MatureFoll", "EndometrialThickness")] %>% 
  ggplot(aes(x=group, y=EndometrialThickness))+
  geom_boxplot()+
  geom_point()+
  geom_signif(comparisons = list(c("FGF21", "HFD")))+
  geom_signif(comparisons = list(c("FGF21", "ND")), 
              position = position_nudge(y=20))
