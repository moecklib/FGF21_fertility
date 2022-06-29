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
FGF_21<-read.csv("data/FGF21_Fertility.csv")%>%
  mutate(group=factor(x=group, levels = c("ND", "HFD", "FGF21")))

FGF21_Repro<-FGF_21[1:22, c("group", "MatureCL", "MatureFoll", "EndometrialThickness")]

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Define colors & source functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colours to be used throughout the manuscript. Colors chosen with Viz Palette and colorgorical
mycolors_fill<-c("#cccccc","#FFE37E","#d94f60")
mycolors_stroke<-c("#9B9B9B","#FFB14E","#A21736")

#Source the graph functions
source("code/Functions.R")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Plots for figure S3 (All qPCR results combined####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

FGF21_Repro %>% 
  ggplot(aes(x=group, y=MatureCL, fill=group, color=group))+
  boxplot_FGF21()+
  labs(y= "Mature Corpus Luteus", x=NULL)

FGF21_Repro %>% 
  ggplot(aes(x=group, y=MatureFoll, fill=group, color=group))+
  boxplot_FGF21(nudge = 0.4)+
  labs(y= "Mature Follicules", x=NULL)

FGF21_Repro %>% 
  ggplot(aes(x=group, y=EndometrialThickness, fill=group, color=group))+
  boxplot_FGF21(nudge = 0.4)+
  labs(y= "Endometrial thickness [μm]", x=NULL)