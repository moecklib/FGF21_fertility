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

FGF21_Repro<-read.csv("data/FGF21_Evaluation.csv")%>%
  mutate(Group=factor(x=Group, levels = c("ND", "HFD", "FGF21")))

#Import of raw result file
FGF_21<-read.csv("data/FGF21_Fertility.csv")%>%
  mutate(group=factor(x=group, levels = c("ND", "HFD", "FGF21")))

FGF21_Repro<-FGF_21[1:22, c("group", "MatureCL", "MatureFoll", "EndometrialThickness")]

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Define colors & source functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colors, used RColorBrewer, display.brewer.pal(12, "Paired")
mycolors_fill<-c("#FFFFFF", RColorBrewer::brewer.pal(8, "Paired")[c(1,5)])
mycolors_stroke<-c("#000000", RColorBrewer::brewer.pal(8, "Paired")[c(2,6)])

#Source the graph functions
source("code/Functions.R")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Plots for figure S3 (All qPCR results combined####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Number of Corpus Luteus
FGF21_Repro %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=MatureCL))+
  boxplot_FGF21()+
  labs(y= "Mature Corpus Luteus", x=NULL)
save_plot("NbCorpousLuteus")

#Mature Follicules
FGF21_Repro %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=MatureFoll))+
  boxplot_FGF21(nudge = 0.4)+
  labs(y= "Mature Follicules", x=NULL)
save_plot("NbFolicules", folder="Fig4")

#Endometrial thickness
FGF21_Repro %>% mutate(Endomet_Thick_mm=EndometrialThickness/1E3)%>%  #Obtain the ET in mm
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Endomet_Thick_mm))+
  boxplot_FGF21(nudge = 0.06)+
  labs(y= "Endometrial thickness [mm]", x=NULL)
save_plot("EndometThickness", folder="Fig4")

#Surface of corpus luteus
FGF21_Repro %>% mutate(Surface_CL_mm2=Surface_CL/1E6)%>%  #Obtain the CL surface in mm2 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Surface_CL_mm2))+
  boxplot_FGF21(nudge = 0.6)+
  labs(y= "Surface CL [mm2]", x=NULL)
save_plot("SurfaceCL")

#This 
CL_surface<- read.csv("data/measurements.csv")%>%
  rename(Image=Image, Area=Area.Âµm.2)%>%
  filter(!is.na(Area))%>%
  group_by(Image)%>%
  summarise(Count=n(), Area_CL=sum(Area), MeanArea_CL=mean(Area))%>%
  mutate(Histo_ID=sub(pattern = ".czi.*", "", x=Image))%>%
  left_join(FGF21_Repro[,c("Histo_ID", "Group")])

CL_surface %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=MeanArea_CL))+
  boxplot_FGF21(nudge = 0.08E6)+
  labs(y= "Endometrial thickness [μm]", x=NULL)

table(FGF21_Repro$Group)


