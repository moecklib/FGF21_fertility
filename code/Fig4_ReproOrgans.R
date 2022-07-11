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

#Load data file
FGF21_Repro<-read.csv("data/FGF21_Evaluation.csv")%>%
  mutate(Group=factor(x=Group, levels = c("ND", "HFD", "FGF21")))

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Define colors & source functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colors, used RColorBrewer, display.brewer.pal(12, "Paired")
mycolors_fill<-c("#FFFFFF", RColorBrewer::brewer.pal(8, "Paired")[c(1,5)])
mycolors_stroke<-c("#000000", RColorBrewer::brewer.pal(8, "Paired")[c(2,6)])

#Source the graph functions
source("code/Functions.R")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Plots for figure 4, reproductive organs####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Number of Corpora Lutea
FGF21_Repro %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=MatureCL))+
  boxplot_FGF21()+
  labs(y= "Mature Corpus Luteus", x=NULL)
save_plot("NbCorpousLuteus", folder="Fig4")

#Mature Follicles
FGF21_Repro %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=MatureFoll))+
  boxplot_FGF21(nudge = 0.25)+
  labs(y= "Mature Follicules", x=NULL)
save_plot("NbMatFollicules", folder="Fig4")

#Total Follicles, not included in the figure
FGF21_Repro %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Follicules))+
  boxplot_FGF21(nudge = 0.5)+
  labs(y= "Follicules", x=NULL)
save_plot("NbFollicules", folder="Fig4")

#Endometrial thickness
FGF21_Repro %>% mutate(Endomet_Thick_mm=EndometrialThickness/1E3)%>%  #Obtain the ET in mm
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Endomet_Thick_mm))+
  boxplot_FGF21(nudge = 0.04)+
  labs(y= "Thickness [mm]", x=NULL)
save_plot("EndometThickness", folder="Fig4")

#Surface of corpora lutea (CL)
FGF21_Repro %>% mutate(Surface_CL_mm2=Surface_CL/1E6)%>%  #Obtain the CL surface in mm2 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Surface_CL_mm2))+
  boxplot_FGF21(nudge = 0.6)+
  labs(y= "Surface CL [mm2]", x=NULL)
save_plot("SurfaceCL", folder="Fig4")

#Surface per individual CL
FGF21_Repro%>%mutate(Surface_CL=Surface_CL/1E6)%>%
  mutate(meanSurfperCL=Surface_CL/MatureCL)%>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=meanSurfperCL))+
  boxplot_FGF21(nudge = 0.2)

#Dosing of LH
FGF21_Repro %>%
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=LH_100ul))+
  boxplot_FGF21(nudge = 15)+
  labs(y= "LH [pg/mL]", x=NULL)
colnames(FGF21_Repro)
save_plot("SurfaceCL", folder="Fig4")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Tryout to plot data per organ and not animal####
#Data is not included in the final figure    ####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#This below code plots the items per organ and not per animal
#Import the data file
CL_surface<- read.csv("data/QuPath_CL_surface.csv")%>%
  mutate(Histo_ID=sub(pattern = ".czi.*", "", x=Image))%>%
  left_join(FGF21_Repro[,c("Histo_ID", "Group")])

#Count of CL per organ, automatically assessed by QuPath
CL_surface %>% 
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=count))+
  boxplot_FGF21(nudge = 1)

#Obtain some basic summary statistics
FGF21_Repro%>%group_by(Group)%>%
  filter(!is.na(MatureCL))%>%
  filter(!is.na(Surface_CL))%>%
  mutate(Surface_CL=Surface_CL/1E6)%>%
  mutate(meanSurfperCL=Surface_CL/MatureCL)%>%
  summarise(median_MatureCL=median(MatureCL), median_Foll=median(MatureFoll),
            median_SurfaceCL=median(Surface_CL), median_SurfacePerCL=median(meanSurfperCL))