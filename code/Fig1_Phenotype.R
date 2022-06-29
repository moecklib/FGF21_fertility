#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#ANALYSIS FGF21 Fertility Fig1 Phenotyping
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Analysis of data for figure 1 regarding weight curve and phenotyping.
#JULY 2022, BEAT MOECKLI

#loading necessary packages
lapply(c("tidyverse", "RColorBrewer", "data.table", "colorspace", "readxl", 
         "ggpubr", "ggbeeswarm"), require, character.only = TRUE)

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Raw results import and annotation####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Import the weight data for all animals per week
FGF21_Weight<-read.csv("data/FGF21_Weight.csv")%>%
  mutate(diet=factor(diet, levels = c("ND", "HFD")))

#Import the weight data for the week after the sacrifice for the FGF21 and control animals
FGF21_WeightImpl<-read.csv("data/FGF21_WeightImplantation.csv")

#Import the Evaluation file
FGF21_Eval<-read.csv("data/FGF21_Evaluation.csv")%>%
  mutate(Group=factor(Group, levels=c("ND", "HFD", "FGF21")))

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Define colors & source functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colours to be used throughout the manuscript. Colors chosen with Viz Palette and colorgorical
mycolors_fill<-c("#cccccc","#FFE37E","#d94f60")
mycolors_stroke<-c("#9B9B9B","#FFB14E","#A21736")

#Source the graph functions
source("code/Functions.R")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Create weight curve for dams between HFD and ND####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Pivot to longer
grow_dams<-FGF21_Weight%>%dplyr::select(lab_id, diet, cage, group, starts_with("w"))%>%
  pivot_longer(cols=starts_with("w"), names_to="week", names_transform=list(week=readr::parse_number), 
               values_to="weight")


#Plot per diet without trend curve but statistical analysis
grow_dams[grow_dams$week%in%seq(2,12,2),]%>%          #Only take even weeks
ggplot(aes(x=diet, y=weight, fill=diet, color=diet))+
  geom_beeswarm(size=1.5, alpha=0.6)+
  facet_wrap(~week, nrow=1, switch = "x")+            #Facet wrap to show statistic
  boxplot_FGF21(point_size = 2)+
  labs(y= "Weight", x="Week after diet start")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

#Save the plot
save_plot("WeightCurve", width=7.5)


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Weight curve after FGF21 pump implantation before breeding (June 2021)####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#pivot data to long format
grow_dams_FGF21<-FGF21_WeightImpl%>%dplyr::select(lab_id, cage, group1, group2, starts_with("drel"))%>%
  pivot_longer(cols=starts_with("d"), names_to="day", names_transform=list(day=readr::parse_number), 
               values_to="weight")%>%
  mutate(group2=factor(group2, levels=c("ND", "HFD", "FGF21")))

#Plot per group with trend curve
ggplot(data=grow_dams_FGF21, aes(x=day, y=weight, color=group2, fill=group2))+
  #geom_point(alpha=0.7)+ #option to have a continuous scale on both axis (point and smooth)
  geom_smooth(size=1)+
  ggtitle("Weight curve breeding dams FGF21")+labs(y= "Relative Weight", x="Day after Intervention")+
  scale_x_continuous(breaks = seq(-1, 12, by = 2))+
  scale_fill_manual(values=mycolors_fill)+scale_color_manual(values=mycolors_stroke)+
  theme_Publication()

#Save the plot
save_plot("TrendCurve_FGF21", width=7.5)


#Plot per diet without trend curve but statistical analysis
grow_dams_FGF21%>%filter(group2!="ND")%>%
ggplot(aes(x=group2, y=weight, color=group2, fill=group2))+
  facet_wrap(~day, nrow=1, switch = "x")+
  labs(y= "Relative Weight", x="Day after Intervention")+
  boxplot_FGF21(nudge = 0.05)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
  scale_fill_manual(values=mycolors_fill[2:3])+
  scale_color_manual(values=mycolors_stroke[2:3])

#Save the plot
save_plot("Boxplot_FGF21", width=7.5)

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Evaluation of dams before sacrifice (Glycemia, Fat mass etc. ####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Fasting glycemia level before sacrifice
ggplot(data = FGF21_Eval, aes(x=Group, fill=Group, color=Group,
                              y=fasting_glycemia))+
  boxplot_FGF21(nudge = 0.75)+ ylim(0, 10)+
  labs(y= "fasting glycemia [mmol/l]", x=NULL)
save_plot("FastingGlycemia")

#Fat mass level
FGF21_Eval[!(FGF21_Eval$fat_mass<5 & FGF21_Eval$Group=="FGF21"),]%>% #Remove animal with tumor
ggplot(aes(x=Group, fill=Group, color=Group,
                              y=fat_mass))+
  boxplot_FGF21(nudge = 2)+ 
  labs(y= "fat mass [%]", x=NULL)
save_plot("FatMass")

#Liver weight at sacrifice
FGF21_Eval%>%
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=liver_weight))+
  boxplot_FGF21(nudge = 0.15)+ 
  labs(y= "liver weight [g]", x=NULL)
save_plot("LiverWeight")
