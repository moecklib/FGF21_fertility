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

#Theme of colors, used RColorBrewer, display.brewer.pal(12, "Paired")
mycolors_fill<-c("#FFFFFF", RColorBrewer::brewer.pal(8, "Paired")[c(1,5)])
mycolors_stroke<-c("#000000", RColorBrewer::brewer.pal(8, "Paired")[c(2,6)])

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
  labs(y= "Weight [g]", x="Week after diet start")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())
save_plot("WeightCurve", width=7.5, folder="Fig1")      #Save the plot

#Simple summary table for the manuscript
summary_stat(df=FGF21_Weight, group_var = "diet", var_interest = "w1")
summary_stat(df=FGF21_Weight, group_var = "diet", var_interest = "w12")


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Weight curve after FGF21 pump implantation before breeding (June 2021)####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#pivot data to long format of FGF21 short term effect on weight graph
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
save_plot("TrendCurve_FGF21", width=7.5, folder="Fig1")     #Save the plot


#Plot per diet without trend curve but statistical analysis
grow_dams_FGF21%>%filter(group2!="ND")%>%
ggplot(aes(x=group2, y=weight, color=group2, fill=group2))+
  facet_wrap(~day, nrow=1, switch = "x")+
  labs(y= "Relative Weight", x="Day after Intervention")+
  boxplot_FGF21(nudge = 0.05)+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
  scale_fill_manual(values=mycolors_fill[2:3])+
  scale_color_manual(values=mycolors_stroke[2:3])
save_plot("WeightBoxplot_FGF21", width=7.5, folder="Fig1")     #Save the plot

#Simple summary table for the manuscript
grow_dams_FGF21%>%filter(group2!="ND")%>%
  group_by(group2, day)%>%
  summarise(weight=median(weight))

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Evaluation of dams before sacrifice (Glycemia, Fat mass etc. ####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Fasting glycemia level before sacrifice
ggplot(data = FGF21_Eval, aes(x=Group, fill=Group, color=Group,
                              y=fasting_glycemia))+
  boxplot_FGF21(nudge = 0.75)+ ylim(0, 10)+
  labs(y= "fasting glycemia [mmol/l]", x=NULL)
save_plot("FastingGlycemia", folder="Fig1")

#Fat mass level
FGF21_Eval[!(FGF21_Eval$fat_mass<5 & FGF21_Eval$Group=="FGF21"),]%>% #Remove animal with tumor
ggplot(aes(x=Group, fill=Group, color=Group,
                              y=fat_mass))+
  boxplot_FGF21(nudge = 2)+ 
  labs(y= "fat mass [%]", x=NULL)
save_plot("FatMass", folder="Fig1")

#Liver weight at sacrifice
FGF21_Eval%>%
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=liver_weight))+
  boxplot_FGF21(nudge = 0.15)+ 
  labs(y= "liver weight [g]", x=NULL)
save_plot("LiverWeight", folder="Fig1")

#Simple summary table for the manuscript
summary_stat(var_interest = "liver_weight")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Quantification of steatosis ####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#The total surface of steatosis was assessed with QuPath (see separate script)
#Import raw data file and process it for the figure
steatosis_surface<- read.csv("data/QuPath_Steatosis_Measure.csv")%>%
  rename(Image=Image, Name=Name, Area=Area.Âµm.2)%>%  #Rename variable to readable format
  pivot_wider(names_from = Name, values_from = Area)%>% #Pivot wider
  mutate(Lab_ID=as.integer(substr(Image, 2, 3)))%>% #Extract the lab_id from the image name as integer
  left_join(FGF21_Eval[,c("Lab_ID", "Group")])%>%   #Left join to obtain the grouping variable
  mutate(Rel_Steatosis=(Steatosis/Tissue)*100)%>%   #Calculate the relative tissue surface of steatosis
  filter(!Lab_ID==10)                               #Filter the data of the sick animal (tumor)

#Create corresponding graph
steatosis_surface%>%
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=Rel_Steatosis))+
  boxplot_FGF21(nudge = 0.8)+ 
  labs(y= "Relative Steatosis [%]", x=NULL)
save_plot("RelSteatosis", folder="Fig1")

#Simple summary statistics
steatosis_surface%>%group_by(Group)%>%
  summarise(mean=mean(Rel_Steatosis),
            median=median(Rel_Steatosis))
