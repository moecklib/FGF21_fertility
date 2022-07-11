#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#*#ANALYSIS FGF21 Fertility Fig3 Analysis of Cycle
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Analysis of vaginal cytology and cycle assessment
#July 2022, BEAT MOECKLI

#loading necessary packages
lapply(c("tidyverse", "RColorBrewer", "data.table", "colorspace", "readxl", 
         "ggpubr", "ggbeeswarm", "gridExtra"), require, character.only = TRUE)

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Raw results import and annotation####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Load and transform cycle data without writing individual steps as objects
FGF21_Cycle<- local({

#Import of raw result file
cycle_dams<-read_csv("data/FGF21_Cycle.csv")%>%
  mutate(Group=factor(x=group, levels = c("ND", "HFD", "FGF21")))%>%
  filter(!(lab_id%in%c("10","15","16","17","18")))  #filter out sacrified animal (16) or with tumor (10)
                                                    #with dermatitis (17) or in utero pregnancy (17, 18)

#Pivot file longer
cycle_dams_2<-cycle_dams%>%dplyr::select(lab_id, diet, cage, Group, num_range("",1:14))%>%
  pivot_longer(cols=5:18, names_to="day", values_to="cycle")

#Convert cycles to different nomenclature: L(leucocytes)=Diestrus or Metestrus, 
#N(nucleated)=proestrus, C(cornified)=estrus 
cycle_dams_3<-cycle_dams_2%>%mutate(cycle=case_when(cycle=="M"~"L",
                                                  cycle=="D"~"L",
                                                  cycle=="P"~"N",
                                                  cycle=="E"~"C",TRUE~"MOC"))

#Reorder variable according to pre-specified factors
FGF21_Cycle<-cycle_dams_3%>%
  mutate(cycle=factor(cycle, levels=c("L", "N", "C")))%>% #Put the different cycle stages in order
  mutate(day=factor(day, levels=c(1:14)))%>%
  mutate(lab_id=factor(lab_id, levels=c(1:2, 4:6, 11,3, 8:9, 12:14,19:24)))

FGF21_Cycle
})

#Read data file with some summaries about the cycles
FGF21_Eval <- read.csv("data/FGF21_Evaluation.csv")%>%
  mutate(Group=factor(x=Group, levels = c("ND", "HFD", "FGF21")))

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Define colors & source functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colors, used RColorBrewer, display.brewer.pal(12, "Paired")
mycolors_fill<-c("#FFFFFF", RColorBrewer::brewer.pal(8, "Paired")[c(1,5)])
mycolors_stroke<-c("#000000", RColorBrewer::brewer.pal(8, "Paired")[c(2,6)])

#Source the graph functions
source("code/Functions.R")

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Plots for figure 3 regarding cycle####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Write Plot, version 1 (In one plot with different colors)
ggplot(data = FGF21_Cycle, aes(x=factor(day), y=cycle, colour=Group, group=lab_id))+
  geom_line(size=1)+
  facet_wrap(~lab_id)+
  theme_Publication()+
  labs(x=NULL, y=NULL)+
  scale_colour_manual(values = mycolors_stroke)+
  ggtitle("Cycle per group")

#List of common elements for the later plot
plot<-list(facet_grid(lab_id~.),
           theme_Publication(base_size=22),
           labs(x="Day", y=NULL),
           theme(strip.text.y = element_blank()),
           scale_x_discrete(breaks=as.character(seq(2,14, 2)))
           )

#Write the single plots
#ND
FGF21_Cycle%>%filter(Group=="ND")%>%
  ggplot(aes(x=day, y=cycle, group=lab_id))+
  geom_line(size=1.2, color=mycolors_stroke[1])+
  ggtitle("ND")+
  plot
save_plot("ND",folder = "Fig3", width = 3.5, height = 7)

#HFD
FGF21_Cycle%>%filter(Group=="HFD")%>%
  ggplot(aes(x=factor(day), y=cycle, group=lab_id))+
  geom_line(size=1.2, color=mycolors_stroke[2])+
  ggtitle("HFD")+
  plot
save_plot("HFD",folder = "Fig3",width = 3.5, height = 7)

#FGF21
FGF21_Cycle%>%filter(Group=="FGF21")%>%
  ggplot(aes(x=factor(day), y=cycle, color=cycle, group=lab_id))+
  geom_line(size=1.2, color=mycolors_stroke[3])+
  ggtitle("FGF21")+
  plot
save_plot("FGF21",folder = "Fig3", width = 3.5, height = 7)

#Load the library that allows easy insertion of proportions 
#(https://ggobi.github.io/ggally/articles/ggally_stats.html#stat-prop-)
library(GGally, quietly=TRUE)

#Theme of colors, used RColorBrewer, display.brewer.pal(12, "Paired")
mycolors_cycle<-RColorBrewer::brewer.pal(8, "Blues")[c(2,4,6)]

#Create Bar Plot to illustrate the different proportions
FGF21_Cycle%>%
  ggplot(aes(x=Group, fill=cycle, by=Group))+
  geom_bar(position="fill", color="white")+
  scale_fill_manual(values=mycolors_cycle)+
  theme_Publication(base_size=22)+
  labs(x=NULL, y="Proportion of cycle")+
  geom_text(stat = "prop", position = position_fill(.5),
            size=7)
save_plot("PropCycle", folder ="Fig3", height=7)


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Boxplots for data exploration regarding cycle, graphs not included in figure####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Boxplot with number of cycles
FGF21_Eval%>%
  ggplot(aes(x=Group, fill=Group, color=Group,
                         y=Nb_Cycles))+
  boxplot_FGF21()

#Add some new variables out of the old ones
cycle_dams_manuscript <- FGF21_Eval%>%mutate(DaysInCycle=DaysInProestrus+DaysInEstrus, DaysPerCycle=DaysInCycle/Nb_Cycles)%>%
    mutate(DaysInEstrusPerCycle=DaysInEstrus/Nb_Cycles)

#Graph to map days per cycle
cycle_dams_manuscript%>%filter(!is.na(DaysPerCycle))%>%
  select(Lab_ID, Group, Nb_Cycles, starts_with("Days"))%>%  #Select only some of the variables
    ggplot(aes(x=Group, fill=Group, color=Group,
               y=DaysPerCycle))+
    boxplot_FGF21()

#Days in estrous per cycle
cycle_dams_manuscript%>%
    filter(!is.na(DaysInEstrusPerCycle))%>%
    ggplot(aes(x=Group, fill=Group, color=Group,
               y=DaysInEstrusPerCycle))+
    boxplot_FGF21()

#Total days in estrous over 14 day period
cycle_dams_manuscript%>%
  ggplot(aes(x=Group, fill=Group, color=Group,
             y=DaysInEstrus))+
  boxplot_FGF21(nudge = 1.1)

#Summary statistics for the manuscript  
cycle_dams_manuscript%>%group_by(Group)%>%
  filter(!is.na(DaysInEstrusPerCycle))%>%
  summarise(Nb_Cycles=median(Nb_Cycles), DInEstrous=median(DaysInEstrusPerCycle), 
            n=n())

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Statistical analysis regarding proportions of cycle####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
  
##Statistical analysis to compare proportions
#Summarize data to be processed for statistical testing
prop_data<-FGF21_Cycle%>%group_by(Group, cycle)%>%
  summarise(value=n())%>%
  mutate(total=sum(value), failures=total-value)
prop_matrix<- data.matrix(prop_data[,c(3,5)])

#Do proportional testing by first converting dataframe into a datamatrix (data.matrix)
prop.test(prop_matrix[c(1,4,7),])  #calculate proportion in diestrus
prop.test(prop_matrix[c(2,5,8),])  #calculate proportion in proestrus
prop.test(prop_matrix[c(3,6,9),])  #calculate proportion in estrus
