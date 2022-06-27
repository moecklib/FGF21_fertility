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
FGF_21$group<-factor(x=FGF_21$group, levels = c("ND", "HFD", "FGF21"))
FGF21_Repro<-FGF_21[1:22, c("group", "MatureCL", "MatureFoll", "EndometrialThickness")]

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Plot structure & functions####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Theme of colours to be used throughout the manuscript. Colors chosen with Viz Palette and colorgorical
mycolors_fill<-c("#cccccc","#FFE37E","#d94f60")
mycolors_stroke<-c("#9B9B9B","#FFB14E","#A21736")

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

boxplot_FGF21<-function(nudge=0.6,
                        size=0.8,
                        textsize=5){
  list(
    geom_boxplot(size=1),
    geom_point(),
    scale_fill_manual(values=mycolors_fill),
    scale_color_manual(values=mycolors_stroke),
    geom_signif(comparisons = list(c("FGF21", "HFD"),
                                   c("ND", "HFD")),
                color="black",
                size=size,
                textsize = textsize),
    geom_signif(comparisons = list(c("FGF21", "ND")), 
                position = position_nudge(y=nudge),
                color="black",
                size=size,
                textsize = textsize),
    theme_Publication(base_size=22),
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  )
}

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