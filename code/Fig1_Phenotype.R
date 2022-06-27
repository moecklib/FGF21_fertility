#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#ANALYSIS FGF21 Fertility Fig1 Phenotyping
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
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

#Function to create the standard boxplot, returns list element
boxplot_FGF21<-function(nudge=0.6,
                        size=0.8,
                        textsize=5){
  list(
    geom_boxplot(size=1),
    geom_point(),
    scale_fill_manual(values=mycolors_fill),
    scale_color_manual(values=mycolors_stroke),
    geom_signif(comparisons = list(c("FGF21", "HFD")),
                color="black",
                size=size,
                textsize = textsize),
    geom_signif(comparisons = list(c("ND", "HFD")),
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



#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Create weight curve for dams between HFD and ND####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Pivot to longer
grow_dams<-FGF21_Weight%>%dplyr::select(lab_id, diet, cage, group, starts_with("w"))%>%
  pivot_longer(cols=starts_with("w"), names_to="week", names_transform=list(week=readr::parse_number), 
               values_to="weight")

ggplot(grow_dams, aes(x=week, y=weight))+
  geom_point()

x <- seq(0,12,2)
y<- grow_dams%>%filter(week == c(0,2,4))

?filter

grow_dams[grow_dams$week%in%seq(0,12,2),]

#Plot per diet without trend curve but statistical analysis
grow_dams[grow_dams$week%in%seq(0,12,2),]%>%
ggplot(aes(x=diet, y=weight, fill=diet))+geom_boxplot()+
  geom_beeswarm(color="black", size=1, alpha=0.6)+
  facet_wrap(~week, nrow=1, switch = "x")+
  boxplot_FGF21()+
  labs(y= "Weight", x="Week after diet start")+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())+
  ggtitle("Weight curve breeding dams for Interventions")
