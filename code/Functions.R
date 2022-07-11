#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Functions to be sourced for ANALYSIS FGF21
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Analysis of data for figure 1 regarding weight curve and phenotyping.
#JULY 2022, BEAT MOECKLI

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#Theme Publication function####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

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


#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
#General boxplot function####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

#Function to create the standard boxplot, returns list element
boxplot_FGF21<-function(nudge=0.6,
                        size=0.9,
                        textsize=6,
                        p_level=FALSE,
                        point_size=3){
  list(
    geom_boxplot(size=1),
    geom_beeswarm(size=point_size),
    scale_fill_manual(values=mycolors_fill),
    scale_color_manual(values=mycolors_stroke),
    geom_signif(comparisons = list(c("FGF21", "HFD")),
                color="black",
                size=size,
                textsize = textsize,
                map_signif_level = p_level),
    geom_signif(comparisons = list(c("ND", "HFD")),
                color="black",
                size=size,
                textsize = textsize,
                map_signif_level = p_level),
    geom_signif(comparisons = list(c("FGF21", "ND")), 
                position = position_nudge(y=nudge),
                color="black",
                size=size,
                textsize = textsize,
                map_signif_level = p_level),
    theme_Publication(base_size=22),
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  )
}

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Save the last plot function####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Create a function that saves the plots directly to the output folder
save_plot<-function(plot,
                    folder="",
                    width=5,
                    height=5.5){
  ggsave(
    filename=paste(plot,".tiff", sep = ""),
    device="tiff",
    width=width,
    height=height,
    # units="mm",
    path=paste("output/graphs/", folder, sep = ""),
    dpi = "retina")
}

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Summary stat for manuscript####
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#Function to calculate some simple summary statistics
summary_stat <- function(x=FGF21_Repro,
                         group_var="Group",
                         var_interest="MatureCL",
                         stat=median)
  {
  
  #Eliminate NA values in the variable of interest
  y <- x[complete.cases(x[,var_interest]),]
  
  #Compute summary statistics
  tapply(y[,var_interest], y[,group_var], stat)
  
}