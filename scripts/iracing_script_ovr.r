library(tidyverse)
library(patchwork)
library(gt)
library(tvthemes)
library(ggplot2)
library(gapminder)
library(scales)
library(glue)
library(png)
library(cowplot)
library(extrafont)
loadfonts(quiet = TRUE)

# sg <- c("fixed") #Series to filter in the bar chart by division
# pw_title <- c("iRacing - Fanatec GT3 Challenge - Fixed")
# sg <- c("vrs") #Series to filter in the bar chart by division
# pw_title <- c("iRacing - VRS GT Sprint Series")

div_from<-1
div_to<-10

ir_fixed<-readr::read_csv("./Data/iracing_fixed_gt3.csv") %>%
  mutate_at(.,vars(division),as.factor)%>%
  mutate(series="fixed",season="21s2")%>%
  filter(!clubname %in% c("Celtic","Connecticut"))

ir_vrs<-readr::read_csv("./Data/iracing.csv") %>%
  mutate_at(.,vars(division),as.factor)%>%
  mutate(series="vrs",season="21s2")%>%
  filter(!clubname %in% c("Celtic","Connecticut"))

ir_vrs_21s1<-readr::read_csv("./Data/iracing21s1.csv") %>%
  mutate_at(.,vars(division),as.factor)%>%
  mutate(series="vrs",season="21s1")%>%
  filter(!clubname %in% c("Celtic","Connecticut"))

ir<-rbind(ir_fixed,ir_vrs) %>%
  filter(between(as.numeric(division), div_from,div_to)) %>%
  group_by(custid, season)%>%
  mutate(irating=max(irating)) %>%
  ungroup()%>%
  distinct(custid,name,clubname,countrycode,division,irating)

p<-function(x){
  quantile(x,probs = 0.5)
}

# p(ir$irating)

#Boxplot
# rvar<-c("irating")

iracing <- function(bp_by="division",rvar="irating",div_from=1,div_to=10){
  bp_by <- c(bp_by)
  rvar <- c(rvar)
  
  div_from<-div_from
  div_to<-div_to
  
  assign(paste0("ir.",bp_by,".ovr"), ir %>% 
           select(clubname,irating,division,custid) %>%
           group_by_at(bp_by)%>%
           mutate(drivers=n())%>%
           ungroup()%>%
           filter(between(as.numeric(division), div_from,div_to)) %>%
           ggplot(aes(x= fct_reorder(.data[[bp_by]],.data[[rvar]],.fun = p,.desc=TRUE),y=.data[[rvar]],fill=.data[[bp_by]])) +
           geom_boxplot() + 
           geom_jitter(width=0.07,alpha=0.05)+
           annotate("text",x = -Inf, y = -Inf, label = "SHENLONG87", col="gray", size=25,hjust=-0.1,vjust=-1, fontface = "bold", alpha = 0.4)+
           labs(x=if_else(bp_by=="Division",paste0(bp_by),"Club"),
                y=paste0(rvar),
                # title = "iRacing VRS Sprint Series",
                subtitle = paste0(rvar," distribution by ",if_else(tolower(bp_by)=="division",paste0(bp_by),"Club")," (divisions ",div_from," to ",div_to,")")
           ) +
           # caption="As of 21S2W8") + 
           # facet_wrap(~division) +
           theme(axis.text.x = element_text(angle = 60,hjust = 1,family = "Cinzel",size=10),legend.position = "none",
                 title = element_text(family = "Cinzel", size = 12),
                 axis.text.y = element_text(family = "Cinzel", size = 10))+
           scale_y_continuous(name=rvar, breaks=seq(0, 10000, 500),labels=scales::comma) ,
        envir = .GlobalEnv)
  
  # get(paste0("ir.",bp_by,".ovr"))
  
}

iracing(div_from = 1, div_to = 10, bp_by="division")
iracing(div_from = 1, div_to = 10, bp_by="clubname")

#BARCHARTS START HERE
bp_by <- c("division")

#Dataset for division bar chart overall
ir.sum.div.ovr <- ir %>%
  group_by_at(vars(bp_by)) %>%
  summarise(irating = quantile(irating, c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1)), q = c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1), avg=mean(irating), drivers=n_distinct(custid)) %>%
  pivot_wider(names_from=q, values_from=irating)%>%
  ungroup()%>%
  mutate(cum_drivers=cumsum(drivers)/sum(drivers),perc_drivers=drivers/sum(drivers))

#OVERALL Bar Chart Drivers by bp_by, uses ir.sum.ovr
assign(paste0("dr.",bp_by,".ovr"),ir.sum.div.ovr %>%
         filter(between(as.numeric(division), div_from,div_to)) %>%
         ggplot(aes(x=fct_reorder(.data[[bp_by]],as.numeric(division),.desc=FALSE),
                    y=perc_drivers,
                    label=paste0(scales::percent(perc_drivers,accuracy = 0.1),
                                 "\n(n=", scales::comma(drivers), ")",
                                 "\n(iR=", scales::comma(avg), ")"),
                    fill=as.numeric(.data[[bp_by]])))+
         geom_col()+
         scale_fill_gradient(low = "darkgreen", high = "green")+
         # ggplot2::scale_fill_brewer(palette = "Greens",direction = -1) +
         geom_text(position = position_stack(vjust = 0.6), size = 3,color="gray10",angle=0,fontface="plain",family="Cinzel")+
         labs(x="Division",
              y="Percent of Drivers",
              # title = "iRacing VRS Sprint Series",
              subtitle = "Drivers by Division")+
         scale_y_continuous(labels = scales::label_percent(accuracy = 1))+
         annotate("text",x = -Inf, y = -Inf, label = "SHENLONG87", col="gray", size=25,hjust=-0.1,vjust=-1, fontface = "bold", alpha = 0.4)+
         theme(axis.text.x = element_text(angle = 60,hjust = 1,family = "Cinzel"),
               legend.position = "none",
               title = element_text(family = "Cinzel", size = 12),
               axis.text.y = element_text(family = "Cinzel", size = 10))
)

get(paste0("dr.",bp_by,".ovr"))

rm(bp_by)

bp_by <- ("clubname")

#dataset for drivers by club, make sure bp_by is clubname
ir.sum.club.ovr <- ir %>%
  group_by_at(vars(bp_by)) %>%
  summarise(irating = quantile(irating, c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1)), q = c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1), avg=mean(irating), drivers=n_distinct(custid)) %>%
  ungroup()%>%
  pivot_wider(names_from=q, values_from=irating)%>%
  # group_by(series)%>%
  mutate(cum_drivers=cumsum(drivers)/sum(drivers),perc_drivers=drivers/sum(drivers))%>%
  ungroup()

#Bar Chart Drivers by clubname
assign(paste0("dr.",bp_by,".ovr"), ir.sum.club.ovr %>%
  group_by(clubname) %>%
  ggplot(aes(x=fct_reorder(clubname,drivers,.desc=TRUE),
             y=perc_drivers,
             label=paste0("n=", scales::comma(x=drivers,accuracy = 1),
                          "\niR=", scales::comma(avg,accuracy = 1)),
             fill=.data[[bp_by]])
          )+
  geom_col()+
  geom_text(position = position_stack(vjust = 0.6), size = 2.2,color="gray10",angle=90,fontface="plain",family="Cinzel")+
  labs(x="Club",
       y="Percent of Drivers",
       # title = "iRacing VRS Sprint Series",
       subtitle = "Drivers by Club")+
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),breaks = seq(0,1,0.01))+
  annotate("text",x = -Inf, y = -Inf, label = "SHENLONG87", col="gray", size=25,hjust=-0.1,vjust=-1, fontface = "bold", alpha = 0.4)+
  theme(axis.text.x = element_text(angle = 60,hjust = 1,family = "Cinzel"),
        legend.position = "none",
        title = element_text(family = "Cinzel", size = 12),
        axis.text.y = element_text(family = "Cinzel", size = 10))
)

get(paste0("dr.",bp_by,".ovr"))
rm(bp_by)

#PATCHWORK PACKAGE TO PRINT ALL PLOTS IN THE SAME IMAGE
design <- "
  13
  24
"
pw_title <- c("iRacing - Combined VRS GT Sprint and Fanatec GT3 Fixed")

pw_ovr <- dr.division.ovr +
  dr.clubname.ovr +
  ir.division.ovr +
  ir.clubname.ovr +
  
  patchwork::plot_layout(design = design)+
  plot_annotation(
    title = pw_title,
    subtitle = "Unique Drivers - As of 21S2",
    caption = paste0("Source: iRacing Member Site - Series Stats","\nBox Plots ranked by median iRating"),
    theme = theme(title = element_text(family = "Cinzel"))
  )

ggsave(plot=pw_ovr,filename=paste0("./Plots/",pw_title,".png"), dpi=320, width = 18.9,height = 14.96,units = "in")

#MODIFY PLOTS TO SAVE THEM INDIVIDUALLY
capdr<- paste0("Source: iRacing Member Site - Series Stats","\nUnique Drivers - As of 21S2")
capir<- paste0("Source: iRacing Member Site - Series Stats","\nBox Plots ranked by median iRating","\nUnique Drivers - As of 21S2")
tit <- c("iRacing - Combined VRS GT Sprint and Fanatec GT3 Fixed")

ir.clubname.ovr.2 <- ir.clubname.ovr + labs(title = tit, caption=capir)
ir.division.ovr.2 <- ir.division.ovr + labs(title = tit, caption=capir)
dr.clubname.ovr.2 <- dr.clubname.ovr + labs(title = tit, caption=capdr)
dr.division.ovr.2 <- dr.division.ovr + labs(title = tit, caption=capdr)

#Save individual plots
ggsave(filename = paste0("./Plots/Individual/","ir.clubname.ovr.png"), plot=ir.clubname.ovr.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename = paste0("./Plots/Individual/","ir.division.ovr.png"), plot=ir.division.ovr.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename = paste0("./Plots/Individual/","dr.clubname.ovr.png"), plot=dr.clubname.ovr.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename = paste0("./Plots/Individual/","dr.division.ovr.png"), plot=dr.division.ovr.2, dpi=320, width = 9.45,height = 7.48,units = "in")
