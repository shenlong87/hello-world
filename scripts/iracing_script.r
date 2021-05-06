library(tidyverse)
library(gt)
library(patchwork)
library(tvthemes)
library(ggplot2)
library(gapminder)
library(scales)
library(glue)
library(png)
library(dplyr)
library(cowplot)
library(extrafont)
loadfonts(quiet = TRUE)

# sg <- c("fixed")
# pw_title <- c("iRacing - Fanatec GT3 Challenge - Fixed")
# sg <- c("vrs") #Series to filter in the bar chart by division


div_from<-1
div_to<-10


ir_fixed<-readr::read_csv("./Data/iracing_fixed_gt3.csv") %>%
  mutate_at(.,vars(division),as.factor)%>%
  mutate(series="fixed")%>%
  filter(!clubname %in% c("Celtic","Connecticut"))

ir_vrs<-readr::read_csv("./Data/iracing.csv") %>%
  mutate_at(.,vars(division),as.factor)%>%
  mutate(series="vrs")%>%
  filter(!clubname %in% c("Celtic","Connecticut"))

ir<-rbind(ir_fixed,ir_vrs) %>%
filter(between(as.numeric(division), div_from,div_to))

p<-function(x){
  quantile(x,probs = 0.5)
}

# p(ir$irating)

#Boxplot
# rvar<-c("irating")

iracing <- function(bp_by="division",rvar="irating",div_from=1,div_to=10,s="vrs"){
bp_by <- c(bp_by)
s <- c(s)
rvar <- c(rvar)

div_from<-div_from
div_to<-div_to

assign(paste0("ir.",bp_by,".",s), ir %>% 
  select(clubname,irating,division,series) %>%
  group_by_at(bp_by)%>%
  mutate(drivers=n())%>%
  ungroup()%>%
  filter(series==s,between(as.numeric(division), div_from,div_to)) %>%
  ggplot(aes(x= fct_reorder(.data[[bp_by]],.data[[rvar]],.fun = p,.desc=TRUE),y=.data[[rvar]],fill=.data[[bp_by]])) +
  geom_boxplot() + 
  geom_jitter(width=0.07,alpha=0.05)+
  annotate("text",x = -Inf, y = -Inf, label = "SHENLONG87", col="gray", size=25,hjust=-0.1,vjust=-1, fontface = "bold", alpha = 0.4)+
  labs(x=if_else(bp_by=="Division",paste0(bp_by),"Club"),
       y=paste0(rvar),
       # title = "iRacing VRS Sprint Series",
       subtitle = paste0(rvar," distribution by ",if_else(tolower(bp_by)=="division",paste0(bp_by),"Club")," (divisions ",div_from," to ",div_to,")")
      ) +
       # caption="As of 21S2W7") + 
  # facet_wrap(~division) +
  theme(axis.text.x = element_text(angle = 60,hjust = 1,family = "Cinzel",size=10),legend.position = "none",
        title = element_text(family = "Cinzel", size = 12),
         axis.text.y = element_text(family = "Cinzel", size = 10))+
  scale_y_continuous(name=rvar, breaks=seq(0, 10000, 500),labels=scales::comma),
  envir = .GlobalEnv)


# get(paste0("ir.",bp_by,".",s))

}

 iracing(div_from = 1, div_to = 10, bp_by="division", s="vrs")
 iracing(div_from = 1, div_to = 10, bp_by="division", s="fixed")
 
 iracing(div_from = 1, div_to = 10, bp_by="clubname", s="vrs")
 iracing(div_from = 1, div_to = 10, bp_by="clubname", s="fixed")

#BARCHARTS START HERE
 
bp_by <- c("division") #Do not change for bar charts by driver
sg <- c("fixed") #Series to filter in the bar chart by division

ir.sum.div <- assign(paste0("ir.sum.",bp_by,".",sg),ir %>%
  group_by_at(vars(series,bp_by)) %>%
  summarise(irating = quantile(irating, c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1)), q = c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1), avg=mean(irating), drivers=n_distinct(custid)) %>%
  pivot_wider(names_from=q, values_from=irating)%>%
  group_by(series)%>%
  mutate(cum_drivers=cumsum(drivers)/sum(drivers),perc_drivers=drivers/sum(drivers))%>%
  ungroup()
)


#Bar Chart Drivers by bp_by, run one for each series by changing the value of SG
assign(paste0("dr.",bp_by,".",sg),ir.sum.div %>%
  filter(series==sg,between(as.numeric(division), div_from,div_to)) %>%
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

get(paste0("dr.",bp_by,".",sg))
rm(bp_by)


bp_by <- ("clubname")

#dataset for drivers by club
ir.sum.clubname <- assign(paste0("ir.sum.",bp_by,".",sg),ir %>%
                       group_by_at(vars(series,bp_by)) %>%
                       summarise(irating = quantile(irating, c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1)), q = c(0.01,0.05,0.25, 0.5, 0.75,0.95,0.99,1), avg=mean(irating), drivers=n_distinct(custid)) %>%
                       pivot_wider(names_from=q, values_from=irating)%>%
                       group_by(series)%>%
                       mutate(cum_drivers=cumsum(drivers)/sum(drivers),perc_drivers=drivers/sum(drivers))%>%
                       ungroup()
                        )

#Bar Chart Drivers by bp_by - modified manually to get barchart by club name
assign(paste0("dr.",bp_by,".",sg),ir.sum.clubname %>%
  group_by(vars(bp_by)) %>%
  filter(series==sg)%>%
  ggplot(aes(x=fct_reorder(clubname,drivers,.desc=TRUE),
             y=perc_drivers,
             label=paste0("n=", scales::comma(x=drivers,accuracy = 1),
                          "\niR=", scales::comma(avg,accuracy = 1)),
             fill=.data[[bp_by]]
  )
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

get(paste0("dr.",bp_by,".",sg))
rm(bp_by)


#Getting overlap by division in both series
ir_both <- full_join(dplyr::select(ir_fixed,series,custid,clubname,division),
                       dplyr::select(ir_vrs,series,custid,clubname,division),
                       by="custid") %>%
             mutate(clubname=coalesce(clubname.x,clubname.y),
                   division=coalesce(division.x,division.y),
                   series=case_when(is.na(series.x) & !is.na(series.y) ~ "vrs_only",
                               is.na(series.y) & !is.na(series.x) ~ "fixed_only",
                               !is.na(series.x) & !is.na(series.y) ~ "both")) %>%
             select(custid,clubname,division,series) %>%
            filter(division!=11)%>%
            # mutate(total_drivers=n_distinct(custid)) %>%
            group_by(division)%>%
            mutate(total_drivers_div=n_distinct(custid)) %>%
            ungroup()%>%
            group_by(division,series) %>%
            summarise(drivers_series=n_distinct(custid),td_div=mean(total_drivers_div)) %>%
            ungroup()%>%
            # arrange(division,clubname) %>%
            pivot_wider(names_from = series, values_from = c(drivers_series))%>%
            bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.factor), ~"Total"))
            )%>%
            mutate(vrs=vrs_only+both,
                   fixed=fixed_only+both,
                   perc_both=round(both/td_div,4),
                   perc_fixed_only=round(fixed_only/td_div,4),
                   perc_vrs_only=round(vrs_only/td_div,4),
                   perc_fixed=round(fixed/td_div,4),
                   perc_vrs=round(vrs/td_div,4))

ir_table <- gt(ir_both)%>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = md("**iRacing Series Driver Overlap**"),
    subtitle = md("**Fanatec GT3 Challenge Fixed and VRS GT Sprint**")
               ) %>%
  fmt_percent(
    columns = vars(perc_both,perc_vrs_only,perc_fixed_only,perc_fixed,perc_vrs),
    decimals = 0
  ) %>%
  fmt_number(
    columns = vars(td_div,both,fixed_only,vrs_only,fixed,vrs),
    decimals = 0
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgray"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      # columns = vars(species,mean_body_mass),
      rows = td_div >= 12000)
  ) %>%
  tab_spanner(
    label = "Driven",
    columns = vars(both, fixed_only, vrs_only, fixed, vrs)
  ) %>%
  tab_spanner(
    label = "Percent of Drivers",
    columns = vars(perc_both,perc_fixed_only, perc_vrs_only, perc_fixed, perc_vrs)
  ) %>%
  tab_spanner(
    label = "-------",
    columns = vars(division,td_div)
  ) %>%
  cols_label(
    division = "Division",
    td_div = "Total\nDrivers",
    both = "Both",
    fixed_only = "Fixed Only",
    vrs_only = "VRS Only",
    fixed = "Fixed",
    vrs = "VRS",
    perc_both = "Both",
    perc_fixed_only = "Fixed Only",
    perc_vrs_only = "VRS Only",
    perc_fixed = "Fixed",
    perc_vrs = "VRS"
  ) %>%
  tab_source_note(
    source_note = md("**Source:** iRacing Member Site - Series Stats")
  ) %>%
  tab_source_note(
    source_note = md("**Data Updated:** 21S2W7")
  ) %>% 
  tab_footnote(
    footnote = md("Drivers in these columns may have driven the other series"),
    locations = cells_column_labels(
      columns = vars(vrs,fixed,perc_vrs,perc_fixed))
    )%>%
  tab_options(footnotes.font.size = px(11),
              source_notes.font.size = px(12)) %>%
  tab_style(
    style = cell_text(size = px(17)),
    locations = cells_title(groups="title")
  ) %>%
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_title(groups="subtitle")
  ) %>%
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(size = px(15)),
    locations = cells_column_spanners(spanners = c("Driven","Percent of Drivers"))
  ) %>%
  tab_style(
    style = cell_text(size = px(13)),
    locations = cells_body()
  ) %>%
  tab_style(
      style = cell_borders(
        sides = c("right"),
        color = "#BBBBBB",
        weight = px(2),
        style = "solid"
      ),
      locations = cells_body
      (
        columns = vars(td_div,vrs),
        rows = everything()
      )
    )  %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(2),
      style = "dotted"
    ),
    locations = cells_body
    (
      columns = vars(vrs_only,perc_vrs_only),
      rows = everything()
    )
  ) %>%
  data_color(
    columns = vars(td_div),
    colors = scales::col_numeric(
      palette = c("white", "gray25"),
      domain = c(0, 15000))
  )
  
ir_table

gtsave(ir_table,"./Plots/table_series_overlap.png",expand = 10,zoom = 3)

#PATCHWORK PACKAGE TO PRINT ALL PLOTS IN THE SAME IMAGE
pw_title <- c("iRacing - VRS GT Sprint Series")

design <- "
  13
  24
"

pw_vrs <- dr.division.vrs +
  dr.clubname.vrs +
  ir.division.vrs +
  ir.clubname.vrs +
  
  patchwork::plot_layout(design = design)+
  plot_annotation(
    title = pw_title,
    subtitle = "Unique Drivers - As of 21S2W7",
    caption = "Source: iRacing Member Site - Series Stats",
    theme = theme(title = element_text(family = "Cinzel"))
  )
ggsave(plot = pw_vrs, paste0("./Plots/",pw_title,".png"), dpi=320, width = 18.9,height = 14.96, units = "in")

pw_title <- c("iRacing - Fanatec GT3 Challenge - Fixed")
pw_fixed <- dr.division.fixed +
  dr.clubname.fixed +
  ir.division.fixed +
  ir.clubname.fixed +
  
  patchwork::plot_layout(design = design)+
  plot_annotation(
    title = pw_title,
    subtitle = "Unique Drivers - As of 21S2W7",
    caption = "Source: iRacing Member Site - Series Stats",
    theme = theme(title = element_text(family = "Cinzel"))
  )

ggsave(plot = pw_fixed, paste0("./Plots/",pw_title,".png"), dpi=320, width = 18.9,height = 14.96, units = "in")


#MODIFY PLOTS TO SAVE THEM INDIVIDUALLY
cap<- paste0("Source: iRacing Member Site - Series Stats","\nData current as of 21S2W7")

ir.clubname.vrs.2 <- ir.clubname.vrs + labs(title = "iRacing VRS Sprint Series", caption=cap)
ir.division.vrs.2 <- ir.division.vrs + labs(title = "iRacing VRS Sprint Series", caption=cap)
dr.clubname.vrs.2 <- dr.clubname.vrs + labs(title = "iRacing VRS Sprint Series", caption=cap)
dr.division.vrs.2 <- dr.division.vrs + labs(title = "iRacing VRS Sprint Series", caption=cap)

ir.clubname.fixed.2 <- ir.clubname.fixed + labs(title = "iRacing Fanatec GT3 Challenge - Fixed", caption=cap)
ir.division.fixed.2 <- ir.division.fixed + labs(title = "iRacing Fanatec GT3 Challenge - Fixed", caption=cap)
dr.clubname.fixed.2 <- dr.clubname.fixed + labs(title = "iRacing Fanatec GT3 Challenge - Fixed", caption=cap)
dr.division.fixed.2 <- dr.division.fixed + labs(title = "iRacing Fanatec GT3 Challenge - Fixed", caption=cap)

#Save individual plots
ggsave(filename= paste0("./Plots/Individual/vrs/","ir.clubname.vrs.png"), plot=ir.clubname.vrs.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename= paste0("./Plots/Individual/vrs/","ir.division.vrs.png"), plot=ir.division.vrs.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename= paste0("./Plots/Individual/vrs/","dr.clubname.vrs.png"), plot=dr.clubname.vrs.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename= paste0("./Plots/Individual/vrs/","dr.division.vrs.png"), plot=dr.division.vrs.2, dpi=320, width = 9.45,height = 7.48,units = "in")

ggsave(filename= paste0("./Plots/Individual/fixed/","ir.clubname.fixed.png"), plot=ir.clubname.fixed.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename= paste0("./Plots/Individual/fixed/","ir.division.fixed.png"), plot=ir.division.fixed.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename= paste0("./Plots/Individual/fixed/","dr.clubname.fixed.png"), plot=dr.clubname.fixed.2, dpi=320, width = 9.45,height = 7.48,units = "in")
ggsave(filename= paste0("./Plots/Individual/fixed/","dr.division.fixed.png"), plot=dr.division.fixed.2, dpi=320, width = 9.45,height = 7.48,units = "in")

