# libraries ####################################################################
library(openxlsx)
library(purrr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(extrafont)
library(tidyr)
library(ggpubr)
library(cowplot)
library(grid)
library(stringr)
library(tidyverse)
library(plyr)
library(zoo)
library(lubridate)
library(glue)
library(tinytex)

# filters ######################################################################

asia <- c("South-eastern Asia", "Southern Asia", "Eastern Asia", "Central Asia", "Western Asia")
america <- c("Central America", "Caribbean", "North America", "South America")
africa <- c("Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", "Western Africa")
europe <- c("Europe", "Mediterranean")

# data #########################################################################

main_file <- read.xlsx("C:\\Users\\postgres\\Desktop\\Visualization\\Hands On\\trail_of_tears.xlsx", detectDates = TRUE) %>%
            
            mutate(month = case_when((Reported.Month == "January")   ~ 1,
                                     (Reported.Month == "February")  ~ 2,
                                     (Reported.Month == "March")     ~ 3,
                                     (Reported.Month == "April")     ~ 4,
                                     (Reported.Month == "May")       ~ 5,
                                     (Reported.Month == "June")      ~ 6,
                                     (Reported.Month == "July")      ~ 7,
                                     (Reported.Month == "August")    ~ 8,
                                     (Reported.Month == "September") ~ 9,
                                     (Reported.Month == "October")   ~ 10,
                                     (Reported.Month == "November")  ~ 11,
                                     (Reported.Month == "December")  ~ 12),
                   
                   date = lubridate::make_date(year = Incident.year, month = month),
                   
                   region = case_when(Region.of.Incident %in% asia    ~ "Asia",
                                      Region.of.Incident %in% america ~ "America",
                                      Region.of.Incident %in% africa  ~ "Africa",
                                      Region.of.Incident %in% europe  ~ "Europe"))
  
View(main_file)

sum(duplicated(main_file)=='True')
sum(is.na(main_file$Region.of.Incident))
main_file$Region.of.Incident <- ifelse(is.nan(main_file$Region.of.Incident) | is.na(main_file$Region.of.Incident), "Unknown", main_file$Region.of.Incident)

Asia<- main_file[grepl("Asia", main_file$Region.of.Incident), ]
View(Asia)

#################################################### ALL ####################################################
# Trellis Plot Number of Incidents VS Number of Dead and Missing per Continent
data <- main_file%>%group_by(region, Incident.year, month, date) %>%
        arrange(date) %>% dplyr::summarise(count = n(), 
        total_dead_and_missing = sum(Total.Number.of.Dead.and.Missing, na.rm = TRUE))%>%drop_na()

trpl_numinc_vs_dm<-data %>% ggplot() +
                  geom_line(aes(x = date, y = count, color = ifelse(region == "Asia", "Asia", "Other Regions"))) +
                  geom_bar(aes(x = date, y = total_dead_and_missing / 15, fill = ifelse(region == "Asia", "Asia", "Other Regions")),
                           stat = "identity", alpha = 0.5) +
                  facet_wrap(~region, nrow = 4, ncol = 1) +
                  scale_x_date(date_labels = "%Y-%m-%d", 
                               breaks = seq(as.Date("2014-01-01"), as.Date("2022-12-31"), by = "3 months"),
                               labels = function(x) ifelse(format(x, "%Y") == "2013", "", format(x, "%Y"))) +
                  scale_y_continuous(sec.axis = sec_axis(~ . * 15, name = "Number of Dead and Missing")) +
                  scale_color_manual(name = "Lines (Left Vertical Axes)",
                                     values = c("Asia" = "red3", "Other Regions" = "black")) +
                  scale_fill_manual(name = "Bars (Right Vertical Axes)",
                                    values = c("Asia" = "salmon", "Other Regions" = "gray")) +
                  labs(title = "Number of Incidents and Number of Dead and Missing",
                       subtitle = "Asia vs. Other Continents, Monthly, 2014 - 2022",
                       x = "Time", y = "Number of Incidents") +
                  theme_few() +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
                        axis.text.y = element_text(size = 6, family = "Times New Roman"),
                        plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold"),
                        text = element_text(family = "Times New Roman"),
                        legend.position = "bottom")

                  
# Barplot number of incidents per region 
numinc <- main_file %>%
          filter(Region.of.Incident != "Unknown") %>%
          group_by(Region.of.Incident) %>% 
          dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

numinc$Region.of.Incident <- factor(numinc$Region.of.Incident, levels = numinc$Region.of.Incident[order(numinc$Number.of.Incidents, decreasing = FALSE)])
region_levels <- levels(numinc$Region.of.Incident)

bp_numinc <-ggplot(numinc, aes(x = Number.of.Incidents, y = Region.of.Incident, fill = factor(Region.of.Incident, levels = region_levels))) +
            geom_vline(xintercept = c(1000, 3000, 2000), linetype = 'dashed', colour = "gray", alpha = 0.7) +
            geom_bar(stat = "identity", width = 0.5) +
            labs(title = "Total Number of Incidents per Region of Incident", subtitle = "2014 - 2022", x = "", y = "") +
            geom_text(aes(label = Number.of.Incidents), position = position_dodge(0.9), hjust = -0.1, size = 2.5, angle = 0) +
            scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 1000), labels = function(x) paste0(x / 1000, "K")) +
            scale_fill_manual(values = ifelse(grepl("Asia", region_levels), 'red3', 'lightgray'), breaks = region_levels) +
            guides(fill = "none") +
            theme_tufte()


# Number of Incidents per Year and Continent
trpl_cond_df<-main_file %>% filter(region != "NA") %>%
  group_by(region,Incident.year) %>%
  dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

trpl_cont_numic <- ggplot(trpl_cond_df, aes(x = Incident.year, y = round(Number.of.Incidents), color = region)) +
                  geom_line(size = 0.5) +
                  geom_point(size = 0.6) +
                  facet_wrap(~ region, nrow = 4, ncol = 1) +
                  theme_few() +
                  theme(
                    axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
                    axis.text.y = element_text(size = 6, family = "Times New Roman"),
                    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
                    text = element_text(family = "Times New Roman"),
                    legend.position = "none"
                  ) +
                  labs(
                    title = "Number of Incidents per Year & Continent",
                    x = "", y = ""
                  ) +
                  scale_x_continuous(
                    breaks = seq(2014, 2022, by = 1),
                    labels = seq(2014, 2022, by = 1)
                  ) +
                  ylim(0, 1000)+
                  scale_color_manual(values = c("Asia" = "red3", "gray")) +
                  theme(plot.title = element_text(hjust = 0.5))


#Average Number of Deaths per Incident by Year and Continent
eee<-as.data.frame(aggregate(Number.of.Dead~Incident.year+region, data=main_file, FUN=mean))

trpl_cont2 <- ggplot(eee, aes(x = Incident.year, y = round(Number.of.Dead), color = region)) +
              geom_line(size = 0.5) +
              geom_point(size = 0.6) +
              facet_wrap(~ region, nrow = 4, ncol = 1) +
              theme_few() +
              theme(
                axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
                axis.text.y = element_text(size = 6, family = "Times New Roman"),
                plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
                text = element_text(family = "Times New Roman"),
                legend.position = "none"
              ) +
              labs(
                title = "Average Deaths per Incident by Year & Continent",
                x = "", y = ""
              ) +
              scale_x_continuous(
                breaks = seq(2014, 2022, by = 1),
                labels = seq(2014, 2022, by = 1)
              ) +
              ylim(0, 30)+
              scale_color_manual(values = c("Asia" = "red3", "gray")) +
              theme(plot.title = element_text(hjust = 0.5))


#barplot Total Deaths per cause of death without Asia
b_no_asia<-as.data.frame(aggregate(Number.of.Dead~Processed.Cause.of.Death, data=main_file[!main_file$region == "Asia", ], FUN=sum))
b_no_asia['prc']<-round(b_no_asia$Number.of.Dead/sum(b_no_asia$Number.of.Dead),2)
sort(b_no_asia$prc)

b_no_asia$Processed.Cause.of.Death <- factor(b_no_asia$Processed.Cause.of.Death, levels = b_no_asia$Processed.Cause.of.Death[order(b_no_asia$Number.of.Dead, decreasing = TRUE)])

bp_cod_no_asia<-ggplot(b_no_asia, aes(x = Processed.Cause.of.Death, y =Number.of.Dead )) +
                geom_hline(yintercept=c(2000, 4000, 6000,8000,10000,12000,14000), linetype='dashed',colour="salmon",alpha=0.4)+
                geom_bar(stat = "identity", width = 0.5,fill='lightgray') +
                labs(title = expression("Total Number of Deaths per Cause of Death:"~bold("Other Continents")),,subtitle="2014 - 2022",x = "", y = "") +
                geom_text(aes(label = Number.of.Dead),position = position_dodge(0.9),
                          hjust = 0.5,vjust=-1,size=2.5,angle = 0)+
                scale_y_continuous(limits = c(0, 15000), breaks = seq(0, 15000, by = 2000),
                                   labels = function(x) paste0(x/1000, "K"))+
                theme_tufte()

#Number of deaths per region of incident 
d<-as.data.frame(aggregate(Number.of.Dead~Region.of.Incident, data=main_file, FUN=sum))
d$Region.of.Incident<- factor(d$Region.of.Incident, levels = d$Region.of.Incident[order(d$Number.of.Dead, decreasing = FALSE)])

d <- d %>%
  mutate(Region_Color = ifelse(grepl("Asia", Region.of.Incident), "Asia", "Other"))

bp4<-ggplot(d, aes(x = Number.of.Dead, y = Region.of.Incident, fill = Region_Color)) +
    geom_vline(xintercept = c(1000,2000,3000, 4000,5000, 6000,7000, 8000, 9000, 10000), linetype = 'dashed', colour = "salmon", alpha = 0.4) +
    geom_bar(stat = "identity", width = 0.5) +
    labs(title = "Total Number of Deaths per Region of Incident", subtitle = "2014 - 2022", x = "", y = "") +
    geom_text(aes(label = Number.of.Dead), position = position_dodge(0.9), hjust = -0.1, size = 2.5, angle = 0) +
    scale_x_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 1000), labels = function(x) paste0(x / 1000, "K")) +
    scale_fill_manual(values = c("Asia" = "red3", "Other" = "lightgray")) +
    guides(fill = 'none')+
    theme_tufte()

#################################################### ASIA ###################################################

# Trellis plot Number of Incidents per region of incident Asia over time
numinc_grouped <- main_file[grepl("Asia", main_file$Region.of.Incident), ] %>%
                  filter(Region.of.Incident != "Unknown") %>%
                  group_by(Region.of.Incident,Incident.year) %>%
                  dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

trpl_asia_only_numinc <- ggplot(numinc_grouped, aes(x = Incident.year, y = Number.of.Incidents)) +
                        geom_line(aes(color = ifelse(grepl("Asia", Region.of.Incident), "Asia", "Other")), size = 0.5, alpha = 0.8, show.legend = FALSE) +
                        geom_point(aes(color = ifelse(grepl("Asia", Region.of.Incident), "Asia", "Other")), size = 0.5) +
                        facet_wrap(~ Region.of.Incident, nrow = 3, ncol = 2, scales = "free_y") +
                        theme_few() +
                        theme(
                          axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
                          axis.text.y = element_text(size = 6, family = "Times New Roman"),
                          plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold"),
                          text = element_text(family = "Times New Roman"),
                          legend.position = "none") +
                          labs(title = expression("Number of Incidents per Region of Incident:"~bold("Asia")),
                          x = "", y = "") +
                        ylim(0,900)+
                        scale_x_continuous(breaks = seq(2014, 2022, by = 1), labels = seq(2014, 2022, by = 1)) +
                        theme(plot.title = element_text(hjust = 0.5))


# Trellis plot Total deaths pre region of incident Asia over time
tpa_df <- Asia
tpa_df$Number.of.Dead[is.na(tpa_df$Number.of.Dead)] <- 0

tpa_df_grouped <- tpa_df%>%
                  group_by(Region.of.Incident,Incident.year) %>%
                  dplyr::summarise(Number.of.Dead = sum(Number.of.Dead), .groups = 'drop')

trpl_asia_only <- ggplot(tpa_df_grouped, aes(x = Incident.year, y = Number.of.Dead)) +
                  geom_line(aes(color = ifelse(grepl("Asia", Region.of.Incident), "Asia", "Other")), size = 0.5, alpha = 0.8, show.legend = FALSE) +
                  geom_point(aes(color = ifelse(grepl("Asia", Region.of.Incident), "Asia", "Other")), size = 0.5) +
                  facet_wrap(~ Region.of.Incident, nrow = 3, ncol = 2, scales = "free_y") +
                  theme_few() +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
                        axis.text.y = element_text(size = 6, family = "Times New Roman"),
                        plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold"),
                        text = element_text(family = "Times New Roman"),
                        legend.position = "none") +
                        labs(title = expression("Total Deaths per Region of Incident:"~bold("Asia")),
                        x = "", y = "") +
                  ylim(0,900)+
                  scale_x_continuous(breaks = seq(2014, 2022, by = 1), labels = seq(2014, 2022, by = 1)) +
                  theme(plot.title = element_text(hjust = 0.5))


#barplot Total Deaths per cause of death (ASIA)
tdpcd<-as.data.frame(aggregate(Number.of.Dead~Processed.Cause.of.Death, data=Asia, FUN=sum))

tdpcd$Processed.Cause.of.Death <- factor(tdpcd$Processed.Cause.of.Death, levels = tdpcd$Processed.Cause.of.Death[order(tdpcd$Number.of.Dead, decreasing = TRUE)])

tdpcd['prc']<- round(tdpcd$Number.of.Dead/sum(tdpcd$Number.of.Dead),2)

bp_asia_cause_of_death<-ggplot(tdpcd, aes(x = Processed.Cause.of.Death, y =Number.of.Dead )) +
                        geom_hline(yintercept=c(500,1000,1500,2000), linetype='dashed',colour="gray",alpha=0.5)+
                        geom_bar(stat = "identity", width = 0.5,fill='red3') +
                        labs(title = expression("Total Number of Deaths per Cause of Death:"~bold("Asia")) ,subtitle="2014 - 2022",x = "", y = "") +
                        geom_text(aes(label = Number.of.Dead),position = position_dodge(0.9),
                        hjust = 0.5,vjust=-1,size=2.5,angle = 0)+
                        scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500),
                        labels = function(x) paste0(x/1000, "K"))+
                        theme_tufte()


#Scatterplot: Population per Incident VS Number of Deceased 
f<-as.data.frame(aggregate(Number.of.Dead~Incident.year+Region.of.Incident+Incident.year+Reported.Month+Processed.Cause.of.Death+Number.of.Survivors+Total.Number.of.Dead.and.Missing, data=Asia, FUN=sum))
f$Total.of.People <- rowSums(f[, c("Number.of.Survivors", "Total.Number.of.Dead.and.Missing")], na.rm = TRUE)
f['Death.Rate']<- (f$Number.of.Dead/ f$Total.of.People)

scpl_asia<-ggplot(f, aes(x = Total.of.People, y = Number.of.Dead,color = Region.of.Incident)) +
          geom_point() + labs(title = expression("Population per Incident VS Number of Deceased:"~bold("Asia")),subtitle="by Cause of Death & Region ( 2014-2022 )", x = "Population per Incident", y = "Number of Deceased") +
          scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 10))+ 
          scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 100))+
          geom_text_repel(aes(label = Processed.Cause.of.Death), size = 3, nudge_x = 10, nudge_y = 5,max.overlaps = 10,show.legend = FALSE) +
          scale_color_manual(values = c("#EE1289", "orchid2","coral1", "purple4","dodgerblue"),name='Region')+
          theme_tufte()

#barplot Total Deaths per Migration Route Asia
c<-as.data.frame(aggregate(Number.of.Dead~Processed.Migration.route, data=Asia, FUN=sum))
options(scipen = 999)
c['percentage']<-round(c$Number.of.Dead/sum(c$Number.of.Dead),2)
c$Processed.Migration.route <- factor(c$Processed.Migration.route, levels = c$Processed.Migration.route[order(c$Number.of.Dead, decreasing = FALSE)])


bp_asia_mig_route<-ggplot(c, aes(x =Number.of.Dead , y =Processed.Migration.route )) +
                  geom_vline(xintercept=c(500, 1000, 1500, 2000,2500,3000),
                  linetype='dashed',colour="gray",alpha=0.5)+
                  geom_bar(stat = "identity", width = 0.5,fill='red3') +
                  labs(title = expression("Total Number of Deaths per Migration Route:"~bold("Asia")) ,subtitle="2014 - 2022",x = "", y = "") +
                  geom_text(aes(label = Number.of.Dead),position = position_dodge(0.9),
                  hjust = -0.1,size=2.5,angle = 0)+
                  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500),
                  labels = function(x) paste0(x/1000, "K"))+
                  theme_tufte()


#Line plot average deaths per incident Asia VS Rest
rest<- main_file
rest$region <- ifelse(rest$region != "Asia", "Other Regions", rest$region)
e<-as.data.frame(aggregate(Number.of.Dead~Incident.year+region, data=rest, FUN=mean))

lp_Asia_VS_other_1<-ggplot(e, aes(x = Incident.year, y = round(Number.of.Dead), group =region, color = region)) + 
                    geom_line()+geom_point() + labs(title = "Average Deaths per Incident by Region: Asia VS Other Regions", x = "", y = "") + 
                    scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3))+theme_tufte()+
                    scale_x_continuous(breaks = seq(min(e$Incident.year), max(e$Incident.year), by = 1))+
                    scale_color_manual(values = c("Asia" = "red", "Other Regions" = "gray35"), name = "Region of Incident")


#Line plot number of incidents Asia VS Rest
rest_avo<- main_file
rest_avo$region <- ifelse(rest_avo$region != "Asia", "Other Regions", rest_avo$region)

e_avo<-rest_avo %>% filter(region != "NA") %>%
      group_by(region,Incident.year) %>%
      dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

lp_avo<-ggplot(e_avo, aes(x = Incident.year, y = round(Number.of.Incidents), group = region, color = region)) + 
        geom_line()+geom_point() + labs(title = "Number of Incidents by Region: Asia VS Other Regions", x = "", y = "") + 
        scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500))+theme_tufte()+
        scale_x_continuous(breaks = seq(min(e$Incident.year), max(e$Incident.year), by = 1))+
        scale_color_manual(values = c("Asia" = "red", "Other Regions" = "gray35"), name = "Region of Incident")

# Heatmap Region of Incident Vs Country of Origin
f1<-as.data.frame(aggregate(Number.of.Dead~Region.of.Incident+Processed.Country.of.Origin+Number.of.Survivors+Total.Number.of.Dead.and.Missing, data=Asia, FUN=sum))
f1$Total.of.People <- rowSums(f1[, c("Number.of.Survivors", "Total.Number.of.Dead.and.Missing")], na.rm = TRUE)
df_hm<-f1%>%group_by(Region.of.Incident,Processed.Country.of.Origin)%>%dplyr::summarise(Total.of.People = round(mean(Total.of.People)),Number.of.Dead.and.Missing=round(mean(Total.Number.of.Dead.and.Missing)),.groups = 'drop')

grid <- expand.grid(Region.of.Incident = unique(df_hm$Region.of.Incident),
        Processed.Country.of.Origin = unique(df_hm$Processed.Country.of.Origin))

complete_data <- merge(grid, df_hm, all.x = TRUE)
complete_data$Total.of.People[is.na(complete_data$Total.of.People)] <- 0

heatmap <-ggplot(complete_data, aes(x = Region.of.Incident, y = Processed.Country.of.Origin, fill = Total.of.People)) +
          geom_tile() +
          labs(x = "", y = "", title = expression("Average Number of People per Incident:"~ bold("Asia")~"( 2014 - 2022 )"), subtitle = " by Country of Origin & Region of Incident") +
          theme( plot.title = element_text(hjust = 0.5, face = "bold"),
                 plot.subtitle = element_text(hjust = 0.5, size = 10),
                 axis.text = element_text(family = "Times New Roman"),
                 axis.title = element_text(family = "Times New Roman"),
                 legend.text = element_text(family = "Times New Roman")) +
          scale_fill_gradient(low = "gray95",  high = "red3",  
                              name = "Number of People",
                              limits = c(0, max(complete_data$Total.of.People)),
                              breaks = seq(0, max(complete_data$Total.of.People), by = 20),  
                              na.value = 0) + 
          theme_tufte()


#Grouped Barplot Average Number of People per incident Vs Average Number of Dead per incident: Asia

f1_grouped_m<-f1%>%group_by(Region.of.Incident) %>%
              dplyr::summarise(Total.Number.of.Dead.and.Missing = round(mean(Total.Number.of.Dead.and.Missing)),
              Total.of.People=round(mean(Total.of.People)), .groups = 'drop')

f1_grouped_m['prc_dead_n_missing']<-f1_grouped_m$Total.Number.of.Dead.and.Missing/f1_grouped_m$Total.of.People
f1_grouped_m$Region.of.Incident <- factor(f1_grouped_m$Region.of.Incident, levels = f1_grouped_m$Region.of.Incident[order(f1_grouped_m$Total.Number.of.Dead.and.Missing, decreasing = TRUE)])

bp_adm<-ggplot(f1_grouped_m, aes(x = Region.of.Incident)) +
  geom_hline(yintercept = c(10, 20, 30,40,50,60,70), linetype = 'dashed', colour = "gray", alpha = 0.8)+
  geom_bar(aes(y = Total.of.People, fill = "Total"), stat = "identity", width = 0.35, position = "dodge", show.legend = TRUE) +
  geom_bar(aes(y = Total.Number.of.Dead.and.Missing, fill = "Dead and Missing"), stat = "identity", width = 0.35, position = "dodge", show.legend = TRUE) +
  labs(x = "Region of Incident", y = "", subtitle = "2014 - 2022", title = expression(bold("Average")~"Number of Dead and Missing vs."~ bold("Average")~ "Number of People per Incident:" ~ bold("Asia"))) +
  scale_fill_manual(values = c("Total" = "coral1", "Dead and Missing" = "red3"), labels = c("Dead and Missing", "Total")) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  guides(fill = guide_legend(title = NULL))+
  theme(legend.position = "right")+
  theme_tufte() 

# Heatmap Migration Route Vs Country of Origin
f11<-as.data.frame(aggregate(Number.of.Dead~Processed.Migration.route+Processed.Country.of.Origin+Number.of.Survivors+Total.Number.of.Dead.and.Missing, data=Asia, FUN=sum))
f11$Total.of.People <- rowSums(f11[, c("Number.of.Survivors", "Total.Number.of.Dead.and.Missing")], na.rm = TRUE)
df_hm1<-f11%>%group_by(Processed.Migration.route,Processed.Country.of.Origin)%>%dplyr::summarise(Total.of.People = round(mean(Total.of.People)),Number.of.Dead.and.Missing=round(mean(Total.Number.of.Dead.and.Missing)),.groups = 'drop')

grid <- expand.grid( Processed.Migration.route = unique(df_hm1$Processed.Migration.route),
        Processed.Country.of.Origin = unique(df_hm1$Processed.Country.of.Origin))

complete_data <- merge(grid, df_hm1, all.x = TRUE)
complete_data$Total.of.People[is.na(complete_data$Total.of.People)] <- 0

heatmap2 <- ggplot(complete_data, aes(x = Processed.Migration.route, y = Processed.Country.of.Origin, fill = Total.of.People)) +
            geom_tile() +
            labs(x = "", y = "", title = expression("Average Number of People per Incident:"~ bold("Asia")~"( 2014 - 2022 )"), subtitle = " by Country of Origin & Migration Route") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(family = "Times New Roman"),
                  axis.title = element_text(family = "Times New Roman"),
                  legend.text = element_text(family = "Times New Roman")) +
            scale_fill_gradient(low = "gray95",high = "red3",name = "Number of People",
                                limits = c(0, max(complete_data$Total.of.People)),
                                breaks = seq(0, max(complete_data$Total.of.People), by = 20),  
                                na.value = 0) + 
            theme_tufte()


#line plot number of incidents per month per month asia vs other
a <- main_file %>%
  filter(str_detect(Region.of.Incident, "Asia")) %>% 
  group_by(Reported.Month) %>%
  dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

a_other <- main_file %>%
  filter(!str_detect(Region.of.Incident, "Asia")) %>% 
  group_by(Reported.Month) %>%
  dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

lp_tot_inc_per_month <- ggplot() +
                        geom_hline(yintercept = c(100, 200, 300, 400,500,600,700,800,900,1000,1100), linetype = 'dashed', colour = "salmon", alpha = 0.4) +
                        geom_line(data = a, aes(x = Reported.Month, y = Number.of.Incidents, group = 1, colour = "Asia"), size = 0.7) +
                        geom_line(data = a_other, aes(x = Reported.Month, y = Number.of.Incidents, group = 1, colour = "Other Regions"), size = 0.7) +
                        geom_text_repel(data = a, aes(x = Reported.Month, y = Number.of.Incidents, label = Number.of.Incidents), 
                                        position = position_dodge(0.9), hjust = 0.5, vjust = -1, size = 2.5) +
                        geom_text_repel(data = a_other, aes(x = Reported.Month, y = Number.of.Incidents, label = Number.of.Incidents),
                                        position = position_dodge(0.9), hjust = 0.5, vjust = -1, size = 2.5) +
                        labs(title = expression("Number of Incidents per Month"), subtitle = "2014 - 2022", x = "", y = "") +
                        scale_colour_manual(values = c("Asia" = "red3", "Other Regions" = "gray"),name="Region") +
                        scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, by = 100)) +
                        scale_x_discrete(limits = month_order) +
                        theme_tufte()


#lineplot number of incidents per year asia vs other
a_year <- main_file %>%
  filter(str_detect(Region.of.Incident, "Asia")) %>% 
  group_by(Incident.year) %>%
  dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

a_year_other <- main_file %>%
  filter(!str_detect(Region.of.Incident, "Asia")) %>% 
  group_by(Incident.year) %>%
  dplyr::summarise(Number.of.Incidents = n(), .groups = 'drop')

lp_tot_inc_per_year <- ggplot() +
                        geom_hline(yintercept = c(500, 1000, 1500, 2000), linetype = 'dashed', colour = "salmon", alpha = 0.4) +
                        geom_line(data = a_year, aes(x = Incident.year, y = Number.of.Incidents, group = 1, colour = "Asia"), size = 0.7) +
                        geom_line(data = a_year_other, aes(x = Incident.year, y = Number.of.Incidents, group = 1, colour = "Other Regions"), size = 0.7) +
                        geom_point(data = a_year, aes(x = Incident.year, y = Number.of.Incidents), colour = "red3") +
                        geom_point(data = a_year_other, aes(x = Incident.year, y = Number.of.Incidents), colour = "gray") +
                        geom_text_repel(data = a_year, aes(x = Incident.year, y = Number.of.Incidents, label = Number.of.Incidents), 
                                        position = position_dodge(0.9), hjust = 0.5, vjust = -1, size = 2.5) +
                        geom_text_repel(data = a_year_other, aes(x = Incident.year, y = Number.of.Incidents, label = Number.of.Incidents),
                                        position = position_dodge(0.9), hjust = 0.5, vjust = -1, size = 2.5) +
                        labs(title = expression("Number of Incidents per Year"), subtitle = "2014 - 2022", x = "", y = "") +
                        scale_colour_manual(values = c("Asia" = "red3", "Other Regions" = "gray"),name="Region") +
                        scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500)) +
                        scale_x_continuous(breaks = seq(2014, 2022, by = 1)) +
                        theme_tufte()


################################################ VIOLENCE ####################################################

#Barplot Number of Violence-related deaths by Continent 
violence <- main_file[main_file$Processed.Cause.of.Death == "Violence", ]
violence$Number.of.Dead[is.na(violence$Number.of.Dead)] <- 0

violence_grouped <- violence %>% filter(region != "NA") %>% group_by(region) %>%
                    dplyr::summarise(Number.of.Dead = sum(Number.of.Dead), .groups = 'drop')

violence_grouped$region <- factor(violence_grouped$region, levels = violence_grouped$region[order(violence_grouped$Number.of.Dead, decreasing = FALSE)])
bp_violence <- ggplot(violence_grouped, aes(x = Number.of.Dead, y = region,fill = region)) +
              geom_vline(xintercept = c(500, 1000, 1500,2000), linetype = 'dashed', colour = "gray", alpha = 0.7) +
              geom_bar(stat = "identity", width = 0.5) +
              labs(title = expression("Total"~bold("Violence-related") ~ "Deaths per Continent"), subtitle = "2014 - 2022", x = "", y = "") +
              geom_text(aes(label = Number.of.Dead), position = position_dodge(0.9), hjust = -0.1, size = 2.5, angle = 0) +
              scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500), labels = function(x) paste0(x / 1000, "K")) +
              scale_fill_manual(values = c('Asia' = 'red3', 'Africa' = 'gray', 'Europe' = 'gray', 'America' = 'gray'))+
              guides(fill = "none")+
              theme_tufte()


# Violence Related deaths pear migration route

violence_grouped_mr <- violence %>% filter(Processed.Migration.route != "NA") %>%
                      group_by(Processed.Cause.of.Death,Processed.Migration.route) %>%
                      dplyr::summarise(Number.of.Dead = sum(Number.of.Dead), .groups = 'drop')

violence_grouped_mr$Processed.Migration.route <- factor(violence_grouped_mr$Processed.Migration.route, levels = violence_grouped_mr$Processed.Migration.route[order(violence_grouped_mr$Number.of.Dead, decreasing = FALSE)])

bp_violence_mr <- ggplot(violence_grouped_mr, aes(x = Number.of.Dead, y = Processed.Migration.route, fill = Processed.Migration.route)) +
                  geom_vline(xintercept = c(500, 1000, 1500, 2000), linetype = 'dashed', colour = "gray", alpha = 0.7) +
                  geom_bar(stat = "identity", width = 0.5) +
                  labs(title = expression("Total" ~ bold("Violence-related") ~ "Deaths per Migration Route"), subtitle = "2014 - 2022", x = "", y = "") +
                  geom_text(aes(label = Number.of.Dead), position = position_dodge(0.9), hjust = -0.1, size = 2.5, angle = 0) +
                  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500), labels = function(x) paste0(x / 1000, "K")) +
                  scale_fill_manual(values = c('Afghanistan to Iran' = 'red3',
                                               'Belarus-EU border' = 'gray',
                                               'Central Mediterranean' = 'gray',
                                               'DRC to Uganda' = 'gray',
                                               'Darien' = 'gray',
                                               'Eastern Mediterranean' = 'gray',
                                               'English Channel to the UK' = 'gray',
                                               'Haiti to Dominican Republic' = 'gray',
                                               'Horn of Africa to Yemen crossing' = 'red3',
                                               'Iran to Türkiye' = 'red3',
                                               'Italy to France' = 'gray',
                                               'Sahara Desert crossing' = 'gray',
                                               'Syria to Türkiye' = 'red3',
                                               'Türkiye-Europe land route' = 'red3',
                                               'US-Mexico border crossing' = 'gray',
                                               'Ukraine to Europe' = 'gray',
                                               'Unknown' = 'gray',
                                               'Venezuela to Caribbean' = 'gray',
                                               'Western Africa / Atlantic route to the Canary Islands' = 'gray',
                                               'Western Balkans' = 'gray',
                                               'Western Mediterranean' = 'gray')) +
                                                guides(fill = "none") +
                                                theme_tufte()


#trellis Number of Violence-related deaths per year by Continent 
violence_grouped2 <- violence %>% filter(region != "NA") %>%
                    group_by(region,Incident.year) %>%
                    dplyr::summarise(Number.of.Dead = sum(Number.of.Dead), .groups = 'drop')

trpl_violence_deaths_by_cont <- ggplot(violence_grouped2, aes(x=Incident.year, y = Number.of.Dead,color = region)) +
                                geom_line(size=0.5) +geom_point(size=0.6) +
                                facet_wrap(~region,nrow=4,ncol=1) + theme_few() +
                                theme( axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
                                axis.text.y = element_text(size = 6, family = "Times New Roman"),
                                plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
                                text = element_text(family = "Times New Roman"), legend.position = 'none') +
                                labs(title = expression("Total"~bold("Violence-related") ~ "Deaths per Year & Continent"),
                                x = "",y = "") + scale_x_continuous(breaks = seq(2014, 2022, by = 1),
                                labels = seq(2014, 2022, by = 1)) +  scale_y_log10()+
                                scale_color_manual(values = c("Asia" = "red", "darkgray"))+
                                theme(plot.title = element_text(hjust = 0.5))


# Bar plot Violence-Related Deaths per continent
violence_grouped3<-violence %>% filter(region != "NA") %>%
                  group_by(region) %>%
                  dplyr::summarise(Number.of.Dead = sum(Number.of.Dead), .groups = 'drop')

violence_grouped3$region <- factor(violence_grouped3$region, levels = violence_grouped3$region[order(violence_grouped3$Number.of.Dead, decreasing = FALSE)])
violence_grouped3 <- violence_grouped3 %>% mutate(Region_Color = ifelse(grepl("Asia", region), "Asia", "Other"))

bp_violence <-  ggplot(violence_grouped3, aes(x = Number.of.Dead, y = region)) +
                geom_vline(xintercept = c(500, 1000, 1500,2000), linetype = 'dashed', colour = "gray", alpha = 0.7) +
                geom_bar(aes(fill = Region_Color), stat = "identity", width = 0.5) +
                labs(title = expression("Total Number of"~bold("Violence-related")~ "Deaths per Region of Incident"), subtitle = "2014 - 2022", x = "", y = "") +
                geom_text(aes(label = Number.of.Dead), position = position_dodge(0.9), hjust = -0.1, size = 2.5, angle = 0) +
                scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500), labels = function(x) paste0(x / 1000, "K")) +
                scale_fill_manual(values = c("Asia" = "red3", "Other" = "lightgray")) +
                guides(fill = 'none')+
                theme_tufte()

#trellis plot Violence-Related Deaths
violence_grouped4 <- violence %>% filter(Region.of.Incident != "NA") %>%
  group_by(Region.of.Incident,Incident.year) %>%
  dplyr::summarise(Number.of.Dead = sum(Number.of.Dead), .groups = 'drop')

desired_order <- c("Europe","Mediterranean" ,"Caribbean","North America",
                   "Central America","South America", "Eastern Africa","Middle Africa",
                   "Northern Africa","Southern Africa","Western Africa",
                   "Eastern Asia","South-eastern Asia",
                   "Southern Asia","Western Asia" )

violence_grouped4$Region.of.Incident <- factor(violence_grouped4$Region.of.Incident, levels = desired_order)

trpl_violence_all <-ggplot(violence_grouped4, aes(x = Incident.year, y = Number.of.Dead)) +
                    geom_line(aes(color = ifelse(grepl("Asia", Region.of.Incident), "Asia", "Other")), size = 0.5, alpha = 0.8, show.legend = FALSE) +
                    geom_point(aes(color = ifelse(grepl("Asia", Region.of.Incident), "Asia", "Other")), size = 0.5) +
                    facet_wrap(~ Region.of.Incident, nrow = 5, ncol = 3, scales = "free_y") +
                    scale_color_manual(values = c(Asia = "red", Other = "gray30")) +
                    theme_few() +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7, family = "Times New Roman"),
                      axis.text.y = element_text(size = 6, family = "Times New Roman"),
                      plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold"),
                      text = element_text(family = "Times New Roman"),
                      legend.position = "none") +
                      labs(title = "Total Violence-related Deaths per Region of Incident",
                      x = "", y = "") +
                    ylim(0,900)+
                    scale_x_continuous(breaks = seq(2014, 2022, by = 1), labels = seq(2014, 2022, by = 1)) +
                    theme(plot.title = element_text(hjust = 0.5))
                  
                  
