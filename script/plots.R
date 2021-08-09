library(tidyverse)
library(cowplot)
library(gridExtra)
library(data.table)
library(base)

# argentina <- read.csv("data/Argentina/all_data.csv", stringsAsFactors = FALSE)
# names(argentina) <- c("age", "gender", "deaths", "orphans")
# argentina$country <- rep("Argentina", length(argentina$age))
# argentina$age[which(argentina$age == "00-19")] = "0-19"
# 
# brazil <- read.csv("data/Brazil/all_data.csv", stringsAsFactors = FALSE)
# names(brazil) <- c("age", "gender", "deaths", "pop","orphans","mortality")
# brazil$country <- rep("Brazil", length(brazil$age))

colombia <- read.csv("DATA/Colombia/all_data.csv", stringsAsFactors = FALSE)
names(colombia) <- c("age", "gender", "deaths", "pop","orphans","mortality")
colombia$country <- rep("Colombia", length(colombia$age))

england <- read.csv("DATA/England&Wales/england_wales_all_data.csv", stringsAsFactors = FALSE)
#england<-data.table(england)
#england <- england[england$age, england$gender, england$deaths, england$orphans,england$pop,england$mortality,]
names(england) <- c("age", "gender", "deaths", "pop","orphans","mortality")
england$country <- rep("England & Wales", length(england$age))

france <- read.csv("DATA/France/all_data.csv", stringsAsFactors = FALSE)
#france <- select(france, age, gender, nb_deaths, nb_orphans)
names(france) <- c("age", "gender", "deaths", "pop","orphans","mortality")
france$country <- rep("France", length(france$age))

germany <- read.csv("DATA/Germany/all_data.csv", stringsAsFactors = FALSE)
names(germany) <- c("age", "gender", "deaths", "pop","orphans","mortality")
germany$country <- rep("Germany", length(germany$age))

# india <- read.csv("DATA/India/all_data.csv", stringsAsFactors = FALSE)
# names(india) <- c("age", "gender", "deaths", "pop","orphans","mortality")
# india$country <- rep("India", length(india$age))
# 
# iran <- read.csv("DATA/Iran/all_data.csv", stringsAsFactors = FALSE)
# names(iran) <- c("age", "gender", "deaths", "pop","orphans","mortality")
# iran$country <- rep("I.R. Iran", length(iran$age))

italy <- read.csv("DATA/Italy/all_data.csv", stringsAsFactors = FALSE)
#italy <- select(italy, age, gender, max_deaths, nb_orphans)
names(italy) <- c("age", "gender", "deaths", "pop","orphans","mortality")
italy$country <- rep("Italy", length(italy$age))

# kenya <- read.csv("DATA/Kenya/all_data.csv", stringsAsFactors = FALSE)
# names(kenya) <- c("age", "gender", "deaths", "pop","orphans","mortality")
# kenya$country <- rep("Kenya", length(kenya$age))

malawi <- read.csv("DATA/Malawi/all_data.csv", stringsAsFactors = FALSE)
#malawi <- select(malawi, age, gender, COVID19_deaths, nb_orphans)
names(malawi) <- c("age", "gender", "deaths", "pop","orphans","mortality")
malawi$country <- rep("Malawi", length(malawi$age))

mexico <- read.csv("DATA/Mexico/all_data.csv", stringsAsFactors = FALSE)
names(mexico) <- c("age", "gender", "deaths", "pop","orphans","mortality")
mexico$country <- rep("Mexico", length(mexico$age))

# nigeria <- read.csv("DATA/Nigeria/all_data.csv", stringsAsFactors = FALSE)
# names(nigeria) <- c("age", "gender", "deaths", "pop","orphans","mortality")
# nigeria$country <- rep("Nigeria", length(nigeria$age))

peru <- read.csv("DATA/Peru/all_data.csv", stringsAsFactors = FALSE)
names(peru) <- c("age", "gender", "deaths", "pop","orphans","mortality")
peru$country <- rep("Peru", length(peru$age))

poland <- read.csv("DATA/Poland/all_data.csv", stringsAsFactors = FALSE)
#poland <- select(poland, age, gender, max_deaths, nb_orphans)
names(poland) <- c("age", "gender", "deaths", "pop","orphans","mortality")
poland$country <- rep("Poland", length(poland$age))

# russia <- read.csv("DATA/Russia/all_data.csv", stringsAsFactors = FALSE)
# names(russia) <- c("age", "gender", "deaths", "pop","orphans","mortality")
# russia$country <- rep("Russian Federation", length(russia$age))

southafrica <- read.csv("DATA/SouthAfrica/all_data.csv", stringsAsFactors = FALSE)
names(southafrica) <- c("age", "gender", "deaths", "pop","orphans","mortality")
southafrica$country <- rep("South Africa", length(southafrica$age))

spain <- read.csv("DATA/Spain/all_data.csv", stringsAsFactors = FALSE)
#spain <- select(spain, age, gender, max_deaths, nb_orphans)
names(spain) <- c("age", "gender", "deaths", "pop","orphans","mortality")
spain$country <- rep("Spain", length(spain$age))

usa <- read.csv("DATA/USA/all_data.csv", stringsAsFactors = FALSE)
#usa <- select(usa, age, gender, max_deaths, nb_orphans)
names(usa) <- c("age", "gender", "deaths", "pop","orphans","mortality")
usa$country <- rep("USA", length(usa$age))


# zimbabwe <- read.csv("DATA/Zimbabwe/all_data.csv", stringsAsFactors = FALSE)
# zimbabwe <- select(zimbabwe, age, gender, COVID19_deaths, nb_orphans)
# names(zimbabwe) <- c("age", "gender", "deaths", "orphans","pop","mortality")
# zimbabwe$country <- rep("Zimbabwe", length(zimbabwe$age))
# zimbabwe$age[zimbabwe$age == "1-20"] <-"0-20"

#data <- rbind(argentina, brazil, colombia, england, france, germany, india, iran, 
#              italy, kenya, malawi, mexico, nigeria, peru, poland, russia, southafrica, 
#              spain, usa, zimbabwe)

data <- rbind( france,colombia,england,germany,
             italy, malawi, mexico, peru, poland, southafrica,
             spain, usa)



data$gender <- ifelse(data$gender == "female", "Female", data$gender)
data$gender <- ifelse(data$gender == "male", "Male", data$gender)
data=data%>%select(-mortality)
write_csv(path = 'DATA/country_all.csv', data)
data=data.table(data)

# get the total population and then get the age-sex specific mortality rates
data[,mortality:= deaths/pop *100]
write_csv(path = 'DATA/mortality_summary.csv', data)


# # get the total orphan number and deaths
data_summary = data %>% group_by(country,gender) %>% mutate(
  total_orphans = as.integer( round(sum(orphans))),
  total_pop = as.integer(sum(pop)),
  total_deaths = as.integer(sum(deaths))) %>% ungroup() %>%
  select(-age, -deaths,-pop,-orphans) %>%distinct()
data_summary=data.table(data_summary)
data_summary[,mortality:= total_deaths/total_pop *100]


write_csv(path = 'DATA/summary_table.csv', data_summary)


# # get the sex orphanhood ratio
data_sex_orphanhood = data %>% filter(age=='0-14')%>%group_by(country,gender) %>% mutate(
  total_orphans = as.integer( round(sum(orphans))),
  child_pop = as.integer(sum(pop*18/15)),
  total_deaths = as.integer(sum(deaths))) %>% ungroup() %>%
  select(-age, -deaths,-pop,-orphans) %>%distinct()
data_sex_orphanhood=data.table(data_sex_orphanhood)
write_csv(path = 'DATA/child_pop.csv', data_sex_orphanhood)

data_orphanhood = data %>% group_by(country,gender) %>% mutate(
  total_orphans = as.integer( round(sum(orphans))),
  total_deaths = as.integer(sum(deaths))) %>% ungroup() %>%
  select(-age, -deaths,-pop,-orphans) %>%distinct()
write_csv(path = 'DATA/orphan_num.csv', data_orphanhood)# mannually handled

# sex_orphanhood <- cbind(data_orphanhood,data_sex_orphanhood$child_pop)
# names(sex_orphanhood)<-c('gender','country','total_orphan','total_death',
#                          'child_pop')
# sex_orphanhood=data.table(sex_orphanhood)
# sex_orphanhood[,orphanhood_ratio:= total_orphan/child_pop *100]



p1 <- ggplot(data) +
  geom_col(aes(age, mortality, fill = gender), position = "dodge") +
  facet_wrap(~country, scales = "free_x", ncol = 3) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Mortality rates by age for males and females") + labs(fill = "Sex") 
#
print(p1)
cowplot::save_plot("figures/deaths.pdf", p1, base_height = 10, base_width = 15)




orphan_rate <- read.csv("DATA/orphan_ratio.csv", stringsAsFactors = FALSE)

p <- ggplot(orphan_rate) +
  geom_point(aes(country, orphan_rate, color = gender)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm"))+
  xlab("Country") + ylab("Ratio of orphanhood caused by males and females") + labs(fill = "Sex") 


print(p)

cowplot::save_plot("figures/sex_orphan_ratio.pdf", p, base_height = 5, base_width = 10)


country_all <- read.csv("DATA/country_all.csv", stringsAsFactors = FALSE)
p2 <- ggplot(country_all%>% filter(age != "0-14"))+
  geom_col(aes(age, orphans/deaths, fill = gender), position = "dodge") +
  geom_hline(yintercept = 1, alpha = 0.8, col = "grey") + 
  facet_wrap(~country, scales = "free_x", ncol = 3) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Ratio of orphans to parental deaths") + labs(fill = "Sex of parent") 
print(p2)




p2 <- ggplot(data%>% filter(age != "0-14")) +
  geom_col(aes(age, orphans/deaths, fill = gender), position = "dodge") +
  geom_hline(yintercept = 1, alpha = 0.8, col = "grey") + 
  facet_wrap(~country, scales = "free_x", ncol = 4) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Ratio of orphans to parental deaths") + labs(fill = "Sex of parent") 
print(p2)
#  labs(tag = "B")
cowplot::save_plot("figures/age_sex_death_orphan_ratio.pdf", p2, base_height = 10, base_width = 15)



#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)


p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))



p4 <- ggplot(data %>% filter(age != "0-14" & country %in% c("England & Wales", "Brazil", "Kenya"))) +
  geom_col(aes(age, orphans/deaths, fill = gender), position = "dodge") +
  geom_hline(yintercept = 1, alpha = 0.8, col = "grey") + 
  facet_wrap(~country, scales = "free_x", ncol = 3) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Ratio of orphans to parental deaths") + labs(fill = "Sex of parent") 
cowplot::save_plot("figures/ratio_presentation.png", p4, base_height = 3, base_width = 5)
