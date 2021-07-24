library(rworldmap)
library(rgeos)
library(tidyverse)





colombia_father = read.csv('DATA/fertility/colombia_fertility_m_all.csv', stringsAsFactors = FALSE)
colombia_mother = read.csv('DATA/fertility/colombia_fertility_f.csv', stringsAsFactors = FALSE)
colombia_mother$gender = 'Female'
colombia_father$gender = 'Male'
colombia_father = colombia_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
colombia_mother = colombia_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
colombia_mother$fertility_rate = ifelse(colombia_mother$age %in% c('50-54', '55-59', '60-64',
                                                           '65-69', '70-74', '75-79',
                                                           '80+',  '65+', '55+'),
                                    NA, colombia_mother$fertility_rate)
colombia_father$fertility_rate = ifelse(colombia_father$age %in% c('80+'), NA, colombia_father$fertility_rate)
colombia_combine = rbind(colombia_father, colombia_mother)
colombia_combine$fertility_rate = colombia_combine$fertility_rate * 1000
colombia_combine$year = as.character(colombia_combine$year)
colombia_combine = colombia_combine %>% select(year, age, gender, fertility_rate)
colombia_combine$country <- rep("Colombia", length(colombia_combine$age))

#########################################################################################################

england <- read.csv('DATA/fertility/england_wales_fertility_all.csv', stringsAsFactors = FALSE)
#england <- select(england, age, gender, nb_deaths, nb_orphans)
england$gender = ifelse(england$gender == 'M', 'Male', 'Female')
names(england)[which(names(england) == 'rate')] = "fertility_rate"
england$year = as.character(england$year)
england$country <- rep("England & Wales", length(england$age))

#########################################################################################################

process_fertility_plots = function(country){
  if (country == 'england_wales'){
    data_combine = read.csv('DATA/fertility/england_wales_fertility_all.csv')
    data_combine$gender = ifelse(data_combine$gender == 'M', 'Male', 'Female')
    names(data_combine)[which(names(data_combine) == 'rate')] = "fertility_rate"
  }else{
    data_father = read.csv(paste0('DATA/fertility/', country, '_fertility_m_all.csv'))
    data_mother = read.csv(paste0('DATA/fertility/', country, '_fertility_f.csv'))
    data_mother$gender = 'Female'
    data_father$gender = 'Male'
    data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
    data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
    data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+'),
                                        NA, data_mother$fertility_rate)
    data_father$fertility_rate = ifelse(data_father$age %in% c('80+'), NA, data_father$fertility_rate)
    
    if (country %in% c('brazil', 'colombia', 'india', 'kenya', 'nigeria', 'peru', 'south_africa')){
      data_combine = rbind(data_father, data_mother)
      #setnames(data_combine, 'fertility_rate', 'rate')
      data_combine$fertility_rate = data_combine$fertility_rate * 1000
      
    } else {

      data_father = data_father %>% select(year, age, gender, fertility_rate)
      
      data_mother$year = data_mother$date
      
      data_mother = data_mother %>% select(year, age, gender, fertility_rate)
      #setnames(data_mother, 'afr', 'fertility_rate')
      data_combine = rbind(data_father, data_mother)
      #setnames(data_combine, 'fertility_rate', 'rate')
    }
  }
  data_combine$year = as.character(data_combine$year)
  ggplot(data_combine) +
    geom_point(aes(x = age, y = fertility_rate, color = year)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    guides(col = guide_legend(nrow = 7)) +
    labs(x = 'Age') +
    facet_wrap(~ gender,
               strip.position = "left",
               labeller = as_labeller(c(Female = "Fertility Rate per 1000 women",
                                        Male = "Fertility Rate per 1000 men") ) ) +
    theme(strip.background =element_rect(fill="white")) +
    ylab(NULL) +
    theme(strip.background = element_blank(),
          strip.placement = "outside")
  ggsave(paste0("figures/fertility_",country, ".pdf"), width = 10, height = 4)
}



#########################################################################################################

france_father = read.csv('DATA/fertility/france_fertility_m_all.csv', stringsAsFactors = FALSE)
france_mother = read.csv('DATA/fertility/france_fertility_f.csv', stringsAsFactors = FALSE)
france_mother$gender = 'Female'
france_father$gender = 'Male'
france_father = france_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
france_mother = france_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
france_mother$fertility_rate = ifelse(france_mother$age %in% c('50-54', '55-59', '60-64',
                                                                   '65-69', '70-74', '75-79',
                                                                   '80+',  '65+', '55+'),
                                        NA, france_mother$fertility_rate)
france_father$fertility_rate = ifelse(france_father$age %in% c('80+'), NA, france_father$fertility_rate)
france_father = france_father %>% select(year, age, gender, fertility_rate)

france_mother$year = france_mother$date

france_mother = france_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
france_combine = rbind(france_father, france_mother)
france_combine$year = as.character(france_combine$year)
france_combine$country <- rep("France", length(france_combine$age))

#########################################################################################################

germany_father = read.csv('DATA/fertility/germany_fertility_m_all.csv', stringsAsFactors = FALSE)
germany_mother = read.csv('DATA/fertility/germany_fertility_f.csv', stringsAsFactors = FALSE)
germany_mother$gender = 'Female'
germany_father$gender = 'Male'
germany_father = germany_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
germany_mother = germany_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
germany_mother$fertility_rate = ifelse(germany_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+'),
                                      NA, germany_mother$fertility_rate)
germany_father$fertility_rate = ifelse(germany_father$age %in% c('80+'), NA, germany_father$fertility_rate)
germany_father = germany_father %>% select(year, age, gender, fertility_rate)

germany_mother$year = germany_mother$date

germany_mother = germany_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
germany_combine = rbind(germany_father, germany_mother)
germany_combine$year = as.character(germany_combine$year)
germany_combine$country <- rep("Germany", length(germany_combine$age))

#########################################################################################################

italy_father = read.csv('DATA/fertility/italy_fertility_m_all.csv', stringsAsFactors = FALSE)
italy_mother = read.csv('DATA/fertility/italy_fertility_f.csv', stringsAsFactors = FALSE)
italy_mother$gender = 'Female'
italy_father$gender = 'Male'
italy_father = italy_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
italy_mother = italy_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
italy_mother$fertility_rate = ifelse(italy_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+'),
                                      NA, italy_mother$fertility_rate)
italy_father$fertility_rate = ifelse(italy_father$age %in% c('80+'), NA, italy_father$fertility_rate)
italy_father = italy_father %>% select(year, age, gender, fertility_rate)

italy_mother$year = italy_mother$date

italy_mother = italy_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
italy_combine = rbind(italy_father, italy_mother)
italy_combine$year = as.character(italy_combine$year)
italy_combine$country <- rep("Italy", length(italy_combine$age))

#########################################################################################################

malawi_father = read.csv('DATA/fertility/malawi_fertility_m_all.csv', stringsAsFactors = FALSE)
malawi_mother = read.csv('DATA/fertility/malawi_fertility_f.csv', stringsAsFactors = FALSE)
malawi_mother$gender = 'Female'
malawi_father$gender = 'Male'
malawi_father = malawi_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
malawi_mother = malawi_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
malawi_mother$fertility_rate = ifelse(malawi_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+'),
                                      NA, malawi_mother$fertility_rate)
malawi_father$fertility_rate = ifelse(malawi_father$age %in% c('80+'), NA, malawi_father$fertility_rate)
malawi_father = malawi_father %>% select(year, age, gender, fertility_rate)

malawi_mother$year = malawi_mother$date

malawi_mother = malawi_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
malawi_combine = rbind(malawi_father, malawi_mother)
malawi_combine$year = as.character(malawi_combine$year)
malawi_combine$country <- rep("Malawi", length(malawi_combine$age))

#########################################################################################################

mexico_father = read.csv('DATA/fertility/mexico_fertility_m_all.csv', stringsAsFactors = FALSE)
mexico_mother = read.csv('DATA/fertility/mexico_fertility_f.csv', stringsAsFactors = FALSE)
mexico_mother$gender = 'Female'
mexico_father$gender = 'Male'
mexico_father = mexico_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
mexico_mother = mexico_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
mexico_mother$fertility_rate = ifelse(mexico_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+'),
                                      NA, mexico_mother$fertility_rate)
mexico_father$fertility_rate = ifelse(mexico_father$age %in% c('80+'), NA, mexico_father$fertility_rate)
mexico_father = mexico_father %>% select(year, age, gender, fertility_rate)

mexico_mother$year = mexico_mother$date

mexico_mother = mexico_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
mexico_combine = rbind(mexico_father, mexico_mother)
mexico_combine$year = as.character(mexico_combine$year)
mexico_combine$country <- rep("Mexico", length(mexico_combine$age))

#########################################################################################################

peru_father = read.csv('DATA/fertility/peru_fertility_m_all.csv', stringsAsFactors = FALSE)
peru_mother = read.csv('DATA/fertility/peru_fertility_f.csv', stringsAsFactors = FALSE)
peru_mother$gender = 'Female'
peru_father$gender = 'Male'
peru_father = peru_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
peru_mother = peru_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
peru_mother$fertility_rate = ifelse(peru_mother$age %in% c('50-54', '55-59', '60-64',
                                                                         '65-69', '70-74', '75-79',
                                                                         '80+',  '65+', '55+'),
                                           NA, peru_mother$fertility_rate)
peru_father$fertility_rate = ifelse(peru_father$age %in% c('80+'), NA, peru_father$fertility_rate)
peru_combine = rbind(peru_father, peru_mother)
peru_combine$fertility_rate = peru_combine$fertility_rate * 1000
peru_combine$year = as.character(peru_combine$year)
peru_combine = peru_combine %>% select(year, age, gender, fertility_rate)
peru_combine$country <- rep("Peru", length(peru_combine$age))

#########################################################################################################

poland_father = read.csv('DATA/fertility/poland_fertility_m_all.csv', stringsAsFactors = FALSE)
poland_mother = read.csv('DATA/fertility/poland_fertility_f.csv', stringsAsFactors = FALSE)
poland_mother$gender = 'Female'
poland_father$gender = 'Male'
poland_father = poland_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
poland_mother = poland_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
poland_mother$fertility_rate = ifelse(poland_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+'),
                                      NA, poland_mother$fertility_rate)
poland_father$fertility_rate = ifelse(poland_father$age %in% c('80+'), NA, poland_father$fertility_rate)
poland_father = poland_father %>% select(year, age, gender, fertility_rate)

poland_mother$year = poland_mother$date

poland_mother = poland_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
poland_combine = rbind(poland_father, poland_mother)
poland_combine$year = as.character(poland_combine$year)
poland_combine$country <- rep("Poland", length(poland_combine$age))

#########################################################################################################

southafrica_father = read.csv('DATA/fertility/south_africa_fertility_m_all.csv', stringsAsFactors = FALSE)
southafrica_mother = read.csv('DATA/fertility/south_africa_fertility_f.csv', stringsAsFactors = FALSE)
southafrica_mother$gender = 'Female'
southafrica_father$gender = 'Male'
southafrica_father = southafrica_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
southafrica_mother = southafrica_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
southafrica_mother$fertility_rate = ifelse(southafrica_mother$age %in% c('50-54', '55-59', '60-64',
                                                                   '65-69', '70-74', '75-79',
                                                                   '80+',  '65+', '55+'),
                                        NA, southafrica_mother$fertility_rate)
southafrica_father$fertility_rate = ifelse(southafrica_father$age %in% c('80+'), NA, southafrica_father$fertility_rate)
southafrica_combine = rbind(southafrica_father, southafrica_mother)
southafrica_combine$fertility_rate = southafrica_combine$fertility_rate * 1000
southafrica_combine$year = as.character(southafrica_combine$year)
southafrica_combine = southafrica_combine %>% select(year, age, gender, fertility_rate)
southafrica_combine$country <- rep("South Africa", length(southafrica_combine$age))

#########################################################################################################

spain_father = read.csv('DATA/fertility/spain_fertility_m_all.csv', stringsAsFactors = FALSE)
spain_mother = read.csv('DATA/fertility/spain_fertility_f.csv', stringsAsFactors = FALSE)
spain_mother$gender = 'Female'
spain_father$gender = 'Male'
spain_father = spain_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
spain_mother = spain_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
spain_mother$fertility_rate = ifelse(spain_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+'),
                                      NA, spain_mother$fertility_rate)
spain_father$fertility_rate = ifelse(spain_father$age %in% c('80+'), NA, spain_father$fertility_rate)
spain_father = spain_father %>% select(year, age, gender, fertility_rate)

spain_mother$year = spain_mother$date

spain_mother = spain_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
spain_combine = rbind(spain_father, spain_mother)
spain_combine$year = as.character(spain_combine$year)
spain_combine$country <- rep("Spain", length(spain_combine$age))

#########################################################################################################
usa_father = read.csv('DATA/fertility/usa_fertility_m_all.csv', stringsAsFactors = FALSE)
usa_mother = read.csv('DATA/fertility/usa_fertility_f.csv', stringsAsFactors = FALSE)
usa_mother$gender = 'Female'
usa_father$gender = 'Male'
usa_father = usa_father %>% filter(!age %in% c('under 15-NA', '80+-NA'))
usa_mother = usa_mother %>% filter(!age %in% c('under 15-NA', '80+-NA'))
usa_mother$fertility_rate = ifelse(usa_mother$age %in% c('50-54', '55-59', '60-64',
                                                             '65-69', '70-74', '75-79',
                                                             '80+',  '65+', '55+'),
                                     NA, usa_mother$fertility_rate)
usa_father$fertility_rate = ifelse(usa_father$age %in% c('80+'), NA, usa_father$fertility_rate)
usa_father = usa_father %>% select(year, age, gender, fertility_rate)

usa_mother$year = usa_mother$date

usa_mother = usa_mother %>% select(year, age, gender, fertility_rate)
#setnames(data_mother, 'afr', 'fertility_rate')
usa_combine = rbind(usa_father, usa_mother)
usa_combine$year = as.character(usa_combine$year)
usa_combine$country <- rep("USA", length(usa_combine$age))

#########################################################################################################
data <- rbind( france_combine,colombia_combine,england,germany_combine,
               italy_combine, malawi_combine, mexico_combine, peru_combine, poland_combine, southafrica_combine,
               spain_combine, usa_combine)



data$gender <- ifelse(data$gender == "female", "Female", data$gender)
data$gender <- ifelse(data$gender == "male", "Male", data$gender)



write_csv(path = 'DATA/country_fertility_all.csv', data)


#提取男女，分别做图

mother<-data[data$gender=='Female',]
mother = mother %>% filter(!year %in% c('2000', '2001',NA))

mother = mother %>% filter(!age %in% c('50-54', '55-59', '60-64',
                                       '65-69', '70-74', '75-79',
                                       '80+',  '65+', '55+'))

father<-data[data$gender=='Male',]
father = father %>% filter(!year %in% c('2000', '2001',NA))

#mother = mother %>% filter(!age %in% c('50-54', '55-59', '60-64',
 #                                      '65-69', '70-74', '75-79',
  #                                     '80+',  '65+', '55+'))

p1 <- ggplot(mother) +
  geom_point(aes(x = age, y = fertility_rate, color = year)) +
  facet_wrap(~country, scales = "free_x", ncol = 3) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Fertility Rate per 1000 women") + labs(fill = "Year") + labs(tag = "A")

p1
p2 <- ggplot(father) +
  geom_point(aes(x = age, y = fertility_rate, color = year)) +
  facet_wrap(~country, scales = "free_x", ncol = 3) +  
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom",
        strip.background =element_rect(fill="white"),
        plot.margin=unit(c(1,1,0,1), "cm")) +
  xlab("Age") + ylab("Fertility Rate per 1000 men") + labs(fill = "Year") + labs(tag = "B")


cowplot::save_plot("figures/fertility_female.pdf", p1, base_height = 15, base_width = 10)
cowplot::save_plot("figures/fertility_male.pdf", p2, base_height = 15, base_width = 10)




# p2 <- ggplot(data %>% filter(age != "0-19")) +
#   geom_col(aes(age, orphans/deaths, fill = gender), position = "dodge") +
#   geom_hline(yintercept = 1, alpha = 0.8, col = "grey") + 
#   facet_wrap(~country, scales = "free_x", ncol = 4) +  
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.position="bottom",
#         strip.background =element_rect(fill="white"),
#         plot.margin=unit(c(1,1,0,1), "cm")) +
#   xlab("Age") + ylab("Ratio of orphans to parental deaths") + labs(fill = "Sex of parent") + 
#   labs(tag = "B")
# 
# 
# 
# ggplot(data_combine) +
#   geom_point(aes(x = age, y = fertility_rate, color = year)) +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
#   guides(col = guide_legend(nrow = 7)) +
#   labs(x = 'Age') +
#   facet_wrap(~ gender,
#              strip.position = "left",
#              labeller = as_labeller(c(Female = "Fertility Rate per 1000 women",
#                                       Male = "Fertility Rate per 1000 men") ) ) +
#   theme(strip.background =element_rect(fill="white")) +
#   ylab(NULL) +
#   theme(strip.background = element_blank(),
#         strip.placement = "outside")
# ggsave(paste0("figures/fertility_",country, ".pdf"), width = 10, height = 4)
