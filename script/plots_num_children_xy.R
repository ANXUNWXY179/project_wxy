library(tidyverse)

# brazil_m <- read.csv("data/children/brazil_m.csv")
# brazil_m$country <- rep("Brazil", length(brazil_m$age))
# brazil_f <- read.csv("data/children/brazil_f.csv")
# brazil_f$country <- rep("Brazil", length(brazil_f$age))

colombia_m <- read.csv("DATA/children/colombia_m.csv")
colombia_m$country <- rep("Colombia", length(colombia_m$age))
colombia_f <- read.csv("DATA/children/colombia_f.csv")
colombia_f$country <- rep("Colombia", length(colombia_f$age))

england_m <- read.csv("DATA/children/england_wales_m.csv")
england_m$country <- rep("England & Wales", length(england_m$age))
england_f <- read.csv("DATA/children/england_wales_f.csv")
england_f$country <- rep("England & Wales", length(england_f$age))

france_m <- read.csv("DATA/children/france_m.csv")
france_m$country <- rep("France", length(france_m$age))
france_f <- read.csv("DATA/children/france_f.csv")
france_f$country <- rep("France", length(france_f$age))

germany_m <- read.csv("DATA/children/germany_m.csv")
germany_m$country <- rep("Germany", length(germany_m$age))
germany_f <- read.csv("DATA/children/germany_f.csv")
germany_f$country <- rep("Germany", length(germany_f$age))

# india_m <- read.csv("DATA/children/india_m.csv")
# india_m$country <- rep("India", length(india_m$age))
# india_f <- read.csv("DATA/children/india_f.csv")
# india_f$country <- rep("India", length(india_f$age))
# 
# iran_m <- read.csv("DATA/children/iran_m.csv")
# iran_m$country <- rep("I.R. Iran", length(iran_m$age))
# iran_f <- read.csv("DATA/children/iran_f.csv")
# iran_f$country <- rep("I.R. Iran", length(iran_f$age))

italy_m <- read.csv("DATA/children/italy_m.csv")
italy_m$country <- rep("Italy", length(italy_m$age))
italy_f <- read.csv("DATA/children/italy_f.csv")
italy_f$country <- rep("Italy", length(italy_f$age))

# kenya_m <- read.csv("DATA/children/kenya_m.csv")
# kenya_m$country <- rep("Kenya", length(kenya_m$age))
# kenya_f <- read.csv("DATA/children/kenya_f.csv")
# kenya_f$country <- rep("Kenya", length(kenya_f$age))
# 
malawi_m <- read.csv("DATA/children/malawi_m.csv")
malawi_m$country <- rep("Malawi", length(malawi_m$age))
malawi_f <- read.csv("DATA/children/malawi_f.csv")
malawi_f$country <- rep("Malawi", length(malawi_f$age))

mexico_m <- read.csv("DATA/children/mexico_m.csv")
mexico_m$country <- rep("Mexico", length(mexico_m$age))
mexico_f <- read.csv("DATA/children/mexico_f.csv")
mexico_f$country <- rep("Mexico", length(mexico_f$age))

# nigeria_m <- read.csv("DATA/children/nigeria_m.csv")
# nigeria_m$country <- rep("Nigeria", length(nigeria_m$age))
# nigeria_f <- read.csv("DATA/children/nigeria_f.csv")
# nigeria_f$country <- rep("Nigeria", length(nigeria_f$age))

peru_m <- read.csv("DATA/children/peru_m.csv")
peru_m$country <- rep("Peru", length(peru_m$age))
peru_f <- read.csv("DATA/children/peru_f.csv")
peru_f$country <- rep("Peru", length(peru_f$age))

poland_m <- read.csv("DATA/children/poland_m.csv")
poland_m$country <- rep("Poland", length(poland_m$age))
poland_f <- read.csv("DATA/children/poland_f.csv")
poland_f$country <- rep("Poland", length(poland_f$age))

# russia_m <- read.csv("DATA/children/russian_federation_m.csv")
# russia_m$country <- rep("Russian Federation", length(russia_m$age))
# russia_f <- read.csv("DATA/children/russian_federation_f.csv")
# russia_f$country <- rep("Russian Federation", length(russia_f$age))

southafrica_m <- read.csv("DATA/children/south_africa_m.csv")
southafrica_m$country <- rep("South Africa", length(southafrica_m$age))
southafrica_f <- read.csv("DATA/children/south_africa_f.csv")
southafrica_f$country <- rep("South Africa", length(southafrica_f$age))

spain_m <- read.csv("DATA/children/spain_m.csv")
spain_m$country <- rep("Spain", length(spain_m$age))
spain_f <- read.csv("DATA/children/spain_f.csv")
spain_f$country <- rep("Spain", length(spain_f$age))

usa_m <- read.csv("DATA/children/usa_m.csv")
usa_m$country <- rep("USA", length(usa_m$age))
usa_f <- read.csv("DATA/children/usa_f.csv")
usa_f$country <- rep("USA", length(usa_f$age))

# zimbabwe_m <- read.csv("DATA/children/zimbabwe_m.csv")
# zimbabwe_m$country <- rep("Zimbabwe", length(zimbabwe_m$age))
# zimbabwe_f <- read.csv("DATA/children/zimbabwe_f.csv")
# zimbabwe_f$country <- rep("Zimbabwe", length(zimbabwe_f$age))

data = rbind(colombia_m, colombia_f,
             england_m, england_f,
             france_m, france_f,
             germany_m, germany_f,
             italy_m, italy_f,
             malawi_m,malawi_f,
             mexico_m, mexico_f,
             peru_m, peru_f,
             poland_m, poland_f,
             southafrica_m, southafrica_f,
             spain_m, spain_f,
             usa_m, usa_f)

data$gender <- ifelse(data$gender == "female", "Female", data$gender)
data$gender <- ifelse(data$gender == "male", "Male", data$gender)

p <- ggplot(data) + 
  geom_line(aes(age, children, group = interaction(gender, country), col = gender)) + 
  facet_wrap(~country, ncol=3) + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background =element_rect(fill="white")) +
  xlab("Age") + ylab("Average number of children") + labs(col = "Sex of parent")
print(p)

ggsave("figures/num_children.pdf", p, width = 8, height = 8)

p1 <- ggplot(data %>% filter(country %in% c("England & Wales", "Brazil", "Kenya"))) + 
  geom_line(aes(age, children, group = interaction(gender, country), col = gender)) + 
  facet_wrap(~country, ncol=3) + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background =element_rect(fill="white")) +
  xlab("Age") + ylab("Average number of children") + labs(col = "Sex of parent")
ggsave("figures/children_presentation.png", p1, width = 5, height = 3)

