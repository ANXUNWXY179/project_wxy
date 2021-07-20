library(rworldmap)
library(rgeos)
library(tidyverse)

orphans_test <- readRDS("orphans.RDS")
#orphans <- data.frame(orphans)
orphans <- read.csv('DATA/country_all_first.csv',stringsAsFactors = FALSE)
#orphans$country <- rownames(orphans)

sub_orphans <- select(orphans, age,gender,country, orphan_deaths_ratio)


sub_orphans$country[which(sub_orphans$country == "England & Wales")] <- "United Kingdom"
#sub_orphans$country[which(sub_orphans$country == "I.R. Iran")] <- "Iran"

sub_orphans <- sub_orphans[sub_orphans$age=='all',]


# female graph
sub_orphans_female <-sub_orphans[sub_orphans$gender=='Female',]
sub_orphans_female <- select(sub_orphans_female, country, orphan_deaths_ratio)
rownames(sub_orphans_female) <- sub_orphans_female$country

joinData <- joinCountryData2Map(sub_orphans_female,
                                joinCode = "NAME",
                                nameJoinColumn = "country")

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)
df <- as.data.frame(centroids)
df$country <- rownames(df)
#df$country[which(df$country == 'Russia')] <- "Russian Federation"
#df$country[which(df$country == 'United States of America')] <- "USA"
joined <- left_join(sub_orphans_female, df)

joined$country[which(joined$country == "United Kingdom")] <- "England & Wales"
#joined$country[which(joined$country == "Iran")] <- "I.R. Iran"


pdf("figures/map_female_orphan_death_ratio.pdf",width=10,height=5)

theMap <- mapCountryData(joinData, nameColumnToPlot="orphan_deaths_ratio", addLegend=FALSE, catMethod='fixedWidth', mapTitle= '')
do.call( addMapLegend, c(theMap, legendWidth=1, legendMar = 2))
#joined$y[which(joined$country == "Poland")] <- joined$y[which(joined$country == "Poland")] + 5
#text(joined$x, joined$y, label=joined$all, cex=0.6)
dev.off()




#male graph
sub_orphans_male <-sub_orphans[sub_orphans$gender=='Male',]
sub_orphans_male <- select(sub_orphans_male, country, orphan_deaths_ratio)
rownames(sub_orphans_male) <- sub_orphans_male$country

joinData <- joinCountryData2Map(sub_orphans_male,
                                joinCode = "NAME",
                                nameJoinColumn = "country")

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)
df <- as.data.frame(centroids)
df$country <- rownames(df)
#df$country[which(df$country == 'Russia')] <- "Russian Federation"
#df$country[which(df$country == 'United States of America')] <- "USA"
joined <- left_join(sub_orphans_male, df)

joined$country[which(joined$country == "United Kingdom")] <- "England & Wales"
#joined$country[which(joined$country == "Iran")] <- "I.R. Iran"


pdf("figures/map_male_orphan_death_ratio.pdf",width=10,height=5)

theMap <- mapCountryData(joinData, nameColumnToPlot="orphan_deaths_ratio", addLegend=FALSE, catMethod='fixedWidth', mapTitle= '')
do.call( addMapLegend, c(theMap, legendWidth=1, legendMar = 2))
#joined$y[which(joined$country == "Poland")] <- joined$y[which(joined$country == "Poland")] + 5
#text(joined$x, joined$y, label=joined$all, cex=0.6)
dev.off()


