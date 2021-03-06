library(rworldmap)
library(rgeos)
library(tidyverse)

#orphans_test <- readRDS("orphans.RDS")
#orphans <- data.frame(orphans)
orphans <- read.csv('DATA/orphan_summary.csv',stringsAsFactors = FALSE)
#orphans$country <- rownames(orphans)

sub_orphans <- select(orphans, gender,country, orphan_ratio)


sub_orphans$country[which(sub_orphans$country == "England & Wales")] <- "United Kingdom"
#sub_orphans$country[which(sub_orphans$country == "I.R. Iran")] <- "Iran"

#sub_orphans <- sub_orphans[sub_orphans$age=='all',]


# female graph
sub_orphans_female <-sub_orphans[sub_orphans$gender=='Female',]
sub_orphans_female <- select(sub_orphans_female, country, orphan_ratio)
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


pdf("figures/map_female_orphan_ratio.pdf",width=10,height=5)

theMap <- mapCountryData(joinData, nameColumnToPlot="orphan_ratio", addLegend=FALSE, 
                           catMethod=c(0,0.076,0.12,0.17,0.26,0.95,1.3), mapTitle= 'Female',colourPalette = 'diverging')
do.call( addMapLegend, c(theMap, legendWidth=1, legendMar = 2,legendLabels = "all"
                         , legendIntervals = "page",sigFigs=2))
#joined$y[which(joined$country == "Poland")] <- joined$y[which(joined$country == "Poland")] + 5
#text(joined$x, joined$y, label=joined$all, cex=0.6)
dev.off()




#male graph
sub_orphans_male <-sub_orphans[sub_orphans$gender=='Male',]
sub_orphans_male <- select(sub_orphans_male, country, orphan_ratio)
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


pdf("figures/map_male_orphan_ratio.pdf",width=10,height=5)

theMap <- mapCountryData(joinData, nameColumnToPlot="orphan_ratio", addLegend=FALSE, 
                         catMethod=c(0,0.076,0.12,0.17,0.26,0.95,1.3), mapTitle= 'Male',colourPalette = 'diverging')
do.call( addMapLegend, c(theMap,legendWidth=1, legendMar = 2, legendLabels = "all"
                         , legendIntervals = "page",sigFigs=2,legendArgs='Orphanhood ratio'))
#joined$y[which(joined$country == "Poland")] <- joined$y[which(joined$country == "Poland")] + 5
text(joined$x, joined$y, label=joined$all, cex=0.6)
dev.off()
