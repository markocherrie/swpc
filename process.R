##### 
multmerge <- function(path) {
  files <- list.files(path, pattern="csv", full.names = T)
  lst <- lapply(files, function(x){ read.csv(x, header=TRUE, stringsAsFactors=FALSE) })
  do.call("rbind", lst)
}

master<-multmerge("data/")


colnames(master)<-c("PID", "Workplace", "Link", "Participation", "Journeys", "Distance", 
                    "CO2saved", "Caloriesburned", "Moneysaved")

library(stringr)
master$ParticipationNumbers<-str_match_all(master$Participation, "(?<=\\().+?(?=\\))")
master$ParticipationDenominator<-str_extract(master$ParticipationNumbers, '\\b\\w+$')
master$ParticipationNumerator<-sub("\\/.*", "", master$ParticipationNumbers)
master$ParticipationPercentage<-sub("\\%.*", "", master$Participation)


master$Moneysaved<-gsub("Â£", "", master$Moneysaved)
master$CO2saved<-gsub("kg", "", master$CO2saved)
master$Distance<-gsub(",", "", master$Distance)
master$Distance<-gsub(" miles", "", master$Distance)

master$Caloriesburned<-gsub(",", "", master$Caloriesburned)
master$ParticipationNumbers<-NULL
master$Participation<-NULL
master$Link<-NULL


fixnumbers<-function(x){as.numeric(as.character(x))}

library(dplyr)
master<-master %>%
  mutate(across(Journeys:ParticipationPercentage, fixnumbers))

master<-master[master$Journeys!=0,]

master$PID<-seq(1, nrow(master))

#write.csv(master, "output/master.csv", row.names=F)


#################
woch<-read.csv("output/master_withoutcompanieshouse.csv")




library(sf)
geog<-st_read("geography/LAD_MAY_2021_UK_BFE_V2.shp")
geog$LAD21CDFC<-substring(geog$LAD21CD,1,1)
geog<-geog[geog$LAD21CDFC=="S",]

SWPC<-merge(geog, woch, by.x="LAD21NM", by.y="Workplace", all.x=T)
library(mapview)

mapview(SWPC, z=c("ParticipationPercentage"))






