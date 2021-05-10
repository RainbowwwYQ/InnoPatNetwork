
library(dplyr)
library(plyr)
library(stringr)
library(igraph)
library(Hmisc)
library(RecordLinkage)

# ---- prepare ----
rm(list = ls())
dev.off()

assignees <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/original data/Sarnia_Windsor/finaldata_assignees.csv",header = T,as.is = T)
inventors <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/original data/Sarnia_Windsor/finaldata_inventors.csv",header = T,as.is = T)
rownames(inventors) <- NULL
rownames(assignees) <- NULL

# ---- pre cleaning ----
# ---- cities and countries ----
# data cleaning for assignees country

assignees$Country <- gsub("Jiangsu,", "", assignees$Country)
assignees$Country <- gsub("Ontario,", "", assignees$Country)
assignees$Country <- gsub("Alberta,", "", assignees$Country)
assignees$Country <- gsub("Aichi-pref.,", "", assignees$Country)
assignees$Country <- gsub("Jiangsu Province,", "", assignees$Country)
assignees$Country <- gsub("ON, ", "", assignees$Country)
assignees$Country <- gsub("Berkshire,", "", assignees$Country)
assignees$Country <- gsub("British Columbia,", "", assignees$Country)
assignees$Country <- gsub("On,", "", assignees$Country)

assignees$Country <- gsub("Canada", "CA", assignees$Country)
assignees$Country <- gsub("Ontario, unknown", "CA", assignees$Country)
assignees$Country <- gsub("Changshu", "CN", assignees$Country)
assignees$Country <- gsub("Windsor", "CA", assignees$Country)
assignees$Country <- gsub("U.S.", "US", assignees$Country)
assignees$Country <- gsub("unknown", "US", assignees$Country)
assignees$Country <- gsub("China", "CN", assignees$Country)
assignees$Country <- gsub("International", "CA", assignees$Country)
assignees$Country <- gsub("INTERNATIONAL", "CA", assignees$Country)


# data cleaning for source city

assignees$City<- gsub("both of", "DE", assignees$City)
assignees$City<- gsub("all of", "AU", assignees$City)
assignees$City<- gsub("Sarnia Ontario", "Sarnia", assignees$City)
assignees$City<- gsub("East Windsor", "Windsor", assignees$City)
assignees$City<- gsub("New Windsor", "Windsor", assignees$City)
assignees$City<- gsub("South Windsor", "Windsor", assignees$City)
assignees$City<- gsub("Windsor Lock", "Windsor", assignees$City)
assignees$City<- gsub("So. Windsor", "Windsor", assignees$City)
assignees$City<- gsub("S. Windsor", "Windsor", assignees$City)
assignees$City<- gsub("North Windsor", "Windsor", assignees$City)
assignees$City<- gsub("Windsors", "Windsor", assignees$City)


# data cleaning for target country

names(inventors)[2] <- "city"
names(inventors)[3] <- "country"

inventors$country <- gsub(",CA", "", inventors$country)
inventors$country <- gsub("Bucks County,", "", inventors$country)
inventors$country <- gsub("Burlington County,", "", inventors$country)
inventors$country <- gsub("N/A,", "CA", inventors$country)
inventors$country <- gsub("Mercer County,", "", inventors$country)
inventors$country <- gsub("Middlesex County,", "", inventors$country)
inventors$country <- gsub("Mercer County,", "", inventors$country)
inventors$country <- gsub("Hunterdon County,", "", inventors$country)

# data cleaning for target city

inventors$city  <- gsub("Sarnia - N/A", "Sarnia", inventors$city )
inventors$city  <- gsub("late of Sarnia", "Sarnia", inventors$city )
inventors$city  <- gsub("East Windsor", "Windsor", inventors$city )
inventors$city  <- gsub("West Windsor", "Windsor", inventors$city )
inventors$city  <- gsub("E. Windsor", "Windsor", inventors$city )
inventors$city  <- gsub("New Windsor", "Windsor", inventors$city )
inventors$city  <- gsub("South WindsorCT", "Windsor", inventors$city )
inventors$city  <- gsub("Windsor BN/A", "Windsor", inventors$city )
inventors$city  <- gsub("W. Windsor", "Windsor", inventors$city )
inventors$city  <- gsub("South WindsorCT", "Windsor", inventors$city )
inventors$city  <- gsub("South WindsorN/A", "Windsor", inventors$city )
inventors$city  <- gsub("S. Windsor", "Windsor", inventors$city )
inventors$city  <- gsub("So. Windsor", "Windsor", inventors$city )

assignees$Country <- gsub(" ", "", assignees$Country)
inventors$country <- gsub(" ", "", inventors$country)

# ---- companies' names （for assignees)----

assignees$backup <- assignees$Assignees

assignees$backup <- tolower(assignees$backup)
assignees$backup <- capitalize(assignees$backup)
assignees$backup <- gsub("&", "and", assignees$backup)
assignees$backup <- str_replace_all(assignees$backup,"[[:punct:]]", "") # delete all special strings

assignees$backup <- gsub("company$", "", assignees$backup)
assignees$backup <- gsub("limited$", "", assignees$backup)
assignees$backup <- gsub("ltd$", "", assignees$backup)
assignees$backup <- gsub("corporation$", "", assignees$backup)
assignees$backup <- gsub("corpoartion$", "", assignees$backup)
assignees$backup <- gsub("corp$", "", assignees$backup)
assignees$backup <- gsub("co$", "", assignees$backup)
assignees$backup <- gsub("incorporated$", "", assignees$backup)
assignees$backup <- gsub("inc$", "", assignees$backup)
assignees$backup <- gsub("llc$", "", assignees$backup)

#again in case having two similar words together

assignees$backup <- gsub("company", "", assignees$backup)
assignees$backup <- gsub("limited", "", assignees$backup)
assignees$backup <- gsub("ltd", "", assignees$backup)
assignees$backup <- gsub("corporation", "", assignees$backup)
assignees$backup <- gsub("corpoartion", "", assignees$backup)
assignees$backup <- gsub("corp", "", assignees$backup)
assignees$backup <- gsub("co", "", assignees$backup)
assignees$backup <- gsub("incorporated", "", assignees$backup)
assignees$backup <- gsub("inc", "", assignees$backup)
assignees$backup <- gsub("llc", "", assignees$backup)

assignees$backup <- gsub("air liquide canada ltee", "", assignees$backup)
assignees$backup <- gsub("Canadian liquid air ltd", "Air liquide canada", assignees$backup)
assignees$backup <- gsub("8452059 canada", "", assignees$backup)
assignees$Assignees <- gsub("8452059 Canada Inc.", "", assignees$Assignees)

assignees$backup <- gsub(" ", "", assignees$backup)

before <- length(unique(assignees$Assignees))

assignees <- assignees[order(assignees$backup, assignees$Assignees),]
rownames(assignees) <- NULL

# ------------- change numbers here  --------------------
for (i in 1:2){
  assignees[i,"Assignees"] <- NA
}
assignees <- assignees[complete.cases(assignees[,2]),]

# levenshteinSim("Alcaninternatinal","Alcaninternational") = 0.8+

obs <- length(assignees$backup)
for (i in 2:obs){
  if (levenshteinSim(assignees[i,"backup"], assignees[i-1,"backup"])>=0.65){
    assignees[i,"Assignees"] <- assignees[i-1,"Assignees"]
  }
}
after <- length(unique(assignees$Assignees))
# assignees <- assignees[,-7]

# ---- mobility network ----

inventors <- inventors[order(inventors$Inventors),]
rownames(inventors) <- NULL

names(inventors)[2] <- "Actor"
names(assignees)[2] <- "Actor"
inventors$class <- "inventor"
assignees$class <- "assignee"

via <- merge(inventors, assignees, by = "number")
via <- via[order(via$Actor.x, via$Actor.y, via$number),]
row.names(via) <- NULL

merge(inventors, assignees, by = "number")
mobi_link <- data.frame(Source=c(NA), Source_Year =c(NA), Source_city =c(NA), Source_country=c(NA), Source_patent =c(NA),
                        Target=c(NA), Target_Year =c(NA), Target_city =c(NA), Target_country=c(NA), Target_patent =c(NA),
                        stringsAsFactors=FALSE)

obs2 <- length(via$number)
for (i in 2:obs2){
  if (via[i,2]==via[i-1,2]& via[i,1]!=via[i-1,1] & via[i,8]!=via[i-1,8]){
    mobi_link[i,"Source"] <- via[i,"Actor.y"]
    mobi_link[i,"Source_Year"] <- via[i,"year.y"]
    mobi_link[i,"Source_city"] <- via[i,"City"]
    mobi_link[i,"Source_patent"] <- via[i,"number"]
    mobi_link[i,"Source_country"] <- via[i,"Country"]
    
    mobi_link[i,"Target"] <- via[i-1,"Actor.y"]
    mobi_link[i,"Target_Year"] <- via[i-1,"year.y"]
    mobi_link[i,"Target_city"] <- via[i-1,"City"]
    mobi_link[i,"Target_patent"] <- via[i-1,"number"]
    mobi_link[i,"Target_country"] <- via[i-1,"Country"]
    
  }
}

mobi_link <- mobi_link[complete.cases(mobi_link[,1]),]

mobi_link2 <- data.frame(Source=c(NA), Source_Year =c(NA), Source_city =c(NA), Source_country=c(NA), Source_patent =c(NA),
                        Target=c(NA), Target_Year =c(NA), Target_city =c(NA), Target_country=c(NA), Target_patent =c(NA),
                        stringsAsFactors=FALSE)
obs3 <- length(mobi_link$Source)
for (i in 1: obs3){
  if (mobi_link[i,"Source_Year"]>mobi_link[i,"Target_Year"]){
    mobi_link2[i,"Source"] <- mobi_link[i,"Target"]
    mobi_link2[i,"Source_Year"] <- mobi_link[i,"Target_Year"]
    mobi_link2[i,"Source_city"] <- mobi_link[i,"Target_city"]
    mobi_link2[i,"Source_patent"] <- mobi_link[i,"Target_patent"]
    mobi_link2[i,"Source_country"] <- mobi_link[i,"Target_country"]
    
    mobi_link2[i,"Target"] <- mobi_link[i,"Source"]
    mobi_link2[i,"Target_Year"] <- mobi_link[i,"Source_Year"]
    mobi_link2[i,"Target_city"] <- mobi_link[i,"Source_city"]
    mobi_link2[i,"Target_patent"] <- mobi_link[i,"Source_patent"]
    mobi_link2[i,"Target_country"] <- mobi_link[i,"Source_country"]
  } else {
    mobi_link2[i,"Source"] <- mobi_link[i,"Source"]
    mobi_link2[i,"Source_Year"] <- mobi_link[i,"Source_Year"]
    mobi_link2[i,"Source_city"] <- mobi_link[i,"Source_city"]
    mobi_link2[i,"Source_patent"] <- mobi_link[i,"Source_patent"]
    mobi_link2[i,"Source_country"] <- mobi_link[i,"Source_country"]
    
    mobi_link2[i,"Target"] <- mobi_link[i,"Target"]
    mobi_link2[i,"Target_Year"] <- mobi_link[i,"Target_Year"]
    mobi_link2[i,"Target_city"] <- mobi_link[i,"Target_city"]
    mobi_link2[i,"Target_patent"] <- mobi_link[i,"Target_patent"]
    mobi_link2[i,"Target_country"] <- mobi_link[i,"Target_country"]
  }
}

mobi_link3 <- data.frame(Source=mobi_link2$Source, Target=mobi_link2$Target, Weight = c("1"),
                         Source_city = mobi_link2$Source_city, Source_country= mobi_link2$Source_country,
                         Target_city = mobi_link2$Target_city, Target_country = mobi_link2$Target_country,
                         If_across = c("1"), Type = c("directed"),
                         stringsAsFactors=FALSE)

mobi_link3 <- mobi_link3[order(mobi_link3$Source, mobi_link3$Target),]
row.names(mobi_link3) <- NULL
mobi_link3$Weight <- as.numeric(mobi_link3$Weight)
class(mobi_link3$Weight)

for (i in 2:obs3){
  if (mobi_link3[i,"Source"]==mobi_link3[i-1,"Source"]
      &mobi_link3[i,"Target"]==mobi_link3[i-1,"Target"]){
    mobi_link3[i,"Weight"]<- mobi_link3[i-1,"Weight"]+1
    mobi_link3[i-1,"Source_city"] <- NA}
}
mobi_link3 <- mobi_link3[complete.cases(mobi_link3[,4]),]


# if across regions
obs4 <- length(mobi_link3$Source)
for (i in 1:obs4){
  if (mobi_link3[i,"Source_city"]==mobi_link3[i,"Target_city"]){
    mobi_link3[i,"If_across"]<- 0
  }
}
  
# ---- nodes ----

mobi_node <- data.frame(Label=mobi_link3$Source, City = mobi_link3$Source_city, Country=mobi_link3$Source_country,
                        stringsAsFactors=FALSE)
mobi_node2 <- data.frame(Label=mobi_link3$Target, City = mobi_link3$Target_city, Country=mobi_link3$Target_country,
                        stringsAsFactors=FALSE)

mobi_node3 <- bind_rows(mobi_node, mobi_node2)
mobi_node3 <- mobi_node3[!duplicated(mobi_node3$Label), ]

mobi_node3$regions <- NA
mobi_node3$classification <- "unknown"
mobi_node3$nations <- NA

obs5 <- length(mobi_node3$Label)
for (i in 1:obs5){
  if (str_detect(mobi_node3[i,"City"],"Sarnia")|
      str_detect(mobi_node3[i,"City"],"Windsor")){
    mobi_node3[i,"regions"] <- "internal"
  } else {mobi_node3[i,"regions"] <- "external"}
}

for (i in 1:obs5){
  if (str_detect(mobi_node3[i,"Label"],"Recherche")|
      str_detect(mobi_node3[i,"Label"],"Research")|
      str_detect(mobi_node3[i,"Label"],"Hopital")|
      str_detect(mobi_node3[i,"Label"],"Hospital")|
      
      str_detect(mobi_node3[i,"Label"],"Institut")|
      str_detect(mobi_node3[i,"Label"],"Rechecch")|
      str_detect(mobi_node3[i,"Label"],"Institute")|
      str_detect(mobi_node3[i,"Label"],"Academy")|
      str_detect(mobi_node3[i,"Label"],"Center")|
      str_detect(mobi_node3[i,"Label"],"Nationale")|
      str_detect(mobi_node3[i,"Label"],"Laboratories")|
      str_detect(mobi_node3[i,"Label"],"Minister")|
      str_detect(mobi_node3[i,"Label"],"Administrators")|
      str_detect(mobi_node3[i,"Label"],"Department")|
      str_detect(mobi_node3[i,"Label"],"National")|
      
      str_detect(mobi_node3[i,"Label"],"HOSPITAL")|
      str_detect(mobi_node3[i,"Label"],"RESEARCH")|
      str_detect(mobi_node3[i,"Label"],"TECNOLOGIA")|
      str_detect(mobi_node3[i,"Label"],"ACADEMISCH")|
      str_detect(mobi_node3[i,"Label"],"INSTITUTE")|
      
      str_detect(mobi_node3[i,"Label"],"Recherche")|
      str_detect(mobi_node3[i,"Label"],"Research")|
      str_detect(mobi_node3[i,"Label"],"Hopital")|
      str_detect(mobi_node3[i,"Label"],"Hopital")|
      str_detect(mobi_node3[i,"Label"],"Institut")|
      str_detect(mobi_node3[i,"Label"],"Rechecch"))
  {mobi_node3[i,"classification"] <- "public institute"} 
}

for (i in 1:obs5){
  if (str_detect(mobi_node3[i,"Label"],"Inc")|
      str_detect(mobi_node3[i,"Label"],"Company")|
      str_detect(mobi_node3[i,"Label"],"Limited")|
      str_detect(mobi_node3[i,"Label"],"Ltd")|
      str_detect(mobi_node3[i,"Label"],"Corporation")|
      str_detect(mobi_node3[i,"Label"],"Corpoartion")|
      str_detect(mobi_node3[i,"Label"],"Co")|
      str_detect(mobi_node3[i,"Label"],"Corp")|
      str_detect(mobi_node3[i,"Label"],"Incorporated")|
      str_detect(mobi_node3[i,"Label"],"Llc")|
      
      str_detect(mobi_node3[i,"Label"],"INC")|
      str_detect(mobi_node3[i,"Label"],"LLC")|
      str_detect(mobi_node3[i,"Label"],"LIMITED")|
      str_detect(mobi_node3[i,"Label"],"CO.")|
      str_detect(mobi_node3[i,"Label"],"S.R.L.")|
      str_detect(mobi_node3[i,"Label"],"LTD")|
      str_detect(mobi_node3[i,"Label"],"ULC")|
      
      
      
      str_detect(mobi_node3[i,"Label"],"inc")|
      str_detect(mobi_node3[i,"Label"],"company")|
      str_detect(mobi_node3[i,"Label"],"limited")|
      str_detect(mobi_node3[i,"Label"],"ltd")|
      str_detect(mobi_node3[i,"Label"],"corporation")|
      str_detect(mobi_node3[i,"Label"],"corporation")|
      str_detect(mobi_node3[i,"Label"],"corp")|
      str_detect(mobi_node3[i,"Label"],"incorporated")|
      str_detect(mobi_node3[i,"Label"],"llc")|
      str_detect(mobi_node3[i,"Label"],"co"))
  {mobi_node3[i,"classification"] <- "business"} 
}

for (i in 1:obs5){
  if (str_detect(mobi_node3[i,"Label"],"university")|
      str_detect(mobi_node3[i,"Label"],"luniversite")|
      str_detect(mobi_node3[i,"Label"],"univeristy")|
      str_detect(mobi_node3[i,"Label"],"univerisity")|
      str_detect(mobi_node3[i,"Label"],"universite")|
      str_detect(mobi_node3[i,"Label"],"universita")|
      str_detect(mobi_node3[i,"Label"],"universidad")|
      str_detect(mobi_node3[i,"Label"],"universitge")|
      
      str_detect(mobi_node3[i,"Label"],"University")|
      str_detect(mobi_node3[i,"Label"],"Univeristy")|
      str_detect(mobi_node3[i,"Label"],"Univerisity")|
      str_detect(mobi_node3[i,"Label"],"Universite")|
      str_detect(mobi_node3[i,"Label"],"Universita")|
      str_detect(mobi_node3[i,"Label"],"Universidad")|
      str_detect(mobi_node3[i,"Label"],"Universitge")|
      
      
      str_detect(mobi_node3[i,"Label"],"UNIVERSITA")|
      str_detect(mobi_node3[i,"Label"],"UNIVERSITY"))
  {mobi_node3[i,"classification"] <- "university"} 
}

for (i in 1:obs5){
  if (str_detect(mobi_node3[i,"Label"],";"))
  {mobi_node3[i,"classification"] <- "individual"} 
}

mobi_node3$classification <- str_replace(mobi_node3$classification, "unknown","business")

for (i in 1:obs5){
  if (str_detect(mobi_node3[i,"Country"],"CA")){
    mobi_node3[i,"nations"] <- "within"
  } else {mobi_node3[i,"nations"] <- "outside"}
}


# ---- measures ----

# ---- network level ----
net <- graph_from_data_frame(d = mobi_link3, vertices = mobi_node3, directed=TRUE)
overall_stat <- data.frame(measures=c(NA), statistics = c(NA),  percentage = c(NA),stringsAsFactors = FALSE)

overall_stat[1,"measures"] <- "density"
overall_stat[1,"statistics"] <- graph.density(net)

edges <- get.edgelist(net)
overall_stat[2,"measures"] <- "nm_links"
overall_stat[2,"statistics"] <- length(E(net))
overall_stat[3,"measures"] <- "nm_nodes"
overall_stat[3,"statistics"] <- length(V(net))

overall_stat[4,"measures"] <- "nm_university"
overall_stat[4,"statistics"] <- length(which(mobi_node3$classification=="university"))
overall_stat[4,"percentage"] <- length(which(mobi_node3$classification=="university"))/length(V(net))

overall_stat[5,"measures"] <- "nm_company"
overall_stat[5,"statistics"] <- length(which(mobi_node3$classification=="business"))
overall_stat[5,"percentage"] <- length(which(mobi_node3$classification=="business"))/length(V(net))

overall_stat[6,"measures"] <- "nm_individual"
overall_stat[6,"statistics"] <- length(which(mobi_node3$classification=="individual"))
overall_stat[6,"percentage"] <- length(which(mobi_node3$classification=="individual"))/length(V(net))

overall_stat[7,"measures"] <- "nm_public instutites"
overall_stat[7,"statistics"] <- length(which(mobi_node3$classification=="public institute"))
overall_stat[7,"percentage"] <- length(which(mobi_node3$classification=="public institute"))/length(V(net))

overall_stat[8,"measures"] <- "nm_within_region"
overall_stat[8,"statistics"] <- length(which(mobi_node3$regions=="internal"))
overall_stat[8,"percentage"] <- length(which(mobi_node3$regions=="internal"))/length(V(net))

overall_stat[9,"measures"] <- "nm_outside_region"
overall_stat[9,"statistics"] <- length(which(mobi_node3$regions=="external"))
overall_stat[9,"percentage"] <- length(which(mobi_node3$regions=="external"))/length(V(net))

overall_stat[10,"measures"] <- "nm_national"
overall_stat[10,"statistics"] <- length(which(mobi_node3$nations=="within"))
overall_stat[10,"percentage"] <- length(which(mobi_node3$nations=="within"))/length(V(net))

overall_stat[11,"measures"] <- "nm_international"
overall_stat[11,"statistics"] <- length(which(mobi_node3$nations=="outside"))
overall_stat[11,"percentage"] <- length(which(mobi_node3$nations=="outside"))/length(V(net))

#ratio
overall_stat[12,"measures"] <- "ratio of internal to external"
overall_stat[12,"statistics"] <- length(which(mobi_node3$regions=="internal"))/length(which(mobi_node3$regions=="external"))
overall_stat[13,"measures"] <- "ratio of national to international"
overall_stat[13,"statistics"] <- length(which(mobi_node3$nations=="within"))/length(which(mobi_node3$nations=="outside"))

overall_stat$statistics <- round(overall_stat$statistics,4)

# ---- node level ----
# degree centrality
deg_net <- data.frame(Actor = V(net)$name, degree_normalized = degree(net, mode = c("total"), normalized = T), 
                      degree = degree(net, mode = c("total"), normalized = F),
                      stringsAsFactors=FALSE)
deg_net <- deg_net[order(deg_net$degree, decreasing=TRUE),]
row.names(deg_net) <- NULL

# closeness centrality

clo_net <- data.frame(Actor = V(net)$name, clossness_normalized = closeness(net, normalized = T),
                      closeness = closeness(net, normalized = F), 
                      stringsAsFactors=FALSE)
clo_net <- clo_net[order(clo_net$closeness, decreasing=TRUE),]
row.names(clo_net) <- NULL

# betweenness centrality

bet_net <- data.frame(Actor = V(net)$name, betweenness = betweenness(net,normalized = F),
                      betweenness_normalized = betweenness(net,normalized = T),
                      stringsAsFactors=FALSE)
bet_net <- bet_net[order(bet_net$betweenness, decreasing=TRUE),]
row.names(bet_net) <- NULL

# eigenvector centrality

eigenvector <- eigen_centrality(net)
eig_net <- data.frame(Actor = V(net)$name,eigenvector = eigenvector$vector,
                      stringsAsFactors=FALSE)
eig_net <- eig_net[order(eig_net$eigenvector, decreasing=TRUE),]
row.names(eig_net) <- NULL

measures_net <- merge(deg_net, clo_net,by = "Actor")
measures_net <- merge(measures_net, bet_net,by = "Actor")
measures_net <- merge(measures_net, eig_net,by = "Actor")
names(measures_net)[1]<- "Label"

mobi_node3 <- merge(mobi_node3, measures_net, by = "Label")

overall_stat[14,"measures"] <- "mean of degree"
overall_stat[14,"statistics"] <- mean(mobi_node3$degree)
# closeness centralization
overall_stat[15,"measures"] <- "mean of closeness"
overall_stat[15,"statistics"] <- mean(mobi_node3$closeness)
# betweenness centralization
overall_stat[16,"measures"] <- "mean of betweenness"
overall_stat[16,"statistics"] <- mean(mobi_node3$betweenness)
# eigenvector centralization
overall_stat[17,"measures"] <- "mean of eigenvector"
overall_stat[17,"statistics"] <- mean(mobi_node3$eigenvector)












# ---- export ----

write.csv(mobi_link3,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Windsor-Sarnia/all_years/mobility&cooperation/mobi_link.csv")
write.csv(mobi_node3,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Windsor-Sarnia/all_years/mobility&cooperation/mobi_node.csv")
write.csv(overall_stat,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Windsor-Sarnia/all_years/mobility&cooperation/mobi_overall_stat.csv")










