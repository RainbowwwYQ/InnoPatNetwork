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

# ---- cooperation network ----
# ---- links ----





coop_node <- assignees
coop_node <- coop_node[order(coop_node$Assignees),]
row.names(coop_node) <- NULL
# ---- check number here ----
for (i in 0:0){
  coop_node[i,2] <- NA
}
coop_node <- coop_node[complete.cases(coop_node[,2]),]
coop_node <- coop_node[order(coop_node$Assignees),]
row.names(coop_node) <- NULL




# ---- links ----
coop_link <- data.frame(Via = c(NA), Source=c(NA),Target=c(NA), "1" =c(NA), "2" =c(NA),"3" =c(NA),
                        "4" =c(NA),"5" =c(NA),"6" =c(NA),"7" =c(NA),"8" =c(NA),"9" =c(NA),
                        Weight=c(NA),Source_city = c(NA),
                        Source_Country = c(NA), Target_city =c(NA), Target_Country = c(NA),
                        If_across = c(NA),stringsAsFactors=FALSE)

coop_node <- coop_node[order(coop_node$number,decreasing = TRUE),]
row.names(coop_node) <- NULL
countfreq <- as.data.frame(table(coop_node$number))
countfreq <- countfreq[order(countfreq$Freq, decreasing = TRUE),]


# ---- check the largest group ----
#round 1
for (i in 1:(obs-1)){
  if (coop_node[i,"number"]==coop_node[i+1,"number"]){
    coop_link[i,"Source"] <- coop_node[i,"Assignees"]
    coop_link[i,"Source_city"] <- coop_node[i,"City"]
    coop_link[i,"Source_Country"] <- coop_node[i,"Country"]
    coop_link[i,"Target"] <- coop_node[i+1,"Assignees"]
    coop_link[i,"Target_city"] <- coop_node[i+1,"City"]
    coop_link[i,"Target_Country"] <- coop_node[i+1,"Country"]
    coop_link[i,"Via"] <- coop_node[i,"number"]
  }
}

coop_link <- coop_link[complete.cases(coop_link[,1]),]
coop_link <- coop_link[order(coop_link$Via, coop_link$Source,coop_link$Target),]
row.names(coop_link) <- NULL
obs2 <- length(coop_link$Source)

for (i in 1:obs2){
  if (coop_link[i,"Target"]==coop_link[i,"Source"]){
    coop_link[i,"Via"]<-NA
  }
}
coop_link <- coop_link[complete.cases(coop_link[,1]),]
coop_link <- coop_link[order(coop_link$Via, coop_link$Source,coop_link$Target),]
row.names(coop_link) <- NULL

coop_link$X1 <- 1
coop_link$X2 <- 2
coop_link$X3 <- 3
coop_link$X4 <- 4
coop_link$X5 <- 5
coop_link$X6 <- 6
coop_link$X7 <- 7
coop_link$X8 <- 8
coop_link$X9 <- 9

# round 2-？
obs3 <- length(coop_link$Source)
for (i in 1:(obs3-1)){
  if (coop_link[i,"Target"]==coop_link[i+1,"Source"]){
    coop_link[i,"X1"] <- coop_link[i+1,"Target"]}
}

for (i in 1:(obs3-1)){
  if (coop_link[i,"X1"]==coop_link[i+1,"Target"]){
    coop_link[i,"X2"] <- coop_link[i+1,"X1"]
    }
}

for (i in 1:(obs3-1)){
  if (coop_link[i,"X2"]==coop_link[i+1,"X1"]){
    coop_link[i,"X3"] <- coop_link[i+1,"X2"]
  }
}

for (i in 1:(obs3-1)){
  if (coop_link[i,"X3"]==coop_link[i+1,"X2"]){
    coop_link[i,"X4"] <- coop_link[i+1,"X3"]
  }
}

for (i in 1:(obs3-1)){
  if (coop_link[i,"X4"]==coop_link[i+1,"X5"]){
    coop_link[i,"X5"] <- coop_link[i+1,"X4"]
  }
}


for (i in 1:obs3){
  if (nchar(coop_link[i,"X1"])>1){
    coop_link[i+obs3,"Source"] <- coop_link[i,"Source"]
    coop_link[i+obs3,"Source_city"] <- coop_link[i,"Source_city"]
    coop_link[i+obs3,"Source_Country"] <- coop_link[i,"Source_Country"]
    coop_link[i+obs3,"Target"] <- coop_link[i,"X1"]
    coop_link[i+obs3,"Target_city"] <- coop_link[i+1,"Target_city"]
    coop_link[i+obs3,"Target_Country"] <- coop_link[i+1,"Target_Country"]
    coop_link[i+obs3,"Via"] <- coop_link[i,"Via"]
  }
}

obs4 <- 2*obs3

for (i in 1:obs3){
  if (nchar(coop_link[i,"X2"])>1){
    coop_link[i+obs4,"Source"] <- coop_link[i,"Source"]
    coop_link[i+obs4,"Source_city"] <- coop_link[i,"Source_city"]
    coop_link[i+obs4,"Source_Country"] <- coop_link[i,"Source_Country"]
    coop_link[i+obs4,"Target"] <- coop_link[i,"X2"]
    coop_link[i+obs4,"Target_city"] <- coop_link[i+1,"Target_city"]
    coop_link[i+obs4,"Target_Country"] <- coop_link[i+1,"Target_Country"]
    coop_link[i+obs4,"Via"] <- coop_link[i,"Via"]
  }
}

obs5 <- 3*obs3

for (i in 1:obs3){
  if (nchar(coop_link[i,"X3"])>1){
    coop_link[i+obs5,"Source"] <- coop_link[i,"Source"]
    coop_link[i+obs5,"Source_city"] <- coop_link[i,"Source_city"]
    coop_link[i+obs5,"Source_Country"] <- coop_link[i,"Source_Country"]
    coop_link[i+obs5,"Target"] <- coop_link[i,"X3"]
    coop_link[i+obs5,"Target_city"] <- coop_link[i+2,"Target_city"]
    coop_link[i+obs5,"Target_Country"] <- coop_link[i+2,"Target_Country"]
    coop_link[i+obs5,"Via"] <- coop_link[i,"Via"]
  }
}

obs6 <- 4*obs3

for (i in 1:obs3){
  if (nchar(coop_link[i,"X4"])>1){
    coop_link[i+obs6,"Source"] <- coop_link[i,"Source"]
    coop_link[i+obs6,"Source_city"] <- coop_link[i,"Source_city"]
    coop_link[i+obs6,"Source_Country"] <- coop_link[i,"Source_Country"]
    coop_link[i+obs6,"Target"] <- coop_link[i,"X4"]
    coop_link[i+obs6,"Target_city"] <- coop_link[i+3,"Target_city"]
    coop_link[i+obs6,"Target_Country"] <- coop_link[i+3,"Target_Country"]
    coop_link[i+obs6,"Via"] <- coop_link[i,"Via"]
  }
}

obs7 <- 5*obs3

for (i in 1:obs3){
  if (nchar(coop_link[i,"X5"])>1){
    coop_link[i+obs7,"Source"] <- coop_link[i,"Source"]
    coop_link[i+obs7,"Source_city"] <- coop_link[i,"Source_city"]
    coop_link[i+obs7,"Source_Country"] <- coop_link[i,"Source_Country"]
    coop_link[i+obs7,"Target"] <- coop_link[i,"X5"]
    coop_link[i+obs7,"Target_city"] <- coop_link[i+4,"Target_city"]
    coop_link[i+obs7,"Target_Country"] <- coop_link[i+4,"Target_Country"]
    coop_link[i+obs7,"Via"] <- coop_link[i,"Via"]
  }
}

coop_link <- coop_link[complete.cases(coop_link[,1]),]
coop_link <- coop_link[,-(4:12)]

# calculate weight

coop_link <- coop_link[order(coop_link$Source, coop_link$Target),]
row.names(coop_link) <- NULL

coop_link$Weight <- "1"
coop_link$Weight <- as.numeric(coop_link$Weight)
obs10 <- length(coop_link$Source)
for (i in 2:obs10){
  if (coop_link[i,"Source"]==coop_link[i-1,"Source"] 
      & coop_link[i,"Target"]==coop_link[i-1,"Target"]){
    coop_link[i,"Weight"] <- coop_link[i,"Weight"] + coop_link[i-1,"Weight"]
    coop_link[i-1,"Source"] <- NA
  }
}
coop_link <- coop_link[complete.cases(coop_link[,2]),]

# if_cross
obs11 <- length(coop_link$Source)
coop_link$If_across <- 0
for (i in 1: obs11){
  if (coop_link[i,"Source_city"]!=coop_link[i,"Target_city"]){
    coop_link[i,"If_across"] <- 1
  }
}

# ---- nodes ----

coop_link <- coop_link[,-1]

coop_node2 <- data.frame(Label=coop_link$Source, City = coop_link$Source_city, Country=coop_link$Source_Country,
                         stringsAsFactors=FALSE)
coop_node3 <- data.frame(Label=coop_link$Target, City = coop_link$Target_city, Country=coop_link$Target_Country,
                         stringsAsFactors=FALSE)
coop_node4 <- bind_rows(coop_node2, coop_node3)
coop_node4 <- coop_node4[!duplicated(coop_node4$Label), ]

coop_node4$regions <- NA
obs12 <- length(coop_node4$Label)
for (i in 1:obs12){
  if (str_detect(coop_node4[i,"City"],"Sarnia")|
      str_detect(coop_node4[i,"City"],"Windsor")){
    coop_node4[i,"regions"] <- "internal"
  } else {coop_node4[i,"regions"] <- "external"}
}

coop_node4$classification <- "unknown"

for (i in 1:obs12){
  if (str_detect(coop_node4[i,"Label"],"Recherche")|
      str_detect(coop_node4[i,"Label"],"Research")|
      str_detect(coop_node4[i,"Label"],"Hopital")|
      str_detect(coop_node4[i,"Label"],"Hospital")|
      
      str_detect(coop_node4[i,"Label"],"Institut")|
      str_detect(coop_node4[i,"Label"],"Rechecch")|
      str_detect(coop_node4[i,"Label"],"Institute")|
      str_detect(coop_node4[i,"Label"],"Academy")|
      str_detect(coop_node4[i,"Label"],"Center")|
      str_detect(coop_node4[i,"Label"],"Nationale")|
      str_detect(coop_node4[i,"Label"],"Laboratories")|
      str_detect(coop_node4[i,"Label"],"Minister")|
      str_detect(coop_node4[i,"Label"],"Administrators")|
      str_detect(coop_node4[i,"Label"],"Department")|
      str_detect(coop_node4[i,"Label"],"National")|
      
      str_detect(coop_node4[i,"Label"],"HOSPITAL")|
      str_detect(coop_node4[i,"Label"],"RESEARCH")|
      str_detect(coop_node4[i,"Label"],"TECNOLOGIA")|
      str_detect(coop_node4[i,"Label"],"ACADEMISCH")|
      str_detect(coop_node4[i,"Label"],"INSTITUTE")|
      
      str_detect(coop_node4[i,"Label"],"Recherche")|
      str_detect(coop_node4[i,"Label"],"Research")|
      str_detect(coop_node4[i,"Label"],"Hopital")|
      str_detect(coop_node4[i,"Label"],"Hopital")|
      str_detect(coop_node4[i,"Label"],"Institut")|
      str_detect(coop_node4[i,"Label"],"Rechecch"))
  {coop_node4[i,"classification"] <- "public institute"} 
}

for (i in 1:obs12){
  if (str_detect(coop_node4[i,"Label"],"Inc")|
      str_detect(coop_node4[i,"Label"],"Company")|
      str_detect(coop_node4[i,"Label"],"Limited")|
      str_detect(coop_node4[i,"Label"],"Ltd")|
      str_detect(coop_node4[i,"Label"],"Corporation")|
      str_detect(coop_node4[i,"Label"],"Corpoartion")|
      str_detect(coop_node4[i,"Label"],"Co")|
      str_detect(coop_node4[i,"Label"],"Corp")|
      str_detect(coop_node4[i,"Label"],"Incorporated")|
      str_detect(coop_node4[i,"Label"],"Llc")|
      
      str_detect(coop_node4[i,"Label"],"INC")|
      str_detect(coop_node4[i,"Label"],"LLC")|
      str_detect(coop_node4[i,"Label"],"LIMITED")|
      str_detect(coop_node4[i,"Label"],"CO.")|
      str_detect(coop_node4[i,"Label"],"S.R.L.")|
      str_detect(coop_node4[i,"Label"],"LTD")|
      str_detect(coop_node4[i,"Label"],"ULC")|
      
      
      
      str_detect(coop_node4[i,"Label"],"inc")|
      str_detect(coop_node4[i,"Label"],"company")|
      str_detect(coop_node4[i,"Label"],"limited")|
      str_detect(coop_node4[i,"Label"],"ltd")|
      str_detect(coop_node4[i,"Label"],"corporation")|
      str_detect(coop_node4[i,"Label"],"corporation")|
      str_detect(coop_node4[i,"Label"],"corp")|
      str_detect(coop_node4[i,"Label"],"incorporated")|
      str_detect(coop_node4[i,"Label"],"llc")|
      str_detect(coop_node4[i,"Label"],"co"))
  {coop_node4[i,"classification"] <- "business"} 
}

for (i in 1:obs12){
  if (str_detect(coop_node4[i,"Label"],"university")|
      str_detect(coop_node4[i,"Label"],"luniversite")|
      str_detect(coop_node4[i,"Label"],"univeristy")|
      str_detect(coop_node4[i,"Label"],"univerisity")|
      str_detect(coop_node4[i,"Label"],"universite")|
      str_detect(coop_node4[i,"Label"],"universita")|
      str_detect(coop_node4[i,"Label"],"universidad")|
      str_detect(coop_node4[i,"Label"],"universitge")|
      
      str_detect(coop_node4[i,"Label"],"University")|
      str_detect(coop_node4[i,"Label"],"Univeristy")|
      str_detect(coop_node4[i,"Label"],"Univerisity")|
      str_detect(coop_node4[i,"Label"],"Universite")|
      str_detect(coop_node4[i,"Label"],"Universita")|
      str_detect(coop_node4[i,"Label"],"Universidad")|
      str_detect(coop_node4[i,"Label"],"Universitge")|
      
      
      str_detect(coop_node4[i,"Label"],"UNIVERSITA")|
      str_detect(coop_node4[i,"Label"],"UNIVERSITY"))
  {coop_node4[i,"classification"] <- "university"} 
}

for (i in 1:obs12){
  if (str_detect(coop_node4[i,"Label"],";"))
  {coop_node4[i,"classification"] <- "individual"} 
}


# Check and then replace
coop_node4$classification <- str_replace(coop_node4$classification, "unknown","business")


coop_node4$nations <- NA
for (i in 1:obs12){
  if (str_detect(coop_node4[i,"Country"],"CA")){
    coop_node4[i,"nations"] <- "within"
  } else {coop_node4[i,"nations"] <- "outside"}
}  






# ---- network level ----
net <- graph_from_data_frame(d = coop_link, vertices = coop_node4, directed=FALSE)
overall_stat <- data.frame(measures=c(NA), statistics = c(NA), percentage = c(NA), stringsAsFactors = FALSE)

overall_stat[1,"measures"] <- "density"
overall_stat[1,"statistics"] <- graph.density(net)

edges <- get.edgelist(net)
overall_stat[2,"measures"] <- "nm_links"
overall_stat[2,"statistics"] <- length(E(net))
overall_stat[3,"measures"] <- "nm_nodes"
overall_stat[3,"statistics"] <- length(V(net))

overall_stat[4,"measures"] <- "nm_university"
overall_stat[4,"statistics"] <- length(which(coop_node4$classification=="university"))
overall_stat[4,"percentage"] <- length(which(coop_node4$classification=="university"))/length(V(net))

overall_stat[5,"measures"] <- "nm_company"
overall_stat[5,"statistics"] <- length(which(coop_node4$classification=="business"))
overall_stat[5,"percentage"] <- length(which(coop_node4$classification=="business"))/length(V(net))

overall_stat[6,"measures"] <- "nm_individual"
overall_stat[6,"statistics"] <- length(which(coop_node4$classification=="individual"))
overall_stat[6,"percentage"] <- length(which(coop_node4$classification=="individual"))/length(V(net))

overall_stat[7,"measures"] <- "nm_public instutites"
overall_stat[7,"statistics"] <- length(which(coop_node4$classification=="public institute"))
overall_stat[7,"percentage"] <- length(which(coop_node4$classification=="public institute"))/length(V(net))

overall_stat[8,"measures"] <- "nm_within_region"
overall_stat[8,"statistics"] <- length(which(coop_node4$regions=="internal"))
overall_stat[8,"percentage"] <- length(which(coop_node4$regions=="internal"))/length(V(net))

overall_stat[9,"measures"] <- "nm_outside_region"
overall_stat[9,"statistics"] <- length(which(coop_node4$regions=="external"))
overall_stat[9,"percentage"] <- length(which(coop_node4$regions=="external"))/length(V(net))

overall_stat[10,"measures"] <- "nm_national"
overall_stat[10,"statistics"] <- length(which(coop_node4$nations=="within"))
overall_stat[10,"percentage"] <- length(which(coop_node4$nations=="within"))/length(V(net))

overall_stat[11,"measures"] <- "nm_international"
overall_stat[11,"statistics"] <- length(which(coop_node4$nations=="outside"))
overall_stat[11,"percentage"] <- length(which(coop_node4$nations=="outside"))/length(V(net))

#ratio
overall_stat[12,"measures"] <- "ratio of internal to external"
overall_stat[12,"statistics"] <- length(which(coop_node4$regions=="internal"))/length(which(coop_node4$regions=="external"))
overall_stat[13,"measures"] <- "ratio of national to international"
overall_stat[13,"statistics"] <- length(which(coop_node4$nations=="within"))/length(which(coop_node4$nations=="outside"))

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

names(measures_net)[1]<-"Label"

coop_node4 <- merge(coop_node4, measures_net, by = "Label")
coop_link$Type <- "undirected"

# degree centralization
overall_stat[14,"measures"] <- "mean of degree"
overall_stat[14,"statistics"] <- mean(coop_node4$degree)
# closeness centralization
overall_stat[15,"measures"] <- "mean of closeness"
overall_stat[15,"statistics"] <- mean(coop_node4$closeness)
# betweenness centralization
overall_stat[16,"measures"] <- "mean of betweenness"
overall_stat[16,"statistics"] <- mean(coop_node4$betweenness)
# eigenvector centralization
overall_stat[17,"measures"] <- "mean of eigenvector"
overall_stat[17,"statistics"] <- mean(coop_node4$eigenvector)

overall_stat$statistics <- round(overall_stat$statistics,4)

# ---- export ----

write.csv(coop_link,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Windsor-Sarnia/all_years/mobility&cooperation/coop_link.csv")
write.csv(coop_node4,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Windsor-Sarnia/all_years/mobility&cooperation/coop_node.csv")
write.csv(overall_stat,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Windsor-Sarnia/all_years/mobility&cooperation/coop_overall_stat.csv")







