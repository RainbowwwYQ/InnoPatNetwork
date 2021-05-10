
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)

# ---- prepare ----
rm(list = ls())
dev.off()

bonding_top <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_bonding(top).csv",header = T,as.is = T)
connecting_top <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_connecting(top).csv",header = T,as.is = T)
node <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_node.csv",header = T,as.is = T)

node <- node[,-1]
node$Label_backup <- node$Label
node <- node[-(1:2),]

obs <- length(node$Label)
for (i in 1:obs){
  if (node[i,"degree"]<20){
    node[i, "Label_backup"]<-NA
  }
}
colnames(node)[8]<- "bonding"
colnames(node)[11] <- "connecting"

# ---- graphs ----
sp <- ggplot(data=node,aes(x=bonding, y=connecting, 
                             colour = regions, shape=classification))+
  geom_point(size=node$bonding/5, alpha=0.5)+
  scale_color_wsj()+
  scale_shape_manual(values=c(2,0,1,5))

#change background color + baseline

sp <- sp + theme_bw() + theme(panel.border = element_blank(),panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
sp <- sp + geom_hline(aes(yintercept=0),colour="#BB0000",linetype="dashed")
sp <- sp + geom_vline(aes(xintercept=0),colour="#BB0000",linetype="dashed")
sp <- sp + geom_text_repel(aes(y = connecting + .2,label=Label_backup))+theme_classic(base_size = 10)
sp

# ---- measures ----

stat <- data.frame(linking = c(NA), measures = c(NA), count = c(NA),percentage=c(NA))

# ---- bonding ----
stat[1,"linking"] <- "bonding"
stat[1,"measures"]<- "number of core bonding"
stat[1,"count"]<- length(bonding_top$Label)

stat[2,"linking"] <- "bonding"
stat[2,"measures"]<- "number of university"
stat[2,"count"]<- length(which(bonding_top$classification=="university"))
stat[2,"percentage"]<- length(which(bonding_top$classification=="university"))/length(bonding_top$Label)

stat[3,"linking"] <- "bonding"
stat[3,"measures"]<- "number of company"
stat[3,"count"]<- length(which(bonding_top$classification=="business"))
stat[3,"percentage"]<- length(which(bonding_top$classification=="business"))/length(bonding_top$Label)

stat[4,"linking"] <- "bonding"
stat[4,"measures"]<- "number of public institutes"
stat[4,"count"]<- length(which(bonding_top$classification=="public institute"))
stat[4,"percentage"]<- length(which(bonding_top$classification=="public institute"))/length(bonding_top$Label)

stat[5,"linking"] <- "bonding"
stat[5,"measures"]<- "number of individual"
stat[5,"count"]<- length(which(bonding_top$classification=="individual"))
stat[5,"percentage"]<- length(which(bonding_top$classification=="individual"))/length(bonding_top$Label)

stat[6,"linking"] <- "bonding"
stat[6,"measures"]<- "number of internal actor"
stat[6,"count"]<- length(which(bonding_top$regions=="internal"))
stat[6,"percentage"]<- length(which(bonding_top$regions=="internal"))/length(bonding_top$Label)

stat[7,"linking"] <- "bonding"
stat[7,"measures"]<- "number of external actor"
stat[7,"count"]<- length(which(bonding_top$regions=="external"))
stat[7,"percentage"]<- length(which(bonding_top$regions=="external"))/length(bonding_top$Label)

# ---- connecting ----

stat[8,"linking"] <- "connecting"
stat[8,"measures"]<- "number of core connecting"
stat[8,"count"]<- length(connecting_top$Label)

stat[9,"linking"] <- "connecting"
stat[9,"measures"]<- "number of university"
stat[9,"count"]<- length(which(connecting_top$classification=="university"))
stat[9,"percentage"]<- length(which(connecting_top$classification=="university"))/length(connecting_top$Label)

stat[10,"linking"] <- "connecting"
stat[10,"measures"]<- "number of company"
stat[10,"count"]<- length(which(connecting_top$classification=="business"))
stat[10,"percentage"]<- length(which(connecting_top$classification=="business"))/length(connecting_top$Label)

stat[11,"linking"] <- "connecting"
stat[11,"measures"]<- "number of public institutes"
stat[11,"count"]<- length(which(connecting_top$classification=="public institute"))
stat[11,"percentage"]<- length(which(connecting_top$classification=="public institute"))/length(connecting_top$Label)

stat[12,"linking"] <- "connecting"
stat[12,"measures"]<- "number of individual"
stat[12,"count"]<- length(which(connecting_top$classification=="individual"))
stat[12,"percentage"]<- length(which(connecting_top$classification=="individual"))/length(connecting_top$Label)

stat[13,"linking"] <- "connecting"
stat[13,"measures"]<- "number of internal actor"
stat[13,"count"]<- length(which(connecting_top$regions=="internal"))
stat[13,"percentage"]<- length(which(connecting_top$regions=="internal"))/length(connecting_top$Label)

stat[14,"linking"] <- "connecting"
stat[14,"measures"]<- "number of external actor"
stat[14,"count"]<- length(which(connecting_top$regions=="external"))
stat[14,"percentage"]<- length(which(connecting_top$regions=="external"))/length(connecting_top$Label)

# ---- output ----
write.csv(stat,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_linking_stat.csv")
