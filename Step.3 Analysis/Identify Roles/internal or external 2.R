

# install.packages("ggrepel")
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)
library(scales)
# ---- prepare ----
rm(list = ls())
dev.off()

M <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_crossing_Montreal.csv",header = T,as.is = T)
noM <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_crossing_notMontreal.csv",header = T,as.is = T)
gather <- read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_crossing_list.csv",header = T,as.is = T)
# gather <- gather[,-1]
# external_top <- external_top[complete.cases(external_top$enternal_relation),]
# internal_top <- internal_top[complete.cases(internal_top$internal_relation),]


# external_top$oriented <- external_top$external_relation - external_top$internal_relation
# internal_top$oriented <- internal_top$internal_relation - internal_top$external_relation

#external_top$mark <- "top_external"
#internal_top$mark <- "top_internal"
#gather <- bind_rows(external_top, internal_top)

gather <- gather[order(gather$Label),]
obs <- length(gather$Label)
gather$Label_backup <- gather$Label
for (i in 1:obs){
  if (gather[i,"degree"]<6){
    gather[i,"Label_backup"] <- NA
  }
}

# obs2 <- length(noM$Label)
# noM$Label_backup <- noM$Label
# for (i in 1:obs){
#   if (noM[i,"degree"]<3){
#     noM[i,"Label_backup"] <- NA
#   }
# }

#gather <- gather[complete.cases(gather$mark),]
#rownames(gather) <- NULL

colnames(gather)[2]<- "internal_links"
colnames(gather)[3] <- "external_links"

# ---- classification ----
# draw graphs
sp <- ggplot(data=gather,aes(x=internal_links, y=external_links, 
                             colour = regions, shape=classification))+
  geom_point(size=gather$degree/2, alpha=0.5)+
  scale_color_wsj()+
  scale_shape_manual(values=c(2,0,1,5))

#change background color + baseline

sp <- sp + theme_bw() + theme(panel.border = element_blank(),panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
# sp <- sp + geom_hline(aes(yintercept=0),colour="#BB0000",linetype="dashed")
# sp <- sp + geom_vline(aes(xintercept=0),colour="#BB0000",linetype="dashed")
sp <- sp + geom_text_repel(aes(y = gather$external_links + .2,label=gather$Label_backup))+theme_classic(base_size = 10)
sp


# ---- regional ----
#dev.off()
#sp2 <- ggplot(data=gather,aes(x=internal_relation, y=external_relation, 
#                              colour = mark,shape=regions))+
#  geom_point(size=gather$degree/2, alpha=0.5)+
#  scale_color_wsj()+
#  scale_shape_manual(values=c(2,0,1,5))

#change background color + baseline

#sp2 <- sp2 + theme_bw() + theme(panel.border = element_blank(),panel.grid.major = element_blank(),
#                                panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
#sp2 <- sp2 + geom_hline(aes(yintercept=0),colour="#BB0000",linetype="dashed")
#sp2 <- sp2 + geom_vline(aes(xintercept=0),colour="#BB0000",linetype="dashed")
#sp2





# png(file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/graph_crossing_classification.png", bg="transparent")


# add labels: not good here 
#  geom_text_repel(aes(label=Label))+
#  theme_classic(base_size = 16)
#  geom_text(aes(label=Label,size=3))
  
 


coop_pre <- data.frame(measures=c(NA),count=c(NA))

coop_pre[1,1] <- "Montreal"
coop_pre[2,1] <- "number of nodes"
coop_pre[2,2] <- length(M$Label)
coop_pre[3,1] <- "external relations(include weight)"
coop_pre[3,2] <- sum(M$external_relation) # greater than No. links since links *2 for both sides
coop_pre[4,1] <- "internal relations(include weight)"
coop_pre[4,2] <- sum(M$internal_relation) 

coop_pre[5,1] <- "average external per person"
coop_pre[5,2] <- mean(M$external_relation)
coop_pre[6,1] <- "average internal per person"
coop_pre[6,2] <- mean(M$internal_relation)

M$ratio_internal_to_external <- M$internal_relation / M$external_relation
coop_pre[7,1] <- "average ratio_ItoE"
coop_pre[7,2] <- mean(M$ratio_internal_to_external)

coop_pre[8,1] <- "largest external relations"
coop_pre[8,2] <- max(M$external_relation)

coop_pre[9,1] <- "largest internal relations"
coop_pre[9,2] <- max(M$internal_relation)

coop_pre[10,1] <- "largest ratio"
coop_pre[10,2] <- max(M$ratio_internal_to_external)

coop_pre[11,1] <- "not in Montreal"
coop_pre[12,1] <- "number of nodes"
coop_pre[12,2] <- length(noM$Label)
coop_pre[13,1] <- "external relations(include weight)"
coop_pre[13,2] <- sum(noM$external_relation) # greater than No. links since links *2 for both sides
coop_pre[14,1] <- "internal relations(include weight)"
coop_pre[14,2] <- sum(noM$internal_relation) 

coop_pre[15,1] <- "average external per person"
coop_pre[15,2] <- mean(noM$external_relation)
coop_pre[16,1] <- "average internal per person"
coop_pre[16,2] <- mean(noM$internal_relation)

noM$ratio_internal_to_external <- noM$internal_relation / noM$external_relation
coop_pre[17,1] <- "average ratio_ItoE"
coop_pre[17,2] <- mean(noM$ratio_internal_to_external)

coop_pre[18,1] <- "largest external relations"
coop_pre[18,2] <- max(noM$external_relation)

coop_pre[19,1] <- "largest internal relations"
coop_pre[19,2] <- max(noM$internal_relation)

coop_pre[20,1] <- "largest ratio"
coop_pre[20,2] <- max(noM$ratio_internal_to_external)


# ---- stat for top-10 ----
stat <- data.frame(region = c(NA), measures = c(NA), count = c(NA),percentage=c(NA))

M <- M[order(M$internal_relation, decreasing = T),]
top10_M_inter <- M[c(1:10),]
row.names(top10_M_inter)<- NULL

M <- M[order(M$external_relation, decreasing = T),]
top10_M_exter <- M[c(1:10),]
row.names(top10_M_exter)<- NULL

noM <- noM[order(noM$internal_relation, decreasing = T),]
top10_noM_inter <- noM[c(1:10),]
row.names(top10_noM_inter)<- NULL

noM <- noM[order(noM$external_relation, decreasing = T),]
top10_noM_exter <- noM[c(1:10),]
row.names(top10_noM_exter)<- NULL

# ---- Montreal ----

stat[1,"region"] <- "M_external"
stat[1,"measures"]<- "number of university"
stat[1,"count"]<- length(which(top10_M_exter=="university"))
stat[1,"percentage"]<- percent(length(which(top10_M_exter=="university"))/10)

stat[2,"region"] <- "M_external"
stat[2,"measures"]<- "number of company"
stat[2,"count"]<- length(which(top10_M_exter=="business"))
stat[2,"percentage"]<- percent(length(which(top10_M_exter$classification=="business"))/10)

stat[3,"region"] <- "M_external"
stat[3,"measures"]<- "number of public institutes"
stat[3,"count"]<- length(which(top10_M_exter$classification=="public institute"))
stat[3,"percentage"]<- percent(length(which(top10_M_exter$classification=="public institute"))/10)

stat[4,"region"] <- "M_external"
stat[4,"measures"]<- "number of individual"
stat[4,"count"]<- length(which(top10_M_exter$classification=="individual"))
stat[4,"percentage"]<- percent(length(which(top10_M_exter$classification=="individual"))/10)

stat[5,"region"] <- "M_internal"
stat[5,"measures"]<- "number of university"
stat[5,"count"]<- length(which(top10_M_inter=="university"))
stat[5,"percentage"]<- percent(length(which(top10_M_inter=="university"))/10)

stat[6,"region"] <- "M_internal"
stat[6,"measures"]<- "number of company"
stat[6,"count"]<- length(which(top10_M_inter=="business"))
stat[6,"percentage"]<- percent(length(which(top10_M_inter$classification=="business"))/10)

stat[7,"region"] <- "M_internal"
stat[7,"measures"]<- "number of public institutes"
stat[7,"count"]<- length(which(top10_M_inter$classification=="public institute"))
stat[7,"percentage"]<- percent(length(which(top10_M_inter$classification=="public institute"))/10)

stat[8,"region"] <- "M_internal"
stat[8,"measures"]<- "number of individual"
stat[8,"count"]<- length(which(top10_M_inter$classification=="individual"))
stat[8,"percentage"]<- percent(length(which(top10_M_inter$classification=="individual"))/10)

# ---- not in Montreal ----

stat[11,"region"] <- "noM_external"
stat[11,"measures"]<- "number of university"
stat[11,"count"]<- length(which(top10_noM_exter=="university"))
stat[11,"percentage"]<- percent(length(which(top10_noM_exter=="university"))/10)

stat[12,"region"] <- "noM_external"
stat[12,"measures"]<- "number of company"
stat[12,"count"]<- length(which(top10_noM_exter=="business"))
stat[12,"percentage"]<- percent(length(which(top10_noM_exter$classification=="business"))/10)

stat[13,"region"] <- "noM_external"
stat[13,"measures"]<- "number of public institutes"
stat[13,"count"]<- length(which(top10_noM_exter$classification=="public institute"))
stat[13,"percentage"]<- percent(length(which(top10_noM_exter$classification=="public institute"))/10)

stat[14,"region"] <- "noM_external"
stat[14,"measures"]<- "number of individual"
stat[14,"count"]<- length(which(top10_noM_exter$classification=="individual"))
stat[14,"percentage"]<- percent(length(which(top10_noM_exter$classification=="individual"))/10)

stat[15,"region"] <- "noM_internal"
stat[15,"measures"]<- "number of university"
stat[15,"count"]<- length(which(top10_noM_inter=="university"))
stat[15,"percentage"]<- percent(length(which(top10_noM_inter=="university"))/10)

stat[16,"region"] <- "noM_internal"
stat[16,"measures"]<- "number of company"
stat[16,"count"]<- length(which(top10_noM_inter=="business"))
stat[16,"percentage"]<- percent(length(which(top10_noM_inter$classification=="business"))/10)

stat[17,"region"] <- "noM_internal"
stat[17,"measures"]<- "number of public institutes"
stat[17,"count"]<- length(which(top10_noM_inter$classification=="public institute"))
stat[17,"percentage"]<- percent(length(which(top10_noM_inter$classification=="public institute"))/10)

stat[18,"region"] <- "noM_internal"
stat[18,"measures"]<- "number of individual"
stat[18,"count"]<- length(which(top10_noM_inter$classification=="individual"))
stat[18,"percentage"]<- percent(length(which(top10_noM_inter$classification=="individual"))/10)

# ---- both ----

# external_top$oriented <- external_top$external_relation - external_top$internal_relation
# internal_top$oriented <- internal_top$external_relation - internal_top$internal_relation

# stat[15,"region"]<- "external"
# stat[15,"measures"]<-"number of external_oriented"
# stat[15,"count"]<- length(which(external_top$oriented>0))
# stat[15,"percentage"]<- length(which(external_top$oriented>0))/length(external_top$Label)


# stat[16,"region"]<- "internal"
# stat[16,"measures"]<-"number of external_oriented"
# stat[16,"count"]<- length(which(internal_top$oriented>0))
# stat[16,"percentage"]<- length(which(internal_top$oriented>0))/length(internal_top$Label)

# ---- output ----
write.csv(stat,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_crossing_stat_update.csv")
write.csv(coop_pre,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_pre_update.csv")
write.csv(top10_M_exter,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/top10_M_exter.csv")
write.csv(top10_M_inter,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/top10_M_inter.csv")
write.csv(top10_noM_exter,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/top10_noM_exter.csv")
write.csv(top10_noM_inter,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/top10_noM_inter.csv")
