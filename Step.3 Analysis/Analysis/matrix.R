
# install.packages("caret")

library(dplyr)
library(plyr)
library(stringr)
library(scales)
library(caret)

# ---- prepare ----
rm(list = ls())
dev.off()

bonding <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/all_bonding(top).csv",header = T,as.is = T)
connecting <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/all_connecting(top).csv",header = T,as.is = T)
external <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/coop_crossing_list(top10per)_external.csv",header = T,as.is = T)
internal <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/coop_crossing_list(top10per)_internal.csv",header = T,as.is = T)
outdegree <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/mobi_outdegree (top10).csv",header = T,as.is = T)
indegree <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/mobi_indegree (top10).csv",header = T,as.is = T)

stat <- data.frame(couple = c(NA), university = c(NA), company=c(NA), 
                   public_institute=c(NA), individual=c(NA),
                   internal_actor = c(NA), external_actor=c(NA),
                   count=c(NA))

# ---- bonding * connecting ----

B_C <- inner_join(bonding, connecting, by="Label")
stat[1,1] <- "B_C"
stat[1,2] <- length(which(B_C$classification.x=="university"))
stat[1,3] <- length(which(B_C$classification.x=="business"))
stat[1,4] <- length(which(B_C$classification.x=="public institute"))
stat[1,5] <- length(which(B_C$classification.x=="individual"))
stat[1,6] <- length(which(B_C$regions.x=="internal"))
stat[1,7] <- length(which(B_C$regions.x=="external"))
stat[1,8] <- length(B_C$Label)
# ---- bonding * external ----

B_EXT <- inner_join(bonding, external, by="Label")
stat[2,1] <- "B_EXT"
stat[2,2] <- length(which(B_EXT$classification.x=="university"))
stat[2,3] <- length(which(B_EXT$classification.x=="business"))
stat[2,4] <- length(which(B_EXT$classification.x=="public institute"))
stat[2,5] <- length(which(B_EXT$classification.x=="individual"))
stat[2,6] <- length(which(B_EXT$regions.x=="internal"))
stat[2,7] <- length(which(B_EXT$regions.x=="external"))
stat[2,8] <- length(B_EXT$Label)

# ---- bonding * internal ----

B_INT <- inner_join(bonding, internal, by="Label")
stat[3,1] <- "B_INT"
stat[3,2] <- length(which(B_INT$classification.x=="university"))
stat[3,3] <- length(which(B_INT$classification.x=="business"))
stat[3,4] <- length(which(B_INT$classification.x=="public institute"))
stat[3,5] <- length(which(B_INT$classification.x=="individual"))
stat[3,6] <- length(which(B_INT$regions.x=="internal"))
stat[3,7] <- length(which(B_INT$regions.x=="external"))
stat[3,8] <- length(B_INT$Label)

# ---- bonding * outdegree ----

B_OUT <- inner_join(bonding, outdegree, by="Label")
stat[4,1] <- "B_OUT"
stat[4,2] <- length(which(B_OUT$classification.x=="university"))
stat[4,3] <- length(which(B_OUT$classification.x=="business"))
stat[4,4] <- length(which(B_OUT$classification.x=="public institute"))
stat[4,5] <- length(which(B_OUT$classification.x=="individual"))
stat[4,6] <- length(which(B_OUT$regions.x=="internal"))
stat[4,7] <- length(which(B_OUT$regions.x=="external"))
stat[4,8] <- length(B_OUT$Label)

# ---- bonding * indegree ----

B_IN <- inner_join(bonding, indegree, by="Label")
stat[5,1] <- "B_IN"
stat[5,2] <- length(which(B_IN$classification.x=="university"))
stat[5,3] <- length(which(B_IN$classification.x=="business"))
stat[5,4] <- length(which(B_IN$classification.x=="public institute"))
stat[5,5] <- length(which(B_IN$classification.x=="individual"))
stat[5,6] <- length(which(B_IN$regions.x=="internal"))
stat[5,7] <- length(which(B_IN$regions.x=="external"))
stat[5,8] <- length(B_IN$Label)

# ---- connecting * external ----

C_EXT <- inner_join(connecting, external, by="Label")
stat[6,1] <- "C_EXT"
stat[6,2] <- length(which(C_EXT$classification.x=="university"))
stat[6,3] <- length(which(C_EXT$classification.x=="business"))
stat[6,4] <- length(which(C_EXT$classification.x=="public institute"))
stat[6,5] <- length(which(C_EXT$classification.x=="individual"))
stat[6,6] <- length(which(C_EXT$regions.x=="internal"))
stat[6,7] <- length(which(C_EXT$regions.x=="external"))
stat[6,8] <- length(C_EXT$Label)

# ---- connecting * internal ----

C_INT <- inner_join(connecting, internal, by="Label")
stat[7,1] <- "C_INT"
stat[7,2] <- length(which(C_INT$classification.x=="university"))
stat[7,3] <- length(which(C_INT$classification.x=="business"))
stat[7,4] <- length(which(C_INT$classification.x=="public institute"))
stat[7,5] <- length(which(C_INT$classification.x=="individual"))
stat[7,6] <- length(which(C_INT$regions.x=="internal"))
stat[7,7] <- length(which(C_INT$regions.x=="external"))
stat[7,8] <- length(C_INT$Label)

# ---- connecting * outdegree ----

C_OUT <- inner_join(connecting, outdegree, by="Label")
stat[8,1] <- "C_OUT"
stat[8,2] <- length(which(C_OUT$classification.x=="university"))
stat[8,3] <- length(which(C_OUT$classification.x=="business"))
stat[8,4] <- length(which(C_OUT$classification.x=="public institute"))
stat[8,5] <- length(which(C_OUT$classification.x=="individual"))
stat[8,6] <- length(which(C_OUT$regions.x=="internal"))
stat[8,7] <- length(which(C_OUT$regions.x=="external"))
stat[8,8] <- length(C_OUT$Label)

# ---- connecting * indegree ----

C_IN <- inner_join(connecting, indegree, by="Label")
stat[9,1] <- "C_IN"
stat[9,2] <- length(which(C_IN$classification.x=="university"))
stat[9,3] <- length(which(C_IN$classification.x=="business"))
stat[9,4] <- length(which(C_IN$classification.x=="public institute"))
stat[9,5] <- length(which(C_IN$classification.x=="individual"))
stat[9,6] <- length(which(C_IN$regions.x=="internal"))
stat[9,7] <- length(which(C_IN$regions.x=="external"))
stat[9,8] <- length(C_IN$Label)

# ---- external * internal ----

EXT_INT <- inner_join(external, internal, by="Label")
stat[10,1] <- "EXT_INT"
stat[10,2] <- length(which(EXT_INT$classification.x=="university"))
stat[10,3] <- length(which(EXT_INT$classification.x=="business"))
stat[10,4] <- length(which(EXT_INT$classification.x=="public institute"))
stat[10,5] <- length(which(EXT_INT$classification.x=="individual"))
stat[10,6] <- length(which(EXT_INT$regions.x=="internal"))
stat[10,7] <- length(which(EXT_INT$regions.x=="external"))
stat[10,8] <- length(EXT_INT$Label)

# ---- external * outdegree ----

EXT_OUT <- inner_join(external, outdegree, by="Label")
stat[11,1] <- "EXT_OUT"
stat[11,2] <- length(which(EXT_OUT$classification.x=="university"))
stat[11,3] <- length(which(EXT_OUT$classification.x=="business"))
stat[11,4] <- length(which(EXT_OUT$classification.x=="public institute"))
stat[11,5] <- length(which(EXT_OUT$classification.x=="individual"))
stat[11,6] <- length(which(EXT_OUT$regions.x=="internal"))
stat[11,7] <- length(which(EXT_OUT$regions.x=="external"))
stat[11,8] <- length(EXT_OUT$Label)

# ---- external * indegree ----

EXT_IN <- inner_join(external, indegree, by="Label")
stat[12,1] <- "EXT_IN"
stat[12,2] <- length(which(EXT_IN$classification.x=="university"))
stat[12,3] <- length(which(EXT_IN$classification.x=="business"))
stat[12,4] <- length(which(EXT_IN$classification.x=="public institute"))
stat[12,5] <- length(which(EXT_IN$classification.x=="individual"))
stat[12,6] <- length(which(EXT_IN$regions.x=="internal"))
stat[12,7] <- length(which(EXT_IN$regions.x=="external"))
stat[12,8] <- length(EXT_IN$Label)

# ---- internal * outdegree ----

INT_OUT <- inner_join(internal, outdegree, by="Label")
stat[13,1] <- "INT_OUT"
stat[13,2] <- length(which(INT_OUT$classification.x=="university"))
stat[13,3] <- length(which(INT_OUT$classification.x=="business"))
stat[13,4] <- length(which(INT_OUT$classification.x=="public institute"))
stat[13,5] <- length(which(INT_OUT$classification.x=="individual"))
stat[13,6] <- length(which(INT_OUT$regions.x=="internal"))
stat[13,7] <- length(which(INT_OUT$regions.x=="external"))
stat[13,8] <- length(INT_OUT$Label)

# ---- internal * indegree ----

INT_IN <- inner_join(internal, indegree, by="Label")
stat[14,1] <- "INT_IN"
stat[14,2] <- length(which(INT_IN$classification.x=="university"))
stat[14,3] <- length(which(INT_IN$classification.x=="business"))
stat[14,4] <- length(which(INT_IN$classification.x=="public institute"))
stat[14,5] <- length(which(INT_IN$classification.x=="individual"))
stat[14,6] <- length(which(INT_IN$regions.x=="internal"))
stat[14,7] <- length(which(INT_IN$regions.x=="external"))
stat[14,8] <- length(INT_IN$Label)

# ---- outdegree * indegree ----

OUT_IN <- inner_join(outdegree, indegree, by="Label")
stat[15,1] <- "OUT_IN"
stat[15,2] <- length(which(OUT_IN$classification.x=="university"))
stat[15,3] <- length(which(OUT_IN$classification.x=="business"))
stat[15,4] <- length(which(OUT_IN$classification.x=="public institute"))
stat[15,5] <- length(which(OUT_IN$classification.x=="individual"))
stat[15,6] <- length(which(OUT_IN$regions.x=="internal"))
stat[15,7] <- length(which(OUT_IN$regions.x=="external"))
stat[15,8] <- length(OUT_IN$Label)

# ---- next level ----
# ---- B_C_EXT ----

B_C_EXT <- inner_join(B_C, external, by="Label")
stat[17,1] <- "B_C_EXT"
stat[17,2] <- length(which(B_C_EXT$classification.x=="university"))
stat[17,3] <- length(which(B_C_EXT$classification.x=="business"))
stat[17,4] <- length(which(B_C_EXT$classification.x=="public institute"))
stat[17,5] <- length(which(B_C_EXT$classification.x=="individual"))
stat[17,6] <- length(which(B_C_EXT$regions.x=="internal"))
stat[17,7] <- length(which(B_C_EXT$regions.x=="external"))
stat[17,8] <- length(B_C_EXT$Label)

# ---- B_C_INT ----

B_C_INT <- inner_join(B_C, internal, by="Label")
stat[18,1] <- "B_C_INT"
stat[18,2] <- length(which(B_C_INT$classification.x=="university"))
stat[18,3] <- length(which(B_C_INT$classification.x=="business"))
stat[18,4] <- length(which(B_C_INT$classification.x=="public institute"))
stat[18,5] <- length(which(B_C_INT$classification.x=="individual"))
stat[18,6] <- length(which(B_C_INT$regions.x=="internal"))
stat[18,7] <- length(which(B_C_INT$regions.x=="external"))
stat[18,8] <- length(B_C_INT$Label)

# ---- B_C_OUT ----

B_C_OUT <- inner_join(B_C, outdegree, by="Label")
stat[19,1] <- "B_C_OUT"
stat[19,2] <- length(which(B_C_OUT$classification.x=="university"))
stat[19,3] <- length(which(B_C_OUT$classification.x=="business"))
stat[19,4] <- length(which(B_C_OUT$classification.x=="public institute"))
stat[19,5] <- length(which(B_C_OUT$classification.x=="individual"))
stat[19,6] <- length(which(B_C_OUT$regions.x=="internal"))
stat[19,7] <- length(which(B_C_OUT$regions.x=="external"))
stat[19,8] <- length(B_C_OUT$Label)

# ---- B_C_IN ----

B_C_IN <- inner_join(B_C, indegree, by="Label")
stat[20,1] <- "B_C_IN"
stat[20,2] <- length(which(B_C_IN$classification.x=="university"))
stat[20,3] <- length(which(B_C_IN$classification.x=="business"))
stat[20,4] <- length(which(B_C_IN$classification.x=="public institute"))
stat[20,5] <- length(which(B_C_IN$classification.x=="individual"))
stat[20,6] <- length(which(B_C_IN$regions.x=="internal"))
stat[20,7] <- length(which(B_C_IN$regions.x=="external"))
stat[20,8] <- length(B_C_IN$Label)

# ---- C_EXT_INT ----

C_EXT_INT <- inner_join(C_EXT, internal, by="Label")
stat[21,1] <- "C_EXT_INT"
stat[21,2] <- length(which(C_EXT_INT$classification.x=="university"))
stat[21,3] <- length(which(C_EXT_INT$classification.x=="business"))
stat[21,4] <- length(which(C_EXT_INT$classification.x=="public institute"))
stat[21,5] <- length(which(C_EXT_INT$classification.x=="individual"))
stat[21,6] <- length(which(C_EXT_INT$regions.x=="internal"))
stat[21,7] <- length(which(C_EXT_INT$regions.x=="external"))
stat[21,8] <- length(C_EXT_INT$Label)

# ---- C_EXT_OUT ----

C_EXT_OUT <- inner_join(C_EXT, outdegree, by="Label")
stat[22,1] <- "C_EXT_OUT"
stat[22,2] <- length(which(C_EXT_OUT$classification.x=="university"))
stat[22,3] <- length(which(C_EXT_OUT$classification.x=="business"))
stat[22,4] <- length(which(C_EXT_OUT$classification.x=="public institute"))
stat[22,5] <- length(which(C_EXT_OUT$classification.x=="individual"))
stat[22,6] <- length(which(C_EXT_OUT$regions.x=="internal"))
stat[22,7] <- length(which(C_EXT_OUT$regions.x=="external"))
stat[22,8] <- length(C_EXT_OUT$Label)

# ---- C_EXT_IN ----

C_EXT_IN <- inner_join(C_EXT, indegree, by="Label")
stat[23,1] <- "C_EXT_IN"
stat[23,2] <- length(which(C_EXT_IN$classification.x=="university"))
stat[23,3] <- length(which(C_EXT_IN$classification.x=="business"))
stat[23,4] <- length(which(C_EXT_IN$classification.x=="public institute"))
stat[23,5] <- length(which(C_EXT_IN$classification.x=="individual"))
stat[23,6] <- length(which(C_EXT_IN$regions.x=="internal"))
stat[23,7] <- length(which(C_EXT_IN$regions.x=="external"))
stat[23,8] <- length(C_EXT_IN$Label)

# ---- EXT_INT_OUT ----

EXT_INT_OUT <- inner_join(EXT_INT, outdegree, by="Label")
stat[24,1] <- "EXT_INT_OUT"
stat[24,2] <- length(which(EXT_INT_OUT$classification.x=="university"))
stat[24,3] <- length(which(EXT_INT_OUT$classification.x=="business"))
stat[24,4] <- length(which(EXT_INT_OUT$classification.x=="public institute"))
stat[24,5] <- length(which(EXT_INT_OUT$classification.x=="individual"))
stat[24,6] <- length(which(EXT_INT_OUT$regions.x=="internal"))
stat[24,7] <- length(which(EXT_INT_OUT$regions.x=="external"))
stat[24,8] <- length(EXT_INT_OUT$Label)

# ---- EXT_INT_IN ----

EXT_INT_IN <- inner_join(EXT_INT, indegree, by="Label")
stat[25,1] <- "EXT_INT_IN"
stat[25,2] <- length(which(EXT_INT_IN$classification.x=="university"))
stat[25,3] <- length(which(EXT_INT_IN$classification.x=="business"))
stat[25,4] <- length(which(EXT_INT_IN$classification.x=="public institute"))
stat[25,5] <- length(which(EXT_INT_IN$classification.x=="individual"))
stat[25,6] <- length(which(EXT_INT_IN$regions.x=="internal"))
stat[25,7] <- length(which(EXT_INT_IN$regions.x=="external"))
stat[25,8] <- length(EXT_INT_IN$Label)

# ---- INT_OUT_IN ----

INT_OUT_IN <- inner_join(INT_OUT, indegree, by="Label")
stat[26,1] <- "INT_OUT_IN"
stat[26,2] <- length(which(INT_OUT_IN$classification.x=="university"))
stat[26,3] <- length(which(INT_OUT_IN$classification.x=="business"))
stat[26,4] <- length(which(INT_OUT_IN$classification.x=="public institute"))
stat[26,5] <- length(which(INT_OUT_IN$classification.x=="individual"))
stat[26,6] <- length(which(INT_OUT_IN$regions.x=="internal"))
stat[26,7] <- length(which(INT_OUT_IN$regions.x=="external"))
stat[26,8] <- length(INT_OUT_IN$Label)

# ---- next level ----

# ---- B_C_EXT_INT ----

B_C_EXT_INT <- inner_join(B_C_EXT, internal, by="Label")
stat[28,1] <- "B_C_EXT_INT"
stat[28,2] <- length(which(B_C_EXT_INT$classification.x=="university"))
stat[28,3] <- length(which(B_C_EXT_INT$classification.x=="business"))
stat[28,4] <- length(which(B_C_EXT_INT$classification.x=="public institute"))
stat[28,5] <- length(which(B_C_EXT_INT$classification.x=="individual"))
stat[28,6] <- length(which(B_C_EXT_INT$regions.x=="internal"))
stat[28,7] <- length(which(B_C_EXT_INT$regions.x=="external"))
stat[28,8] <- length(B_C_EXT_INT$Label)

# ---- B_C_EXT_OUT ----

B_C_EXT_OUT <- inner_join(B_C_EXT, outdegree, by="Label")
stat[29,1] <- "B_C_EXT_OUT"
stat[29,2] <- length(which(B_C_EXT_OUT$classification.x=="university"))
stat[29,3] <- length(which(B_C_EXT_OUT$classification.x=="business"))
stat[29,4] <- length(which(B_C_EXT_OUT$classification.x=="public institute"))
stat[29,5] <- length(which(B_C_EXT_OUT$classification.x=="individual"))
stat[29,6] <- length(which(B_C_EXT_OUT$regions.x=="internal"))
stat[29,7] <- length(which(B_C_EXT_OUT$regions.x=="external"))
stat[29,8] <- length(B_C_EXT_OUT$Label)

# ---- B_C_EXT_IN ----

B_C_EXT_IN <- inner_join(B_C_EXT, indegree, by="Label")
stat[30,1] <- "B_C_EXT_IN"
stat[30,2] <- length(which(B_C_EXT_IN$classification.x=="university"))
stat[30,3] <- length(which(B_C_EXT_IN$classification.x=="business"))
stat[30,4] <- length(which(B_C_EXT_IN$classification.x=="public institute"))
stat[30,5] <- length(which(B_C_EXT_IN$classification.x=="individual"))
stat[30,6] <- length(which(B_C_EXT_IN$regions.x=="internal"))
stat[30,7] <- length(which(B_C_EXT_IN$regions.x=="external"))
stat[30,8] <- length(B_C_EXT_IN$Label)

# ---- C_EXT_INT_OUT ----

C_EXT_INT_OUT <- inner_join(C_EXT_INT, outdegree, by="Label")
stat[31,1] <- "C_EXT_INT_OUT"
stat[31,2] <- length(which(C_EXT_INT_OUT$classification.x=="university"))
stat[31,3] <- length(which(C_EXT_INT_OUT$classification.x=="business"))
stat[31,4] <- length(which(C_EXT_INT_OUT$classification.x=="public institute"))
stat[31,5] <- length(which(C_EXT_INT_OUT$classification.x=="individual"))
stat[31,6] <- length(which(C_EXT_INT_OUT$regions.x=="internal"))
stat[31,7] <- length(which(C_EXT_INT_OUT$regions.x=="external"))
stat[31,8] <- length(C_EXT_INT_OUT$Label)

# ---- C_EXT_INT_IN ----

C_EXT_INT_IN <- inner_join(C_EXT_INT, indegree, by="Label")
stat[32,1] <- "C_EXT_INT_IN"
stat[32,2] <- length(which(C_EXT_INT_IN$classification.x=="university"))
stat[32,3] <- length(which(C_EXT_INT_IN$classification.x=="business"))
stat[32,4] <- length(which(C_EXT_INT_IN$classification.x=="public institute"))
stat[32,5] <- length(which(C_EXT_INT_IN$classification.x=="individual"))
stat[32,6] <- length(which(C_EXT_INT_IN$regions.x=="internal"))
stat[32,7] <- length(which(C_EXT_INT_IN$regions.x=="external"))
stat[32,8] <- length(C_EXT_INT_IN$Label)

# ---- EXT_INT_OUT_IN ----

EXT_INT_OUT_IN <- inner_join(EXT_INT_OUT, indegree, by="Label")
stat[33,1] <- "EXT_INT_OUT_IN"
stat[33,2] <- length(which(EXT_INT_OUT_IN$classification.x=="university"))
stat[33,3] <- length(which(EXT_INT_OUT_IN$classification.x=="business"))
stat[33,4] <- length(which(EXT_INT_OUT_IN$classification.x=="public institute"))
stat[33,5] <- length(which(EXT_INT_OUT_IN$classification.x=="individual"))
stat[33,6] <- length(which(EXT_INT_OUT_IN$regions.x=="internal"))
stat[33,7] <- length(which(EXT_INT_OUT_IN$regions.x=="external"))
stat[33,8] <- length(EXT_INT_OUT_IN$Label)

# ---- next level ----

# ---- B_C_EXT_INT_OUT ----

B_C_EXT_INT_OUT <- inner_join(B_C_EXT_INT, outdegree, by="Label")
stat[35,1] <- "B_C_EXT_INT_OUT"
stat[35,2] <- length(which(B_C_EXT_INT_OUT$classification.x=="university"))
stat[35,3] <- length(which(B_C_EXT_INT_OUT$classification.x=="business"))
stat[35,4] <- length(which(B_C_EXT_INT_OUT$classification.x=="public institute"))
stat[35,5] <- length(which(B_C_EXT_INT_OUT$classification.x=="individual"))
stat[35,6] <- length(which(B_C_EXT_INT_OUT$regions.x=="internal"))
stat[35,7] <- length(which(B_C_EXT_INT_OUT$regions.x=="external"))
stat[35,8] <- length(B_C_EXT_INT_OUT$Label)

# ---- B_C_EXT_INT_IN ----

B_C_EXT_INT_IN <- inner_join(B_C_EXT_INT, indegree, by="Label")
stat[36,1] <- "B_C_EXT_INT_IN"
stat[36,2] <- length(which(B_C_EXT_INT_IN$classification.x=="university"))
stat[36,3] <- length(which(B_C_EXT_INT_IN$classification.x=="business"))
stat[36,4] <- length(which(B_C_EXT_INT_IN$classification.x=="public institute"))
stat[36,5] <- length(which(B_C_EXT_INT_IN$classification.x=="individual"))
stat[36,6] <- length(which(B_C_EXT_INT_IN$regions.x=="internal"))
stat[36,7] <- length(which(B_C_EXT_INT_IN$regions.x=="external"))
stat[36,8] <- length(B_C_EXT_INT_IN$Label)

# ---- C_EXT_INT_OUT_IN ----

C_EXT_INT_OUT_IN <- inner_join(C_EXT_INT_OUT, indegree, by="Label")
stat[37,1] <- "C_EXT_INT_OUT_IN"
stat[37,2] <- length(which(C_EXT_INT_OUT_IN$classification.x=="university"))
stat[37,3] <- length(which(C_EXT_INT_OUT_IN$classification.x=="business"))
stat[37,4] <- length(which(C_EXT_INT_OUT_IN$classification.x=="public institute"))
stat[37,5] <- length(which(C_EXT_INT_OUT_IN$classification.x=="individual"))
stat[37,6] <- length(which(C_EXT_INT_OUT_IN$regions.x=="internal"))
stat[37,7] <- length(which(C_EXT_INT_OUT_IN$regions.x=="external"))
stat[37,8] <- length(C_EXT_INT_OUT_IN$Label)

# ---- next level ----

# ---- B_C_EXT_INT_OUT_IN ----

B_C_EXT_INT_OUT_IN <- inner_join(B_C_EXT_INT_OUT, indegree, by="Label")
stat[39,1] <- "B_C_EXT_INT_OUT_IN"
stat[39,2] <- length(which(B_C_EXT_INT_OUT_IN$classification.x=="university"))
stat[39,3] <- length(which(B_C_EXT_INT_OUT_IN$classification.x=="business"))
stat[39,4] <- length(which(B_C_EXT_INT_OUT_IN$classification.x=="public institute"))
stat[39,5] <- length(which(B_C_EXT_INT_OUT_IN$classification.x=="individual"))
stat[39,6] <- length(which(B_C_EXT_INT_OUT_IN$regions.x=="internal"))
stat[39,7] <- length(which(B_C_EXT_INT_OUT_IN$regions.x=="external"))
stat[39,8] <- length(B_C_EXT_INT_OUT_IN$Label)

# ---- calculation ----

stat$university_per <- percent((stat$university / stat$count),accuracy = .01)
stat$company_per <- percent((stat$company / stat$count),accuracy = .01)
stat$public_institute_per <- percent((stat$public_institute / stat$count),accuracy = .01)
stat$individual_per <- percent((stat$individual / stat$count),accuracy = .01)

stat$internal_actor_per <- percent((stat$internal_actor / stat$count),accuracy = .01)
stat$external_actor_per <- percent((stat$external_actor / stat$count),accuracy = .01)

# ---- output ----
write.csv(stat,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/stat.csv")

write.csv(B_C,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C.csv")
write.csv(B_EXT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_EXT.csv")
write.csv(B_INT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_INT.csv")
write.csv(B_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_OUT.csv")
write.csv(B_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_IN.csv")
write.csv(C_EXT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_EXT.csv")
write.csv(C_INT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_INT.csv")
write.csv(C_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_OUT.csv")
write.csv(C_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_IN.csv")
write.csv(EXT_INT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/EXT_INT.csv")
write.csv(EXT_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/EXT_OUT.csv")
write.csv(EXT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/EXT_IN.csv")
write.csv(INT_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/INT_OUT.csv")
write.csv(INT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/INT_IN.csv")
write.csv(OUT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/OUT_IN.csv")

write.csv(B_C_EXT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_EXT.csv")
write.csv(B_C_INT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_INT.csv")
write.csv(B_C_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_OUT.csv")
write.csv(B_C_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_IN.csv")
write.csv(C_EXT_INT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_EXT_INT.csv")
write.csv(C_EXT_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_EXT_OUT.csv")
write.csv(C_EXT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_EXT_IN.csv")
write.csv(EXT_INT_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/EXT_INT_OUT.csv")
write.csv(EXT_INT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/EXT_INT_IN.csv")
write.csv(INT_OUT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/INT_OUT_IN.csv")

write.csv(B_C_EXT_INT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_EXT_INT.csv")
write.csv(B_C_EXT_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_EXT_OUT.csv")
write.csv(B_C_EXT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_EXT_IN.csv")
write.csv(C_EXT_INT_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_EXT_INT_OUT.csv")
write.csv(C_EXT_INT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_EXT_INT_IN.csv")
write.csv(EXT_INT_OUT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/EXT_INT_OUT_IN.csv")

write.csv(B_C_EXT_INT_OUT,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_EXT_INT_OUT.csv")
write.csv(B_C_EXT_INT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_EXT_INT_IN.csv")
write.csv(C_EXT_INT_OUT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/C_EXT_INT_OUT_IN.csv")

write.csv(B_C_EXT_INT_OUT_IN,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/B_C_EXT_INT_OUT_IN.csv")










  
  