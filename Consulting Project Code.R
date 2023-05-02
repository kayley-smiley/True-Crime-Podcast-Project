# Read XLSX

# library(readxl)
# Podcast_Data_XL <- read_excel("~/Math Master/Math 6330 Statistical Consulting/Copy of Crime Podcast Survey_February 15%2C 2023_15.49.xlsx")
# View(Podcast_Data_XL)
# summary(Podcast_Data_XL)

# Read CSV

library(dplyr)
library(ggplot2)

Crime_Podcast_Data0 <- read.csv(file = '~/Math Master/Math 6330 Statistical Consulting/Crime Podcast Survey RO.csv')
View(Crime_Podcast_Data0)
summary(Crime_Podcast_Data0)

# Clean Data a Bit

Crime_Podcast_Data <- Crime_Podcast_Data0

Crime_Podcast_Data1 <- Crime_Podcast_Data0

# # Code to reverse numbering if desired
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]==1] <- "A"
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]==2] <- "B"
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]==3] <- "C"
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]==4] <- "D"
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]==5] <- "E"
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]=="A"] <- 5
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]=="B"] <- 4
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]=="C"] <- 3
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]=="D"] <- 2
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]=="E"] <- 1
# 
# View(Crime_Podcast_Data)
# 
# # End of reverse numbering
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]=="NA"] <- NA
# 
# Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
#                       'Q7.1_4')][Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                                        'Q7.1_3', 'Q7.1_4')]==""] <- NA
# 
# Crime_Podcast_Data[,'Q4.5'][Crime_Podcast_Data[,'Q4.5']=="NA"] <- NA
# 
# Crime_Podcast_Data[,'Q4.5'][Crime_Podcast_Data[,'Q4.5']==""] <- NA
# 
# Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q4.5))
# 
# # Crime_Podcast_Data<-subset(Crime_Podcast_Data,!is.na(
# #   Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
# #                         'Q7.1_3', 'Q7.1_4')]))
# 
# Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q7.1_1))
# Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q7.1_2))
# Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q7.1_3))
# Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q7.1_4))
# 
# View(Crime_Podcast_Data)
# 
# Crime_Podcast_Data$sum_7.1 <- rowSums(Crime_Podcast_Data[,c('Q7.1_1', 'Q7.1_2',
#                                          'Q7.1_3', 'Q7.1_4')])
# 
# #sum_7.1
# #Crime_Podcast_Data$Q4.5
# 
# (Interact_4.5 <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$Q4.5))
# 
# View(Crime_Podcast_Data)
# 
# write.table(Interact_4.5, file = "Interaction Table 4.5")
# write.csv(Interact_4.5, "Interaction Table 4.5")

# n <- nrow(Crime_Podcast_Data)
# sum_7.1 <- numeric(n)

#for (i in 1:n) {


# Test Complete using Q4.5


# ===== Start of Comparing Data ===== #

# Table of Q7.1 x Q11.3

# Q 7.1 Sum Cleaning
Crime_Podcast_Data1 <- Crime_Podcast_Data0

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]==1] <- "A"

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]==2] <- "B"

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]==3] <- "C"

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]==4] <- "D"

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]==5] <- "E"

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]=="A"] <- 5

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]=="B"] <- 4

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]=="C"] <- 3

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]=="D"] <- 2

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]=="E"] <- 1

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]=="NA"] <- NA

Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2', 'Q7.1_3',
                      'Q7.1_4')][Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                       'Q7.1_3', 'Q7.1_4')]==""] <- NA

Crime_Podcast_Data1 <- subset(Crime_Podcast_Data1,!is.na(Crime_Podcast_Data$Q7.1_1))
Crime_Podcast_Data1 <- subset(Crime_Podcast_Data1,!is.na(Crime_Podcast_Data$Q7.1_2))
Crime_Podcast_Data1 <- subset(Crime_Podcast_Data1,!is.na(Crime_Podcast_Data$Q7.1_3))
Crime_Podcast_Data1 <- subset(Crime_Podcast_Data1,!is.na(Crime_Podcast_Data$Q7.1_4))

Crime_Podcast_Data1$Q7.1_1 <- as.numeric(Crime_Podcast_Data1$Q7.1_1)
Crime_Podcast_Data1$Q7.1_2 <- as.numeric(Crime_Podcast_Data1$Q7.1_2)
Crime_Podcast_Data1$Q7.1_3 <- as.numeric(Crime_Podcast_Data1$Q7.1_3)
Crime_Podcast_Data1$Q7.1_4 <- as.numeric(Crime_Podcast_Data1$Q7.1_4)

# Sum of numbers in Q7.1 (1-4)
Crime_Podcast_Data1$sum_7.1 <- rowSums(Crime_Podcast_Data1[,c('Q7.1_1', 'Q7.1_2',
                                                            'Q7.1_3', 'Q7.1_4')])

# Q11.3 Cleanup

Crime_Podcast_Data <- Crime_Podcast_Data1

Crime_Podcast_Data[,c('Q11.3_1', 'Q11.3_2', 'Q11.3_3',
                      'Q11.3_4')][Crime_Podcast_Data[,c('Q11.3_1', 'Q11.3_2', 'Q11.3_3',
                                                        'Q11.3_4')]=="NA"] <- NA

Crime_Podcast_Data[,c('Q11.3_1', 'Q11.3_2', 'Q11.3_3',
                      'Q11.3_4')][Crime_Podcast_Data[,c('Q11.3_1', 'Q11.3_2', 'Q11.3_3',
                                                        'Q11.3_4')]==""] <- NA

Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.3_1))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.3_2))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.3_3))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.3_4))

# Crime_Podcast_Data %>%
#   rowwise %>%
#   mutate(vic11.3 = rowSums(across(Q11.3_1:Q11.3_4, ~ .x)) > 0)

Crime_Podcast_Data$vic11.3 <- rowSums(Crime_Podcast_Data[,c('Q11.3_1', 'Q11.3_2',
                                                            'Q11.3_3', 'Q11.3_4')])<8
Crime_Podcast_Data$prop11.3 <- rowSums(Crime_Podcast_Data[,c('Q11.3_1', 'Q11.3_2')])<4
Crime_Podcast_Data$threat11.3 <- rowSums(Crime_Podcast_Data[,c('Q11.3_3', 'Q11.3_4')])<4

View(Crime_Podcast_Data)

Interact_11.3 <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$vic11.3)
Interact_11.3prop <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$prop11.3)
Interact_11.3threat <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$threat11.3)

Interact_11.3 <- as.data.frame(Interact_11.3)
Interact_11.3[,2] <- factor(Interact_11.3[,2], levels=c("TRUE", "FALSE"))
Interact_11.3

Interact_11.3prop <- as.data.frame(Interact_11.3prop)
Interact_11.3prop[,2] <- factor(Interact_11.3prop[,2], levels=c("TRUE", "FALSE"))
Interact_11.3prop

Interact_11.3threat <- as.data.frame(Interact_11.3threat)
Interact_11.3threat[,2] <- factor(Interact_11.3threat[,2], levels=c("TRUE", "FALSE"))
Interact_11.3threat

write.csv(Interact_11.3, "Interaction Table 11_3.csv")
write.csv(Interact_11.3prop, "Interaction Table 11_3_Prop.csv")
write.csv(Interact_11.3threat, "Interaction Table 11_3_Threat.csv")

cor(Crime_Podcast_Data$vic11.3, Crime_Podcast_Data$sum_7.1)
cor(Crime_Podcast_Data$prop11.3, Crime_Podcast_Data$sum_7.1)
cor(Crime_Podcast_Data$threat11.3, Crime_Podcast_Data$sum_7.1)

# ======== Heat Map Stuff ==========

heatmap(Interact_11.3, Colv = NA, Rowv = NA, scale="column", xlab="Q11.3 Response")

# install.packages("pheatmap")               # Install pheatmap package
library(pheatmap)


pheatmap(Interact_11.3,                         # Create heatmap with values
         display_numbers = TRUE)

# ====== Done with Heat Map Stuff =========

cor(Crime_Podcast_Data$vic11.3, Crime_Podcast_Data$sum_7.1)

# Grouped
# (sum_7.1_count <- count(Crime_Podcast_Data, sum_7.1))
# Interact_11.3$count <- sum_7.1_count[,2]
# Interact_11.3

# install.packages("plotrix")
# library(plotrix)
# SE <- std.error(Interact_11.3[,3])
# SE
# 
# ggplot(Interact_11.3, aes(fill=Interact_11.3[,2], y=Interact_11.3[,3], x=Interact_11.3[,1])) + 
#   geom_bar(position="dodge", stat="identity")+geom_errorbar(aes(ymax = Interact_11.3[,3] + SE, ymin = Interact_11.3[,3] - SE), position = position_dodge(width = 0.9), width = 0.2)

# ===== Bar Plot ====== #

# Considers All Types of Victimization (Q11.3)
ggplot(Interact_11.3, aes(fill=Interact_11.3[,2], y=Interact_11.3[,3],
                          x=Interact_11.3[,1])) + labs(title = "Q7.1/Q11.3 Interaction",
                                                       x = "Q7.1 Sums", y = "Frequency",
                                                       fill = "Q11.3 Lifetime") +
  scale_fill_discrete(labels = c("\"Victim\"", "\"Non-Victim\"")) +
  geom_bar(position="dodge", stat="identity")

# Considers Only (Q11.3_1, Q11.3_2)
ggplot(Interact_11.3prop, aes(fill=Interact_11.3prop[,2], y=Interact_11.3prop[,3],
                          x=Interact_11.3prop[,1])) + labs(title = "Q7.1/Q11.3_1-2 Interaction",
                                                       x = "Q7.1 Sums", y = "Frequency",
                                                       fill = "Q11.3 Lifetime") +
  scale_fill_discrete(labels = c("\"Victim\"", "\"Non-Victim\"")) +
  geom_bar(position="dodge", stat="identity")

# Considers Only (Q11.3_3, Q11.3_4)
ggplot(Interact_11.3threat, aes(fill=Interact_11.3threat[,2], y=Interact_11.3threat[,3],
                          x=Interact_11.3threat[,1])) + labs(title = "Q7.1/Q11.3_3-4 Interaction",
                                                       x = "Q7.1 Sums", y = "Frequency",
                                                       fill = "Q11.3 Lifetime") +
  scale_fill_discrete(labels = c("\"Victim\"", "\"Non-Victim\"")) +
  geom_bar(position="dodge", stat="identity")

# Next interaction:

# Table of Q7.1 x Q11.4

Crime_Podcast_Data <- Crime_Podcast_Data1

# Q11.4 Cleanup

Crime_Podcast_Data[,c('Q11.4_1', 'Q11.4_2', 'Q11.4_3',
                      'Q11.4_4')][Crime_Podcast_Data[,c('Q11.4_1', 'Q11.4_2', 'Q11.4_3',
                                                        'Q11.4_4')]=="NA"] <- NA

Crime_Podcast_Data[,c('Q11.4_1', 'Q11.4_2', 'Q11.4_3',
                      'Q11.4_4')][Crime_Podcast_Data[,c('Q11.4_1', 'Q11.4_2', 'Q11.4_3',
                                                        'Q11.4_4')]==""] <- NA

Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.4_1))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.4_2))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.4_3))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.4_4))

Crime_Podcast_Data$vic11.4 <- rowSums(Crime_Podcast_Data[,c('Q11.4_1', 'Q11.4_2',
                                                            'Q11.4_3', 'Q11.4_4')])<8
Crime_Podcast_Data$prop11.4 <- rowSums(Crime_Podcast_Data[,c('Q11.4_1', 'Q11.4_2')])<4
Crime_Podcast_Data$threat11.4 <- rowSums(Crime_Podcast_Data[,c('Q11.4_3', 'Q11.4_4')])<4

Interact_11.4 <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$vic11.4)
Interact_11.4prop <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$prop11.4)
Interact_11.4threat <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$threat11.4)

(Interact_11.4 <- as.data.frame(Interact_11.4))
Interact_11.4[,2] <- factor(Interact_11.4[,2], levels=c("TRUE", "FALSE"))
Interact_11.4

Interact_11.4prop <- as.data.frame(Interact_11.4prop)
Interact_11.4prop[,2] <- factor(Interact_11.4prop[,2], levels=c("TRUE", "FALSE"))
Interact_11.4prop

Interact_11.4threat <- as.data.frame(Interact_11.4threat)
Interact_11.4threat[,2] <- factor(Interact_11.4threat[,2], levels=c("TRUE", "FALSE"))
Interact_11.4threat

# Write Tables

write.csv(Interact_11.4, "Interaction Table 11_4.csv")
write.csv(Interact_11.4prop, "Interaction Table 11_4_Prop.csv")
write.csv(Interact_11.4threat, "Interaction Table 11_4_Threat.csv")

View(Crime_Podcast_Data)

cor(Crime_Podcast_Data$vic11.4, Crime_Podcast_Data$sum_7.1)
cor(Crime_Podcast_Data$prop11.4, Crime_Podcast_Data$sum_7.1)
cor(Crime_Podcast_Data$threat11.4, Crime_Podcast_Data$sum_7.1)

# ===== Bar Plot ====== #

# Considers All Types of Victimization (Q11.4)
ggplot(Interact_11.4, aes(fill=Interact_11.4[,2], y=Interact_11.4[,3],
                          x=Interact_11.4[,1])) + labs(title = "Q7.1/Q11.4 Interaction",
                                                       x = "Q7.1 Sums", y = "Frequency",
                                                       fill = "Q11.4 Last Year") +
  scale_fill_discrete(labels = c("\"Victim\"", "\"Non-Victim\"")) +
  geom_bar(position="dodge", stat="identity")

# Considers Only (Q11.4_1, Q11.4_2)
ggplot(Interact_11.4prop, aes(fill=Interact_11.4prop[,2], y=Interact_11.4prop[,3],
                              x=Interact_11.4prop[,1])) + labs(title = "Q7.1/Q11.4_1-2 Interaction",
                                                               x = "Q7.1 Sums", y = "Frequency",
                                                               fill = "Q11.4 Last Year") +
  scale_fill_discrete(labels = c("\"Victim\"", "\"Non-Victim\"")) +
  geom_bar(position="dodge", stat="identity")

# Considers Only (Q11.4_3, Q11.4_4)
ggplot(Interact_11.4threat, aes(fill=Interact_11.4threat[,2], y=Interact_11.4threat[,3],
                                x=Interact_11.4threat[,1])) + labs(title = "Q7.1/Q11.4_3-4 Interaction",
                                                                   x = "Q7.1 Sums", y = "Frequency",
                                                                   fill = "Q11.4 Last Year") +
  scale_fill_discrete(labels = c("\"Victim\"", "\"Non-Victim\"")) +
  geom_bar(position="dodge", stat="identity")

# Next interaction:

# Table of Q7.1 x Q11.8

Crime_Podcast_Data <- Crime_Podcast_Data1

# Q11.8 Cleanup

Crime_Podcast_Data[,'Q11.8'][Crime_Podcast_Data[,'Q11.8']=="NA"] <- NA

Crime_Podcast_Data[,'Q11.8'][Crime_Podcast_Data[,'Q11.8']==""] <- NA

Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.8))

View(Crime_Podcast_Data)

Interact_11.8 <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$Q11.8)

write.csv(Interact_11.8, "Interaction Table 11_8.csv")

# Interact_11.4[,2] <- factor(Interact_11.4[,2], levels=c("TRUE", "FALSE"))
# Interact_11.4

# cor(Crime_Podcast_Data$Q11.8, Crime_Podcast_Data$sum_7.1)

(Interact_11.8 <- as.data.frame(Interact_11.8))
#Interact_11.8[,2] <- factor(Interact_11.8[,2], levels=c("TRUE", "FALSE"))

# Bar Plot

ggplot(Interact_11.8, aes(fill=Interact_11.8[,2], y=Interact_11.8[,3],
                          x=Interact_11.8[,1])) + labs(title = "Q7.1/Q11.8 Interaction",
                                                       x = "Q7.1 Sums", y = "Frequency",
                                                       fill = "Q11.8 Last Year") +
  scale_fill_discrete(labels = c("Victim", "Non-Victim")) +
  geom_bar(position="dodge", stat="identity")

# ggplot(Interact_11.8, aes(fill=Interact_11.8[,2], y=Interact_11.8[,3],
#                           x=Interact_11.8[,1])) + labs(title = "Q7.1/Q11.8 Interaction",
#                                                        x = "Q7.1 Sums", y = "Frequency") +
#   geom_bar(position="dodge", stat="identity") +
#   scale_fill_manual(name = "Q11.8 Response", labels = c("Yes", "No"),
#                                                                   values=c("#18A558", "#DC143C"))

# Next interaction:

# Table of Q7.1 x Q11.10

Crime_Podcast_Data <- Crime_Podcast_Data1

# Q11.10 Cleanup

Crime_Podcast_Data[,'Q11.10'][Crime_Podcast_Data[,'Q11.10']=="NA"] <- NA

Crime_Podcast_Data[,'Q11.10'][Crime_Podcast_Data[,'Q11.10']==""] <- NA

Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q11.10))

View(Crime_Podcast_Data)

(Interact_11.10 <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$Q11.10))

write.csv(Interact_11.10, "Interaction Table 11_10.csv")

# cor(Crime_Podcast_Data$Q11.10, Crime_Podcast_Data$sum_7.1)

(Interact_11.10 <- as.data.frame(Interact_11.10))

# Bar Plot

ggplot(Interact_11.10, aes(fill=Interact_11.10[,2], y=Interact_11.10[,3],
                          x=Interact_11.10[,1])) + labs(title = "Q7.1/Q11.10 Interaction",
                                                       x = "Q7.1 Sums", y = "Frequency",
                                                       fill = "Q11.10 Last Year") +
  scale_fill_discrete(labels = c("Victim", "Non-Victim")) +
  geom_bar(position="dodge", stat="identity")

# ggplot(Interact_11.10, aes(fill=Interact_11.10[,2], y=Interact_11.10[,3],
#                           x=Interact_11.10[,1])) + labs(title = "Q7.1/Q11.10 Interaction",
#                                                        x = "Q7.1 Sums", y = "Frequency") +
#   geom_bar(position="dodge", stat="identity") +
#   scale_fill_manual(name = "Q11.10 Response", labels = c("Yes", "No"),
#                     values=c("#18A558", "#DC143C"))

# Next interaction:

# Table of Q7.1 x Q8.1

View(Crime_Podcast_Data)

# Q8.1 Cleanup

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]==1] <- "A"

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                      'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                  'Q8.1_2',
                                                                  'Q8.1_3',
                                                                  'Q8.1_4',
                                                                  'Q8.1_5')]==2] <- "B"

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                      'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                  'Q8.1_2',
                                                                  'Q8.1_3',
                                                                  'Q8.1_4',
                                                                  'Q8.1_5')]==3] <- "C"

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                      'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                  'Q8.1_2',
                                                                  'Q8.1_3',
                                                                  'Q8.1_4',
                                                                  'Q8.1_5')]==4] <- "D"

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                      'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                  'Q8.1_2',
                                                                  'Q8.1_3',
                                                                  'Q8.1_4',
                                                                  'Q8.1_5')]==5] <- "E"

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]=="A"] <- 5

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]=="B"] <- 4

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]=="C"] <- 3

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]=="D"] <- 2

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]=="E"] <- 1

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]=="NA"] <- NA

Crime_Podcast_Data1[,c('Q8.1_1', 'Q8.1_2', 'Q8.1_3',
                       'Q8.1_4', 'Q8.1_5')][Crime_Podcast_Data1[,c('Q8.1_1',
                                                                   'Q8.1_2',
                                                                   'Q8.1_3',
                                                                   'Q8.1_4',
                                                                   'Q8.1_5')]==""] <- NA

Crime_Podcast_Data <- Crime_Podcast_Data1

Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q8.1_1))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q8.1_2))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q8.1_3))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q8.1_4))
Crime_Podcast_Data <- subset(Crime_Podcast_Data,!is.na(Crime_Podcast_Data$Q8.1_5))

Crime_Podcast_Data$Q8.1_1 <- as.numeric(Crime_Podcast_Data$Q8.1_1)
Crime_Podcast_Data$Q8.1_2 <- as.numeric(Crime_Podcast_Data$Q8.1_2)
Crime_Podcast_Data$Q8.1_3 <- as.numeric(Crime_Podcast_Data$Q8.1_3)
Crime_Podcast_Data$Q8.1_4 <- as.numeric(Crime_Podcast_Data$Q8.1_4)
Crime_Podcast_Data$Q8.1_5 <- as.numeric(Crime_Podcast_Data$Q8.1_5)

# Sum of numbers in Q8.1 (1-5)
Crime_Podcast_Data$sum_8.1 <- rowSums(Crime_Podcast_Data[,c('Q8.1_1', 'Q8.1_2',
                                                              'Q8.1_3', 'Q8.1_4',
                                                              'Q8.1_5')])

View(Crime_Podcast_Data)

Crime_Podcast_Data$sum_8.1_cat = Crime_Podcast_Data$sum_8.1

Crime_Podcast_Data$sum_8.1_cat[Crime_Podcast_Data$sum_8.1 > 4 &
                                 Crime_Podcast_Data$sum_8.1 < 12] <- "Negative View of Police"

Crime_Podcast_Data$sum_8.1_cat[Crime_Podcast_Data$sum_8.1 >= 12 &
                                 Crime_Podcast_Data$sum_8.1 < 19] <- "Middling View of Police"

Crime_Podcast_Data$sum_8.1_cat[Crime_Podcast_Data$sum_8.1 >= 19] <- "Positive View of Police"

Interact_8.1 <- table(Crime_Podcast_Data$sum_7.1, Crime_Podcast_Data$sum_8.1_cat)

write.csv(Interact_8.1, "Interaction Table 8.1_cat.csv")
View(Interact_8.1)

# cor(Crime_Podcast_Data$sum_8.1, Crime_Podcast_Data$sum_7.1)

Interact_8.1 <- as.data.frame(Interact_8.1)
Interact_8.1[,2] <- factor(Interact_8.1[,2], levels=c("Negative View of Police",
                                                      "Middling View of Police",
                                                      "Positive View of Police"))
View(Interact_8.1)

# Bar Plot

ggplot(Interact_8.1, aes(fill=Interact_8.1[,2], y=Interact_8.1[,3],
                          x=Interact_8.1[,1])) + labs(title = "Q7.1/Q8.1 Interaction",
                                                       x = "Q7.1 Sums", y = "Frequency",
                                                       fill = "Q8.1 Sums") +
  geom_bar(position="dodge", stat="identity")


# =========================================================================== #

library(faraway)
library(effects)
library(car)
library(perturb)
library(leaps)

# Clean Data
Crime_Podcast_Data$sex <- factor(bestupdate$sex)
bestupdate$education <- factor(bestupdate$education)
bestupdate$hand <- factor(bestupdate$hand)

# Level Sex
levels(bestupdate$sex)
S1 <- with(bestupdate, (sex == levels(sex)[1]) + 0)
S2 <- with(bestupdate, (sex == levels(sex)[2]) + 0)
head(data.frame(sex = bestupdate$sex, S1, S2), 10)

# Level Education
levels(bestupdate$education)
E1 <- with(bestupdate, (education == levels(education)[1]) + 0)
E2 <- with(bestupdate, (education == levels(education)[3]) + 0)
E3 <- with(bestupdate, (education == levels(education)[4]) + 0)
E4 <- with(bestupdate, (education == levels(education)[2]) + 0)
E7 <- E3+E4
head(data.frame(education = bestupdate$education, E1, E2, E3, E4), 10)

# Level Hand
levels(bestupdate$hand)
H1 <- with(bestupdate, (hand == levels(hand)[2]) + 0)
H2 <- with(bestupdate, (hand == levels(hand)[1]) + 0)
head(data.frame(hand = bestupdate$hand, H1, H2), 10)

#Plug Dummy Variables In
bestupdate$S1 = S1
bestupdate$S2 = S2
bestupdate$E1 = E1
bestupdate$E2 = E2
bestupdate$E3 = E3
bestupdate$E4 = E4
bestupdate$H1 = H1
bestupdate$H2 = H2
bestupdate$E7 = E7

# Fit Model
f0 <- standardeng ~ age + S2 + E2 + E3 + E4 + H2 + ageeng + expoeng + selfeng
fm <- standardeng ~ age + sex + education + hand + ageeng + expoeng + selfeng
# If want to combine educations to create "university degree and above"
#fm <- standardeng ~ age + S2 + E2 + E7 + H2 + ageeng + expoeng + selfeng
fm1 <- standardeng ~ age + sex + hand + ageeng + expoeng + selfeng
lmod <- lm(fm, data = bestupdate)
lmod0 <- lm(f0, data = bestupdate)
lmod1 <- lm(fm1, data = bestupdate)

#Summary of Response
summary(lmod)
summary(lmod0)