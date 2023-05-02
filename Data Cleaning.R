library(tidyverse)

#load the data
crime <- read.csv("C:/Users/Kayley/OneDrive/Spring 2023/Crime Podcast Project/Crime Podcast Survey_February 22, 2023_10.45.csv", 
                  header=TRUE, comment.char="#")


#remove first two rows (duplicate/unnecessary info)
crime <- crime[-c(1,2),]

#remove unnecessary variables (start/end time, language, etc.)
crime<- crime[,-c(1:17)]

#turn all blanks values into NAs 
crime[crime==""]<- NA


###############################################################################

#demographic questions (not sure if we should use Q2.8 because it's where they
#found the survey, which is mostly Reddit)

#I didn't add NA as its own level for the demographic questions because
#most people answered those. 

crime$Q2.1 <- factor(crime$Q2.1, levels = c("18-22", "23-27", 
                                          "28-35", "36-45",
                                          "46-55", "56 or older"))


names <- c("Q2.2", "Q2.3", "Q2.4", "Q2.5", "Q2.6","Q2.7", "Q2.8")
crime[,names] <- lapply(crime[,names] , factor)

###############################################################################

#social media consumption time
crime$Q3.1 <- factor(crime$Q3.1, levels = c("None at all", "Less than an hour",
                                            "1-2 hours", "3-4 hours", 
                                            "More than 4 hours", NA),
                     exclude = NULL)

##############################################################################

#media consumption (TV, podcasts, etc.)

names <- c("Q4.1_1", "Q4.1_2", "Q4.1_3", "Q4.1_4")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("None at all", "Less than an hour", "1-2 hours", "3-4 hours",
                       "5-6 hours", "6 hours or more", NA), 
         exclude = NULL)
})

new <- cbind(as.numeric(crime$Q4.1_1), as.numeric(crime$Q4.1_2), 
             as.numeric(crime$Q4.1_3), as.numeric(crime$Q4.1_4))
na_count <- rowSums(new==7)

crime$avg_Q4.1 <- (rowSums(new) - (7*na_count))/(4-na_count)
crime$avg_Q4.1[is.nan(crime$avg_Q4.1)] <- NA

  
###############################################################################

#not using Q4.2 because some people didn't put numbers since it was free response.

################################################################################

#Q4.3, binary variables for which podcasts they listen to
crime$Q4.3_casefile <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Casefile", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_crimejunkie <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Crime Junkie", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_criminal <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Criminal", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_dateline <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Dateline/Dateline Productions", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_generationwhy <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Generation Why", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_missing <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Missing", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_morbid <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Morbid", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_myfavmurder <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("My Favorite Murder", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_obsessed <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Obsessed with Disappeared", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_truecrimeob <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("True Crime Obsessed", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.3_vanished <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Up and Vanished", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

#added in a binary variable for "other" podcast
crime$Q4.3_other <- sapply(seq_along(crime$Q4.3), function(x){
  if(is.na(crime$Q4.3[x]) == TRUE){
    return(NA)
  }
  else if(grepl("Other", crime$Q4.3[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

###############################################################################

#not using Q4.4 because Melissa doesn't want to

################################################################################

#behaviors after listening to crime podcasts (not the checkbox question)

names <- c("Q4.5", "Q4.6", "Q4.7", "Q4.8")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Strongly disagree", "Disagree", "Neither agree or disagree",
                       "Agree", "Strongly agree", NA), 
         exclude = NULL)
})

new <- cbind(as.numeric(crime$Q4.5), as.numeric(crime$Q4.6), 
             as.numeric(crime$Q4.7), as.numeric(crime$Q4.8))
na_count <- rowSums(new==6)

crime$avg_Q4.5_4.8 <- (rowSums(new) - (6*na_count))/(4-na_count)
crime$avg_Q4.5_4.8[is.nan(crime$avg_Q4.5_4.8)] <- NA

###############################################################################

#creating binary variables for checklist question 4.9
crime$Q4.9_1 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Regularly changed up a walking or jogging route for safety", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.9_2 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Avoided certain clubs or areas of town for safety", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.9_3 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Completed a self-defense class", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.9_4 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Carried mace, pepper spray or a screamer", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})


crime$Q4.9_5 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Carried a knife, stun gun or Taser©", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.9_6 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Purchased a handgun or rifle", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.9_7 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Obtained a concealed carry license", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.9_8 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Started carrying a handgun", crime$Q4.9[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

###############################################################################

#creating binary variables for checklist question 4.10
crime$Q4.10_1 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Changed a walking or jogging routine", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.10_2 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Avoided certain clubs or areas of town that you used to go to before listening", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.10_3 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Completed self-defense class", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.10_4 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Started carrying mace, pepper spray or a screamer", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.10_5 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Started carrying a knife, stun gun or Taser©", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.10_6 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Purchased a handgun or rifle", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.10_7 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Obtained a concealed carry license", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

crime$Q4.10_8 <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9[x]) == TRUE & is.na(crime$Q4.10[x])==TRUE){
    return(NA)
  }
  else if(grepl("Started carrying a handgun", crime$Q4.10[x]) == TRUE){
    return(1)
  }
  else{
    return(0)
  }
})

###############################################################################

#behaviors before and after listening (total)
crime$total_behaviors <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9_1[x]) == FALSE & is.na(crime$Q4.10_1[x])==FALSE){
    total <- 0
  if(crime$Q4.9_1[x] == 1 | crime$Q4.10_1[x] == 1){
    total <- total + 1
  }
  if(crime$Q4.9_2[x] == 1 | crime$Q4.10_2[x] == 1){
    total <- total + 1
  }
  if(crime$Q4.9_3[x] == 1 | crime$Q4.10_3[x] == 1){
    total <- total + 1
  }
  if(crime$Q4.9_4[x] == 1 | crime$Q4.10_4[x] == 1){
    total <- total + 1
  }
  if(crime$Q4.9_5[x] == 1 | crime$Q4.10_5[x] == 1){
    total <- total + 1
  }
  if(crime$Q4.9_6[x] == 1 | crime$Q4.10_6[x] == 1){
    total <- total + 1
  }
  if(crime$Q4.9_7[x] == 1 | crime$Q4.10_7[x] == 1){
    total <- total + 1
  }
  if(crime$Q4.9_8[x] == 1 | crime$Q4.10_8[x] == 1){
    total <- total + 1
  }
    return(total)
  }
  else{
    return(NA)
  }
})
###############################################################################

#new behaviors after listening (total)
crime$new_behaviors <- sapply(seq_along(crime$Q4.9), function(x){
  if(is.na(crime$Q4.9_1[x]) == FALSE & is.na(crime$Q4.10_1[x])==FALSE){
    total <- 0
    if(crime$Q4.9_1[x] == 0 & crime$Q4.10_1[x] == 1){
      total <- total + 1
    }
    if(crime$Q4.9_2[x] == 0 & crime$Q4.10_2[x] == 1){
      total <- total + 1
    }
    if(crime$Q4.9_3[x] == 0 & crime$Q4.10_3[x] == 1){
      total <- total + 1
    }
    if(crime$Q4.9_4[x] == 0 & crime$Q4.10_4[x] == 1){
      total <- total + 1
    }
    if(crime$Q4.9_5[x] == 0 & crime$Q4.10_5[x] == 1){
      total <- total + 1
    }
    if(crime$Q4.9_6[x] == 0 & crime$Q4.10_6[x] == 1){
      total <- total + 1
    }
    if(crime$Q4.9_7[x] == 0 & crime$Q4.10_7[x] == 1){
      total <- total + 1
    }
    if(crime$Q4.9_8[x] == 0 & crime$Q4.10_8[x] == 1){
      total <- total + 1
    }
    return(total)
  }
  else{
    return(NA)
  }
})

###############################################################################
#Melissa does not want us to account for why they listened to podcasts 

names <- c("Q4.15_1", "Q4.15_2", "Q4.15_3", "Q4.15_4", "Q4.15_5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree",
                       "Somewhat agree", "Strongly agree", NA), 
         exclude = NULL)
})

###############################################################################
#overall worry questions

names <- c("Q5.1_1", "Q5.1_2", "Q5.1_3", "Q5.1_4")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Not concerned at all", "Slightly concerned", 
                       "Moderately concerned", "Extremely concerned", NA), 
         exclude = NULL)
})

new <- cbind(as.numeric(crime$Q5.1_1), as.numeric(crime$Q5.1_2), 
             as.numeric(crime$Q5.1_3), as.numeric(crime$Q5.1_4))
na_count <- rowSums(new==5)

crime$avg_Q5.1 <- (rowSums(new) - (5*na_count))/(4-na_count)
crime$avg_Q5.1[is.nan(crime$avg_Q5.1)] <- NA

##############################################################################
#neighborhood

crime$Q6.1 <- factor(crime$Q6.1)

##############################################################################
#worry in neighborhood

names <- c("Q7.1_1", "Q7.1_2", "Q7.1_3", "Q7.1_4")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Never", "Rarely", "Sometimes",
                       "Very Often", "Always", NA), exclude = NULL)
})

new <- cbind(as.numeric(crime$Q7.1_1), as.numeric(crime$Q7.1_2), 
                   as.numeric(crime$Q7.1_3), as.numeric(crime$Q7.1_4))
na_count <- rowSums(new==6)

crime$avg_Q7.1 <- (rowSums(new) - (6*na_count))/(4-na_count)
crime$avg_Q7.1[is.nan(crime$avg_Q7.1)] <- NA

##############################################################################
#general worry

names <- c("Q7.2_1", "Q7.2_2", "Q7.2_3", "Q7.2_4", "Q7.2_5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Not at all", "A little", "A moderate amount",
                       "A lot", "A great deal", NA), exclude = NULL)
})

new <- cbind(as.numeric(crime$Q7.2_1), as.numeric(crime$Q7.2_2), 
             as.numeric(crime$Q7.2_3), as.numeric(crime$Q7.2_4),
             as.numeric(crime$Q7.2_5))
na_count <- rowSums(new==6)

crime$avg_Q7.2 <- (rowSums(new) - (6*na_count))/(5-na_count)
crime$avg_Q7.2[is.nan(crime$avg_Q7.2)] <- NA

##############################################################################
#police attitudes (general)

names <- c("Q8.1_1", "Q8.1_2", "Q8.1_3", "Q8.1_4", "Q8.1_5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Strongly Disagree", "Disagree", "Undecided",
                       "Agree", "Strongly Agree", NA), exclude = NULL)
})

new <- cbind(as.numeric(crime$Q8.1_1), as.numeric(crime$Q8.1_2), 
             as.numeric(crime$Q8.1_3), as.numeric(crime$Q8.1_4),
             as.numeric(crime$Q8.1_5))
na_count <- rowSums(new==6)

crime$avg_Q8.1 <- (rowSums(new) - (6*na_count))/(5-na_count)
crime$avg_Q8.1[is.nan(crime$avg_Q8.1)] <- NA

##############################################################################
#police attitudes (in your area)

names <- c("Q8.2_1", "Q8.2_2", "Q8.2_4", "Q8.2_5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Strongly Disagree", "Disagree", "Undecided",
                       "Agree", "Strongly Agree", NA), exclude = NULL)
})

crime$Q8.2_3 <- factor(crime$Q8.2_3, levels = c("Strongly Agree", "Agree", 
                                                "Undecided", "Disagree", 
                                                "Strongly Disagree", NA),
                       exclude = NULL)


new <- cbind(as.numeric(crime$Q8.2_1), as.numeric(crime$Q8.2_2), 
             as.numeric(crime$Q8.2_3), as.numeric(crime$Q8.2_4),
             as.numeric(crime$Q8.2_5))
na_count <- rowSums(new==6)

crime$avg_Q8.2 <- (rowSums(new) - (6*na_count))/(5-na_count)
crime$avg_Q8.2[is.nan(crime$avg_Q8.2)] <- NA

##############################################################################
#police opinions changed since podcasts? (binary)

crime$Q8.3 <- factor(crime$Q8.3)

##############################################################################
#judges and court system

names <- c("Q9.1_1", "Q9.1_4", "Q9.1_5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Strongly Disagree", "Disagree", "Undecided",
                       "Agree", "Strongly Agree", NA),
         exclude = NULL)
})

crime$Q9.1_2 <- factor(crime$Q9.1_2, levels = c("Strongly Agree", "Agree", 
                                                "Undecided", "Disagree", 
                                                "Strongly Disagree", NA),
                       exclude = NULL)
                       

crime$Q9.1_3 <- factor(crime$Q9.1_3, levels = c("Strongly Agree", "Agree", 
                                                "Undecided", "Disagree", 
                                                "Strongly Disagree", NA),
                       exclude =NULL)


new <- cbind(as.numeric(crime$Q9.1_1), as.numeric(crime$Q9.1_2), 
             as.numeric(crime$Q9.1_3), as.numeric(crime$Q9.1_4),
             as.numeric(crime$Q9.1_5))
na_count <- rowSums(new==6)

crime$avg_Q9.1 <- (rowSums(new) - (6*na_count))/(5-na_count)
crime$avg_Q9.1[is.nan(crime$avg_Q9.1)] <- NA

##############################################################################
#opinions about judges and juries changed since listening? (binary)

crime$Q9.2 <- factor(crime$Q9.2)

##############################################################################
#criminal justice system

names <- c("Q9.4_1", "Q9.4_2", "Q9.4_4", "Q9.4_5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Strongly Disagree", "Somewhat Disagree", "Undecided",
                       "Somewhat Agree", "Strongly Agree", NA),
         exclude = NULL)
})

crime$Q9.4_3 <- factor(crime$Q9.4_3, levels = c("Strongly Agree", "Somewhat Agree", 
                                                "Undecided", "Somewhat Disagree", 
                                                "Strongly Disagree", NA),
                       exclude = NULL)

new <- cbind(as.numeric(crime$Q9.4_1), as.numeric(crime$Q9.4_2), 
             as.numeric(crime$Q9.4_3), as.numeric(crime$Q9.4_4),
             as.numeric(crime$Q9.4_5))
na_count <- rowSums(new==6)

crime$avg_Q9.4 <- (rowSums(new) - (6*na_count))/(5-na_count)
crime$avg_Q9.4[is.nan(crime$avg_Q9.4)] <- NA

##############################################################################
#opinions about prosecutors changed since listening? (binary)

crime$Q9.5 <- factor(crime$Q9.5)

#############################################################################
#punishment


names <- c("Q10.1_2", "Q10.1_3", "Q10.1_5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Strongly Agree", "Somewhat agree", "Undecided",
                       "Somewhat disagree", "Strongly disagree", NA),
         exclude = NULL)
})

crime$Q10.1_1 <- factor(crime$Q10.1_1, levels = c("Strongly disagree", "Somewhat disagree", 
                                                  "Undecided", "Somewhat agree", 
                                                  "Strongly Agree", NA),
                        exclude = NULL)

crime$Q10.1_4 <- factor(crime$Q10.1_4, levels = c("Strongly disagree", "Somewhat disagree", 
                                                  "Undecided", "Somewhat agree", 
                                                  "Strongly Agree", NA),
                        exclude = NULL)

new <- cbind(as.numeric(crime$Q10.1_1), as.numeric(crime$Q10.1_2), 
             as.numeric(crime$Q10.1_3), as.numeric(crime$Q10.1_4),
             as.numeric(crime$Q10.1_5))
na_count <- rowSums(new==6)

crime$avg_Q10.1 <- (rowSums(new) - (6*na_count))/(5-na_count)
crime$avg_Q10.1[is.nan(crime$avg_Q10.1)] <- NA

##############################################################################
#lifetime victim

names <- c("Q11.3_1", "Q11.3_2", "Q11.3_3", "Q11.3_4")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("No", "Yes", NA),
         exclude = NULL)
})

###############################################################################
#lifetime sexual assault

crime$Q11.10 <- factor(crime$Q11.10, levels = c("No", "Yes", NA),
                       exclude = NULL)

###############################################################################
#SEANS CODE
sum_Q11.3 <- rowSums(cbind(as.numeric(crime$Q11.3_3), as.numeric(crime$Q11.3_4), as.numeric(crime$Q11.10)), na.rm = TRUE)

# Refactor NAs for 11.3, 11.4, 11.10
crime$Q11.3_1 <- as.character(crime$Q11.3_1)
crime$Q11.3_2 <- as.character(crime$Q11.3_2)
crime$Q11.3_3 <- as.character(crime$Q11.3_3)
crime$Q11.3_4 <- as.character(crime$Q11.3_4)
crime$Q11.10 <- as.character(crime$Q11.10)

crime$Q11.3_1[is.na(crime$Q11.3_1)] <- 0
crime$Q11.3_2[is.na(crime$Q11.3_2)] <- 0
crime$Q11.3_3[is.na(crime$Q11.3_3)] <- 0
crime$Q11.3_4[is.na(crime$Q11.3_4)] <- 0
crime$Q11.10[is.na(crime$Q11.10)] <- 0

crime$Q11.3_1 <- factor(crime$Q11.3_1)
crime$Q11.3_2 <- factor(crime$Q11.3_2)
crime$Q11.3_3 <- factor(crime$Q11.3_3)
crime$Q11.3_4 <- factor(crime$Q11.3_4)
crime$Q11.10 <- factor(crime$Q11.10)

crime$victim_life <- sapply(seq_along(crime$Q11.10), function(x){
  if(as.numeric(crime$Q11.3_3)[x] == 2){
    return (1)
  }
  else if(as.numeric(crime$Q11.3_4)[x] == 2){
    return (1)
  }
  else if(as.numeric(crime$Q11.10)[x] == 2){
    return (1)
  }
  else{
    return(0)
  }
})

# Add on Q11.3_1&2, Q11.3_3&4

# Q11.3_1&2 = prop11.3
crime$prop11.3 <- sapply(seq_along(crime$Q11.3_1), function(x){
  if(as.numeric(crime$Q11.3_1)[x] + as.numeric(crime$Q11.3_2)[x] > 4){
    return (3)
  }
  else if(as.numeric(crime$Q11.3_1)[x] + as.numeric(crime$Q11.3_2)[x] == 4){
    return (2)
  }
  else if(as.numeric(crime$Q11.3_1)[x] + as.numeric(crime$Q11.3_2)[x] < 4){
    return (1)
  }
})

crime$prop11.3 <- factor(crime$prop11.3, labels = c(0, "No", "Yes"))

# Q11.3_3&4 = threat11.3
crime$threat11.3 <- sapply(seq_along(crime$Q11.3_3), function(x){
  if(as.numeric(crime$Q11.3_3)[x] + as.numeric(crime$Q11.3_4)[x] > 4){
    return (3)
  }
  else if(as.numeric(crime$Q11.3_3)[x] + as.numeric(crime$Q11.3_4)[x] == 4){
    return (2)
  }
  else if(as.numeric(crime$Q11.3_3)[x] + as.numeric(crime$Q11.3_4)[x] < 4){
    return (1)
  }
})

crime$threat11.3 <- factor(crime$threat11.3, labels = c(0, "No", "Yes"))

###############################################################################

names <- c("Q11.4_1", "Q11.4_2", "Q11.4_3", "Q11.4_4")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("No", "Yes"))
})

# Refactor NAs for 11.4
crime$Q11.4_1 <- as.character(crime$Q11.4_1)
crime$Q11.4_2 <- as.character(crime$Q11.4_2)
crime$Q11.4_3 <- as.character(crime$Q11.4_3)
crime$Q11.4_4 <- as.character(crime$Q11.4_4)

crime$Q11.4_1[is.na(crime$Q11.4_1)] <- 0
crime$Q11.4_2[is.na(crime$Q11.4_2)] <- 0
crime$Q11.4_3[is.na(crime$Q11.4_3)] <- 0
crime$Q11.4_4[is.na(crime$Q11.4_4)] <- 0

crime$Q11.4_1 <- factor(crime$Q11.4_1)
crime$Q11.4_2 <- factor(crime$Q11.4_2)
crime$Q11.4_3 <- factor(crime$Q11.4_3)
crime$Q11.4_4 <- factor(crime$Q11.4_4)

# Add on Q11.4_1&2, Q11.4_3&4

# Q11.4_1&2 = prop11.4
crime$prop11.4 <- sapply(seq_along(crime$Q11.4_1), function(x){
  if(as.numeric(crime$Q11.4_1)[x] + as.numeric(crime$Q11.4_2)[x] > 4){
    return (3)
  }
  else if(as.numeric(crime$Q11.4_1)[x] + as.numeric(crime$Q11.4_2)[x] == 4){
    return (2)
  }
  else if(as.numeric(crime$Q11.4_1)[x] + as.numeric(crime$Q11.4_2)[x] < 4){
    return (1)
  }
})

crime$prop11.4 <- factor(crime$prop11.4, labels = c(0, "No", "Yes"))

# Q11.4_3&4 = threat11.4
crime$threat11.4 <- sapply(seq_along(crime$Q11.4_3), function(x){
  if(as.numeric(crime$Q11.4_3)[x] + as.numeric(crime$Q11.4_4)[x] > 4){
    return (3)
  }
  else if(as.numeric(crime$Q11.4_3)[x] + as.numeric(crime$Q11.4_4)[x] == 4){
    return (2)
  }
  else if(as.numeric(crime$Q11.4_3)[x] + as.numeric(crime$Q11.4_4)[x] < 4){
    return (1)
  }
})

crime$threat11.4 <- factor(crime$threat11.4, labels = c(0, "No", "Yes"))

###############################################################################

crime$Q11.5 <- factor(crime$Q11.5, levels = c("No", "Yes", NA),
                      exclude = NULL)

crime$Q11.6 <- factor(crime$Q11.6, levels = c("Very unsatisfied", "Unsatisfied",
                                            "Neither satisfied or unsatisfied", "Satisfied", 
                                            "Very satisfied", NA),
                      exclude = NULL)

crime$Q11.7 <- factor(crime$Q11.7, levels = c("No", "Yes", NA),
                      exclude = NULL)

crime$Q11.8 <- factor(crime$Q11.8, levels = c("No", "Yes", NA),
                      exclude = NULL)

crime$Q11.11 <- factor(crime$Q11.11, levels = c("No", "Yes", NA),
                       exclude = NULL)

crime$Q11.12 <- factor(crime$Q11.12, levels = c("Very unsatisfied", "Unsatisfied",
                                              "Neither satisfied or unsatisfied", "Satisfied", 
                                              "Very satisfied", NA),
                       exclude = NULL)

###############################################################################
#more demographic questions
#didn't add NA as a category on these

crime$Q67 <- factor(crime$Q67)

crime$Q13.1 <- factor(crime$Q13.1, levels = c("Less than high school", "High school diploma or GED",
                                                "Some college", "2 year degree", "4 year degree", 
                                                "Master's degree", "Doctorate, M.D., J.D."))

crime$Q13.2 <- factor(crime$Q13.2, levels = c("Less than $19,000", "$20,000 - $39,999",
                                                "$40,000 - $59,999", "$60,000 - $79,999", 
                                                "$80,000 - $99,999", "$100,000 - $149,999",
                                              "More than $150,000"))

crime$Q13.3 <- factor(crime$Q13.3)

names <- c("Q13.4", "Q13.5")
crime[,names] <- lapply(crime[,names], function(x){
  factor(x, levels = c("Very liberal", "Liberal", "Middle of the road", "Conservative",
                       "Very conservative"))
})

###############################################################################

#save crime data frame
setwd("C:/Users/Kayley/OneDrive/Spring 2023/Crime Podcast Project")
saveRDS(crime, file = "cleaned_crime_data_updated.rds")






