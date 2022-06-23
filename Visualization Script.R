library("ggplot2"); library("lme4")
library("dplyr")
library("lmerTest"); library("corrplot"); library("tidyr");
setwd("C:/Users/u1214829/Box/Dissertation/Experiment 1 (Soccer)/Data Analysis")


Final<-read.csv("./Final.csv", header = TRUE, sep=",",  
                    na.strings=c("NA","NaN"," ",""))

Both_Hemis<-read.csv("./Both_Hemis.csv", header = TRUE, sep=",",  
                    na.strings=c("NA","NaN"," ",""))
#Alpha Hem data
Alpha <- Final %>%
  filter(Hz > 7 & Hz < 14)
Alpha_Hem <- Alpha %>%
  group_by(Condition, Epoch, Hemisphere, subID, Skill) %>%
  summarise(CP_Avg_Hem = mean(CP_Avg),
            CP_Avg_Single = mean(CP_Single),
            CP_Avg_Single_Med = mean(CP_Single_Med),
            P_Avg_Hem = mean(P_Avg),
            P_Avg_Single = mean(P_Single),
            P_Avg_Single_Med = mean(P_Single_Med))
Alpha_Hem <- merge(Alpha, Alpha_Hem, by = c("Condition", "Epoch", "Hemisphere", "subID", "Skill"))
Alpha_Hem <- Alpha_Hem %>%
  filter(Hz == "13") %>%
  dplyr::select(-CP_Avg, -P_Avg, - CP_Single, -P_Single, -CP_Single_Med, -P_Single_Med)

#Beta Hem data
Beta <- Final %>%
  filter(Hz > 13 & Hz < 31)
Beta_Hem <- Beta %>%
  group_by(Condition, Epoch, Hemisphere, subID, Skill) %>%
  summarise(CP_Avg_Hem = mean(CP_Avg),
            CP_Avg_Single = mean(CP_Single),
            CP_Avg_Single_Med = mean(CP_Single_Med),
            P_Avg_Hem = mean(P_Avg),
            P_Avg_Single = mean(P_Single),
            P_Avg_Single_Med = mean(P_Single_Med))
Beta_Hem <- merge(Beta, Beta_Hem, by = c("Condition", "Epoch", "Hemisphere", "subID", "Skill"))
Beta_Hem <- Beta_Hem %>%
  filter(Hz == "30") %>%
  dplyr::select(-CP_Avg, -P_Avg, - CP_Single, -P_Single, -CP_Single_Med, -P_Single_Med)

Alpha <- Both_Hemis %>%
  filter(Hz > 7 & Hz < 14)
Alpha_Both <- Alpha %>%
  group_by(Condition, Epoch, subID, Skill) %>%
  summarise(CP_Avg_Both = mean(CP_Avg),
            P_Avg_Both = mean(P_Avg))
Alpha_Both <- merge(Alpha, Alpha_Both, by = c("Condition", "Epoch", "subID", "Skill"))
Alpha_Both <- Alpha_Both %>%
  filter(Hz == "13") %>%
  dplyr::select(-CP_Avg, -P_Avg)

Beta <- Both_Hemis %>%
  filter(Hz > 13 & Hz < 33)
Beta_Both <- Beta %>%
  group_by(Condition, Epoch, subID, Skill) %>%
  summarise(CP_Avg_Both = mean(CP_Avg),
            P_Avg_Both = mean(P_Avg))
Beta_Both <- merge(Beta, Beta_Both, by = c("Condition", "Epoch", "subID", "Skill"))
Beta_Both <- Beta_Both %>%
  filter(Hz == "32") %>%
  dplyr::select(-CP_Avg, -P_Avg)

#Relative beta - alpha per hemisphere

Rel_A <- Alpha_Hem %>%
  dplyr::select(CP_Avg_Hem, P_Avg_Hem, CP_Avg_Single, P_Avg_Single, CP_Avg_Single_Med, P_Avg_Single_Med)
Rel_B <- Beta_Hem %>%
  dplyr::select(CP_Avg_Hem, P_Avg_Hem, CP_Avg_Single, P_Avg_Single, CP_Avg_Single_Med, P_Avg_Single_Med)
Rel_Hem <- Rel_B - Rel_A
Rel_Hem <- rename(Rel_Hem, CP_Avg_Hem_r = "CP_Avg_Hem", P_Avg_Hem_r = "P_Avg_Hem", CP_Avg_Single_r = "CP_Avg_Single",
                  P_Avg_Single_r = "P_Avg_Single", CP_Avg_Single_Med_r = "CP_Avg_Single_Med", P_Avg_Single_Med_r = "P_Avg_Single_Med")
Rel_Hem <- cbind(Alpha_Hem, Rel_Hem)
Rel_Hem <- Rel_Hem %>%
  dplyr::select(-CP_Avg_Hem, -P_Avg_Hem, - CP_Avg_Single, -P_Avg_Single, CP_Avg_Single_Med, P_Avg_Single_Med)

#Relative beta - alpha Both hemispheres
Rel_A <- Alpha_Both %>%
  dplyr::select(CP_Avg_Both, P_Avg_Both)
Rel_B <- Beta_Both %>%
  dplyr::select(CP_Avg_Both, P_Avg_Both)
Rel_Both <- Rel_B - Rel_A
Rel_Both <- rename(Rel_Both, CP_Avg_Both_r = "CP_Avg_Both", P_Avg_Both_r = "P_Avg_Both")
Rel_Both <- cbind(Alpha_Both, Rel_Both)
Rel_Both <- Rel_Both %>%
  dplyr::select(-CP_Avg_Both, -P_Avg_Both)

rm(list=ls()[! ls() %in% c("Final","Both_Hemis", "Alpha_Hem", "Beta_Hem",
                           "Alpha_Both", "Beta_Both", "Rel_Hem", "Rel_Both")])

write.csv(Alpha_Hem, "Alpha_Hem.csv")
write.csv(Beta_Hem, "Beta_Hem.csv")
write.csv(Alpha_Both, "Alpha_Both.csv")
write.csv(Beta_Both, "Beta_Both.csv")
write.csv(Rel_Hem, "Rel_Hem.csv")
write.csv(Rel_Both, "Rel_Both.csv")





#Average Frequency plots
head(Alpha_Hem)
test <- filter(Alpha_Hem, !(subID == "P05" | subID == "P27" |
                                subID == "P06" | subID == "P09"))

ggplot(filter(Beta_Hem, Epoch == "E1"))+
  geom_boxplot(aes(Condition, P_Avg_Hem, color = Hemisphere))+
  geom_point(aes(Condition, P_Avg_Hem, color = Hemisphere))+
  facet_wrap(~subID)


ggplot(filter(Alpha_Hem, Epoch == "E1"))+
  geom_smooth(method = "lm", aes(Age, CP_Avg_Hem, color = Skill), se = FALSE)+
  geom_point(aes(Age, CP_Avg_Hem, color = Skill))+
  facet_wrap(~Hemisphere)


test <- filter(Beta, !(subID == "P08"))

ggplot(filter(Beta, Hz == 14, Epoch == "E2")) + 
  geom_boxplot(aes(Condition, Beta_Avg))+
  geom_point(aes(Condition, Beta_Avg))+
  facet_wrap(~Skill)

  

ggplot(filter(experiment,Epoch == "E2"))+
  geom_boxplot(aes(Condition, Alpha_Avg, color = Skill))+
  geom_point(aes(Condition, Alpha_Avg, color = Skill))

library("lme4"); library("lmerTest")

(lmer(AlphaAvg ~ Skill * Condition +
                     (1|subID) + (1|Condition:subID), data = Alpha_Avg))

