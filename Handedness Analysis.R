library("ggplot2"); library("lme4"); 
library("dplyr"); 
library("lmerTest"); library("tidyr");
library("Rcpp"); library("sjstats"); library("pwr");
library("MuMIn")

setwd("C:/Users/newuser/Box/Dissertation/Experiment 1 (Soccer)/Data Analysis")
rm(list = ls())
list.files()

Alpha_Hem<-read.csv("./Alpha_Hem.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))
Beta_Hem<-read.csv("./Beta_Hem.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))
Alpha_Both<-read.csv("./Alpha_Both.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))
Beta_Both<-read.csv("./Beta_Both.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))
Rel_Hem<-read.csv("./Rel_Hem.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))
Rel_Both<-read.csv("./Rel_Both.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))





list <- list(Alpha_Both, Alpha_Hem, Rel_Both, Rel_Hem, Beta_Both, Beta_Hem)
list <- lapply(list, function (x) filter(x, !(subID == "P05" | subID == "P27" |
                                                subID == "P06"))) #maybe not P09 is 38 years opld

list <- lapply(list, function (x) filter(x, Footedness == "R"))
                                                
head(list[[1]])

rm(list=ls()[! ls() %in% c("list")])
Alpha_Both <- list[[1]]
Alpha_Hem <- list [[2]]
Rel_Both <- list[[3]]
Rel_Hem <- list[[4]]
Beta_Both <- list[[5]]
Beta_Hem <- list[[6]]




#analyze data
mod1 <- (lmer(P_Avg_Hem ~ Skill * Condition  * Hemisphere  +
                     (1|subID) + (1|Condition:subID) + (1|Hemisphere:subID), 
                   REML = FALSE, data = filter(Alpha_Hem, Epoch == "E2")))
anova_stats(mod1)



    #Alpha Effects
    mod1 <- (lmer(P_Avg_Hem ~ Skill + Condition   + Hemisphere +
                         (1|subID) + (1|Condition:subID) + (1|Hemisphere:subID), 
                       REML = FALSE, data = filter(Alpha_Hem, Epoch == "E2", !(Condition == "B"))))
    anova_stats(mod1)
    
    mod1 <- (lmer(P_Avg_Hem ~ Skill + Condition   + Hemisphere +
                    (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), 
                  REML = FALSE, data = filter(Alpha_Hem, Epoch == "E2", !(Condition == "N"))))
    anova_stats(mod1)
    
    mod1 <- (lmer(P_Avg_Hem ~ Skill + Condition   + Hemisphere +
                    (1|subID) + (1|Condition:subID)+  (1|Hemisphere:subID), 
                  REML = FALSE, data = filter(Alpha_Hem, Epoch == "E2", !(Condition == "O"))))
    anova_stats(mod1)
    



    
Alpha_Hem$Skill <-  as.factor(Alpha_Hem$Skill)
    

Alpha_Plot <- Alpha_Hem %>%
  group_by(subID, Condition, Epoch, Skill) %>%
  summarise(P_Avg = mean(P_Avg_Hem))
  
Alpha_Plot$Condition <- factor(Alpha_Plot$Condition, levels=c("N", "B", "O"))
levels(Alpha_Plot$Condition) <- c('Control', 'Blur', 'Hips-Only')
levels(Alpha_Plot$Skill) <- c('Less-Skilled', 'Skilled')


ggplot(filter(Alpha_Plot, Epoch == "E2"))+
  geom_boxplot(aes(Condition, P_Avg, fill = Condition),
                       outlier.shape = NA)+
  geom_point(aes(Condition, P_Avg), alpha = .5)+  
  geom_line(aes(Condition, P_Avg, group = subID), alpha = .3, color = "black")+
  ylab(expression(Parietal~"Alpha"~Power~(µV^{"2"})))+ 
  coord_cartesian(ylim = c(-1.2, 1))+
  scale_y_continuous(breaks = c(-1, -.5,  0,  .5, 1))+
  facet_wrap(~Skill)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=18, colour="black"))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 18, face = "bold"),
        strip.text = element_text(size = 16))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 20, face = "bold"))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Control" = "grey95",
                               "Blur" = "grey80",
                               "Hips-Only" = "grey60"))









#P_Hemis_Avg---------

#analyze data
mod1 <- (lmer(P_Avg_Hem ~ Skill * Condition  * Hemisphere+ 
                     (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
              data = filter(Beta_Hem, Epoch == "E2")))
anova_stats(mod1)

#Alpha Effects Less-Skilled

LSkill_Beta <- Beta_Hem %>%
  filter(Epoch == "E2") %>%
  filter(Skill == "LS")

      mod1 <- (lmer(P_Avg_Hem ~  Condition   + Hemisphere +
                      (1|subID) + (1|Condition:subID) + (1|Hemisphere:subID), 
                    REML = FALSE, data = filter(LSkill_Beta, !(Condition == "B" ))))
      anova_stats(mod1)
      
      mod1 <- (lmer(P_Avg_Hem ~ Condition   + Hemisphere +
                      (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), 
                    REML = FALSE, data = filter(LSkill_Beta, !(Condition == "N" ))))
      anova_stats(mod1)
      
      mod1 <- (lmer(P_Avg_Hem ~  Condition   + Hemisphere +
                      (1|subID) + (1|Condition:subID)+  (1|Hemisphere:subID), 
                    REML = FALSE, data = filter(LSkill_Beta, !(Condition == "O" ))))
      anova_stats(mod1)
      
      #summarize
      mod1 <- (lmer(P_Avg_Hem ~ Condition  * Hemisphere+ 
                      (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), REML = FALSE, 
                    data = filter(Beta_Hem, Epoch == "E2" & Skill == "LS")))
      anova_stats(mod1)
      
      #Skill Condition Interaction
      Skill_Beta <- Beta_Hem %>%
        filter(Epoch == "E2") %>%
        filter(Skill == "S")
        
      mod1 <- (lmer(P_Avg_Hem ~  Condition   + Hemisphere +
                      (1|subID) + (1|Condition:subID) + (1|Hemisphere:subID), 
                    REML = FALSE, data = filter(Skill_Beta, !(Condition == "B" ))))
      anova_stats(mod1)
      
      mod1 <- (lmer(P_Avg_Hem ~  Condition   + Hemisphere +
                      (1|subID) + (1|Condition:subID)+ (1|Hemisphere:subID), 
                    REML = FALSE, data = filter(Skill_Beta, !(Condition == "N" ))))
      anova_stats(mod1)
      
      mod1 <- (lmer(P_Avg_Hem ~  Condition   + Hemisphere +
                      (1|subID) + (1|Condition:subID)+  (1|Hemisphere:subID), 
                    REML = FALSE, data = filter(Skill_Beta, !(Condition == "O" ))))
      anova_stats(mod1)



      
Beta_Hem$Skill <-  as.factor(Beta_Hem$Skill)
    
Beta_Plot <- Beta_Hem %>%
  group_by(subID, Condition, Epoch, Skill) %>%
  summarise(P_Avg = mean(P_Avg_Hem))

Beta_Plot$Condition <- factor(Beta_Plot$Condition, levels=c("N", "B", "O"))
levels(Beta_Plot$Condition) <- c('Control', 'Blur', 'Hips-Only')
levels(Beta_Plot$Skill) <- c('Less-Skilled', 'Skilled')


ggplot(filter(Beta_Plot, Epoch == "E2"))+
  geom_boxplot(aes(Condition, P_Avg, fill = Condition),
               outlier.shape = NA)+
  geom_point(aes(Condition, P_Avg), alpha = .5)+  
  geom_line(aes(Condition, P_Avg, group = subID), alpha = .3, color = "black")+
  ylab(expression(Parietal~"Beta"~Power~(µV^{"2"})))+ 
  coord_cartesian(ylim = c(-1, 1))+
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1))+
  facet_wrap(~Skill)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=18, colour="black"))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 18, face = "bold"),
        strip.text = element_blank())+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 20, face = "bold"))+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Control" = "grey95",
                               "Blur" = "grey80",
                               "Hips-Only" = "grey60"))
#Explore interactions CP









mod1 <- (lmer(P_Avg_Hem_r ~  Skill * Condition  *Hemisphere+
                (1|subID) + (1|Condition:subID)+ (1|subID:Hemisphere), REML = FALSE, 
              data = filter(Rel_Hem, Epoch == "E2")))

anova_stats(mod1)


mod1 <- (lmer(P_Avg_Hem_r ~   Skill * Condition +
                (1|subID) , REML = FALSE, 
              data = filter(Rel_Hem, Epoch == "E2" & Hemisphere == "Right")))

anova_stats(mod1)

mod1 <- (lmer(P_Avg_Hem_r ~   Condition  *Hemisphere+
                (1|subID) + (1|Condition:subID)+ (1|subID:Hemisphere), REML = FALSE, 
              data = filter(Rel_Hem, Epoch == "E2" & Skill == "S")))

anova_stats(mod1)


Rel_Plot <- Rel_Hem %>%
  group_by(subID, Epoch, Skill, Hemisphere) %>%
  summarise(P_Avg = mean(P_Avg_Hem_r))

Rel_Plot$Condition <- factor(Rel_Plot$Condition, levels=c("N", "B", "O"))
levels(Rel_Plot$Condition) <- c('Control', 'Blur', 'Hips-Only')

ggplot(filter(Rel_Plot, Epoch == "E2"))+
  geom_boxplot(aes(Hemisphere, P_Avg),
               outlier.shape = NA)+
  geom_point(aes(Hemisphere, P_Avg), alpha = .5)+  
  geom_line(aes(Hemisphere, P_Avg, group = subID), alpha = .3, color = "black")+
  ylab(expression(Parietal~"Beta"~-~"Alpha"~Power~(µV^{"2"})))+ 
  coord_cartesian(ylim = c(-.4, .45))+
  scale_y_continuous(breaks = c(-.4, -.2, 0, .2, .4))+
  facet_wrap(~Skill)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=18, colour="black"))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 18, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 20, face = "bold"))+
  scale_fill_manual(values = c("Control" = "grey95",
                               "Blur" = "grey80",
                               "Hips-Only" = "grey60"))






Rel_Plot2 <- Beta_Hem %>%
  group_by(subID, Condition, Epoch) %>%
  summarise(P_Avg = mean(P_Avg_Hem))


ggplot((Rel_Plot2))+
  geom_boxplot(aes(Condition, P_Avg), outlier.shape = NA)+
  geom_point(aes(Condition, P_Avg, color = Condition), alpha = .5)+  
  geom_line(aes(Condition, P_Avg, group = subID), alpha = .3, color = "black")+
  ylab(expression(Log~Transformed~Parietal~"Beta"~-~"Alpha"~Power~(µV^{"2"})))+ 
  facet_wrap(~Epoch)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=20, colour="black"))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 22, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 22, face = "bold"))+
  theme(legend.position = "none")+
  scale_color_manual(values = c("Control" = "royalblue",
                                "Blur" = "red",
                                "Hips-Only" = "purple"))












mod1 <- (lmer(P_Avg_Hem ~  Skill * Condition  *Hemisphere*Epoch+
                (1|subID) + (1|Condition:subID)+  (1|subID:Hemisphere), REML = FALSE, 
              data = filter(Alpha_Hem, Epoch == "E2")))


anova_stats(mod1)






Rel_Plot2 <- Rel_Hem %>%
  group_by(subID, Skill, Epoch) %>%
  summarise(P_Avg = mean(P_Avg_Hem_r))

Rel_Plot2$Condition <- factor(Rel_Plot2$Condition, levels=c("N", "B", "O"))
levels(Rel_Plot2$Condition) <- c('Control', 'Blur', 'Hips-Only')


ggplot(filter(Rel_Plot2))+
  geom_boxplot(aes(Skill, P_Avg, color = Epoch), outlier.shape = NA)+
  geom_point(aes(Skill, P_Avg, color = Epoch), alpha = .5)+  
  geom_line(aes(Skill, P_Avg, group = subID), alpha = .3, color = "black")+
  ylab(expression(Log~Transformed~Parietal~"Beta"~-~"Alpha"~Power~(µV^{"2"})))+ 
  #facet_wrap(~Skill)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=20, colour="black"))+ 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5),
                                    size = 22, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(t = 35, b = 0),
                                    size = 22, face = "bold"))
  









#Direction is associated with greater right beta activity in both groups. Interactions show that experts have 
#slightly less right beta activity when Direction is better... might imply more left hemisphere beta is necessary to improve performance in experts

Alpha_Hem<-read.csv("./Alpha_Hem.csv", header = TRUE, sep=",",  
                    na.strings=c("NA","NaN"," ",""))
Beta_Hem<-read.csv("./Beta_Hem.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))
Alpha_Both<-read.csv("./Alpha_Both.csv", header = TRUE, sep=",",  
                     na.strings=c("NA","NaN"," ",""))
Beta_Both<-read.csv("./Beta_Both.csv", header = TRUE, sep=",",  
                    na.strings=c("NA","NaN"," ",""))
Rel_Hem<-read.csv("./Rel_Hem.csv", header = TRUE, sep=",",  
                  na.strings=c("NA","NaN"," ",""))
Rel_Both<-read.csv("./Rel_Both.csv", header = TRUE, sep=",",  
                   na.strings=c("NA","NaN"," ",""))
Perf <- Rel_Hem %>%
  group_by(subID) %>%
  summarize(Direction_mean = mean(Direction, na.rm = TRUE),
            Direction_SD = sd(Rel_Hem$Direction,na.rm = TRUE),
            Height_mean = mean(Height, na.rm=TRUE),
            Height_SD = sd(Rel_Hem$Height, na.rm = TRUE))
Rel_Hem <- merge(Perf, Rel_Hem, by = c("subID"))
tail(Rel_Hem)
Rel_Hem <- Rel_Hem %>%
  mutate(DirectionZ = (Direction - Direction_mean)/Direction_SD,
         HeightZ = (Height - Height_mean)/Height_SD)

tail(Rel_Hem)  

mod1 <- (lmer(P_Avg_Hem_r ~ Skill    * DirectionZ + Epoch  +Condition + 
                     (1|subID) + (1|Condition:subID), REML = FALSE, data = Rel_Hem))

summary(mod1)


mod1 <- (lmer(P_Avg_Hem ~ Hemisphere * Direction + Epoch  +Condition + 
                     (1|subID) + (1|Condition:subID), REML = FALSE, data = filter(Beta_Hem, Skill == "LS")))

summary(mod1)



ggplot((Beta_Hem))+
  geom_smooth(method = loess, se = FALSE, aes(Direction, P_Avg_Hem, color = Hemisphere))+
  geom_jitter(aes(Direction, P_Avg_Hem, color = Hemisphere), alpha = .3)+
  facet_wrap(~Skill) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "Central-Parietal Alpha Activity")


































hist(Beta_Hem$CP_Avg_Hem ,probability=T, main="Histogram of normal
        data",xlab="Approximately normally distributed data")

mod1 <- (lmer(CP_Avg_Hem ~ Skill * Condition  * Hemisphere *Epoch +
                (1|subID) + (1|Condition:subID), REML = FALSE, data = Beta_Hem))
#Screening for outliers in fitness data
cooksd <- cooks.distance(mod1)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

Beta_Hem_CP <- Beta_Hem[-c(159, 193, 59, 25, 274, 308, 2, 36), ]

#new data
hist(Beta_Hem_CP$CP_Avg_Hem ,probability=T, main="Histogram of normal
        data",xlab="Approximately normally distributed data")

mod1 <- (lmer(CP_Avg_Hem ~ Skill * Condition  * Hemisphere *Epoch +
                (1|subID) + (1|Condition:subID), REML = FALSE, data = Beta_Hem_CP))
#Screening for outliers in fitness data
cooksd <- cooks.distance(mod1)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels



mod1 <- anova(lmer(CP_Avg_Hem ~ Skill * Condition  * Hemisphere +Epoch * Hemisphere +
                     (1|subID) + (1|Condition:subID)+ (1|Epoch:subID)+ (1|Hemisphere:subID), REML = FALSE, data = Beta_Hem))

mod1

#Explore interactions CP
ggplot((Beta_Hem))+
  geom_boxplot(aes(Skill, CP_Avg_Hem, color = Hemisphere))+
  geom_jitter(aes(Skill, CP_Avg_Hem, color = Hemisphere), alpha = .3)+
  #facet_wrap(~subID) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "Central-Parietal Alpha Activity")

ggplot((Beta_Hem))+
  geom_boxplot(aes(Epoch, CP_Avg_Hem, color = Condition))+
  geom_jitter(aes(Epoch, CP_Avg_Hem, color = Condition), alpha = .3)+
  #facet_wrap(~subID) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(name = "Central-Parietal Alpha Activity")
