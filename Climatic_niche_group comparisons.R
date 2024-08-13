library(dplyr)
library(ggplot2)

dat <- read.csv("C:\\Users\\camil\\OneDrive - SELVA Investigación para la conservación en el neotropico\\Documentos\\Camila\\Publicaciones\\Historical migration Phenology\\Niche_Overla_All_Groups.csv", sep = ";", h = T)
head(dat)

p <- ggplot(dat, aes(x= Class, y= D, fill = Period)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(colour = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = "", y = "Niche Overlap D")

p


p <- ggplot(dat, aes(x= Class, y= D, fill = Period)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(shape=16, colour = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = "", y = "Niche Overlap D")

p

intra <- subset(dat, dat$Class == "Intra group")
inter <- subset(dat, dat$Class == "Inter group")


intra_s <- subset(sum, sum$Class == "Intra group")
inter_s <- subset(sum, sum$Class == "Inter group")

Mintra <- mean(intra_s$D)
Minter <- mean(inter_s$D)

intra_w <- subset(win, win$Class == "Intra group")
inter_w <- subset(win, win$Class == "Inter group")

Mintra_w <- mean(intra_w$D)
Minter_w <- mean(inter_w$D)

sum <- subset(dat, dat$Period == "Breeding")
win <- subset(dat, dat$Period == "Winter")

p <- ggplot(sum, aes(x= Class, y= D, fill = Class)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(shape=16,position=position_jitter(0.2), colour = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "none",
        text = element_text(size = 20)) +
  labs(x = "", y = "Niche Overlap D")+
  ggtitle("Breeding niche overlap")

p

p <- ggplot(win, aes(x= Class, y= D, fill = Class)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(shape=16,position=position_jitter(0.2), colour = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "none",
        text = element_text(size = 20)) +
  labs(x = "", y = "Niche Overlap D")+
  ggtitle("Winter niche overlap")

p

windows()
p <- ggplot(intra, aes(x= Group_1, y= D, fill = Period)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(colour = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = "", y = "Niche Overlap D")+
  ggtitle("Intra group comparisons")

p

p <- ggplot(inter, aes(x= Group_1, y= D, fill = Period)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(colour = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = "", y = "Niche Overlap D")+
  ggtitle("Intra group comparisons")

p

windows()
p <- ggplot(intra, aes(x= Group_1, y= D, fill = Group_1)) + 
  geom_boxplot(outlier.shape = NA)+ 
  geom_jitter(colour = "black") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = "", y = "Niche Overlap D")+
  ggtitle("Intra group comparisons")

p

#Randomizartion to compare means between intra and inter group D


exsum_s <- subset(sum, Group_1 =="Extended Summer")
exsum_w <- subset(win, Group_1 =="Extended Summer")
exsum <- rbind(exsum_s, exsum_w)

exwin_s <- subset(sum, Group_1 == "Extended Winter")
exwin_w <- subset(win, Group_1 == "Extended Winter")
exwin <- rbind(exwin_s, exwin_w)

latwin_s <- subset(sum, Group_1 == "Late Winter Shift")
latwin_w <- subset(win, Group_1 == "Late Winter Shift")


#Extended Summer
Mintra <- mean(exsum_s$D)
Minter <- mean(exsum_w$D)

obsDifAB <- Mintra - Minter 

iter    <- 999;          # Total iterations (+1 for observed data = 10k)
diff_AB    <- NULL;# To add difference between groups

N_obs <- 82; # Total number of observations

for(i in 1:iter){   
  reshufled <- exsum
  reshufled$Class   <- sample(reshufled$Class, N_obs, replace = FALSE);
  
  mean_intra <- mean(reshufled %>% filter(Class == "Intra group") %>% pull(D))
  mean_inter <- mean(reshufled %>% filter(Class == "Inter group") %>% pull(D))
  
  
  diff_simAB <- mean_intra - mean_inter 

  
  diff_AB[i] <- diff_simAB

}

#P value 
windows()
ggplot() +
  ylab("Count") + xlab("Simulated mean difference") +
  geom_histogram(aes(x = diff_AB), bins = 30, 
                 fill = "grey", alpha = 0.4, colour = "black") +
  geom_vline(xintercept = obsDifAB, linewidth = 1, 
             linetype = "dashed", colour = "black") + 
  theme_classic()


less_or_equal_obs <- sum(diff_AB <= obsDifAB) + 1;
total_generated   <- length(diff_AB) + 1;
new_p_value       <- less_or_equal_obs / total_generated;
# p = 0.09

#Extended Winter

Mintra <- mean(exwin_s$D)
Minter <- mean(exwin_w$D)

obsDifAB <- Mintra - Minter 

iter    <- 999;          # Total iterations (+1 for observed data = 10k)
diff_AB    <- NULL;# To add difference between groups

N_obs <- 22; # Total number of observations

for(i in 1:iter){   
  reshufled <- exwin
  reshufled$Class   <- sample(reshufled$Class, N_obs, replace = FALSE);
  
  mean_intra <- mean(reshufled %>% filter(Class == "Intra group") %>% pull(D))
  mean_inter <- mean(reshufled %>% filter(Class == "Inter group") %>% pull(D))
  
  
  diff_simAB <- mean_intra - mean_inter 
  
  
  diff_AB[i] <- diff_simAB
  
}

#P value 
windows()
ggplot() +
  ylab("Count") + xlab("Simulated mean difference") +
  geom_histogram(aes(x = diff_AB), bins = 30, 
                 fill = "grey", alpha = 0.4, colour = "black") +
  geom_vline(xintercept = obsDifAB, linewidth = 1, 
             linetype = "dashed", colour = "black") + 
  theme_classic()


less_or_equal_obs <- sum(diff_AB <= obsDifAB) + 1;
total_generated   <- length(diff_AB) + 1;
new_p_value       <- less_or_equal_obs / total_generated;
# p = 0.016

#Inter and Intra

#Summer
intra_s <- subset(intra, intra$Period == "Breeding")
intra_w <- subset(intra, intra$Period == "Winter")

inter_s <- subset(inter, inter$Period == "Breeding")
inter_w <- subset(inter, inter$Period == "Winter")

bre <- rbind(intra_s, inter_s)
wi <- rbind(intra_w, inter_w)


Mintra <- mean(intra_s$D)
Minter <- mean(inter_s$D)

obsDifAB <- Mintra - Minter 

iter    <- 999;          # Total iterations (+1 for observed data = 10k)
diff_AB    <- NULL;# To add difference between groups

N_obs <- 66; # Total number of observations

for(i in 1:iter){   
  reshufled <- bre
  reshufled$Class   <- sample(reshufled$Class, N_obs, replace = FALSE);
  
  mean_intra <- mean(reshufled %>% filter(Class == "Intra group") %>% pull(D))
  mean_inter <- mean(reshufled %>% filter(Class == "Inter group") %>% pull(D))
  
  
  diff_simAB <- mean_intra - mean_inter 
  
  
  diff_AB[i] <- diff_simAB
  
}

#P value 
windows()
ggplot() +
  ylab("Count") + xlab("Simulated mean difference") +
  geom_histogram(aes(x = diff_AB), bins = 30, 
                 fill = "grey", alpha = 0.4, colour = "black") +
  geom_vline(xintercept = obsDifAB, linewidth = 1, 
             linetype = "dashed", colour = "black") + 
  theme_classic()


less_or_equal_obs <- sum(diff_AB <= obsDifAB) + 1;
total_generated   <- length(diff_AB) + 1;
new_p_value       <- less_or_equal_obs / total_generated;
# p = 0.94

#Winter

Mintra <- mean(intra_w$D)
Minter <- mean(inter_w$D)

obsDifAB <- Mintra - Minter 

iter    <- 999;          # Total iterations (+1 for observed data = 10k)
diff_AB    <- NULL;# To add difference between groups

N_obs <- 66; # Total number of observations

for(i in 1:iter){   
  reshufled <- wi
  reshufled$Class   <- sample(reshufled$Class, N_obs, replace = FALSE);
  
  mean_intra <- mean(reshufled %>% filter(Class == "Intra group") %>% pull(D))
  mean_inter <- mean(reshufled %>% filter(Class == "Inter group") %>% pull(D))
  
  
  diff_simAB <- mean_intra - mean_inter 
  
  
  diff_AB[i] <- diff_simAB
  
}

#P value 
windows()
ggplot() +
  ylab("Count") + xlab("Simulated mean difference") +
  geom_histogram(aes(x = diff_AB), bins = 30, 
                 fill = "grey", alpha = 0.4, colour = "black") +
  geom_vline(xintercept = obsDifAB, linewidth = 1, 
             linetype = "dashed", colour = "black") + 
  theme_classic()


less_or_equal_obs <- sum(diff_AB <= obsDifAB) + 1;
total_generated   <- length(diff_AB) + 1;
new_p_value       <- less_or_equal_obs / total_generated;
# p = 0.94


