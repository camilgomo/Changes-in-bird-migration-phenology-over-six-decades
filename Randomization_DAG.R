#Randomiization analysis
setwd("C:/Users/USER/OneDrive/Documentos/Pregrado/Evolvert/Proyecto migratorias/Fase 4/Datos Fenología DAG")
library(ggplot2)
library(ggpubr)
library(ggeasy)
library(pracma)
library(dplyr)
library(tidyr)
library(magick)
library(cowplot)

#Load data
data <- read.table("all_data50.txt", h = T, sep = "t")

#Distribution of historical records by year
hist_records <- subset(data, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

# Count records per year
records_per_year <- table(hist_records$YEAR)

# Convert to dataframe for ggplot
records_df <- data.frame(year = names(records_per_year), count = as.numeric(records_per_year))

# Plot
ggplot(records_df, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Records per Year",
       x = "Year",
       y = "Number of Records") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Generate days of max density with historical records
#Separate spring from fall to extract day of max density

#Extract day of max density 
#Definir función para calcular máximos locales
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#Get density data for historical data
density_data_hist <- density(hist_records$day_of_year)
#Make a dataframe for historical data
density_df_hist <- data.frame(
  x = density_data_hist$x,
  density = density_data_hist$y
)
#Calculate local maxima
loc.max <- density_data_hist$x[localMaxima(density_data_hist$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(data, period == "modern")

#Generate matrix for blank dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Generate data frames for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")
day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"


#Randomization
for(xx in 1:1000) {
  #Extract a random sample from the modern database with an equal number of records to the historical database.
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line())


#Generate confidence intervals for spring and fall
stderror_spring <- sd(spring$Difference)/sqrt(nrow(spring))
t.score_spring = qt(p=0.05/2, df=nrow(spring),lower.tail=F)
margin.error_spring <- stderror_spring * t.score_spring
upperCI_spring <- mean(spring$Difference)+margin.error_spring
lowerCI_spring <- mean(spring$Difference)-margin.error_spring
cat(lowerCI_spring, upperCI_spring)


stderror_fall <- sd(fall$Difference)/sqrt(nrow(fall))
t.score_fall = qt(p=0.05/2, df=nrow(fall),lower.tail=F)
margin.error_fall <- stderror_fall * t.score_fall
upperCI_fall <- mean(fall$Difference)+margin.error_fall
lowerCI_fall <- mean(fall$Difference)-margin.error_fall
cat(lowerCI_fall, upperCI_fall)


#Alternative visualization of the data
hist(spring$`Max_density`)
hist(fall$`Max_density`)

hist(spring$Difference)
hist(fall$Difference)

ggplot(spring, aes(x = Difference)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black")+
  geom_vline(xintercept = -3.59276, linetype = "dashed", color = "red")+
  geom_vline(xintercept = upperCI_spring) +
  geom_vline(xintercept = lowerCI_spring) 
  

ggplot(fall, aes(x = Difference)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  geom_vline(xintercept = 3.1662, linetype = "dashed", color = "red")+
  geom_vline(xintercept = upperCI_fall) +
  geom_vline(xintercept = lowerCI_fall) # Add vertical line at mean

#--------------------INDIVIDUAL SPECIES------------------------------

#Setophaga petechia
YEWA <- data %>% 
  subset(SPECIES_NAME == "Setophaga petechia")

#Distribution of historical records by year
hist_records <- subset(YEWA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(YEWA, period == "modern")

#Generate matrix for blank dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Generate data frames for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")
day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_yewa <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Yellow Warbler") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Density updated - Yellow warbler.png", gg_yewa)



#Setophaga castanea
BAWA <- data %>% 
  subset(SPECIES_NAME == "Setophaga castanea")

#Distribution of historical records by year
hist_records <- subset(BAWA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(BAWA, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Generate dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)


colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_bawa <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Bay-breasted Warbler") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Bay breasted warbler.png", gg_bawa)


#Myiarchus crinitus
GCFL <- data %>% 
  subset(SPECIES_NAME == "Myiarchus crinitus")

#Distribution of historical records by year
hist_records <- subset(GCFL, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create a subset for modern records
mod_records <- subset(GCFL, period == "modern")

#Create a matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Generate dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx

  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_gcfl <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Great Crested Flycatcher") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Great crested flycatcher.png", gg_gcfl)

#MNIOTILTA VARIA
BWWA <- data %>% 
  subset(SPECIES_NAME == "Mniotilta varia")

#Distribution of historical records by year
hist_records <- subset(BWWA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(BWWA, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_bwwa <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
    ggtitle("Black-and-white Warbler") + 
    theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Black-and-white Warbler") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Black and white warbler.png", gg_bwwa)


#Acadian Flycatcher
ACFL <- data %>% 
  subset(SPECIES_NAME == "Empidonax virescens")

#Distribution of historical records by year
hist_records <- subset(ACFL, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(ACFL, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_acfl <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Acadian Flycatcher") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Acadian Flycatcher.png", gg_acfl)


#Mourning Warbler
MOWA <- data %>% 
  subset(SPECIES_NAME == "Geothlypis philadelphia")

#Distribution of historical records by year
hist_records <- subset(MOWA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(MOWA, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

density_df <- data.frame()

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_mowa <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Mourning Warbler") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Mourning warbler.png", gg_mowa)


#Tennessee Warbler
TEWA <- data %>% 
  subset(SPECIES_NAME == "Leiothlypis peregrina")

#Distribution of historical records by year
hist_records <- subset(TEWA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(TEWA, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomizations
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_tewa <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Tennessee Warbler") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Tennessee warbler.png", gg_tewa)


#Blackburnian Warbler
BLWA <- data %>% 
  subset(SPECIES_NAME == "Setophaga fusca")

#Distribution of historical records by year
hist_records <- subset(BLWA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(BLWA, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_blwa <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Blackburnian Warbler") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Blackburnian warbler.png", gg_blwa)


#Parkesia noveboracensis
NOWA <- data %>% 
  subset(SPECIES_NAME == "Parkesia noveboracensis")

#Distribution of historical records by year
hist_records <- subset(NOWA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(NOWA, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_nowa <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Northern Waterthrush") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Northern waterthrush.png", gg_nowa)


#Catharus ustulatus
SWTH <- data %>% 
  subset(SPECIES_NAME == "Catharus ustulatus")

#Distribution of historical records by year
hist_records <- subset(SWTH, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(SWTH, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_swth <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Swainson's Thrush") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Swainson's Thrush.png", gg_swth)


#Setophaga ruticilla
AMRE <- data %>% 
  subset(SPECIES_NAME == "Setophaga ruticilla")

#Distribution of historical records by year
hist_records <- subset(AMRE, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(AMRE, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_amre <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("American Redstart") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - American Redstart.png", gg_amre)


#Piranga Rubra
SUTA <- data %>% 
  subset(SPECIES_NAME == "Piranga rubra")

#Distribution of historical records by year
hist_records <- subset(SUTA, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(SUTA, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_suta <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Bay-breasted Warbler") + 
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Summer Tanager") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Density updated - Summer Tanager.png", gg_suta)

#Import graphs as images to arrange as a grid
ALL_SPP <- image_read("Hist vs Mod - ALL DATA.png")
YEWA <- image_read("Density updated - Yellow warbler.png")
BAWA <- image_read("Density updated - Bay breasted warbler.png")
GCFL <- image_read("Density updated - Great crested flycatcher.png")
ACFL <- image_read("Density updated - Acadian flycatcher.png")
SUTA <- image_read("Density updated - Summer tanager.png")
TEWA <- image_read("Density updated - Tennessee warbler.png")
SWTH <- image_read("Density updated - Swainson's thrush.png")
MOWA <- image_read("Density updated - Mourning warbler.png")
NOWA <- image_read("Density updated - Northern waterthrush.png")
AMRE <- image_read("Density updated - American redstart.png")
BLWA  <- image_read("Density updated - Blackburnian warbler.png")
BWWA  <- image_read("Density updated - Black and white warbler.png")

#Re-convert to ggplot objects
gg_ALL_SP <- ggdraw() + draw_image(ALL_SPP)
gg_YEWA <- ggdraw() + draw_image(YEWA) 
gg_BAWA <- ggdraw() + draw_image(BAWA) 
gg_GCFL <- ggdraw() + draw_image(GCFL) 
gg_ACFL <- ggdraw() + draw_image(ACFL) 
gg_SUTA <- ggdraw() + draw_image(SUTA) 
gg_TEWA <- ggdraw() + draw_image(TEWA)
gg_SWTH <- ggdraw() + draw_image(SWTH)
gg_MOWA <- ggdraw() + draw_image(MOWA) 
gg_NOWA <- ggdraw() + draw_image(NOWA) 
gg_AMRE <- ggdraw() + draw_image(AMRE) 
gg_BLWA <- ggdraw() + draw_image(BLWA) 
gg_BWWA <- ggdraw() + draw_image(BWWA) 

#Arrange them in a grid
grid <- plot_grid(gg_BWWA, gg_BLWA, gg_YEWA, gg_AMRE,
                  gg_SWTH, gg_GCFL, gg_MOWA, gg_BAWA,
                  gg_TEWA, gg_NOWA, gg_SUTA, gg_ACFL,
                  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
                  ncol = 3, nrow = 4)
grid
#---------------------Phenological shift categories------------------------#
#Extended summer
ExtSum <- data %>% 
  subset(SPECIES_NAME %in% c("Catharus ustulatus", 
                             "Mniotilta varia",
                             "Setophaga ruticilla",
                             "Setophaga petechia",
                             "Setophaga fusca"))

#Distribution of historical records by year
hist_records <- subset(ExtSum, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(ExtSum, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_extsum <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Extended summer") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Hist vs Mod - Extended summer.png", gg_extsum)



#Extended winter
ExtWint <- data %>% 
  subset(SPECIES_NAME %in% c("Myiarchus crinitus",
                             "Geothlypis philadelphia",
                             "Leiothlypis peregrina",
                             "Setophaga castanea"))

#Distribution of historical records by year
hist_records <- subset(ExtWint, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(ExtWint, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_extwint <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Extended winter") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Hist vs Mod - Extended winter.png", gg_extwint)

#Late winter shift
LatWint <- data %>% 
  subset(SPECIES_NAME %in% c("Parkesia noveboracensis",
                             "Piranga rubra"))

#Distribution of historical records by year
hist_records <- subset(LatWint, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(LatWint, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_latwint <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Late winter shift") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Hist vs Mod - Late winter.png", gg_latwint)

#Early winter shift
ACFL <- data %>% 
  subset(SPECIES_NAME == "Empidonax virescens")

#Distribution of historical records by year
hist_records <- subset(ACFL, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Get density data
density_data <- density(hist_records$day_of_year)
#Calculate local maxima
loc.max <- density_data$x[localMaxima(density_data$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(ACFL, period == "modern")

#Create matrix for empty dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Create dataframes for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")

day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"

#Randomization
for(xx in 1:1000) {
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
gg_earwint <- ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())+
  ggtitle("Early winter shift") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Hist vs Mod - Early winter.png", gg_earwint)

#Import graphs as images to arrange as a grid
ExtSum <- image_read("Hist vs Mod - Extended summer.png")
ExtWint <- image_read("Hist vs Mod - Extended winter.png")
LatWint <- image_read("Hist vs Mod - Late winter.png")
EarWint <- image_read("Hist vs Mod - Early winter.png")


#Re-convert to ggplot objects
gg_ExtSum <- ggdraw() + draw_image(ExtSum) 
gg_ExtWint <- ggdraw() + draw_image(ExtWint) 
gg_LatWint <- ggdraw() + draw_image(LatWint) 
gg_EarWint <- ggdraw() + draw_image(EarWint) 

#Arrange them in a grid
grid <- plot_grid(gg_ExtSum, gg_ExtWint, gg_LatWint, gg_EarWint,
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2)
grid

#-------------------All species-----------------------------#
#Load data
data <- read.table("all_data.txt", h = T, sep = "t")

#Distribution of historical records by year
hist_records <- subset(data, period == "historical")
hist_records <- na.omit(hist_records)
hist_records <- hist_records%>% drop_na(YEAR)

#Generate days of max density with historical records
#Separate spring from fall to extract day of max density

#Extract day of max density 
#Definir función para calcular máximos locales
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#Get density data for historical data
density_data_hist <- density(hist_records$day_of_year)
#Make a dataframe for historical data
density_df_hist <- data.frame(
  x = density_data_hist$x,
  density = density_data_hist$y
)
#Calculate local maxima
loc.max <- density_data_hist$x[localMaxima(density_data_hist$y)]
max_density <- max(loc.max)

#Save historical max densities
hist_max_density <- loc.max
hist_max_dens_spring <- min(hist_max_density)
hist_max_dens_fall <- max(hist_max_density)

#Create subset for modern records
mod_records <- subset(data, period == "modern")

#Generate matrix for blank dataframe
matrix_data <- matrix(0, nrow = 1000, ncol = 2)

#Generate data frames for spring and fall
spring <- data.frame(matrix_data)
fall <- data.frame(matrix_data)

colnames(spring) <- c("Max_density" , "Difference")
colnames(fall) <- c("Max_density" , "Difference")
day_of_year_df <- data.frame(day_of_year = integer())
colnames(day_of_year_df) <- "day_of_year"


#Randomization
for(xx in 1:1000) {
  #Extract a random sample from the modern database with an equal number of records to the historical database.
  random_mod <- mod_records %>% sample_n(size = nrow(hist_records), replace = FALSE)
  #Get density data
  density_data <- density(random_mod$day_of_year)
  # Save day_of_year values to a dataframe
  rand_day_of_year <- data.frame(day_of_year = random_mod$day_of_year)
  #Add iteration
  rand_day_of_year$iteration <- xx
  # Append to the overall dataframe
  day_of_year_df <- rbind(day_of_year_df, rand_day_of_year)
  
  #Calculate local maxima
  loc.max <- density_data$x[localMaxima(density_data$y)]
  mod_max_density <- loc.max
  mod_max_density_spring <- min(mod_max_density)
  mod_max_density_fall <- max(mod_max_density)
  
  spring[xx,1] <- mod_max_density_spring
  fall[xx, 1] <- mod_max_density_fall
  
  #Calculate difference
  spring[xx, 2] <- mod_max_density_spring -hist_max_dens_spring  
  fall[xx, 2] <- mod_max_density_fall - hist_max_dens_fall
  print(xx)
  
}

#Add category
day_of_year_df$period <- "modern"

day_of_year_df$iteration <- as.factor(day_of_year_df$iteration)

#Create a plot showing the density curves for all 1000 modern randomizations and 1 curve for the historical data
ggplot(day_of_year_df, aes(x = day_of_year, color = iteration)) +
  geom_density() +
  geom_density(data = hist_records, aes(x = day_of_year), color = "black", lwd=2) +  # Add another density plot
  geom_vline(xintercept = max(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(spring$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = max(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = min(fall$Max_density), linetype = "dashed", color = "grey", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_fall, linetype = "dashed", color = "black", lwd = 1) +
  geom_vline(xintercept = hist_max_dens_spring, linetype = "dashed", color = "black", lwd = 1) + 
  labs(x = "Day of Year",
       y = "Density") +
  theme_minimal()+
  scale_color_manual(values = grey(seq(0.1, 0.9, length.out = 1000))) +  # Shades of grey
  guides(color = "none")+  # Hide color legend
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line())

