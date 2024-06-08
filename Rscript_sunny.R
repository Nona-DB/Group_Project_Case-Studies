#########################
###   THE SUN STUDY   ###
###    Case Studies   ###
###  De Brauwere Nona ###
###  De Jaeger Geike  ###
###  Timmerman Femke  ###
###     Wylin Kaat    ###
#########################

#packages 
library(intergraph)
library(igraph)
library(ggnetwork)
library(tidyverse)
library(ggplot2)
library("bootnet")
library(car)
library("qgraph")
library(corrplot)
library(bnlearn)
library(lavaan)
library(qgraph)
library(psych)

### set workign directory ###
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
### Import data + preprocessing ###
sun_study <- read.csv(file = "sun_study_gefilterd.csv", header = TRUE, sep = ";")
sun_study <- subset(sun_study, select = -c(Time,Group,PSI,R_ambi_conf,SUN_total,GR_cont,GR_bene,SUN_bene))
sun_study$GR_expo <- as.numeric(sun_study$GR_expo)
sun_study$SUN_ind <- as.numeric(sun_study$SUN_ind)
sun_study$SUN_dir <- as.numeric(sun_study$SUN_dir)
sun_study$R_ambi <- as.numeric(sun_study$R_ambi)
sun_study$R_conf <- as.numeric(sun_study$R_conf)
sun_study$J_sat <- as.numeric(sun_study$J_sat)
sun_study$O_com <- as.numeric(sun_study$O_com)
sun_study$Dep <- as.numeric(sun_study$Dep)
sun_study$Anx <- as.numeric(sun_study$Anx)
sun_study$age <- as.numeric(sun_study$age)
sun_study$No.2 <- rep(1:444)

  # Check for missing values in each column
missing_values <- colSums(is.na(sun_study))
print(missing_values)
sun_study <- na.omit(sun_study)

  # Check for outliers 
boxplot(sun_study$GR_expo)
mean_score <- mean(sun_study$GR_expo)
sd_score <- sd(sun_study$GR_expo)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$GR_expo > (mean_score + threshold) | sun_study$GR_expo < (mean_score - threshold)]
print(outliers)

boxplot(sun_study$SUN_ind)
mean_score <- mean(sun_study$SUN_ind)
sd_score <- sd(sun_study$SUN_ind)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$SUN_ind > (mean_score + threshold) | sun_study$SUN_ind < (mean_score - threshold)]
print(outliers)

boxplot(sun_study$SUN_dir)
mean_score <- mean(sun_study$SUN_dir)
sd_score <- sd(sun_study$SUN_dir)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$SUN_dir > (mean_score + threshold) | sun_study$SUN_dir < (mean_score - threshold)]
print(outliers)

boxplot(sun_study$R_ambi)
mean_score <- mean(sun_study$R_ambi)
sd_score <- sd(sun_study$R_ambi)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$R_ambi > (mean_score + threshold) | sun_study$R_ambi < (mean_score - threshold)]
print(outliers)
sun_study <- subset(sun_study, !(No.2 %in% c("83","116","103","148")))

boxplot(sun_study$R_conf)
mean_score <- mean(sun_study$R_conf)
sd_score <- sd(sun_study$R_conf)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$R_conf > (mean_score + threshold) | sun_study$R_conf < (mean_score - threshold)]
print(outliers)
sun_study <- subset(sun_study, !(No.2 %in% c("35")))

boxplot(sun_study$J_sat)
mean_score <- mean(sun_study$J_sat)
sd_score <- sd(sun_study$J_sat)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$J_sat > (mean_score + threshold) | sun_study$J_sat < (mean_score - threshold)]
print(outliers)
sun_study <- subset(sun_study, !(No.2 %in% c("11","15","51","65","78","81","117","123","124","129","130","140","169","120")))

boxplot(sun_study$O_com)
mean_score <- mean(sun_study$O_com)
sd_score <- sd(sun_study$O_com)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$O_com > (mean_score + threshold) | sun_study$O_com < (mean_score - threshold)]
print(outliers)
sun_study <- subset(sun_study, !(No.2 %in% c("27","64","75","115","147","185","195","198")))

boxplot(sun_study$Dep)
mean_score <- mean(sun_study$Dep)
sd_score <- sd(sun_study$Dep)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$Dep > (mean_score + threshold) | sun_study$Dep < (mean_score - threshold)]
print(outliers)
sun_study <- subset(sun_study, !(No.2 %in% c("122","143","157","259","162","362","418")))

boxplot(sun_study$Anx)
mean_score <- mean(sun_study$Anx)
sd_score <- sd(sun_study$Anx)
threshold <- 2.5 * sd_score
outliers <- sun_study$No.2[sun_study$Anx > (mean_score + threshold) | sun_study$Anx < (mean_score - threshold)]
print(outliers)
sun_study <- subset(sun_study, !(No.2 %in% c("187","208","212","217","236","298","273","358","411","429")))

### Descriptives ###
  #age
range_age <- sun_study %>% summarize(min_age = min(age, na.rm = TRUE), max_age = max(age, na.rm = TRUE)); range_age
main_age <- sun_study %>% 
  summarise(
    mean = mean(sun_study$age),
    sd = sd(sun_study$age)
  );main_age
  #sex
sex <- table(sun_study$sex);sex
sex_percentage <- (sex/283)*100;sex_percentage
  #workhour
range_w <- sun_study %>% summarize(min_w = min(workhour, na.rm = TRUE), max_w = max(workhour, na.rm = TRUE)); range_w
main_w <- sun_study %>% 
  summarise(
    mean = mean(sun_study$workhour),
    sd = sd(sun_study$workhour)
  );main_w
  #anxiety
range_a <- sun_study %>% summarize(min_a = min(Anx, na.rm = TRUE), max_a = max(Anx, na.rm = TRUE)); range_a
main_a <- sun_study %>% 
  summarise(
    mean = mean(sun_study$Anx),
    sd = sd(sun_study$Anx)
  );main_a  
  #depression
range_d <- sun_study %>% summarize(min_d = min(Dep, na.rm = TRUE), max_d = max(Dep, na.rm = TRUE)); range_d
main_d <- sun_study %>% 
  summarise(
    mean = mean(sun_study$Dep),
    sd = sd(sun_study$Dep)
  );main_d
  #Job-satisfactie
range_j <- sun_study %>% summarize(min_j = min(J_sat, na.rm = TRUE), max_j = max(J_sat, na.rm = TRUE)); range_j
main_j <- sun_study %>% 
  summarise(
    mean = mean(sun_study$J_sat),
    sd = sd(sun_study$J_sat)
  );main_j
  #Organisational comittment
range_o <- sun_study %>% summarize(min_o = min(O_com, na.rm = TRUE), max_o = max(O_com, na.rm = TRUE)); range_o
main_o <- sun_study %>% 
  summarise(
    mean = mean(sun_study$O_com),
    sd = sd(sun_study$O_com)
  );main_o
  #direct sunlight
range_di <- sun_study %>% summarize(min_di = min(SUN_dir, na.rm = TRUE), max_o = max(SUN_dir, na.rm = TRUE)); range_di
main_di <- sun_study %>% 
  summarise(
    mean = mean(sun_study$SUN_dir),
    sd = sd(sun_study$SUN_dir)
  );main_di
  #indirect sunlight
range_in <- sun_study %>% summarize(min_in = min(SUN_ind, na.rm = TRUE), max_in = max(SUN_ind, na.rm = TRUE)); range_in
main_in <- sun_study %>% 
  summarise(
    mean = mean(sun_study$SUN_ind),
    sd = sd(sun_study$SUN_ind)
  );main_in
  #natural elements
range_g <- sun_study %>% summarize(min_g = min(GR_expo, na.rm = TRUE), max_g = max(GR_expo, na.rm = TRUE)); range_g
main_g <- sun_study %>% 
  summarise(
    mean = mean(sun_study$GR_expo),
    sd = sd(sun_study$GR_expo)
  );main_g

sun_study <- subset(sun_study, select = -c(No.2,age,workhour,sex))
names(sun_study)[names(sun_study) == "GR_expo"] <- "Natural Elements"
names(sun_study)[names(sun_study) == "SUN_ind"] <- "Indirect Sun"
names(sun_study)[names(sun_study) == "SUN_dir"] <- "Direct Sun"
names(sun_study)[names(sun_study) == "Anx"] <- "Anxiety"
names(sun_study)[names(sun_study) == "Dep"] <- "Depression"
names(sun_study)[names(sun_study) == "R_conf"] <- "Role Confidence"
names(sun_study)[names(sun_study) == "R_ambi"] <- "Role Ambiguity"
names(sun_study)[names(sun_study) == "J_sat"] <- "Job Satisfaction"
names(sun_study)[names(sun_study) == "O_com"] <- "Job Commitment"


### Network: method 1 ###
  # Create the network
Network <- estimateNetwork(sun_study, default = "EBICglasso")
  # Plot the network with edge strength
str(Network)
edge_weights <- Network$graph
edge_signs <- sign(edge_weights)
edge_colors <- ifelse(edge_signs > 0, "darkgreen", "darkred")
  # Plot the network with customized edge colors
png("network.png", width = 2400, height = 2200, res = 300)
qgraph::qgraph(
  Network$graph,
  layout = "spring",
  labels = Network$labels,
  edge.color = edge_colors,
  edge.labels = round(edge_weights, 2),  # Use edge.labels instead of edge.label
  asize = 15,  # Adjust the size of the nodes
  esize = 3  # Adjust the size of the edges
)
dev.off()

### Centrality ###
png("centrality.png", width = 2400, height = 1800, res = 300)
centralityPlot(Network,include = c("Strength","Closeness","Betweenness"))
dev.off()

# Centrality stability 
CentralStability <- bootnet(Network, nBoots = 1000, type = "case",statistics = c("strength","closeness","betweenness"))

# Calculate and print Correlation Stability (CS) coefficient
cs_coefficient <- corStability(CentralStability)
print(cs_coefficient)
summary(CentralStability)
png("stability.png", width = 2400, height = 1800, res = 300)
plot(CentralStability,statistics = c("strength","closeness","betweenness"))
dev.off()

### Edge weight accuracy ###
EdgeWgt<- bootnet(Network, nBoots = 2500)
summary(EdgeWgt)
png("weight_acc.png", width = 2400, height = 1800, res = 300)
plot(EdgeWgt, labels = TRUE, order = "sample")
dev.off()

