
#Install Required Packages

install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("DMwR")

# Import Required Libraries 

library(dplyr)
library(ggplot2) 

library(GGally) 

library(DMwR) 

# Import  dataset 

Downtime_Events <- read.csv("Data_For_Clustering.csv", header = T) 
View(Downtime_Events)

# Remove the Planned downtime Reasons
Downtime_Events = Downtime_Events[- grep("Planned Downtime", Downtime_Events$ConcatenatedReasonV3toV7),]
View(Downtime_Events)

#Grouping the data based on downtime reason

Reasons <- group_by(Downtime_Events,ConcatenatedReasonV3toV7)

#Summarizing the other fields based on the grouping field - Downtime Reason

Downtime_Events <- summarise(Reasons, Speed = mean(Interpolated.Time.line.Speed),  Revenue = mean(Difference.in.packs) ,Frequency = n_distinct(DMShiftLineID),Operations = n_distinct(RootCategory))
View(Downtime_Events)

# Removing the null value in the downtime reason

Downtime_Events <- Downtime_Events[!(is.na(Downtime_Events$ConcatenatedReasonV3toV7) | Downtime_Events$ConcatenatedReasonV3toV7==""), ]
View(Downtime_Events)

# Writing the final dataset to a CSV file

write.csv(Downtime_Events,file ='Downtime_Events_Grouped_Dataset.csv') 

# Plot to view the data and check outliers 

ggpairs(Downtime_Events[, which(names(Downtime_Events) != "ConcatenatedReasonV3toV7")], upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Downtime Events before outlier removal") 

# Data Cleaning - Outlier Removal

Downtime_Events.clean <-Downtime_Events[Downtime_Events$ConcatenatedReasonV3toV7 != "RewinderOperationalWork Order", ]  # Two out of 3 values satisfy the outlier criteria 

View(Downtime_Events.clean) 

#Downtime_Events.clean <-Downtime_Events[Downtime_Events$ConcatenatedReasonV3toV7 != "FolderAuto ClassifyFaulted", ]   

#View(Downtime_Events.clean) 

# Plot after outlier removal 

ggpairs(Downtime_Events.clean[,which(names(Downtime_Events.clean)!="ConcatenatedReasonV3toV7")], upper = list(continuous = ggally_points), lower = list(continuous = "points"), title = "Downtime Events after  removing outlier")

# Scaling data to clean data 

#Downtime_Events.scale = scale(Downtime_Events.clean[-1]) 

#View(Downtime_Events.scale) 


Downtime_Events.scale = scale(Downtime_Events[-1]) 

View(Downtime_Events.scale)


#WithinSS function to calculate the withinSS

withinSSrange <-function(data,low,high,maxIter)   
  
{ 
  
  withinss = array(0, dim=c(high-low+1)); 
  
  for(i in low:high) 
    
  { 
    
    withinss[i-low+1] <-kmeans(data, i, maxIter)$tot.withinss 
    
  } 
  
  withinss 
  
} 

# Plot of winthinss
plot(withinSSrange(Downtime_Events.scale,1,50,150)) 

#Finding the K -value(No of clusters)

set.seed(55) 

pkm = kmeans(Downtime_Events.scale, 4, 150)  # K-means clustering performed for cluster size of 4 

pkm$tot.withinss 

View(pkm) 

# Final Unscaled K-means Centroids 

Downtime_Events.realCenters = unscale(pkm$centers,Downtime_Events.scale)

View(Downtime_Events.realCenters) 

# Data frame created with binded cluster number for each record

Downtime_Events.clustered = cbind(Downtime_Events, pkm$cluster) 
View(Downtime_Events.clustered)

plot(pkm$cluster) 

plot(Downtime_Events.clustered[,2:6], col=pkm$cluster) 

write.csv(Downtime_Events.clustered, file = "Clustered_Downtime_Data.csv", col.names = FALSE) # Final dataset created with cluster numbers for profiling and visualization. 

