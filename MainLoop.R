# Read scatterPlots from CSV files and test clusters
require(cluster)
# Read CSV files, check scatterplotID and separate the different scatterplots
# Some have a odd behaviour, with jumps of 1, 17 or 25 datapointID
numScatterPlots2Read <- 20 # Test the algorithm with this number of scatter plots
numscatterPlots2Skip <- 6
fileLetter <- c('a', 'b', 'c', 'd') # A loop over these four values will rejoin the dataset (4 x 20000 = 80000)
scatterPlots_list <- readScatterPlots(numScatterPlots2Read, numscatterPlots2Skip*384, fileLetter[1]) # Only complete (384 points) by now (2017/06/13)

# Analysis of each scatterplot
for (indexScatterPlot in 1:length(scatterPlots_list)) {
  # Change cluster letters to numbers for later comparison
  scatterPlots_list[[indexScatterPlot]]$numericCluster <- NA
  scatterPlots_list[[indexScatterPlot]]$numericCluster[which(scatterPlots_list[[indexScatterPlot]]$cluster == 'X')] <- 1 # X = 1
  scatterPlots_list[[indexScatterPlot]]$numericCluster[which(scatterPlots_list[[indexScatterPlot]]$cluster == 'Y')] <- 2 # Y = 2
  scatterPlots_list[[indexScatterPlot]]$numericCluster[which(scatterPlots_list[[indexScatterPlot]]$cluster == 'XY')] <- 3 # XY = 3
  scatterPlots_list[[indexScatterPlot]]$numericCluster[which(scatterPlots_list[[indexScatterPlot]]$cluster == 'L')] <- 0 # L = 0
  
  plot(scatterPlots_list[[indexScatterPlot]]$signalX, scatterPlots_list[[indexScatterPlot]]$signalY, col = scatterPlots_list[[indexScatterPlot]]$numericCluster)
  scatterPlots_list[[indexScatterPlot]]$angle <- atan2(scatterPlots_list[[indexScatterPlot]]$signalY, scatterPlots_list[[indexScatterPlot]]$signalX)
  scatterPlots_list[[indexScatterPlot]]$radius <- sqrt(scatterPlots_list[[indexScatterPlot]]$signalX^2 + scatterPlots_list[[indexScatterPlot]]$signalY^2)
  plot(scatterPlots_list[[indexScatterPlot]]$radius, scatterPlots_list[[indexScatterPlot]]$angle, col = scatterPlots_list[[indexScatterPlot]]$numericCluster) 
  
  # Test each scatterplot
  if (!all(diff(scatterPlots_list[[indexScatterPlot]]$datapointID) == 1)) {
    warning('Error in datapointID')
  }
  indexSamples <- which(scatterPlots_list[[indexScatterPlot]]$sampletype == 'Sample')
  indexNegative <- which(scatterPlots_list[[indexScatterPlot]]$sampletype == 'Negative')
  indexValid <- which(scatterPlots_list[[indexScatterPlot]]$silhouette != -2)
  validScatterPlot <- scatterPlots_list[[indexScatterPlot]][indexValid, ]
  
  plot(validScatterPlot$signalX, validScatterPlot$signalY, col = validScatterPlot$numericCluster)
  # Change to polar coordinates
  
  validScatterPlot$angle <- atan2(validScatterPlot$signalY, validScatterPlot$signalX)
  validScatterPlot$radius <- sqrt(validScatterPlot$signalX^2 + validScatterPlot$signalY^2)
  plot(validScatterPlot$radius, validScatterPlot$angle, col = validScatterPlot$cluster) 
  kmeans_clusters <- kmeans(validScatterPlot[8], 3)
  plot(validScatterPlot$radius, validScatterPlot$angle, col = kmeans_clusters$cluster, pch = as.numeric(validScatterPlot$cluster)) 
  
  pam_cluster <- pam(validScatterPlot[8], 3)
  silhouette(pam_cluster)
  plot(validScatterPlot$radius, validScatterPlot$angle, col = pam_cluster$clustering, pch = as.numeric(validScatterPlot$cluster))
  percentMatch <- kmeans_clusters$cluster - validScatterPlot$numericCluster
  }

rm(validScatterPlot)





# Plots



# Check with solution