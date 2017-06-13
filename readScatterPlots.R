# Read CSV file and extract the number of scatterplots indicated by checking scatterplotID and separate the different scatterplots
# Some have a odd behaviour, with jumps of 1, 17 or 25 datapointID
readScatterPlots <- function(numScatterPlots, skiplines, fileLetter) {
  dirData <- file.path('G:', 'Particular', 'Proyectos', 'En marcha', 'ScatterPlotsClustering_Innocentive', 'train')
  fileName <- file.path(dirData, paste0('datafile_train20', fileLetter, '.csv'))
  dataInFile <- read.csv(fileName, nrows = 384*numScatterPlots, skip = skiplines) # Read numScatterPlots scatterplots, assuming each has 384 points and skipping skiplines lines
  colnames(dataInFile) <- c('scatterplotID', 'datapointID', 'sampletype', 'signalX', 'signalY', 'cluster', 'silhouette')
  scatterPlots_list = split(dataInFile, f = dataInFile$scatterplotID)
  # Check length of scatterplots, some are truncated when reading the file and others have missing points. Only complete (384 points) scatterplots are considered now
  list_dimensions <- sapply(scatterPlots_list, dim)
  completeScatterPlots_list <- scatterPlots_list[list_dimensions[1, ] == 384]
  return(completeScatterPlots_list)
}