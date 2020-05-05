# Concatenate_tsvs

# This script concatenates count summary files for aquascope images identified using the workflow developed by Peter Isles. 

# Set parent directory 
wd = "/Users/peterisles/Dropbox/Aquascope_classifier_test" 

setwd(wd)

tsvFiles<-list.files(recursive=TRUE, full.names = TRUE)[grep("features.tsv", list.files(recursive=TRUE))] # for the full set of files in the directory, we could subset this to a manageable number of files for each set of tsvs (maybe monthly?)

for (i in 1:length(tsvFiles)) {
  if (i == 1) {
    x<-read.table(tsvFiles[i], stringsAsFactors = FALSE, sep = "\t", header=TRUE)
  }
  else {
    y<-read.table(tsvFiles[i], stringsAsFactors = FALSE, sep = "\t", header=TRUE)
    x<-merge(x, y, all=TRUE)
  }
}

head(x)
write.csv(x, file = "combined_tsvs.csv", row.names=FALSE)

x$timestamp<-as.POSIXct(x$timestamp)
x$hour<-as.POSIXlt(x$timestamp)$hour # add column of hour
x$yday<-as.POSIXlt(x$timestamp)$yday + 1 # add day of year
# x$hourday<-paste(x$yday, x$hour, sep = "_") # this we can use for casting data from 2019-2020 (for 2018 we will need to do more subsetting because of the different data collection)


breaks<-seq(min(log10(x$area)), max(log10(x$area)), length.out=15) # set breaks for binning. Here, we can define a min and max value and number of breaks that is consistent across folders (rather than as here, where we are defining our cut points based on the range of data in this folder). This would be useful if working with one giant tsv file proves to be too complicated.

x$area_bin<-cut(log10(x$area), breaks = breaks) 

# For size spectrum stuff, I would suggest figuring out lower and upper thresholds. 

bin_melt<-melt(
  x[,c("yday", "hour", "area_bin")], id.vars = c("yday", "hour")
)
bin_melt$
area_bin_cast<-cast(
  ,
  yday + hour ~ value,
  fun.aggregate = "length"
)

