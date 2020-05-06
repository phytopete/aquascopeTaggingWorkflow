# This script concatenates count summary files for aquascope images identified using the workflow developed by Peter Isles. 

# Set parent directory 
wd = "/Users/peterisles/Dropbox/Aquascope_classifier_test" 

setwd(wd)

summaryFiles<-list.files(recursive=TRUE, full.names = TRUE)[grep("/classification_summary.csv", list.files(recursive=TRUE))]

for (i in 1:length(summaryFiles)) {
  if (i == 1) {
    c<-read.csv(summaryFiles[i], check.names = FALSE)
  }
  else {
    y<-read.csv(summaryFiles[i], check.names = FALSE)
    c<-merge(c, y, all=TRUE)
  }
}

head(c)

idVars<-which(names(c) %in% c("Directory", "timerange"))
c<-c[,c(idVars, c(1:length(c))[-idVars])] # reorder the columns so that the ID columns show up in front

idVars<-which(names(c) %in% c("Directory", "timerange")) # get new indices for id vars

# c[,-idVars]<-c[,-idVars][,order(names(c[,-idVars]))] 

c<-c[,c(idVars, order(names(c)[-idVars]) + length(idVars))] # reorder so that additional columns are in alphabetical order (I am sure there is a better way, but this works)

head(c)

write.csv(c, file = "combined_classification_summary.csv", row.names=FALSE)


