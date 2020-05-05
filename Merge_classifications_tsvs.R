# Merge_classifications_tsvs
# Peter Isles
# Eawag

# This script takes the classifications from the tag-based workflow and the ExtractMacTagsForClassification.R script and merges it with the tsv file. In the case that there are multiple folders associated with each tsv file (i.e. if there are more than 6000 images so multiple tar folders are created), this script will look for all classification files and merge them with the single tsv. 

# As it is now, there is a hard coded edit to the path names, because the tsv paths are relative while those in the extract-mac-tags script are absolute. This could be pretty easily solved once we have this in the directory structure where we will be doing most of our work.

# Set working directory
wd<-"/Users/peterisles/Dropbox/Aquascope_classifier_test/5p0xMAG_1" # note that this working directory could have subdirectories if SPC_convert had multiple tar files (esp. with zooplankton).
setwd(wd)

# Read the tsv file
tsvFile<-read.table("features.tsv", stringsAsFactors = FALSE, sep = "\t", header=TRUE)

# generate a list of all classification files within the parent directory.
classificationFiles<-list.files(pattern = "classified_images.csv", recursive = TRUE, full.names = TRUE)

tsvFile$taxonomic_ID<-NA

# loop through all classification files (will often be only one) and merge IDs onto hte tsv file
for (i in 1:length(classificationFiles)) {
  x<-read.csv(classificationFiles[i]) # read csv classification file
  x$file<-gsub("/Users/peterisles/Dropbox/Aquascope_classifier_test/5p0xMAG_1/", "", x$file) # truncate or modify the path name, because here I used the root directory for the paths in the classification files but relative paths are written in the tsv.
  
  if (i == 1) {
    tsvFile$taxonomic_ID<-x$taxonomicID[match(tsvFile$url, x$file)]
  }
  else {
    missingIDs<-which(is.na(tsvFile$taxonomic_ID))
    tsvFile$taxonomic_ID[missingIDs]<-x$taxonomicID[match(tsvFile$url[missingIDs], x$file)] # only match and replace values which were not filled in previous files.
    # Could easily modify the previous line to also append other columns from classification script (location, identified by, additional tags)
  }
}

# Demonstrate that we can now look at e.g. size distributions for different species:
par(mar = c(10, 4, 2, 1))
boxplot(log10(tsvFile$area) ~ tsvFile$taxonomic_ID, las = 3, xlab = "")

write.csv(tsvFile, file = "tsv_with_classes.csv", row.names=FALSE) # This may end up being a very convenient format for Marco also; if we concatenate a master list of all SPC tsvs with associated classificaitons, this would contain all information needed for the classifier in one place (image path plus SPC attributes plus taxonomic ID).
