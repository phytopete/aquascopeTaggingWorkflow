# R script for extracting MacOS user tags and filenames from files in a directory, and writing a csv with the file path and the tag.
# Peter Isles

# This script is part of the workflow for identifying images from aquascope. There is no need for taxonify, but this approach does rely on using Mac computers; it may be possible to set up an analog in Windows, too, but I haven't been able to track down how Windows stores user Tags, and some internet sources seem to indicate that Windows tags are unstable and be lost easily when copying, emailing, etc.. In this approach, we carry out identification using custom tags in Finder. The basic steps of the workflow are described here: 

# PROCEDURE:
# 1. First, set to tile view, and increase the icon size (in the view menu in Finder) so that individual images can be seen well enough without opening them in preview (this can still of course be done when necessary). Sorting by file size will also sort by image size, so can help for grouping images. You can also enter "NOT binary NOT rawcolor" in the search bar in the top right of the finder window, and make sure the current directory is selected in the top-right of the finder window rather than "This Mac". This means you won't need to tag or sort through the raw color or binary images.
# 3. Make a first pass tagging the images sorted by size, assigning taxonomic IDs with the prefix "1_" (e.g. "1_Microcystis", "1_Asterionella") 
# 4. then click the "use groups" option and sort by tag (both in the View Menu). This will make separate scrolling panes for each taxonomic group. Scroll down and see if any images have no tags (untagged images will be in a separate category at the bottom). If you are unsure of the images, you can leave them untagged until the expert taxonomist (Marta) makes the final determination. We could come up with a system for tagging folders that need to be checked by an expert (e.g. with a red circle pre-check, with a green circle when approved)
# 5. In this view, Marta can also easily check all IDs done by whoever does the inital counting -- this reduces errors from misidentification. If something has been consistently misidentified, it is easy to select all of these and re-tag.
# 6. once all images have been tagged with taxonomic IDs (starting in "1_" as in "1_Microcystis"), select all images that you have tagged and add a tag for your name, starting with "2_" to identify who did the tagging (e.g. "2_Peter"). This will allow someone (e.g. Marta, always Marta :) ) to search the computer for anything that you have tagged and update it if s/he finds you have been consistently mis-identifying something (e.g. you can search for items with the tags "1_Sphaerocystis" and "2_Peter" and update to "1_Oocystis" if Peter has consistently misidentified Oocystis, but you don't have to re-assign all 1_Sphaerocystis that have been correctly tagged by Marta). NOTE: IT IS BEST TO DO THIS AFTER ALL IMAGES HAVE BEEN TAGGED WITH TAXONOMY, BECAUSE AFTER THAT UNIDENTIFIED IMAGES WILL NO LONGER SHOW UP IN THE "NO TAG" GROUP, MAKING THEM HARD TO FIND. If you need to find untagged images after the fact, you can delete the 2_ and 3_ tags, find the unidentified images, then reapply the 2_ and 3_ tags.
# 7. At the same time, select all and add a tag for the location where the sample came from, with the prefix "3_" (e.g. "3_Greifensee", "3_Greifensee_profile", "3_Lab", "3_Zurichsee_profile"). This way we can quickly make training sets for different lakes, or excluding lab samples, or whatever.
# 8. You can also add whatever other tags you want, but they should not start with numbers (unless we agree on new categories for different attributes. These will be stored in extra columns of the output csv, but won't be sorted consistently.)
# 9. we can then run this script to export a list of filenames with tags, which is what is needed by training sets, etc. It also means that for validation we have classifications by image instead of by date/folder for easy comparison with Marco. This should be easy to merge with the tsv files produced by SPC_Convert, making it possible to look at size distributions or other shape/color parameters within species.
# 10. This script also produces a summary with counts of each taxonomic class, along with the folder name and time range. When you have identified a number of folders (e.g. a time series), you can then run the Concatenate_summaries.R script from a parent directory, which will search for all appropriately named summary files and concatenate them into a single csv file. THis file can then be used as a time series for plotting, etc. 
# 11. When it is time to put together training sets, we can just search for all images with a given tag in finder, select all, copy the file paths for all images, and save that to a text file. No need to move the images around into different sub folders (although that can also be done easily when we have a final version of training sets to be associated with publications, etc.). 

# code to get me started on this came from here: https://www.r-bloggers.com/os-secrets-exposed-extracting-extended-file-attributes-and-exploring-hidden-download-urls-with-the-xattrs-package/


# IDENTIFY WHO IS DOING THE FINAL APPROVAL (this is different from the person who did the original tagging, which is set using a tag:
taxonomist<-"Peter"

# IDENTIFY WORKING DIRECTORY:
wd<-"/Users/peterisles/Dropbox/Aquascope_classifier_test/5p0xMAG_1/images/00000" # assign working directory

# SET THE TARGET FILE FOR THE CLASSIFIED IMAGES (SHOULD LEAVE AS "classified_images.csv" IN GENERAL FOR COMPATABILITY WITH OTHER SCRIPTS)
targetFile<-"classified_images.csv"

# SET THE TARGET FILE FOR THE CLASSIFIED IMAGES (SHOULD LEAVE AS "classification_summary.csv" IN GENERAL FOR COMPATABILITY WITH OTHER SCRIPTS)
targetFileSummary<-"classification_summary.csv"

# install.packages("xattrs", repos = "https://cinc.rud.is")
library(xattrs)
library(reshape)
library(stringr)
library(lubridate)



# # 1. Sample code demonstrate how the basic functions work (can update with a path to a tagged image file on your machine):
# # list the attributes associated with an aquascope image. User tags will appear in the item "com.apple.metadata:_kMDItemUserTags". These are stored as a "bplist" file, which can be extracted with the read_bplist function
# list_xattrs("/Users/peterisles/Downloads/images_00000_SPC-EAWAG-0P5X-1566928859384181-110890690537-000499-219-1966-1568-804-1031.jpeg")
# # [1] "com.apple.lastuseddate#PS"                "com.apple.metadata:_kMDItemUserTags"      "com.apple.metadata:kMDItemDownloadedDate"
# # [4] "com.apple.metadata:kMDItemWhereFroms"     "com.apple.quarantine"                    
# 
# x<-read_bplist( # read bplist finction
#   get_xattr_raw( # import the raw bplist attribute into the r session
#     "/Users/peterisles/Downloads/images_00000_SPC-EAWAG-0P5X-1566928859384181-110890690537-000499-219-1966-1568-804-1031.jpeg", "com.apple.metadata:_kMDItemUserTags")
#   )
# 
# unlist(x) # unlist x to get a vector of tags that can be appended to a file


extractMacTagsForClassification<- function (wd, targetFile = "classified_images.csv", targetFileSummary = "classification_summary.csv", taxonomist = taxonomist) {
  # wd                      : working directory
  # targetFile              : File to write the image-by-image IDs and other tags csv fie
  # targetFileSummary       : File to write the csv with total counts by taxonomic ID
  # taxonomist              : Who is approving the final IDs (different from the individual doing the tagging: I envision this as a final quality assurance stamp)
  
  

  setwd(wd)

  fileTags<-data.frame(file = NA, timestamp = NA, taxonomicID=NA, identifiedBy = NA, location = NA) # Initialize a data frame to store results. if it is desired to enable additional classes of tags, we can initialize them here, by adding columns for "Partial_image", "Egg_counts", etc. I propose a tagging convention like this: taxonomic tag will be something like 1_Asterionella, 1_Microcystis, etc. and our taxonomist tag could be 2_Marta, 2_Peter, etc., location is 3_Greifensee, 3_Lab, etc., and our extra features could be added with additional prefixes. I ensure that tags are reordered alphabetically, so they should fall into the right columns in this script.
  
  myfiles<-list.files(path = ".", pattern = ".jpeg") # list files in the current directory with a jpeg extension
  myfiles<-paste(wd, myfiles, sep = "/") # add the directory to the filename so that Marco/whoever has the full file path
  head(myfiles)
  
  # remove the raw color files (which are not going to be used by the classifier and to avoid double counting)
  rawfiles<-grep("rawcolor", myfiles)
  if (length(rawfiles)>0) {
    myfiles<-myfiles[-rawfiles]
  }
  
  # Loop to append tags associated with each file:
  for (i in 1:length(myfiles)) {
    fileTags[i,1]<-myfiles[i]
    x<-read_bplist( # read bplist finction
      get_xattr_raw( # import the raw bplist attribute into the r session
        myfiles[i], "com.apple.metadata:_kMDItemUserTags")
    )
    x1<-unlist(x) # unlist x to get a vector of tags that can be appended to a file
    x1<-x1[order(x1)] # Explicitly enforce the tag order which we want to define as alphabetical, so that the 1_, 2_ naming category convention works.
    if (length(grep("1_", x1)) > 0) { # If there is a tag with taxonomic ID, put it in the third colum
      fileTags[i, 3] <-x1[grep("1_", x1)]
    }
    if (length(grep("2_", x1)) > 0) { # If there is a tag with person who tagged the image
      fileTags[i, 4] <-x1[grep("2_", x1)]
    }
    if (length(grep("3_", x1)) > 0) { # If there is a tag with location (Greifensee, Lab, etc.)
      fileTags[i, 5] <-x1[grep("3_", x1)]
    }
    
    definedTags<-grep(paste(c("1_", "2_", "3_"), collapse = "|"), x1) # identify which tags we have already inserted. Can add other defined groups as necessary (add additional if statement as above also)
    
    otherTags<-x1[-definedTags] # Remove tags that we have already inserted
    if (length(otherTags) > 0) {
      fileTags[i,c(1:length(otherTags)) + 5] <- otherTags # Add remaining tags in extra columns in alphabetical order. Data frame can dynamically update column number with however many tags are required
    }
  }
  
  fileTags$ApprovedBy<-taxonomist # append the name of the person who signed off on the identification/run this script (set at top of script)
  
  ### Append capture time (from unixtime embedded in file name) as a column to the data frame. This should make it easier for humans to look at.
  # first extract the capture time from the file name:
  
  x<-as.data.frame(str_split_fixed(fileTags$file, pattern = "-", n = 11))
  
  names(x)<-c("Directory", "Owner", "Mag", "unixtime", "cameratime", "frameNumber", "roiNumber", "roiLeft", "roiTop", "roiWidth", "roiHeight")
  
  x1<-as.numeric(as.character(x$unixtime))
  fileTags$timestamp<-as.POSIXct(x1/1000000, origin = "1970-01-01") - (3600 * 2) # This correction is needed to match the time zones to the tsv files. Need to make sure that this is correct during daylight savings and non-daylight savings, etc., or if we need a more explicit handling of time zones.
  
  # write csv with the tags applied to each image. This can be used to populate the training set, and to assess the assignment of individual images to classes. This can also be easily merged with the tsv file.
  write.csv(fileTags, file = targetFile, row.names=FALSE)
  
  # Create a summary file which lists the counts for each type of particle (assuming that tag1 contains the most important taxonomic information). A separate script runs through all of the folders and merges them into a single table.
  x<-melt(fileTags[,c(1,3)], id.vars=c("file"))
  names(x)[3]<-"taxa"
  x$value = paste(range(fileTags$timestamp)[1], range(fileTags$timestamp)[2], sep = " to ")
  summaryData<- # this needs to be edited, I don't have a classified folder now so can't debug this properly
    cast(
      x, 
      value ~ taxa, 
      fun.aggregate = "length"
    )
  names(summaryData)[1]<-"timerange"
  summaryData$Directory<-wd
  idVars<-c(which(names(summaryData) == "Directory"), which(names(summaryData) == "timerange"))
  summaryData<-summaryData[,c(idVars, c(1:length(summaryData))[-idVars])] # reorder the columns so that the ID columns show up in front
  
  # head(summaryData) # if there is a column entitled "NA", then some images in the directory have not been identified with taxonomy.
  
  write.csv(summaryData, file = targetFileSummary, row.names=FALSE)
  
  return(fileTags)
}


### Run the function (this can be put inside a loop of working directories to run through mulitple folders). The function returns the list of files with tags to the workspace, which can be saved in an object, as in x below.
x<-extractMacTagsForClassification(wd = wd, targetFile = targetFile, targetFileSummary = targetFileSummary, taxonomist = taxonomist)


### Just to demonstrate how to extract tags from the extra columns by first pasting together all of the extra rows, then doing a grep command to search through them. We separate the rows with "__" so that it will be easy to split them again if needed using str_split(). 
tags<-paste(x$V6, x$V7, x$V8, x$V9, sep = "__")

grep("partial", tags)
x[grep("partial", tags),]
