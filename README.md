# aquascopeTaggingWorkflow
This repository contains documents and R scripts detailing the workflow for tagging aquascope images, generating summary tables, and preparing training sets for classification

The script "ExtractMacTagsForClassification.R" is the core of this, and appends the mac user tags containing taxonomic IDs to a csv containing the file names. It also writes a summary file with counts for each taxa in the folder.
The script "Merge_classifications_tsvs.R" merges the table created by the above script with the features.tsv file produced by SPC_convert containing various metrics of the aquascope images (size, color, shape, etc.)
The script "Concatenate_summaries.R" merges the summary files produced by the first script, creating a data frame which can be used to plot timeseries of manual phytotplankton IDs.

Detailed instructions for the complete workflow, inncluding file handling in Finder on Mac and tagging conventions, are given in the "ExtractMacTagsForClassification.R" file
