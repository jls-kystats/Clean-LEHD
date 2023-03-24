# CleanLEHD 
# This script prompts the user for the location of LEHD wage record files
# (provided by UI) and prepares them for use in QUEST. The cleaned files will  
# be placed in the source folder and will have "_Cleaned" appended to their
# file names. They will replace any file with the same name, and the source 
# files will not be changed. Status msgs are displayed during the "cleaning". 
#
# Written by Larry Sturm, KYStats, March 2023
#
library(tidyverse)
library(readr)
library(fs)
library(tools)

# Custom func to write msg to user and immediately exit the script
exit <- function(msg) {
  if (!is.na(msg)) print(msg)
  invokeRestart("abort")
}

# Get a char vector of file paths to be cleaned (prompt user for files)
messy_file_list <- choose.files(caption = "Select file(s) to be cleaned",
                                multi = TRUE)
numFIles <- length(messy_file_list)
if (numFIles == 0) {
  exit("No files were chosen - exiting script.")
}

iter <- 0
for (fullPath in messy_file_list) {
  #
  # Parse this file path into components
  #
  folder <- paste(path_dir(fullPath),"/",sep="")
  fName <- path_file(fullPath)
  baseName <- file_path_sans_ext(fName)
  fExt <- file_ext(fName)
  newPath <- paste(folder,baseName,"_Cleaned.",fExt,sep="")
 
  cat("Reading file",iter <- iter + 1, "of",numFIles,":",fName,"\n")
  #
  # Read data from the next non-delimited user-specified file
  #
  lehd_file <- read_fwf(fullPath,
                        fwf_widths(c(9,36,2,10,35),
                                   c("SSN","Name","FIPS","UIN","Fill")),
                        col_types = "ccccc",
                        trim_ws = FALSE
                        # n_max = 100  #For testing only
  )
  
  cat(format(nrow(lehd_file), nsmall=0, big.mark=","),
      "Observations were found. \nBegining cleaning of",fName,
      "- This may take several minutes.\n")
  #
  # Clean this file
  #
  lehd_file %>% 
    #
    #  Remove any lines w/o 'KY' in FIPS col (e.g. lines w/NULL values)
    #
    subset(FIPS=='KY') %>% 
    #
    # Replace values in FIPS col with '21'
    #
    mutate(FIPS='21') %>%
    #
    # For each UIN that ends with an alpha character, pre-pend with a zero and 
    # remove the alpha character
    #
    mutate(UIN=str_replace(UIN,"([0-9]{9})[A-Z]","0\\1")) %>% 
    #
    # Look for the unprintable 'DC2' chars in the Name col. 
    # If found repl w/"'"
    #
    mutate(Name=str_replace_all(Name,"\x12","'")) %>%
    #
    # Look for the unprintable 'SYN' char in the Name col. If found repl w/'-'
    #
    mutate(Name=str_replace_all(Name,"\x16","-")) %>% 
    #
    # Write results to new non-delimited file in same folder as source, but
    # with "_Cleaned" appended to the file name
    #
    write_delim(newPath, 
                col_names = FALSE, 
                delim = "",
                quote = "none",
                escape = "none",
                eol="\r\n")
  
    cat("File successfully cleaned and saved as",newPath,"\n")
} # End of for loop over messy_file_list