rm(list = ls()) ## clears the memory
library(stringr)

#this is the directory where the individual text files are stored
p_in = "C:/Users/lvaughan/Desktop/ElizabethsRead/1958" 


fileNames <- list.files(path=p_in, pattern=".txt", full.names = TRUE)#make a list of all the text files in the text files directory
#these will be processed in the loop
check <- NULL

## First to try read in all files. Any errors should show up in the console. 
for (fileName in fileNames) {
  
  skip_to_next <- FALSE
  txt <- tryCatch(read.table(fileName,
                             sep="\t", 
                             header = TRUE, row.names = NULL, comment.char = "", check.names = FALSE),
                  error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }  
}

### check the files that show up as an error. Fix errors or remove files as appropriate then rerun script below
### if you don't get errors then all the files have loaded ok. Move onto the next step below

check <- NULL
for (fileName in fileNames) {
  
  # read data: need to include comment.char= "" because otherwise the # in the 4th column and everything after is interpreted as a comment 
  #check.names=false deals with spaces in headers
  txt <- read.table(fileName,
                    sep="\t", 
                    header = TRUE, row.names = NULL, comment.char = "", check.names = FALSE)
  colnames(txt) <- c("ID","year", "age", "LH", "circN.LH", "space.mm")
  
  # check all text files only have 1 ID in them
  A <- levels(as.factor(as.character(txt$ID)))
  N_ID <- length(A) # gives the number of ID's in the dataframe (2 or higher needs to be checked)
  file <- gsub("^.*/", "", fileName)
  file <- gsub(file, pattern=".txt$", replacement="")
  ID_correct <- file %in% A # checks if dataframe contains the correct filename
  D <- data.frame(file, N_ID, ID_correct)
  check <- rbind(check,D)
}

### if you have removed any files that caused errors in part 1 of this script it should run without errors
### if you get an error running above loop check which file caused the error
### this will be the file name in the vector listed in "file". Check this file and correct error
### you may also need to check the next file in the list as it may also cause the error
### eg file name is 1007 this seems fine when checked, error is actually caused by file 1008
### which is next in the list to be analysed
  
subset(check, check$N_ID >= 2) # files which have 2 or more ID's in them
subset(check, check$ID_correct == "FALSE") # files where the ID in the file is different from the filename

## Once files are checked you can run the data through the file processing script




