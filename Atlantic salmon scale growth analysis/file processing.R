library(tools)
library(tidyr)
library(zoo)
#this is the directory where the individual text files are stored
p_in = "C:/Users/lvaughan/Desktop/Louise- April 2020/GrowthAis" 
#this is the directory where the master files are stored (any time you run this script you will add to these files)*
p_out = "C:/Users/lvaughan/Desktop/Louise- April 2020/output" 
#this is the directory containing the phenotype files*
p_pheno = "C:/Users/lvaughan/Desktop/Louise- April 2020" 

readerID<-paste("AD")#put in whatever name or code you want to appear for your reader.*
region <- paste("E")#enter "E" for European and "A" for American. If "European" script will constrain search for 
### PSG between circuli 15 - 40, if "American" it will constrain search between 16-38

fileNames <- list.files(path=p_in, pattern=".txt", full.names = TRUE)#make a list of all the text files in the text files directory
#these will be processed in the loop

out<-NULL#an empty object that will be populated with the data from each text file

for (fileName in fileNames) {
  
  # read data: need to include comment.char= "" because otherwise the # in the 4th column and everything after is interpreted as a comment 
  #check.names=false deals with spaces in headers
  txt <- read.table(fileName,
                    sep="\t", 
                    header = TRUE, row.names = NULL, comment.char = "", check.names = FALSE)
  #some of the characters in the column names of the txt file cause problems later - change (removed some caps too cause I don't like them!)
  colnames(txt) <- c("ID","year", "age", "LH", "circN.LH", "space.mm")
  txt$reader<-paste(readerID)#paste in the reader ID
  txt$circN<-1:nrow(txt)#add a column with a unique number for each circulus measurement
  txt$lifestage<-ifelse(grepl("F", txt$LH), "FW", "M")#make a new column that categorizes each measurement as FW or M
  txt$age<-as.character(txt$age) #split the age column into FW age and Marine age, could remove this if the count is not reliable
  txt <-separate(txt, age, into=c("FW.age", "M.age"), remove=FALSE)
  txt$tot.rad<-sum(txt$space.mm)#sum all the circuli widths to get the total scale radius
  txt$FW.rad<-sum(txt[txt$lifestage =="FW",]$space.mm)#find the total width of the FW circuli
  txt$FW.count<-sum(txt$lifestage == 'FW')#find the total number of FW circuli
  
  #we will also count and sum each year of the FW phase, up to a max of 5 FW years. Could modify this if necessary to suit the population
  #if there are no measurements for that year it just returns a zero
  txt$FW1.count<-sum(txt$LH == 'F1')
  txt$FW1.rad<-sum(txt[txt$LH =="F1",]$space.mm)
  txt$FW2.count<-sum(txt$LH == 'F2')
  txt$FW2.rad<-sum(txt[txt$LH =="F2",]$space.mm)
  txt$FW3.count<-sum(txt$LH == 'F3')
  txt$FW3.rad<-sum(txt[txt$LH =="F3",]$space.mm)
  txt$FW4.count<-sum(txt$LH == 'F4')
  txt$FW4.rad<-sum(txt[txt$LH =="F4",]$space.mm)
  txt$FW5.count<-sum(txt$LH == 'F5')
  txt$FW5.rad<-sum(txt[txt$LH =="F5",]$space.mm)

  #calculate PSG and PSG circuli number
  sw<-subset(txt, lifestage=="M")#subset out the marine part of the scale
  sw$circN.M<-1:nrow(sw)#add a column with a unique number for each marine circulus measurement
  sw$rm <-rollapply(sw$space.mm, 5,mean, align="center", fill = NA)# calculate the 5 point moving average of the circuli measurements
  start <- ifelse(region == "E", 15,17)#define which circulus to start search for winter minimum
  end <- ifelse(region == "E", 40,37)#define which circulus to end search for winter minimum
  sw2 <- subset(sw, sw$circN.M >= start & sw$circN.M <= end)#reduce the sw data to portion containing 1st winter min
  min<-min(sw2$rm, na.rm = TRUE)#find the width of the first winter minimum
  sw3 <- subset(sw2, sw2$rm == min)#subset out the row containing the winter minimum
  wmin<-min(sw3$circN.M)#identify the circulus number of the winter minimum (use min here so that it finds the first circulus that is equal to the min, in case there are two identical measurements)
  sw4<-subset(sw, circN.M<=wmin)#subset the sw data to isolate the PSG portion (SW entry to 1st winter minimum)
  PSG<-sum(sw4$space.mm)#calculate the total width of the circuli between SW entry and 1st wmin
  txt$PSG<-PSG#now enter this onto the spreadsheet
  txt$PSG.N<-wmin#enter the number of circuli in the PSG region. This could be used later to check for issues caused by summer checks
  out<-rbind(out, txt)
}

#now read in the phenotype file or files
dat<-NULL
fileNames <- list.files(path=p_pheno, pattern=".csv", full.names = TRUE)
for (fileName in fileNames) {
  ph <- read.csv(fileName, header = TRUE, na.strings=c("", "NA"))
  dat<-rbind(dat, ph)
}


## check files for missing files and discrepancies between growth and phenotype data
A = levels(as.factor(as.character(out$ID)))
B = levels(as.factor(as.character(dat$ID)))
A %in% B

intersect(A,B) # all values that are the same between the 2 datasets
setdiff(A,B) # all values that are in out(growth data) but not in dat(phenotype data)
setdiff(B,A) # all values that are in dat(phenotype data) but not in out(growth data)

#and merge with the combined txt files (using ID and reader columns to match)
out<-merge(out, dat, by=c("ID", "reader"), all = TRUE)

#rbind the complete data file to the master and save
master<-read.csv(paste0(p_out, "/master.csv"))#read in the existing master file
master<-rbind(master, out)
master<-unique(master)#removes any duplicate entries
mypath <- file.path(p_out, paste("master.csv", sep = ""))
  write.csv(master, file=mypath, row.names = FALSE)
  
  
#make another dataframe that contains just one row for each fish. This contains just the totals and counts for each fish, which is usually what we will analyse. 
out1<-subset(out, circN==1)
out1$space.mm<-NULL  #remove the circulus measurement - don't need this
out1$circN<-NULL#and the circ number - don't need this


### check again for differences
A = levels(as.factor(as.character(out1$ID)))
B = levels(as.factor(as.character(dat$ID)))
A %in% B

intersect(A,B) # all values that are the same between the 2 datasets
setdiff(A,B) 
setdiff(B,A)



#rbind that to the aggregated master and save
master.sum<-read.csv(paste0(p_out, "/master summary.csv"))#read in the existing master file
master.sum<-rbind(master.sum, out1)
master.sum<-unique(master.sum)#removes any duplicate entries
mypath <- file.path(p_out, paste("master summary.csv", sep = ""))
write.csv(master.sum, file=mypath, row.names = FALSE)
  

### merge in the drupal export

master<-read.csv(paste0(p_out, "/master.csv"))#read in the existing master file
master.sum<-read.csv(paste0(p_out, "/master summary.csv"))#read in the existing master file

require(xlsx)
drup <- read.xlsx(paste0(p_pheno, "/DrupalExport_ForAssembly.xlsx"), sheetName = "ALL")

## check for any missing data
A = levels(as.factor(as.character(master.sum$ID)))
B = levels(as.factor(as.character(drup$id)))
A %in% B

setdiff(A,B) # data that's in master sheet but not in drupal, enter into drupal where possible
setdiff(B,A) # data that's in drupal but not in Master sheet

## merge masters and drupal
drup$ID <- drup$id
drup$id <- NULL
master.sum2 <- merge(master.sum, drup, by = "ID", all = TRUE)
master.sum2 <- master.sum2[is.na(master.sum2$reader),] # subset out data from Drupal with no growth data
mypath <- file.path(p_out, paste("master.sum_drup.csv", sep = ""))
write.csv(master.sum2, file=mypath, row.names = FALSE)


master2 <- merge(master, drup, by = "ID", all = TRUE)
master2 <- master2[is.na(master2$reader),]
mypath <- file.path(p_out, paste("master.drup.csv", sep = ""))
write.csv(master2, file=mypath, row.names = FALSE)
