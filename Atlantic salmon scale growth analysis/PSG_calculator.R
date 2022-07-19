#-Clears all memory in R console------------------------------------------------
rm(list=ls())

in_file <- "Boyne_data.csv"    ## input file name
out_file <- "output_psg_calculator_WG.csv"   ## output file name
region <- "E"       ### enter "E" for European and "A" for American. If "European" script will constrain search for 
                    ### PSG between circuli 15 - 40, if "American" it will constrain search between 16-38

psg <- read.csv(in_file) ## read in data for analysis
n <- ncol(psg)  ### enters number of variables in dataset
m <- nrow(psg) ### enters number of Fish in dataset

library(zoo)
library(reshape2)
library(plyr)
library(dplyr)

col <- colnames(psg[,7:n])
psg_wide <- melt(psg,id.vars = "Index_No", measure.vars = c(col), 
                 variable.name = "Circuli_no", value.name = "measurement")
psg_wide <- psg_wide[order(psg_wide$Index_No),]
psg_wide$Index_No <- as.factor(psg_wide$Index_No)

rm <- ddply(
  psg_wide, "Index_No",
  transform,
  rmean = rollapply(measurement, 5,mean, align="center", fill = NA)
) ## need to fix this

max <- tapply(rm$rmean, rm$Index_No, max, na.rm = TRUE) ### calculated in the Friedland spreadsheet but doesn't 
min <- tapply(rm$rmean, rm$Index_No, min, na.rm = TRUE) ### seem to be used for anything

#rm$Circuli_no2 <- substring(rm$Circuli_no,2) ## fix this for times when there is no X in column name
rm$Circuli_no2 <- rm$Circuli_no
rm$Circuli_no2 <- as.numeric(rm$Circuli_no2)
end <- ifelse(region == "E", 40,37)
start <- ifelse(region == "E", 15,17)
rm2 <- subset(rm, rm$Circuli_no2 >= start & rm$Circuli_no2 <= end)
min2 <- tapply(rm2$rmean, rm2$Index_No, min, na.rm = TRUE)
x <- seq(from = 1, to = m, by = 1)
min2 <- cbind(x, min2)
colnames(min2) <- c("Index_No","min")

rm <- merge(rm, min2, by = "Index_No")
rm <- rm[order(rm$Index_No, rm$Circuli_no2),]
rm$CN <- ifelse(rm$rmean - rm$min == 0, rm$Circuli_no2, "")
rm$CN <- as.numeric(rm$CN)
rm4 <- subset(rm, rm$Circuli_no2 >= start & rm$Circuli_no2 <= end) ## seem to be alot of duplicates in measurements need to make sure it picks the one in the area we are looking for
Circ_No <- tapply(rm4$CN, rm4$Index_No, max, na.rm = TRUE)
d <- cbind(x,Circ_No)
colnames(d) <- c("Index_No","Circ_No")
rm <- merge(rm,d, by = "Index_No")
rm3 <- subset(rm, rm$Circuli_no2 <= rm$Circ_No)
psg_1 <- tapply(rm3$measurement, rm3$Index_No, sum, na.rm = TRUE)

### Create new dataframe with the data required
output <- psg[c(1:6)]
output <- cbind(output,Circ_No)
output <- cbind(output,psg_1)
output$avgcs <- output$psg_1/output$Circ_No
colnames(output) <- c("Fish_ID","Index_No","Smolt_Year","Sea_Age","Return_Year","Freshwater_Growth_mm","PSG_Circ_No", "Post_Smolt_Growth_mm","Ave_growth_Circ_mm")

## working out Post-smolt growth by month
o <- ifelse(region == "E", 8,7)
rm$months <- o
rm$ave_month <- ifelse(((rm$Circ_No/rm$months)-round(rm$Circ_No/rm$months)) == 0.5,
                       ceiling(rm$Circ_No/rm$months), round(rm$Circ_No/rm$months))


rm$monthA <- ifelse(rm$Circuli_no2 <= rm$ave_month, 1,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + rm$ave_month) & rm$Circuli_no2 > rm$ave_month, 2,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (2*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 3,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (3*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 4,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (4*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 5,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (5*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 6,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (6*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 7,"")))))))

rm$monthE <- ifelse(rm$Circuli_no2 <= rm$ave_month, 1,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + rm$ave_month) & rm$Circuli_no2 > rm$ave_month, 2,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (2*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 3,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (3*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 4,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (4*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 5,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (5*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 6,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (6*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 7,
             ifelse(rm$Circuli_no2 <= (rm$ave_month + (7*rm$ave_month)) & rm$Circuli_no2 > rm$ave_month, 8,""))))))))


rm$month <- ifelse(region == "E", "E", "A")
rm$month <- ifelse(rm$month == "E", rm$monthE, rm$monthA)

groups <- group_by(rm,Index_No,month)
x <- summarise_all(groups,funs(mean), na.rm = TRUE)
x <- x[!is.na(x$measurement),]
x <- dcast(x, Index_No ~ month, value.var = "measurement")
x$Var.2 <- NULL
ifelse(region == "E", colnames(x) <-  c("Index_No","May", "June", "July", "August", "September", "October", "November", "December"),
       colnames(x) <- c("Index_No","June", "July", "August", "September", "October", "November", "December"))

### Merging all results together
output <- merge(output, x, by = "Index_No")
write.csv(output, file = out_file, na = "", row.names = FALSE)

