# Load Packages
library(dplyr)

#### Set up ####

#Subject numbers of participants who were eligible for pt 2 and whose data was usable
allsubjectnumbers <- c(
                       3001, 3004, 3005, 3006, 3007, 3008, 3010, 3011, 3012, 3013, 3014, 3015,
                       3016, 3017, 3018, 3019, 3021, 3023, 3024, 3026, 3028, 3029, 3032, 3033,
                       3034, 3038, 3039, 3042, 3049, 3050, 3053, 3054, 3058, 3059, 3060, 3063,
                       3064, 3065, 3066, 3067, 3068, 3071, 3075, 3077, 3079, 3081, 3083, 3084,
                       3086, 3088, 3090, 3091, 3093, 3094, 3098, 3100, 3102, 3104, 3105, 3106,
                       3111, 3113, 3115, 3118, 3123, 3126, 3131, 3134, 3135, 3138, 3139
)

#### Missing Trials ####

#Prints out which trials from which participants have zero data points
for (participant_num in allsubjectnumbers){
  filepathdataset = paste("/Volumes/EyeTracker.52/Data/", participant_num, "/Jack Data Processing/VB_",participant_num,"_Data_Processed_Jack.csv", sep = "")
  subjectdataset <- read.table(filepathdataset, sep = ",", header = TRUE)
  number = 1:120
  emptytrials <- c(participant_num, number[!number%in%subjectdataset$Trial])
  if (length(emptytrials) > 1){
    print (emptytrials)
  }
}

#### Transition Function ####

#Converts ROI changes from numbers to characters
changeROIlist <-c(-12, -8, -7, -5, -4, -3, 3, 4, 5, 7, 8, 12)
changeROIlist2 <- c("B->O", "B->I", "P->O", "B->P", "I->O", "P->I", "I->P", "O->I", "P->B", "O->P", "I->B", "O->B")

#Calculates, for each of the transitions between ROI's, what percent it represents of all transitions between ROI's
transitionfunc <- function(participant_num){
  filepathdataset = paste("/Volumes/EyeTracker.52/Data/", participant_num, "/Jack Data Processing/VB_",participant_num,"_Data_Processed_Jack.csv", sep = "")
  subjectdataset <- read.table(filepathdataset, sep = ",", header = TRUE)
  subjectdataset <- subjectdataset %>%
    select(changeinROI) %>%
    filter(changeinROI != 0) %>%
    add_row(changeinROI = changeROIlist) %>%
    group_by(changeinROI) %>%
    summarise (n = n() - 1) %>%
    mutate(freq = n/(sum(n)-length(changeROIlist))) %>%
    select(freq)
}
Transitions <- mapply(transitionfunc, allsubjectnumbers)
Transitions <- data.frame(Transitions)

#Adds column and row labels and a column of means for each transition
colnames(Transitions) <- allsubjectnumbers
rownames(Transitions) <- changeROIlist
Transitions <- Transitions %>%
  mutate(Transitionmean = rowMeans(Transitions)) 
rownames(Transitions) <- changeROIlist2
Transitions <- Transitions[,c(length(Transitions), 1:(length(Transitions)-1))]

#### ROI Function ####  

#Calculates proportion of all fixations taking place in each ROI
ROIlocationfunc <- function(participant_num){
  filepathdataset = paste("/Volumes/EyeTracker.52/Data/", participant_num, "/Jack Data Processing/VB_",participant_num,"_Data_Processed_Jack.csv", sep = "")
  subjectdataset <- read.table(filepathdataset, sep = ",", header = TRUE)
  subjectdataset <- subjectdataset %>%
    group_by(ROIlocation) %>%
    summarise (n = n()) %>%
    mutate(freq = n / sum(n)) %>%
    select(freq)
  }
ROIs <- mapply(ROIlocationfunc, allsubjectnumbers)
ROIs <- data.frame(ROIs)

#Adds column and row labels and a column of means for each ROI
colnames(ROIs) <- allsubjectnumbers
ROIs <- ROIs %>%
  mutate(ROImean = rowMeans(ROIs))
rownames(ROIs) <- c("B", "P", "I", "O")   
ROIs <- ROIs[,c(length(ROIs), 1:(length(ROIs)-1))]
 

#### Best Validity Function ####
  
#Calculates percent of all fixations with ValidityLeft and ValidityRight both equal to 0
ROIdefinedfunc <- function(participant_num){
  filepathdataset = paste("/Volumes/EyeTracker.52/Data/", participant_num, "/Jack Data Processing/VB_",participant_num,"_Data_Processed_Jack.csv", sep = "")
  subjectdataset <- read.table(filepathdataset, sep = ",", header = TRUE)
  subjectdataset <- subjectdataset %>%
    select(Trial, ValidityLeft, ValidityRight) %>%
    add_row(Trial = 1:120, ValidityLeft = 0, ValidityRight = 0) %>%
    group_by(Trial) %>%
    summarise (num = sum(ValidityLeft == 0 & ValidityRight ==0) - 1, totalnumber = n() -1) %>%
    mutate(freq = num / totalnumber) %>%
    ungroup() %>%
    select(freq)
}
ROIdefined <- sapply(allsubjectnumbers,function(x) ROIdefinedfunc(x))
ROIdefined <- data.frame(ROIdefined)

#Adds column and row labels and a column of means for each participant
ROIdefined <- rbind(ROIdefined, colMeans(ROIdefined, na.rm = TRUE))
colnames(ROIdefined) <- allsubjectnumbers
rownames(ROIdefined)[121] <- "Mean"
ROIdefined <- ROIdefined[c(nrow(ROIdefined), 1:(nrow(ROIdefined)-1)),]

#### Purchasing at Diff Budgets Function ####

#Calculates number of items bought at each budget
budgetsfunc <- function(participant_num){
  filepathT = paste("/Volumes/EyeTracker.52/Data/", participant_num, "/VB",participant_num,".csv", sep = "")
  subjectTrials <- read.table(filepathT, sep = ",", header = TRUE)
  subjectTrials <- subjectTrials %>%
    filter(response == 1) %>%
    add_row(budget = c(10, 20, 40)) %>%
    group_by(budget) %>%
    summarise (TimesPurchased = n() - 1) %>%
    select(TimesPurchased)
}
BudgetPurchases <- mapply(budgetsfunc, allsubjectnumbers)
BudgetPurchases <- data.frame(BudgetPurchases)

#Adds column and row labels and a column of means for each budget
colnames(BudgetPurchases) <- allsubjectnumbers
BudgetPurchases <- BudgetPurchases %>%
  mutate(MeanPurchases = rowMeans(BudgetPurchases)) 
rownames(BudgetPurchases) <- c("$10", "$20", "$40")
BudgetPurchases <- BudgetPurchases[,c(length(BudgetPurchases), 1:(length(BudgetPurchases)-1))]
