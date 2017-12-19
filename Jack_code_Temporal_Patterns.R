# Load Packages
library(dplyr)
library(tidyr)

#Subject numbers of participants who were eligible for pt 2 and whose data was usable
allsubjectnumbers <- c(
  3001, 3004, 3005, 3006, 3007, 3008, 3010, 3011, 3012, 3013, 3014, 3015,
  3016, 3017, 3018, 3019, 3021, 3023, 3024, 3026, 3028, 3029, 3032, 3033,
  3034, 3038, 3039, 3042, 3049, 3050, 3053, 3054, 3058, 3059, 3060, 3063,
  3064, 3065, 3066, 3067, 3068, 3071, 3075, 3077, 3079, 3081, 3083, 3084,
  3086, 3088, 3090, 3091, 3093, 3094, 3098, 3100, 3102, 3104, 3105, 3106,
  3111, 3113, 3115, 3118, 3123, 3126, 3131, 3134, 3135, 3138, 3139
)

#Turns original data table into data table of trending ROI's
STA_Algorithm <- function(participant_num) {

  #Imports and reads processed ET data
  filepathdataset = paste("/Volumes/EyeTracker.52/Data/", participant_num, "/Jack Data Processing/VB_",participant_num,"_Data_Processed_Jack.csv", sep = "")
  subjectdataset <- read.table(filepathdataset, sep = ",", header = TRUE)
  
  #Adds column to imported data table indicating change in trial
  changeinTrial <- c(0, diff(subjectdataset$Trial))
  subjectdataset <- cbind(subjectdataset, changeinTrial)
  
  #Adds column to imported data table grouping consecutive fixations with same ROI and trial
  allNewFixations = c()
  newFixationCount = 1
  for (i in 1:nrow(subjectdataset))
    if (subjectdataset[i, "changeinROI"] != 0 | subjectdataset[i, "changeinTrial"] != 0) {
      newFixationCount <- newFixationCount + 1
      allNewFixations[i] <- newFixationCount
    }  else {
      allNewFixations[i] <- newFixationCount
    }
  subjectdataset <- cbind(subjectdataset,
                          allNewFixations)
  
  #Groups ROI's by this newly added column, ranks them against same ROI's in trial
  Step1 <- subjectdataset %>% group_by(allNewFixations, Trial, ROIlocation) %>%
    mutate(diff = max(syncedTime) - min(syncedTime)) %>% 
    group_by(allNewFixations, Trial, ROIlocation, diff) %>%
    summarise(NumFixations=length(unique(FixationIndex))) %>%
    group_by(ROIlocation, Trial) %>%
    mutate(rank=rank(-diff, ties.method = c("first")),
           merge = paste(ROIlocation, rank, sep = "-")) %>%
    arrange(Trial, allNewFixations)
  
  #Tallies num fixations, num trials it appears in, and duration of fixations for each rank of each ROI
  Step2 <- Step1 %>%
    group_by(merge, ROIlocation) %>%
    summarise(TotalFixations =  n(),
              TotalTrials = sum(diff),
              FixationDuration = sum(NumFixations))
  
  #Calculates threshold for non-trending ROI's to be trending, returns all trending ROI's
  numoftrials <- length(unique(Step1$Trial))
  Trendingminduration <- min(Step2[which(Step2$TotalFixations >= .85*numoftrials), "TotalTrials"])
  Trendingminfixations <- min(Step2[which(Step2$TotalFixations >= .85*numoftrials), "FixationDuration"])
  Step3 <- Step2 %>%
    filter(TotalFixations >= .85*numoftrials | (TotalTrials >= Trendingminduration &
                                                FixationDuration >= Trendingminfixations))
  
  #Runs fixations that are trending through Eraslan et al.'s algorithm
  #The algorithm weights fixation by its order in trending scanpath of trial
  #and returns a value that is between .1 and 1, which also depends on total
  #num of ROI's in that trending scanpath
  Step4 <- Step1 %>% filter(merge %in% Step3$merge == TRUE) %>%
    group_by(Trial) %>%
    mutate(PriorityValue = 0.9/(n() - 1),
           PriorityValue = 1-((rank(allNewFixations) - 1)*PriorityValue),
           PriorityValue = replace(PriorityValue, PriorityValue == "NaN", 1.000) # This choice might be worth discussion—if only one item left from trial, it gets value of 1 rather than say .1 (min of scanpath) or .55 (avg of .1 and 1)
    )
  
  # Returns trending ROI's in descending order by score from Eraslan et al.'s algorithm
  Step5 <- Step4 %>%
    group_by(merge, ROIlocation) %>%
    summarise(TrendingValues = sum(PriorityValue)) %>%
    arrange(desc(TrendingValues)) %>%
    mutate(ROIlocation = replace(ROIlocation, ROIlocation == 1, "B"),
           ROIlocation = replace(ROIlocation, ROIlocation == 6, "P"),
           ROIlocation = replace(ROIlocation, ROIlocation == 9, "I"),
           ROIlocation = replace(ROIlocation, ROIlocation == 13, "O")) %>% 
    as.data.frame()
  
  #Creates and returns string of non-repetetive trending ROI's, returns it and ptct num
  Trendingelements = rle(Step5$ROIlocation)$values
  TrendingString <-paste0(Trendingelements, collapse = "")
  return (c(participant_num, TrendingString))
}

# calls function that turns original datatable into datatable of trending ROI's
STA_output <- data.frame(mapply(STA_Algorithm, allsubjectnumbers))

#formats new datatable
STA_output <- t(STA_output)
STA_output <- as.data.frame(STA_output)
colnames(STA_output) <- c("PtcpNums", "Scanpath")
rownames(STA_output) <- STA_output[,1]
STA_output <- select(STA_output, -PtcpNums)



# once I'm done, use this to make the code look cleaner: library(formatR)
# tidy_source("11-10 after connection barv8.R")
# see more here https://www.youtube.com/watch?v=mRTIj3jCIZA

#alternatively- Rstudio can now format code to look neat. Select the
#lines of interest and then navigate to Code >> Reformat code or use the keyboard shortcut ctrl + shift + A.

# https://yihui.name/formatr/


# checklist- http://adv-r.had.co.nz/Style.html
# also variables that have multiple words should have some kind of capitalization

#shorten variable names in Jack_code_Raw_Data->Organized.R, then see if lines look better/worse and change accordingly