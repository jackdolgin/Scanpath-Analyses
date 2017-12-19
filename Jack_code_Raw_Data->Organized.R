# Load Packages
library(dplyr)

#Subject numbers of participants who were eligible for pt 2 and whose data was usable
allsubjectnumbers <- c(
  3001, 3004, 3005, 3006, 3007, 3008, 3010, 3011, 3012, 3013, 3014, 3015,
  3016, 3017, 3018, 3019, 3021, 3023, 3024, 3026, 3028, 3029, 3032, 3033,
  3034, 3038, 3039, 3042, 3049, 3050, 3053, 3054, 3058, 3059, 3060, 3063,
  3064, 3065, 3066, 3067, 3068, 3071, 3075, 3077, 3079, 3081, 3083, 3084,
  3086, 3088, 3090, 3091, 3093, 3094, 3098, 3100, 3102, 3104, 3105, 3106,
  3111, 3113, 3115, 3118, 3123, 3126, 3131, 3134, 3135, 3138, 3139
)

#Turns imported ET and behavioral data into organized datatable 
organize <- function(participant){
  
  #Imports ET and behavioral data as subjectET and subjectT, respectively
  location1 <- paste("/Volumes/EyeTracker.52/Data/", participant, "/NEFE_VB_Data_Export.tsv", sep = "")
  location2 <- paste("/Volumes/EyeTracker.52/Data/", participant, "/VB_NEFE_Data_Export_", participant, "_combined.tsv", sep = "")
  location3 <- paste("/Volumes/EyeTracker.52/Data/", participant, "/NEFE_VB_Data_Export_", participant, "_combined.tsv", sep = "")
  location4 <- paste("/Volumes/EyeTracker.52/Data/", participant, "/VB_NEFE_Data_Export.tsv", sep = "")
  location5 <- paste("/Volumes/EyeTracker.52/Data/", participant, "/VB_NEFE_Data_Export_", participant, ".tsv", sep = "")
  if (file.exists(location1) == TRUE) {
    filepathET <- location1
  }
  else if (file.exists(location2) == TRUE) {
    filepathET <- location2
  }
  else if (file.exists(location3) == TRUE) {
    filepathET <- location3
  }
  else if (file.exists(location4) == TRUE) {
    filepathET <- location4
  }
  else if (file.exists(location5) == TRUE) {
    filepathET <- location5
  }
  filepathT = paste("/Volumes/EyeTracker.52/Data/", participant, "/VB",participant,".csv", sep = "")
  subjectET <- read.table(filepathET, sep = "\t", header = TRUE)
  subjectT <- read.table(filepathT, sep = ",", header = TRUE)

  #Defines ROIs, which are 345 x 345 pixels but we round to 346 x 346 for even values
  ROI_L <- c(147, 493, 584, 930)
  ROI_R <- c(787, 1133, 584, 930)
  ROI_item <- c(467, 813, 94, 440)

  #Determines when each run starts, which is indicated in ET data by a "Q"
  qlocations <- which(subjectET[,"KeyPressEvent"] == "Q")
  firstq <- min(qlocations)
  lastq <- max(qlocations)
  FirstStartTime <- subjectET[firstq, "RecordingTimestamp"]
  LastStartTime <- subjectET[lastq, "RecordingTimestamp"]

  #### First Run ####

  #Syncs timing of ET data with behavioral data—including converting ET time to seconds)—
  #corrects flipped Y values, and excludes most variables from raw data
  subjectET <- subjectET %>%  
    mutate(syncedTime = 0.001 * (RecordingTimestamp - FirstStartTime)) %>%
    mutate(AvgYValOnScreen = 1024 - GazePointY..ADCSpx.) %>%
    rename(AvgXValOnScreen = GazePointX..ADCSpx.) %>%
    select(FixationIndex, GazeEventType, GazeEventDuration, AvgXValOnScreen, AvgYValOnScreen,
           ValidityLeft, ValidityRight, syncedTime, RecordingTimestamp)
  
  #Condenses behavioral data to trials 1-60, returns time points occuring during
  #any of those trials
  ref_point1 <- subjectT - subjectT[1,"runStart"]
  ref_point1 <- ref_point1 %>%
    filter(row_number() <= 60) %>%
    select(choiceStart, feedbackStart)
  duringTrial <- function(syncedPoint){
    Trial <- apply(ref_point1, 1, function(row){
      (between(syncedPoint, row[1], row[2]))
    })
    return(which(Trial > 0))
  }
  subjectET$Trial <- sapply(subjectET$syncedTime, duringTrial)

  #Removes ET rows that 1) don't fall within timepoints created in previous function,
  #2) have NA for either X and/orY coordinate, and 3) are not fixations
  Run1 <- subjectET %>%
    filter(row_number() >= firstq & row_number() < lastq) %>%
    filter(Trial == TRUE) %>%
    filter(is.na(syncedTime) == FALSE) %>%
    filter(is.na(AvgYValOnScreen) == FALSE) %>%
    filter(GazeEventType == "Fixation") %>%
    select(-GazeEventType, -RecordingTimestamp)

  #Returns which ROI each fixation is in, accounts for side of screen w/ budget
  budgetlocation <- subjectT[1,"budgetPos"]
  if (budgetlocation == 0) {
    partofscreen <- function(x,y) {
      if (between(x, ROI_L[1], ROI_L[2]) & between(y, ROI_L[3], ROI_L[4])
      ) { #Left ROI == Budget
        return(1)     
      }
      else if (between(x, ROI_R[1], ROI_R[2]) & between(y, ROI_R[3], ROI_L[4])
      ) { #Right ROI == Price
        return(6)
      }
      else if (between(x, ROI_item[1], ROI_item[2]) & between(y, ROI_item[3], ROI_item[4])
      ) { #ROI Item
        return(9)     
      }
      else { #Located elsewhere on screen
        return(13)
      }
    }
  }
  else {
    partofscreen <- function(x,y) {
      if (between(x, ROI_R[1], ROI_R[2]) & between(y, ROI_R[3], ROI_L[4])
      ) { #Right ROI == Budget
        return(1)     
      }
      else if (between(x, ROI_L[1], ROI_L[2]) & between(y, ROI_L[3], ROI_L[4])
      ) { #Left ROI == Price
        return(6)     
      }
      else if (between(x, ROI_item[1], ROI_item[2]) & between(y, ROI_item[3], ROI_item[4])
      ) { #ROI Item
        return(9)
      }
      else { #Located elsewhere on screen
        return(13)
      }    
    }
  }

  #Sorts ET data in trial 1-60 into ROI's, then turns Trial column values into integers
  Run1$ROIlocation  <- mapply(partofscreen, Run1$AvgXValOnScreen, Run1$AvgYValOnScreen)
  Run1$Trial <- as.integer(matrix(Run1$Trial))

  #### Second Run  ####
  
  #Syncs timing of ET data with behavioral data—need to change reference point
  # to start of second run, represented by LastStartTime—and excludes most variables from raw data
  subjectET <- subjectET %>%
    mutate(syncedTime = 0.001 * (RecordingTimestamp - LastStartTime))

  #Condenses behavioral data to trials 61-120, returns time points occuring during
  #any of those T
  ref_point2 <- subjectT - subjectT[2,2]
  ref_point2 <- ref_point2 %>%
    filter(row_number() > 60) %>%     
    select(choiceStart, feedbackStart)
  duringTrial <- function(synced_time_point){
    Trial <- apply(ref_point2, 1, function(row){
      (between(synced_time_point, row[1], row[2]))
    })
    return(which(Trial > 0))
  }
  subjectET$Trial <- sapply(subjectET$syncedTime, duringTrial)

  #Removes ET rows that 1) don't fall within timepoints created in previous function,
  #2) have NA for either X and/orY coordinate, and 3) are not fixations
  Run2 <- subjectET %>%
    filter(row_number() >= lastq) %>%
    filter(Trial == TRUE) %>%
    filter(is.na(AvgYValOnScreen) == FALSE) %>%
    filter(GazeEventType == "Fixation") %>%
    select(-GazeEventType, -RecordingTimestamp)
  
  #Sorts ET data in T 1-60 into ROI's, then turns Trial column values into integers
  Run2$ROIlocation  <- mapply(partofscreen, Run2$AvgXValOnScreen, Run2$AvgYValOnScreen)
  Run2$Trial <- as.integer(matrix(Run2$Trial))

  #### Combine run 1 and 2 ####
  MergeRuns <- Run2 %>%
    mutate(Trial = Trial + 60)
  allT <- bind_rows(Run1, MergeRuns)

  #Adds a column of change in ROI's, equals 0 at start of each trial;
  #another column indicating bought (==1) or passed (==2) that trial
  changeinROI <- c(0, diff(allT$ROIlocation))
  allT <- cbind(allT, changeinROI)
  allT <- allT %>% mutate(Buy_or_Pass = subjectT[allT$Trial, "response"])

  #returns(allT) and save it as a .csv
  write.csv(allT,
            paste("VB_", participant, "_Data_Processed_Jack.csv", sep = ""))
}

#### Call Function ####
alldatasets <- lapply(allsubjectnumbers, organize)

