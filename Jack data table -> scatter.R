# Load Packages
library(ggplot2)

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

#Formats appearance
customtheme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)
setcolor <-
  scale_color_gradientn(colours = rainbow(20, start = .17, end = .67))
scale_scatter <-
  coord_cartesian(xlim = c(0, 1280), ylim = c(0, 1024))


#Defines ROIs, which are 345 x 345 pixels but we round to 346 x 346 for even values
ROI_L <- c(147, 493, 584, 930)
ROI_R <- c(787, 1133, 584, 930)
ROI_item <- c(467, 813, 94, 440)

#Creates top-left rectangle
left_rectangle <-   geom_rect(
  aes(
    xmin = ROI_L[1],
    xmax = ROI_L[2],
    ymin = ROI_L[3],
    ymax = ROI_L[4]
  ),
  color = "black",
  alpha = 0,
)

#Creates top-right rectangle
right_rectangle <-  geom_rect(
  aes(
    xmin = ROI_R[1],
    xmax = ROI_R[2],
    ymin = ROI_R[3],
    ymax = ROI_R[4]
  ),
  color = "black",
  alpha = 0
)

#Creates bottom rectangle
bottom_rectangle <- geom_rect(
  aes(
    xmin = ROI_item[1],
    xmax = ROI_item[2],
    ymin = ROI_item[3],
    ymax = ROI_item[4]
  ),
  color = "black",
  alpha = 0
)

#### Plotting ####

#Creates scatterplot
createsplot <- function(participant_num){
  
  #imports data table
  filepathdataset = paste("/Volumes/EyeTracker.52/Data/", participant_num, "/Jack Data Processing/VB_",participant_num,"_Data_Processed_Jack.csv", sep = "")
  subjectdataset <- read.table(filepathdataset, sep = ",", header = TRUE)
  
  #Sets axes and variable that will decide color
  scatterall = ggplot(subjectdataset, 
                      aes(AvgXValOnScreen, AvgYValOnScreen, color = Trial))
  
  #Combines all the pieces
  scatter =
    scatterall +
    geom_point(size = .001) +
    theme_bw() +
    customtheme +
    setcolor +
    scale_scatter +
    left_rectangle +
    right_rectangle +
    bottom_rectangle
  
  #Saves scatterplot with 4x5 ratio since original ratio is 1024 by 1280
  ggsave(paste("scatter", participant_num, ".png", sep = ""), width = 5, height = 4)
}

#### Call Function ####
allscatterplots <- lapply(allsubjectnumbers, createsplot)