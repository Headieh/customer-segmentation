# change this based on users settings
setwd("~/Documents/code/customer-segmentation")

#INITIALIZE
set.seed(999)

listofpackages <- c("rattle","gridExtra",
                    "rpart.plot","tree",
                    "Hmisc", "dplyr",
                    "RColorBrewer",
                    "tidyverse", "cluster",
                    "factoextra","rpart",
                    'caret')
new.packages <- listofpackages[!(listofpackages %in%
                                   installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(listofpackages, require, character.only = TRUE)

#EXPLORE DATA
pc = proc.time()
source('analytics/explore.r')
proc.time() - pc

pc = proc.time()
source('analytics/ttest.r')
proc.time() - pc

pc = proc.time()
source('analytics/Kmeans.r')
proc.time() - pc

pc = proc.time()
source('analytics/dtree.r')
proc.time() - pc