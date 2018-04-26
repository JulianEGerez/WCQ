# Run analyses: This file runs the necessary code to produce output for project:
# "How effectively does World Cup qualification performance predict success in the World Cup finals?"

# Created by Julian Gerez

# Load required libraries

# install.packages(c('rvest', 'readr', 'plyr', 'dplyr', 'stringr', 'tidyr', 'ggplot2', 'stargazer'))

library('rvest')
library('readr')
library('plyr')
library('dplyr')
library('stringr')
library('tidyr')
library('ggplot2')
library('stargazer')

# Set directory

directory <- "###/WCQ/" # Insert directory name here
setwd(paste0(directory))

# Decide whether to create files with intermediate computation results
saveIntermediate <- FALSE #TRUE or FALSE

source(paste0(directory, "Code/", "1. Read in WCF data.R"))
source(paste0(directory, "Code/", "2. Read in WCQ data.R"))
source(paste0(directory, "Code/", "3. Clean WCF data.R"))
source(paste0(directory, "Code/", "4. Clean WCQ data.R"))
source(paste0(directory, "Code/", "5. Create WC Match Data.R"))
source(paste0(directory, "Code/", "6. Create WC Results Data.R"))
# Script 7 matches the analyses posted on my website, but it is simply meant to serve as a starting
# point for your own exploration. Explore the script individually and change the models or figures
# to your own volition.
source(paste0(directory, "Code/", "7. Analyze WCQ and WCF data.R"))
# Script 8 is explicity created for exploration and won't even work if sourced
# as variables need to be replaced and defined in the models.