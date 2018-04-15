# This file cleans WCQ data using regular expressions

# Created by Julian Gerez

# Convert from factors to character

wcq_data[,2] <- as.character(wcq_data[,2])

# Read in match dates

# There are three types: DD.MM.YY, DD- M-YY, and DD-MM-YY

wcq_data$date1 <- str_match(wcq_data$combinedtext, "[0-9]{2}.[0-9]{1,2}.[0-9]{2}")
wcq_data$date2 <- str_match(wcq_data$combinedtext, "[0-9]{1,2}- [0-9]{1}-[0-9]{2}")
wcq_data$date3 <- str_match(wcq_data$combinedtext, "[0-9]{1,2}-[0-9]{2}-[0-9]{2}")

wcq_data$date1 <- gsub("(\\d{2})$", "19\\1", wcq_data$date1)
wcq_data$date1 <- as.Date(wcq_data$date1, format = "%d.%m.%Y")
wcq_data$date2 <- as.Date(wcq_data$date2, format = "%d- %m-%y")
wcq_data$date3 <- as.Date(wcq_data$date3, format = "%d-%m-%y")

# Combine into one column, then remove concatenated NAs

wcq_data$date <- str_replace(str_replace(str_replace(
  paste(wcq_data$date1, wcq_data$date2, wcq_data$date3), "NA", ""), "NA", ""), "NA", "")

# Remove extraneous columns

wcq_data <- wcq_data[ -c(3:5)]

# Create special circumstances variable (abandonded/anulled/awarded)

wcq_data$sc <- str_match(wcq_data$combinedtext, " [A,a]bd |abandoned|\\[annulled]|nullified|awd|awarded|n\\/p")
wcq_data$sc[is.na(wcq_data$sc)] <- 0
wcq_data$sc[wcq_data$sc!=0] <- 1

# Create et variable

wcq_data$et <- str_match(wcq_data$combinedtext, "\\[aet]|aet|\\[asdet]")
wcq_data$et[is.na(wcq_data$et)] <- 0
wcq_data$et[wcq_data$et!=0] <- 1

# Save original combined text, then remove all special circumstances and et and reorder

wcq_data$oldcombinedtext <- wcq_data$combinedtext
wcq_data$combinedtext <- gsub(" [A,a]bd |abandoned|\\[annulled]|nullified|awd|awarded|n\\/p", "", wcq_data$combinedtext)
wcq_data$combinedtext <- gsub("\\[aet]|aet|\\[asdet]", "", wcq_data$combinedtext)
wcq_data <- wcq_data[, c(1,6,2,3,4,5)]

# Read in match scores

# First we need to delete "type 2" and "type 3" dates)

wcq_data$combinedtext <- str_replace(wcq_data$combinedtext, "[0-9]{1,2}. [0-9]{1}.[0-9]{2}", "")
wcq_data$combinedtext <- str_replace(wcq_data$combinedtext, "[0-9]{1,2}-[0-9]{2}-[0-9]{2}", "")

# Now let's extract the scores

wcq_data$scores <- str_match(wcq_data$combinedtext, " [0-9]{1,2}-[0-9]{1,2} ")

# Split scores

wcq_data$scoreA <- str_split_fixed(wcq_data$scores, "-", 2)[,1]
wcq_data$scoreB <- str_split_fixed(wcq_data$scores, "-", 2)[,2]

# Read in match teams

# Extract raw text

wcq_data$teamA <- str_match(wcq_data$combinedtext, "[A-z]*\\.*? +[0-9]{1,2}-[0-9]{1,2}")
wcq_data$teamB <- str_match(wcq_data$combinedtext, "[0-9]{1,2}-[0-9]{1,2} [A-z]* *[A-z]*")

# Clean the scores out

wcq_data$teamA <- str_replace(wcq_data$teamA, " [0-9]{1,2}-[0-9]{1,2}", "")
wcq_data$teamB <- str_replace(wcq_data$teamB, "[0-9]{1,2}-[0-9]{1,2} ", "")

# Further cleaning

# Remove non-match observations

wcq_data$date <- gsub("^  $", NA, wcq_data$date)
wcq_data <- wcq_data[complete.cases(wcq_data$date),]

# Reset row names

rownames(wcq_data) <- seq(length=nrow(wcq_data))

# Trim whitespace

wcq_data$teamA <- trimws(wcq_data$teamA)
wcq_data$teamB <- trimws(wcq_data$teamB)

# Fix teamA and teamB

{ 
  
# Remove punctuation and other symbols

wcq_data$teamA <- gsub("[[:punct:]]", "", wcq_data$teamA)
wcq_data$teamB <- gsub("[[:punct:]]", "", wcq_data$teamB)

  # No teams or scores for sc matches

  wcq_data$teamA <- ifelse(wcq_data$sc==1, NA, wcq_data$teamA)
  wcq_data$teamB <- ifelse(wcq_data$sc==1, NA, wcq_data$teamB)
  wcq_data$scores <- ifelse(wcq_data$sc==1, NA, wcq_data$scores)
  wcq_data$scoreA <- ifelse(wcq_data$sc==1, NA, wcq_data$scoreA)
  wcq_data$scoreB <- ifelse(wcq_data$sc==1, NA, wcq_data$scoreB)
  wcq_data$et <- ifelse(wcq_data$sc==1, NA, wcq_data$et)
  
  # Error with "and"
  
  wcq_data <- wcq_data[-5419,]
  rownames(wcq_data) <- seq(length=nrow(wcq_data))
  
  # Fix issue with odd number of spaces
  
  wcq_data$teamB <- gsub(" {2,}[A-z]+$", "", wcq_data$teamB)

  # Bosnia-Herzegovina
  
  wcq_data$teamB <- recode(wcq_data$teamB, "Bosnia" = "Bosnia-Herzegovina")
  wcq_data$teamA <- recode(wcq_data$teamA, "H" = "Bosnia-Herzegovina")
  wcq_data$teamA <- recode(wcq_data$teamA, "Herz" = "Bosnia-Herzegovina")
  wcq_data$teamA <- recode(wcq_data$teamA, "Herzegovina" = "Bosnia-Herzegovina")
  wcq_data$teamB <- recode(wcq_data$teamB, "Bosnia" = "Bosnia-Herzegovina")
  
  # Costa Rica
  
  wcq_data$teamB <- recode(wcq_data$teamB, "Costa" = "Costa Rica")
  
  # Czech Republic, Czechoslovakia, etc.
  
  wcq_data$teamA <- recode(wcq_data$teamA, "Republic" = "Czech Republic/CSFR")
  wcq_data$teamB <- recode(wcq_data$teamB, "Czech Republic" = "Czech Republic/CSFR")
  wcq_data$teamA <- recode(wcq_data$teamA, "Czechoslovakia" = "Czech Republic/CSFR")
  wcq_data$teamB <- recode(wcq_data$teamB, "Czechoslovakia" = "Czech Republic/CSFR")
  
  # DRC/Zaire
  
  wcq_data$teamA <- recode(wcq_data$teamA, "Zaire" = "DRC/Zaire")
  wcq_data$teamB <- recode(wcq_data$teamB, "Zaire" = "DRC/Zaire")
  
  # East Germany
  
  wcq_data$teamB <- recode(wcq_data$teamB, "East Germany" = "German Dem. Rep. (East Germany)")
  
  # Indonesia
  
  wcq_data$teamB <- recode(wcq_data$teamB, "Dutch Indies" = "Indonesia/Nether. Indies")
  wcq_data$teamB <- recode(wcq_data$teamB, "Dutch Indies" = "Indonesia/Nether. Indies")
  
  # Russia
  
  wcq_data$teamA <- recode(wcq_data$teamA, "Russia" = "Russia/USSR")
  wcq_data$teamB <- recode(wcq_data$teamB, "Russia" = "Russia/USSR")
  
  wcq_data$teamA <- recode(wcq_data$teamA, "Union" = "Russia/USSR")
  wcq_data$teamB <- recode(wcq_data$teamB, "Soviet Union" = "Russia/USSR")
  
  # West Germany
  
  wcq_data$teamB <- recode(wcq_data$teamB, "West Germany" = "Germany")
  
  # United Arab Emirates
  
  wcq_data$teamA <- recode(wcq_data$teamA, "UAE" = "United Arab Emirates")
  wcq_data$teamB <- recode(wcq_data$teamB, "UAE" = "United Arab Emirates")
  
  # Yugoslavia
  
  wcq_data$teamA <- recode(wcq_data$teamA, "Montenegro" = "Serbia/Yugoslavia")
  wcq_data$teamA <- recode(wcq_data$teamA, "Serbia" = "Serbia/Yugoslavia")
  wcq_data$teamB <- recode(wcq_data$teamB, "Serbia" = "Serbia/Yugoslavia")
  wcq_data$teamA <- recode(wcq_data$teamA, "Yugoslavia" = "Serbia/Yugoslavia")
  wcq_data$teamB <- recode(wcq_data$teamB, "Yugoslavia" = "Serbia/Yugoslavia")
  
  # Fix countries that are missing other word
  
  wcq_data$teamA <- recode(wcq_data$teamA, "Africa" = "South Africa")
  wcq_data$teamA <- recode(wcq_data$teamA, "Arabia" = "Saudi Arabia")
  wcq_data$teamA <- recode(wcq_data$teamA, "Coast" = "Ivory Coast")
  wcq_data$teamB <- recode(wcq_data$teamB, "El" = "El Salvador")
  wcq_data$teamA <- recode(wcq_data$teamA, "Rica" = "Costa Rica")
  wcq_data$teamA <- recode(wcq_data$teamA, "Tobago" = "Trinidad and Tobago")
  wcq_data$teamB <- recode(wcq_data$teamB, "Trinidad" = "Trinidad and Tobago")
  wcq_data$teamB <- recode(wcq_data$teamB, "Trinidad And" = "Trinidad and Tobago") 
  wcq_data$teamA <- recode(wcq_data$teamA, "Zealand" = "New Zealand")
  wcq_data$teamA <- recode(wcq_data$teamA, "Emirates" = "United Arab Emirates")
  wcq_data$teamA <- recode(wcq_data$teamA, "United Arab" = "United Arab Emirates")
  
  # Manually fix troublesome countries
  
  # East Germany
  
  wcq_data[c(153, 157, 245:246, 366, 370, 469:470, 654, 656, 660, 866, 872, 873, 1216:1217, 1455,
             1465, 1467, 1472, 1738, 1741, 1744, 1751), 10] <- "German Dem. Rep. (East Germany)"
  
  # Northern Ireland
  
  wcq_data[c(50, 89, 179, 182, 237, 241, 354, 357, 361, 474, 476, 675, 677, 678, 885:886, 888, 1196,
             1204, 1209, 1211, 1434, 1437, 1439, 1441, 1787:1788, 1794, 1802, 2090, 2096, 2100, 2103,
             2125, 2129, 2719, 2721, 2728, 2730, 2739), 10] <- "Northern Ireland"
  
  # North Korea
  
  wcq_data[c(441, 815:816, 821, 1375, 1381, 1384, 1690, 1692, 2000, 2002, 2003, 2011, 2427:2428, 2430,
             2445, 2498, 2501, 2504, 4586, 4589, 4591, 4646, 4648, 4652, 5396, 5441, 5445, 5447, 5497,
             5503, 5505, 5509, 6212, 6214, 6218), 10] <- "North Korea"
  # South Korea
  
  wcq_data$teamA <- recode(wcq_data$teamA, "Korea" = "South Korea")
  
  # West Germany
  
  wcq_data$teamB <- recode(wcq_data$teamB, "West Germany" = "Germany")
  
# Trim whitespace

wcq_data$teamA <- trimws(wcq_data$teamA)
wcq_data$teamB <- trimws(wcq_data$teamB)

}

# Rename id column, remove text and score column, reorder the rest of the columns

colnames(wcq_data)[1] <- "id"
wcq_data <- wcq_data[ ,-c(2:3, 7)]
wcq_data <- wcq_data[, c(1,2,7,5,6,8,4,3)]

# Keep just countries that went to the World Cup Finals

wcq_data <- subset(wcq_data, wcq_data$teamA %in% unique(c(wcf_data$teamA, wcf_data$teamB)) |
                         wcq_data$teamB %in% unique(c(wcf_data$teamA, wcf_data$teamB)) |
                         wcq_data$sc == 1)

# Save as .csv 

if (saveIntermediate == TRUE) { write.csv(wcq_data, file = paste0(directory, "wcq_matchdata.csv"), 
                                          row.names = FALSE)
  
}