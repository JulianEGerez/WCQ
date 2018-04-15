# This file cleans WCF data using regular expressions

# Created by Julian Gerez

# Convert from factors to character

wcf_data[,2] <- as.character(wcf_data[,2])

# Read in match dates

  # There are three types: DD.MM.YY, DD- M-YY, and text

  wcf_data$date1 <- str_match(wcf_data$combinedtext, "[0-9]{2}.[0-9]{1,2}.[0-9]{2}")
  wcf_data$date2 <- str_match(wcf_data$combinedtext, "[0-9]{1,2}- [0-9]{1}-[0-9]{2}")
  wcf_data$date3 <- str_match(wcf_data$combinedtext, "[A-z]{4} [0-9]{1,2}, [0-9]{4}")
  
  wcf_data$date1 <- gsub("(\\d{2})$", "19\\1", wcf_data$date1)
  wcf_data$date1 <- as.Date(wcf_data$date1, format = "%d.%m.%Y")
  wcf_data$date2 <- as.Date(wcf_data$date2, format = "%d- %m-%y")
  wcf_data$date3 <- as.Date(wcf_data$date3, format = "%B %d, %Y")

  # Combine into one column, then remove concatenated NAs

  wcf_data$date <- str_replace(str_replace(str_replace(
      paste(wcf_data$date1, wcf_data$date2, wcf_data$date3), "NA", ""), "NA", ""), "NA", "")

  # Remove extraneous columns

  wcf_data <- wcf_data[ -c(3:5)]

# Create observations from finals
  
  {

  wcf_data[46, 2]   <- "Uruguay 4-2 Argentina" #1930
  wcf_data[101, 2]  <- "Italy 2-1 Czechoslovakia [aet]" #1934
  wcf_data[157, 2]  <- "Italy 4-2 Hungary" #1938
  wcf_data[212, 2]  <- "Uruguay 2-1 Brazil" #1950
  wcf_data[281, 2]  <- "West Germany 3-2 Hungary" #1954
  wcf_data[357, 2]  <- "Brazil 5-2 Sweden" #1958
  wcf_data[427, 2]  <- "Brazil 3-1 Czechoslovakia" #1962
  wcf_data[497, 2]  <- "England 4-2 West Germany [aet]" #1966
  wcf_data[567, 2]  <- "Brazil 4-1 Italy" #1970
  wcf_data[639, 2]  <- "West Germany 2-1 Netherlands" #1974
  wcf_data[711, 2]  <- "Argentina 3-1 Netherlands [aet]" #1978
  wcf_data[810, 2]  <- "Italy 3-1 West Germany" #1982
  wcf_data[918, 2]  <- "Argentina 3-2 West Germany" #1986
  wcf_data[1027, 2] <- "West Germany 1-0 Argentina" #1990
  wcf_data[1134, 2] <- "Brazil 0-0 Italy [aet]" #1994
  wcf_data[1260, 2] <- "France 3-0 Brazil" #1998
  wcf_data[1430, 2] <- "Brazil 2-0 Germany" #2002
  wcf_data[1603, 2] <- "Italy 1-1 France [aet]" #2006
  wcf_data[1780, 2] <- "Spain 1-0 Netherlands [aet]" #2010
  wcf_data[1957, 2] <- "Germany 1-0 Argentina [aet]" #2014
  
  }

# Read in match scores

  # First we need to delete "type 2" dates)

  wcf_data$combinedtext <- str_replace(wcf_data$combinedtext, "[0-9]{1,2}. [0-9]{1}.[0-9]{2}", "")

 # Now let's extract the scores

  wcf_data$scores <- str_match(wcf_data$combinedtext, " [0-9]{1,2}-[0-9]{1,2} ")

  # Split scores

  wcf_data$scoreA <- str_split_fixed(wcf_data$scores, "-", 2)[,1]
  wcf_data$scoreB <- str_split_fixed(wcf_data$scores, "-", 2)[,2]

# Read in match teams

  # Extract raw text

  wcf_data$teamA <- str_match(wcf_data$combinedtext, "[A-z]* +[0-9]{1,2}-[0-9]{1,2}")
  wcf_data$teamB <- str_match(wcf_data$combinedtext, "[0-9]{1,2}-[0-9]{1,2} [A-z]* *[A-z]*")
  
  # Clean the scores out

  wcf_data$teamA <- str_replace(wcf_data$teamA, " [0-9]{1,2}-[0-9]{1,2}", "")
  wcf_data$teamB <- str_replace(wcf_data$teamB, "[0-9]{1,2}-[0-9]{1,2} ", "")

# Create et variable

  # Create a.e.t. variable

  wcf_data$aet <- str_match(wcf_data$teamB, "\\[aet]")
  wcf_data$teamB <- gsub("\\[aet]", "", wcf_data$teamB)

  # Create a.s.d.e.t. variable

  wcf_data$asdet <- str_match(wcf_data$teamB, "\\[asdet]")
  wcf_data$teamB <- gsub("\\[asdet]", "", wcf_data$teamB)

  # Convert to one column then recode and remove unncessary columns

  wcf_data$et <- paste(wcf_data$aet, wcf_data$asdet)
  
  wcf_data$et[wcf_data$et=="[aet] NA"] <- 1
  wcf_data$et[wcf_data$et=="NA [asdet]"] <- 1
  wcf_data$et[wcf_data$et=="NA NA"] <- 0
  
  wcf_data <- wcf_data[ -c(9:10)]
  
# Further cleaning

  # Remove non-match observations

  wcf_data$date <- gsub("^  $", NA, wcf_data$date) 
  wcf_data <- wcf_data[complete.cases(wcf_data$date),]

  # Remove extra rows
  
  wcf_data <- wcf_data[-40,]
  wcf_data <- wcf_data[-c(69:70),]

  # Reset row names
  
  rownames(wcf_data) <- seq(length=nrow(wcf_data))

# Fix countries with different names over the years and other issues related to reading in

{
  
  # Trim whitespace
  
  wcf_data$teamA <- trimws(wcf_data$teamA)
  wcf_data$teamB <- trimws(wcf_data$teamB)

  # Bosnia

  wcf_data$teamB <- recode(wcf_data$teamB, "Bosnia" = "Bosnia-Herzegovina")

  # Czech Republic, Czechoslovakia, etc.

  wcf_data$teamA <- recode(wcf_data$teamA, "Republic" = "Czech Republic/CSFR")
  wcf_data$teamB <- recode(wcf_data$teamB, "Czech Republic" = "Czech Republic/CSFR")
  wcf_data$teamA <- recode(wcf_data$teamA, "Czechoslovakia" = "Czech Republic/CSFR")
  wcf_data$teamB <- recode(wcf_data$teamB, "Czechoslovakia" = "Czech Republic/CSFR")

  # East Germany

  wcf_data$teamB <- recode(wcf_data$teamB, "East Germany" = "German Dem. Rep. (East Germany)")

  # Indonesia

  wcf_data$teamB <- recode(wcf_data$teamB, "Dutch Indies" = "Indonesia/Nether. Indies")
  wcf_data$teamB <- recode(wcf_data$teamB, "Nether. Indies/Indonesia" = "Indonesia/Nether. Indies")

  # Russia

  wcf_data$teamA <- recode(wcf_data$teamA, "Russia" = "Russia/USSR")
  wcf_data$teamB <- recode(wcf_data$teamB, "Russia" = "Russia/USSR")
  
  wcf_data$teamA <- recode(wcf_data$teamA, "Union" = "Russia/USSR")
  wcf_data$teamB <- recode(wcf_data$teamB, "Soviet Union" = "Russia/USSR")

  # West Germany

  wcf_data$teamB <- recode(wcf_data$teamB, "West Germany" = "Germany")

  # United Arab Emirates

  wcf_data$teamA <- recode(wcf_data$teamA, "UAE" = "United Arab Emirates")
  wcf_data$teamB <- recode(wcf_data$teamB, "UAE" = "United Arab Emirates")

  # Yugoslavia

  wcf_data$teamA <- recode(wcf_data$teamA, "Montenegro" = "Serbia/Yugoslavia")
  wcf_data$teamA <- recode(wcf_data$teamA, "Serbia" = "Serbia/Yugoslavia")
  wcf_data$teamB <- recode(wcf_data$teamB, "Serbia" = "Serbia/Yugoslavia")
  wcf_data$teamA <- recode(wcf_data$teamA, "Yugoslavia" = "Serbia/Yugoslavia")
  wcf_data$teamB <- recode(wcf_data$teamB, "Yugoslavia" = "Serbia/Yugoslavia")

  # Zaire/DRC

  wcf_data$teamA <- recode(wcf_data$teamA, "Zaire" = "DRC/Zaire")
  wcf_data$teamB <- recode(wcf_data$teamB, "Zaire" = "DRC/Zaire")

# Fix countries that are missing other word

  wcf_data$teamA <- recode(wcf_data$teamA, "Africa" = "South Africa")
  wcf_data$teamA <- recode(wcf_data$teamA, "Arabia" = "Saudi Arabia")
  wcf_data$teamA <- recode(wcf_data$teamA, "Coast" = "Ivory Coast")
  wcf_data$teamA <- recode(wcf_data$teamA, "Rica" = "Costa Rica")
  wcf_data$teamA <- recode(wcf_data$teamA, "Tobago" = "Trinidad and Tobago")
  wcf_data$teamB <- recode(wcf_data$teamB, "Trinidad" = "Trinidad and Tobago") 
  wcf_data$teamA <- recode(wcf_data$teamA, "Zealand" = "New Zealand")

# Fix countries with space errors

  wcf_data[91, 8]  <- "England"
  wcf_data[117, 8] <- "Mexico"
  wcf_data[124, 8] <- "Austria"
  wcf_data[160, 8] <- "England"

# Manually fix troublesome countries

  # Bosnia

  wcf_data[807, 7] <- "Bosnia-Herzegovina"

  # East Germany
  
  wcf_data[c(234, 238, 259), 7] <- "German Dem. Rep. (East Germany)"

  # Northern Ireland
  
  wcf_data[c(102, 108, 355:356, 382:383), 7] <- "Northern Ireland"

  # North Korea
  
  wcf_data[c(189, 191, 749), 7] <- "North Korea"

  # South Korea

  wcf_data$teamA <- recode(wcf_data$teamA, "Korea" = "South Korea")

  # West Germany

  wcf_data$teamB <- recode(wcf_data$teamB, "West Germany" = "Germany")
  
  }
  
# Rename id column, remove text and score column, reorder the rest of the columns

colnames(wcf_data)[1] <- "id" 
wcf_data <- wcf_data[ ,-c(2,4)]
wcf_data <- wcf_data[, c(1,2,5,3,4,6,7)]

# Save as .csv 

if (saveIntermediate == TRUE) { write.csv(wcf_data, file = paste0(directory, "wcf_matchdata.csv"), 
                                          row.names = FALSE)
}