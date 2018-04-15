# This file creates World Cup finals and qualifying match data

# Created by Julian Gerez

wcq_match_data <- wcq_data
wcf_match_data <- wcf_data

rm(wcq_data, wcf_data)

# Convert to single matches dataframe

colnames(wcf_match_data)[1] <- "tourn_id"
colnames(wcq_match_data)[1] <- "tourn_id"

wc_match_data <- dplyr::bind_rows(wcf_match_data, wcq_match_data, .id = "id")

wc_match_data$id <- recode(wc_match_data$id, "1" = "finals")
wc_match_data$id <- recode(wc_match_data$id, "2" = "qualifier")

wc_match_data$tourn_id <- gsub("^(wc[f,q])", "wc", wc_match_data$tourn_id)

# Trim whitespace

wc_match_data$scoreA <- trimws(wc_match_data$scoreA)
wc_match_data$scoreB <- trimws(wc_match_data$scoreB)
wcf_match_data$scoreA <- trimws(wcf_match_data$scoreA)
wcf_match_data$scoreB <- trimws(wcf_match_data$scoreB)
wcq_match_data$scoreA <- trimws(wcq_match_data$scoreA)
wcq_match_data$scoreB <- trimws(wcq_match_data$scoreB)

# Convert to numeric

wc_match_data[,c(5:6,8)] <- lapply(wc_match_data[,c(5:6,8)], function(x) as.numeric(x))
wcf_match_data[,c(4:5,7)] <- lapply(wcf_match_data[,c(4:5,7)], function(x) as.numeric(x))
wcq_match_data[,c(4:5,7)] <- lapply(wcq_match_data[,c(4:5,7)], function(x) as.numeric(x))

# Define draws

wc_match_data$d  <- ifelse(wc_match_data$scoreA == wc_match_data$scoreB, TRUE, FALSE)
wc_match_data$dA <- ifelse(wc_match_data$d == TRUE, 
                           as.character(wc_match_data$teamA), NA)
wc_match_data$dB <- ifelse(wc_match_data$d == TRUE, 
                           as.character(wc_match_data$teamB), NA)

# Define wins and losses

wc_match_data$w <- ifelse(wc_match_data$d == TRUE, "Draw",
                          ifelse(wc_match_data$scoreA > wc_match_data$scoreB, 
                                 as.character(wc_match_data$teamA), 
                                 as.character(wc_match_data$teamB)))

wc_match_data$l <- ifelse(wc_match_data$d == TRUE, "Draw",
                          ifelse(wc_match_data$w == wc_match_data$teamA, 
                                 as.character(wc_match_data$teamB), 
                                 as.character(wc_match_data$teamA)))

# Reorder columns

wc_match_data <- wc_match_data[ ,c(1:9, 13, 14, 10:12)]

# Save as .csv 

write.csv(wc_match_data, file = paste0(directory, "wc_match_data.csv"), 
                                          row.names = FALSE)