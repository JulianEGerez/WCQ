# This file creates World Cup finals and qualifying results data

# Created by Julian Gerez

# Create new results dataframe with unique ids

colnames(wcf_match_data)[1] <- "tourn_id"

wcf_match_data_temp <- wcf_match_data
wcf_match_data_temp$tourn_id <- gsub("^(wcf)", "wc", wcf_match_data_temp$tourn_id)

rm(wcf_match_data, wcq_match_data)

temp_x <- unique(wcf_match_data_temp[c("tourn_id", "teamA")])
temp_y <- unique(wcf_match_data_temp[c("tourn_id", "teamB")])

colnames(temp_x)[2] <- "team"
colnames(temp_y)[2] <- "team"

wc_results_data <- dplyr::bind_rows(temp_x, temp_y)

rm(temp_x, temp_y, wcf_match_data_temp)

wc_results_data <- unique(wc_results_data[c("tourn_id", "team")])

wc_results_data <- wc_results_data[order(wc_results_data$tourn_id, wc_results_data$team),]
rownames(wc_results_data) <- seq(length=nrow(wc_results_data))

wc_results_data$id <- paste0(wc_results_data$tourn_id, "_", wc_results_data$team)

# Read in match data

matches <- wc_match_data

matches$teamA <- paste0(matches$tourn_id, "_", matches$teamA)
matches$teamB <- paste0(matches$tourn_id, "_", matches$teamB)

# Define draws

matches$d  <- ifelse(matches$scoreA == matches$scoreB, TRUE, FALSE)
matches$dA <- ifelse(matches$d == TRUE, 
                           as.character(matches$teamA), NA)
matches$dB <- ifelse(matches$d == TRUE, 
                           as.character(matches$teamB), NA)

# Define wins and losses

matches$w <- ifelse(matches$d == TRUE, "Draw",
                          ifelse(matches$scoreA > matches$scoreB, 
                                 as.character(matches$teamA), 
                                 as.character(matches$teamB)))

matches$l <- ifelse(matches$d == TRUE, "Draw",
                          ifelse(matches$w == matches$teamA, 
                                 as.character(matches$teamB), 
                                 as.character(matches$teamA)))
# Fill in the columns

# Qualifying wins

wc_results_data$qw <- sapply(wc_results_data$id, function(x)
sum(matches$w == x & matches$id == 'qualifier', na.rm = TRUE))

# Qualifying draws

wc_results_data$qd <- sapply(wc_results_data$id, function(x)
  sum(sum(matches$dA == x & matches$id == 'qualifier', na.rm = TRUE),
      sum(matches$dB == x & matches$id == 'qualifier', na.rm = TRUE)))

# Qualifying losses

wc_results_data$ql <- sapply(wc_results_data$id, function(x)
  sum(matches$l == x & matches$id == 'qualifier', na.rm = TRUE))

# Qualifying games played, points, points possible, old points, old points possible

wc_results_data$qgp <- wc_results_data$qw + wc_results_data$qd + wc_results_data$ql
wc_results_data$qp <- wc_results_data$qw*3 + wc_results_data$qd*1
wc_results_data$qpp <- wc_results_data$qp / (wc_results_data$qgp*3)
wc_results_data$qop <- wc_results_data$qw*2 + wc_results_data$qd*1
wc_results_data$qopp <- wc_results_data$qop / (wc_results_data$qgp*2)

# Qualifying goals for, against, and goal difference

# Finals wins

wc_results_data$fw <- sapply(wc_results_data$id, function(x)
  sum(matches$w == x & matches$id == 'finals', na.rm = TRUE))

# Finals draws

wc_results_data$fd <- sapply(wc_results_data$id, function(x)
  sum(sum(matches$dA == x & matches$id == 'finals', na.rm = TRUE),
      sum(matches$dB == x & matches$id == 'finals', na.rm = TRUE)))

# Finals losses

wc_results_data$fl <- sapply(wc_results_data$id, function(x)
  sum(matches$l == x & matches$id == 'finals', na.rm = TRUE))

# Finals games played, points, points possible, old points, old points possible

wc_results_data$fgp <- wc_results_data$fw + wc_results_data$fd + wc_results_data$fl
wc_results_data$fp <- wc_results_data$fw*3 + wc_results_data$fd*1
wc_results_data$fpp <- wc_results_data$fp / (wc_results_data$fgp*3)
wc_results_data$fop <- wc_results_data$fw*2 + wc_results_data$fd*1
wc_results_data$fopp <- wc_results_data$fop / (wc_results_data$fgp*2)

# Finals goals for, against, and goal difference

temp_AA  <- aggregate(as.numeric(scoreA) ~ teamA + id, data=matches, sum)
temp_BB <- aggregate(as.numeric(scoreB) ~ teamB + id, data=matches, sum)

colnames(temp_AA)[1] <- "team"
colnames(temp_AA)[3] <- "score"
colnames(temp_BB)[1] <- "team"
colnames(temp_BB)[3] <- "score"

temp_goals_for <- merge(temp_AA, temp_BB, by = intersect(names(temp_AA)[1:2], names(temp_BB)[1:2]), 
                        all.x = TRUE, all.y = TRUE)

rm(temp_AA, temp_BB)

temp_AB <- aggregate(as.numeric(scoreA) ~ teamB + id, data=matches, sum)
temp_BA <- aggregate(as.numeric(scoreB) ~ teamA + id, data=matches, sum)

colnames(temp_AB)[1] <- "team"
colnames(temp_AB)[3] <- "score"
colnames(temp_BA)[1] <- "team"
colnames(temp_BA)[3] <- "score"

temp_goals_against <- merge(temp_AB, temp_BA, by = intersect(names(temp_AB)[1:2], names(temp_BA)[1:2]),
                            all.x = TRUE, all.y = TRUE)

rm(temp_AB, temp_BA)

temp_goals <- cbind(temp_goals_for, temp_goals_against)

rm(temp_goals_for, temp_goals_against)

temp_goals[is.na(temp_goals)] <- 0

temp_goals$gf <- temp_goals[,3] + temp_goals[,4]
temp_goals$ga <- temp_goals[,7] + temp_goals[,8]

identical(temp_goals[,1], temp_goals[,5])
identical(temp_goals[,2], temp_goals[,6])

temp_goals <- temp_goals[,-c(3:8)]

temp_goals$gd <- temp_goals$gf - temp_goals$ga

wc_results_data$fgf <- sapply(wc_results_data$id, function(x)
  temp_goals[which(temp_goals$team == x & temp_goals$id == "finals"), 3])

wc_results_data$fga <- sapply(wc_results_data$id, function(x)
  temp_goals[which(temp_goals$team == x & temp_goals$id == "finals"), 4])

wc_results_data$fgd <- sapply(wc_results_data$id, function(x)
  temp_goals[which(temp_goals$team == x & temp_goals$id == "finals"), 5])

wc_results_data$qgf <- sapply(wc_results_data$id, function(x)
  temp_goals[which(temp_goals$team == x & temp_goals$id == "qualifier"), 3])

wc_results_data$qga <- sapply(wc_results_data$id, function(x)
  temp_goals[which(temp_goals$team == x & temp_goals$id == "qualifier"), 4])

wc_results_data$qgd <- sapply(wc_results_data$id, function(x)
  temp_goals[which(temp_goals$team == x & temp_goals$id == "qualifier"), 5])

rm(temp_goals)

# If qualifying games played = 0, data should be missing

is.na(wc_results_data$fgf) <- sapply(wc_results_data$fgf, length) == 0
is.na(wc_results_data$fga) <- sapply(wc_results_data$fga, length) == 0
is.na(wc_results_data$fgd) <- sapply(wc_results_data$fgd, length) == 0

is.na(wc_results_data$qgf) <- sapply(wc_results_data$qgf, length) == 0
is.na(wc_results_data$qga) <- sapply(wc_results_data$qga, length) == 0
is.na(wc_results_data$qgd) <- sapply(wc_results_data$qgd, length) == 0

wc_results_data[,23:25] <- unlist(wc_results_data[,23:25])

for (i in c(4:11)) {
  
  wc_results_data[,i]  <- ifelse(wc_results_data$qgp == 0, NA, wc_results_data[,i])
  
}

rm(i)

# Host dummy

wc_results_data$hd <- recode(wc_results_data$id, 
                      "wc_1930_Uruguay" = 1, "wc_1934_Italy" = 1, "wc_1938_France" = 1, "wc_1950_Brazil" = 1, 
                      "wc_1954_Switzerland" = 1, "wc_1958_Sweden" = 1, "wc_1962_Chile" = 1, 
                      "wc_1966_England" = 1, "wc_1970_Mexico" = 1, "wc_1974_Germany" = 1,
                      "wc_1978_Argentina" = 1, "wc_1982_Spain" = 1, "wc_1986_Mexico" = 1, "wc_1990_Italy" = 1,
                      "wc_1994_USA" = 1, "wc_1998_France" = 1, "wc_2002_South Korea" = 1, "wc_2006_Germany" = 1,
                      "wc_2010_South Africa" = 1, "wc_2014_Brazil" = 1, .default = 0)

# Champion dummy

wc_results_data$cd <- recode(wc_results_data$id, 
                      "wc_1930_Uruguay" = 1, "wc_1934_Italy" = 1, "wc_1938_Italy" = 1, "wc_1950_Uruguay" = 1, 
                      "wc_1954_Germany" = 1, "wc_1958_Brazil" = 1, "wc_1962_Brazil" = 1, 
                      "wc_1966_England" = 1, "wc_1970_Brazil" = 1, "wc_1974_Germany" = 1,
                      "wc_1978_Argentina" = 1, "wc_1982_Italy" = 1, "wc_1986_Argentina" = 1, 
                      "wc_1990_Germany" = 1, "wc_1994_Brazil" = 1, "wc_1998_France" = 1, "wc_2002_Brazil" = 1,
                      "wc_2006_Italy" = 1, "wc_2010_Spain" = 1, "wc_2014_Germany" = 1, .default = 0)

# Reorder columns

wc_results_data <- wc_results_data[,-3]
wc_results_data <- wc_results_data[,c(1:2, 6, 3:5, 7:10, 22:24, 14, 11:13, 15:21, 25:26)]

rm(matches)

# Save as .csv

write.csv(wc_results_data, file = paste0(directory, "wc_results_data.csv"),
                                          row.names = FALSE)