# This file scrapes World Cup Finals data

# Created by Julian Gerez

# Create pre-2002 year vector

year_vec_pre2002 <- c("30", "34","38","50","54","58","62","66","70","74","78","82","86","90","94","98")

# Create pre-2002 list of data frames

wcf_pre2002 = lapply(year_vec_pre2002, function(year) data.frame(read_lines(html_text(
  html_nodes(read_html(paste0("http://www.rsssf.com/tables/", year, "f.html")), "pre"), trim = TRUE))))

# Rename individual list items pre-2002

names(wcf_pre2002) <- c("wcf_1930", "wcf_1934","wcf_1938","wcf_1950","wcf_1954","wcf_1958","wcf_1962","wcf_1966", "wcf_1970",
                        "wcf_1974","wcf_1978","wcf_1982","wcf_1986","wcf_1990","wcf_1994","wcf_1998")

# Save pre-2002 WCF data as .csv

if (saveIntermediate == TRUE) { lapply(1:length(wcf_pre2002), function(i) write.csv(wcf_pre2002[[i]], 
                                                    file = paste0(directory, names(wcf_pre2002[i]), ".csv"),
                                                    row.names = FALSE))
}

# Do the same for post-2002 (create vector, create data frames, rename data frames, write as .csv)

year_vec_post2002 <- c("2002", "2006", "2010", "2014")

wcf_post2002 = lapply(year_vec_post2002, function(year) data.frame(read_lines(html_text(
  html_nodes(read_html(paste0("http://www.rsssf.com/tables/", year, "f.html")), "pre"), trim = TRUE))))

names(wcf_post2002) <- c("wcf_2002", "wcf_2006", "wcf_2010", "wcf_2014")

if (saveIntermediate == TRUE) { lapply(1:length(wcf_post2002), function(i) write.csv(wcf_post2002[[i]],
                                                     file = paste0(directory, names(wcf_post2002[i]), ".csv"),
                                                     row.names = FALSE))
}

# Now that the data is read in, we can combine the separate lists into one

wcf_dataframes       <- c(wcf_pre2002, wcf_post2002)

# Let's keep copies of pre2002 and post2002 just in case

wcf_pre2002_data     <- ldply(wcf_pre2002, data.frame)
wcf_post2002_data    <- ldply(wcf_post2002, data.frame)

# Clean workspace

rm(wcf_post2002, wcf_pre2002, year_vec_pre2002, year_vec_post2002)

# Combine all data frames into single data frame with id, merge columns

wcf_data <- ldply(wcf_dataframes, data.frame)
wcf_data <- cbind(wcf_data[1], combinedtext = na.omit(unlist(wcf_data[-1])))

# Save this new data frame as .csv

rm(wcf_dataframes, wcf_post2002_data, wcf_pre2002_data)

if (saveIntermediate == TRUE) { write.csv(wcf_data, file = paste0(directory, "wcf_rawdata.csv"), row.names = FALSE)
  
}