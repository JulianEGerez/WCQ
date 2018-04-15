# This file scrapes World Cup Qualifying data

# Created by Julian Gerez

# Create pre-2002 year vector

year_vec_pre2002 <- c("34","38","50","54","58","62","66","70","74","78","82","86","90","94","98");

# Create pre-2002 list of data frames

wcq_pre2002 = lapply(year_vec_pre2002, function(year) data.frame(read_lines(html_text(
              html_nodes(read_html(paste0("http://www.rsssf.com/tables/", year, "q.html")), "pre"), trim = TRUE))))

# Rename individual list items pre-2002

names(wcq_pre2002) <- c("wcq_1934","wcq_1938","wcq_1950","wcq_1954","wcq_1958","wcq_1962","wcq_1966", "wcq_1970",
                        "wcq_1974","wcq_1978","wcq_1982","wcq_1986","wcq_1990","wcq_1994","wcq_1998")

# Save pre-2002 WCQ data as .csv

if (saveIntermediate == TRUE) { lapply(1:length(wcq_pre2002), function(i) write.csv(wcq_pre2002[[i]], 
                                               file = paste0(directory, names(wcq_pre2002[i]), ".csv"),
                                               row.names = FALSE))
}

# Do the same for post-2002 (create vector, create data frames, rename data frames, write as .csv)

year_vec_post2002 <- c("2002", "2006", "2010", "2014")

wcq_post2002 = lapply(year_vec_post2002, function(year) data.frame(read_lines(
               paste0("http://www.rsssf.com/tables/", year, "q.html"))))

names(wcq_post2002) <- c("wcq_2002", "wcq_2006", "wcq_2010", "wcq_2014")

if (saveIntermediate == TRUE) { lapply(1:length(wcq_post2002), function(i) write.csv(wcq_post2002[[i]],
                                                     file = paste0(directory, names(wcq_post2002[i]), ".csv"),
                                                    row.names = FALSE))
}

# Now that the data is read in, we can combine the separate lists into one

wcq_dataframes       <- c(wcq_pre2002, wcq_post2002)

# Let's keep copies of pre2002 and post2002 just in case

wcq_pre2002_data     <- ldply(wcq_pre2002, data.frame)
wcq_post2002_data    <- ldply(wcq_post2002, data.frame)

# Clean workspace

rm(wcq_post2002, wcq_pre2002, year_vec_pre2002, year_vec_post2002)

# Let's just do one more for 2018

wcq_2018 <- data.frame(read_lines(paste0("http://www.rsssf.com/tables/", "2018", "q.html")))

# Combine all data frames into single data frame with id, rename columns, merge columns

wcq_data <- ldply(wcq_dataframes, data.frame)
wcq_data <- dplyr::rename(wcq_data, 
                        pre2002  = read_lines.html_text.html_nodes.read_html.paste0..http...www.rsssf.com.tables....,
                        post2002 = read_lines.paste0..http...www.rsssf.com.tables....year...q.html...)

wcq_data <- cbind(wcq_data[1], combinedtext = na.omit(unlist(wcq_data[-1])))

# Save this new data frame as .csv

rm(wcq_dataframes, wcq_2018, wcq_post2002_data, wcq_pre2002_data)

if (saveIntermediate == TRUE) { 
  write.csv(wcq_data, file = paste0(directory, "wcq_rawdata.csv"), row.names = FALSE)
}