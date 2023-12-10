library(ggplot2)
library(dplyr)
library(tidyr)


df <- read.csv("Seattle_Neighborhood_Demographics.csv")

beacon_2020 <- c(pull(filter(df, neighborhood == "Beacon Hill"), total_pop_2020), 
                 pull(filter(df, neighborhood == "Beacon Hill"), white_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), black_alone_2020), 
                 pull(filter(df, neighborhood == "Beacon Hill"), asian_alone_2020), 
                 pull(filter(df, neighborhood == "Beacon Hill"), am_native_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), pi_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), hispanic_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), other_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), two_more_2020))

beacon_2010 <- c(pull(filter(df, neighborhood == "Beacon Hill"), total_pop_2010), 
                 pull(filter(df, neighborhood == "Beacon Hill"), white_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), black_alone_2010), 
                 pull(filter(df, neighborhood == "Beacon Hill"), asian_alone_2010), 
                 pull(filter(df, neighborhood == "Beacon Hill"), am_native_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), pi_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), hispanic_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), other_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), two_more_2010)) 

beacon_df <- data.frame(beacon_2010, beacon_2020)

beacon_df$demographic <- 3

beacon_df[beacon_df$beacon_2010 == 8022, "demographic"] <- "Total"
beacon_df[beacon_df$beacon_2010 == 1711, "demographic"] <- "White"
beacon_df[beacon_df$beacon_2010 == 922, "demographic"] <- "Black"
beacon_df[beacon_df$beacon_2010 == 4389, "demographic"] <- "Asian"
beacon_df[beacon_df$beacon_2010 == 37, "demographic"] <- "Native"
beacon_df[beacon_df$beacon_2010 == 93, "demographic"] <- "Pacific Islander"
beacon_df[beacon_df$beacon_2010 == 484, "demographic"] <- "Hispanic"
beacon_df[beacon_df$beacon_2010 == 19, "demographic"] <- "Other"
beacon_df[beacon_df$beacon_2010 == 498, "demographic"] <- "Two or More"

beacon_df <- beacon_df %>% gather("year", "population", beacon_2010, beacon_2020)

beacon_df[beacon_df$year == "beacon_2010", "year"] <- "2010"
beacon_df[beacon_df$year == "beacon_2020", "year"] <- "2020"

# beacon_bar <- ggplot(data = beacon_df, aes(x = demographic, y = population, fill = year)) +
#         geom_col(position = position_dodge()) +
#       labs(x = "Demographic", y = "Population", fill = "Year")

plot(beacon_bar)


central_2020 <- c(pull(filter(df, neighborhood == "Central District"), total_pop_2020), 
                 pull(filter(df, neighborhood == "Central District"), white_alone_2020),
                 pull(filter(df, neighborhood == "Central District"), black_alone_2020), 
                 pull(filter(df, neighborhood == "Central District"), asian_alone_2020), 
                 pull(filter(df, neighborhood == "Central District"), am_native_alone_2020),
                 pull(filter(df, neighborhood == "Central District"), pi_alone_2020),
                 pull(filter(df, neighborhood == "Central District"), hispanic_alone_2020),
                 pull(filter(df, neighborhood == "Central District"), other_alone_2020),
                 pull(filter(df, neighborhood == "Central District"), two_more_2020))

central_2010 <- c(pull(filter(df, neighborhood == "Central District"), total_pop_2010), 
                 pull(filter(df, neighborhood == "Central District"), white_alone_2010),
                 pull(filter(df, neighborhood == "Central District"), black_alone_2010), 
                 pull(filter(df, neighborhood == "Central District"), asian_alone_2010), 
                 pull(filter(df, neighborhood == "Central District"), am_native_alone_2010),
                 pull(filter(df, neighborhood == "Central District"), pi_alone_2010),
                 pull(filter(df, neighborhood == "Central District"), hispanic_alone_2010),
                 pull(filter(df, neighborhood == "Central District"), other_alone_2010),
                 pull(filter(df, neighborhood == "Central District"), two_more_2010)) 

central_df <- data.frame(central_2010, central_2020)

central_df$demographic <- 3

central_df[central_df$central_2010 == 11296, "demographic"] <- "Total"
central_df[central_df$central_2010 == 6228, "demographic"] <- "White"
central_df[central_df$central_2010 == 2737, "demographic"] <- "Black"
central_df[central_df$central_2010 == 729, "demographic"] <- "Asian"
central_df[central_df$central_2010 == 53, "demographic"] <- "Native"
central_df[central_df$central_2010 == 78, "demographic"] <- "Pacific Islander"
central_df[central_df$central_2010 == 893, "demographic"] <- "Hispanic"
central_df[central_df$central_2010 == 24, "demographic"] <- "Other"
central_df[central_df$central_2010 == 722, "demographic"] <- "Two or More"

central_df <- central_df %>% gather("year", "population", central_2010, central_2020)

central_df[central_df$year == "central_2010", "year"] <- "2010"
central_df[central_df$year == "central_2020", "year"] <- "2020"

# central_bar <- ggplot(data = central_df, aes(x = demographic, y = population, fill = year)) +
#   geom_col(position = position_dodge()) +
#   labs(x = "Demographic", y = "Population", fill = "Year")

plot(central_bar)

international_2020 <- c(pull(filter(df, neighborhood == "International District"), total_pop_2020), 
                  pull(filter(df, neighborhood == "International District"), white_alone_2020),
                  pull(filter(df, neighborhood == "International District"), black_alone_2020), 
                  pull(filter(df, neighborhood == "International District"), asian_alone_2020), 
                  pull(filter(df, neighborhood == "International District"), am_native_alone_2020),
                  pull(filter(df, neighborhood == "International District"), pi_alone_2020),
                  pull(filter(df, neighborhood == "International District"), hispanic_alone_2020),
                  pull(filter(df, neighborhood == "International District"), other_alone_2020),
                  pull(filter(df, neighborhood == "International District"), two_more_2020))

international_2010 <- c(pull(filter(df, neighborhood == "International District"), total_pop_2010), 
                  pull(filter(df, neighborhood == "International District"), white_alone_2010),
                  pull(filter(df, neighborhood == "International District"), black_alone_2010), 
                  pull(filter(df, neighborhood == "International District"), asian_alone_2010), 
                  pull(filter(df, neighborhood == "International District"), am_native_alone_2010),
                  pull(filter(df, neighborhood == "International District"), pi_alone_2010),
                  pull(filter(df, neighborhood == "International District"), hispanic_alone_2010),
                  pull(filter(df, neighborhood == "International District"), other_alone_2010),
                  pull(filter(df, neighborhood == "International District"), two_more_2010)) 

international_df <- data.frame(international_2010, international_2020)

international_df$demographic <- 3

international_df[international_df$international_2010 == 3860, "demographic"] <- "Total Population"
international_df[international_df$international_2010 == 1122, "demographic"] <- "White"
international_df[international_df$international_2010 == 558, "demographic"] <- "Black"
international_df[international_df$international_2010 == 1769, "demographic"] <- "Asian"
international_df[international_df$international_2010 == 37, "demographic"] <- "Native"
international_df[international_df$international_2010 == 46, "demographic"] <- "Pacific Islander"
international_df[international_df$international_2010 == 206, "demographic"] <- "Hispanic"
international_df[international_df$international_2010 == 9, "demographic"] <- "Other"
international_df[international_df$international_2010 == 191, "demographic"] <- "Two or More"

international_df <- international_df %>% gather("year", "population", international_2010, international_2020)

international_df[international_df$year == "international_2010", "year"] <- "2010"
international_df[international_df$year == "international_2020", "year"] <- "2020"

# international_bar <- ggplot(data = international_df, aes(x = demographic, y = population, fill = year)) +
#   geom_col(position = position_dodge()) +
#   labs(x = "Demographic", y = "Population", fill = "Year")

plot(international_bar)

