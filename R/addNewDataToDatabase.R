
# Requirement -----------------------------------------------------------------------------------------------------

# Clear the Workspace
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/clearWorkspace.R")  # Clear the Workspace
clearWorkspace()

# Load Requirement R Source (Internet Require)
source(file = "https://raw.githubusercontent.com/shirazipooya/Useful-R-Functions/master/R/installPackages.R") # Check, Install and Load CRAN Packages

# Load Required Packages
installPackages("tidyverse", "lubridate")
library(package = "dplyr")

# Load Required Function
# 01: Data Cleansing
dataCleansing <- function(data,
                          startDate  = "2018-08-01 00:00",
                          endDate    = "2018-08-31 21:00",
                          formatDate = "%Y-%m-%d %H:%M")
{
    # Create Base Date
    baseDate <- base::seq.POSIXt(from = base::as.POSIXct(x = startDate, tz = "GMT", format = formatDate),
                                 to   = base::as.POSIXct(x = endDate,   tz = "GMT", format = formatDate),
                                 by   = 3 * 3600) %>%
        base::as.data.frame.POSIXct()
    base::names(baseDate) <- "date"
    
    # Extract Info.
    station_i <- base::unique(x = data$station_i)
    lat <- base::unique(x = data$lat)
    lon <- base::unique(x = data$lon)
    hight <- base::unique(x = data$hight)
    
    # Find Minute Error and Adj. 
    for (mE in base::which(lubridate::minute(data$date) != 0))
    {
        data$date[mE] <- data$date[mE] - lubridate::minute(data$date[mE]) * 60
    }
    
    # Find Hour Error and Adj.
    hEn <- base::which(lubridate::hour(data$date) %in% c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23))
    data$date[hEn] <- data$date[hEn] - lubridate::hour(data$date[hEn]) * 3600
    
    # Cheek Missing Date:
    if (base::max(base::dim(baseDate)) != base::max(base::dim(data)))
    {
        data <- dplyr::left_join(x = baseDate, y = data, by = "date")
    }
    
    # Replace Info.
    data$station_i <- station_i
    data$lat <- lat
    data$lon <- lon
    data$hight <- hight
    
    # return clean data:
    return(data)
}

# Choose New *.csv File -------------------------------------------------------------------------------------------

# Bind All *.csv Files by Row
naStrings <- c("null", "nul", "nu", "n", "station_i", "lat", "lon", "hight", "data","dd", "ff", "t", "td",
               "p0", "p", "a", "ppp", "rrr", "ww", "w1", "w2", "tmin", "tmax", "nh", "cl", "cm", "ch", "rrr24")

# Select New *.csv File
rawData <- read.csv(file = choose.files(caption = "Select New *.csv File:"), header = TRUE, skip = 2, na.strings = naStrings) %>% 
    dplyr::select(-X) %>% 
    dplyr::filter(!station_i %in% c(NA)) # Remove Empty Rows and Reset Row Names
base::row.names(rawData) <- NULL

# Change Class of Date Cloumn
base::colnames(rawData)[5] <- "date"
rawData[,"date"] <- lubridate::ymd_hm(rawData[,"date"])


# Data Cleansing --------------------------------------------------------------------------------------------------

# Load RBSN-3Hourly-rawData.RDS and Use dataCleansing Function
newData <- rawData %>%
    base::split(f = .$station_i) %>%
    base::lapply(FUN = dataCleansing) %>%
    dplyr::bind_rows(.id = NULL)

# Add New Data to Database ----------------------------------------------------------------------------------------

oldData <- readRDS(file = choose.files(caption = "Select Old *.RDS File:"))

data <- list(oldData, newData) %>%
    dplyr::bind_rows(.id = NULL) %>% 
    arrange(station_i, date)

# Save data
base::saveRDS(object = data, file = "data/RBSN-3Hourly-toAugust2018.RDS")
