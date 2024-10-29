# Load necessary libraries
if (!require("httr")) {
  install.packages("httr")
}
library(httr)

library(jsonlite)

library(dotenv)

# Set the working directory to the R folder
#setwd(paste(getwd(), "/R", sep = ""))

load_dot_env(file = "../.env")

# SETUP ---------------------

# Create the directory to save
if (!dir.exists("../tmp")) {
  dir.create("../tmp", recursive = TRUE)
}

# fit the sensors interval
increment <- "1 sec"

# Attribute of the device to retrieve data for
attribute <- "sin"

# To timestamp in milliseconds
to_timestamp <- round(now <- as.numeric(Sys.time()) * 1000)
# From timestamp in milliseconds (last 5 minutes)
from_timestamp <- round(now - 300000)


# !SETUP --------------------



# Login to ThingsBoard
response <- POST(
  url = "https://thingsboard.gruenecho.de/api/auth/login",
  body = list(
    username = Sys.getenv("USERNAME"),
    password = Sys.getenv("PASSWORD")
  ),
  encode = "json",
  verbose()
)
content <- content(response, as = "parsed")
token <- content$token

if (is.null(token)) {
  stop("Failed to retrieve token. Please check your credentials.")
}

header <- c(
  "X-Authorization" = paste("Bearer", token, sep = " ")
)


# Define the start and end timestamps



query <- list(
  keys = attribute,
  limit = "50000",
  startTs = from_timestamp,
  endTs = to_timestamp
)

# Make the GET request to retrieve telemetry data for the device
url <- paste0(
  "https://thingsboard.gruenecho.de/api/plugins/telemetry/DEVICE/",
  Sys.getenv("DEVICE_ID"),
  "/values/timeseries"
)
data <- GET(url, query = query, add_headers(.headers = header), verbose())
res <- content(data, as = "parsed")


# Check if the response is empty
if (length(res) == 0) {
  stop("No data found for the specified time range.")
}



# Check if the response has attribute data
if (length(get(attribute, res)) == 0) {
  stop("No data found for the specified attribute.")
}

# Parse value attribute in res["sin"] to float
data_list <- lapply(get(attribute, res), function(x) {
  x$value <- as.numeric(x$value)
  return(x)
})

# Convert the list to a data frame
df <- do.call(rbind, lapply(data_list, as.data.frame))

# Convert the timestamp to a POSIXct object
df$ts <- as.POSIXct(round(df$ts / 1000), origin = "1970-01-01", tz = "UTC")

# Save the plot to ../temp directory
png(
  paste0(
    "../tmp/",
    Sys.getenv("DEVICE_ID"),
    "_",
    Sys.Date(),
    ".png"
  )
)

# Plot the data
plot(
  x = df$ts,
  y = df$value,
  type = "p",
  xlab = "Time",
  ylab = attribute,
  main = paste("Test Device (", attribute, ")", sep = "")
)

# Close the PNG device
dev.off()

# Pause and ask the user to continue
readline(prompt = "Press [Enter] to continue...")


## Fill missing values in a time series data frame with linear interpolation
# Install and load the zoo package
if (!require("zoo")) {
  install.packages("zoo")
}
library(zoo)

# Create a complete sequence of timestamps at 1-second intervals
complete_ts <- seq(from = min(df$ts), to = max(df$ts), by = increment)

# Merge the sequence with your existing data frame
df_complete <- data.frame(ts = complete_ts)
df_all <- merge(df_complete, df, by = "ts", all.x = TRUE)

# Identify the interpolated values
is_interpolated <- is.na(df_all$value)

# Fill gaps of missing values in the time series
df_all$value <- na.approx(df_all$value, x = df_all$ts, na.rm = FALSE)

# Save the plot to ../temp directory
png(
  paste0(
    "../tmp/",
    Sys.getenv("DEVICE_ID"),
    "_",
    Sys.Date(),
    "_interpolated.png"
  )
)

# Plot the data
plot(
  x = df_all$ts,
  y = df_all$value,
  type = "p",
  xlab = "Time",
  ylab = attribute,
  main = paste("Test Device (", attribute, ")", sep = ""),
  col = ifelse(is_interpolated, "red", "black")
)

# Add a legend
legend(
  "topright",
  legend = c("Original", "Interpolated"),
  col = c("black", "red"),
  pch = 1
)

# Close the PNG device
dev.off()

# Pause and ask the user to continue
readline(prompt = "Press [Enter] to continue...")


## Send the interpolated data to Thingsboard

# Filter the data frame to include only interpolated values
df_interpolated <- df_all[is_interpolated, ]

# Create a list of data points to send to ThingsBoard
interpolated_data_points <- lapply(seq_len(nrow(df_interpolated)), function(i) {
  list(
    ts = as.numeric(
      df_interpolated$ts[i]
    ) * 1000, # Convert the timestamp to milliseconds
    values = setNames(
      list(
        df_interpolated$value[i]),
        paste0(attribute, "_interpolated")
      ) # Use the attribute variable
  )
})

# Stop if there are no interpolated values
if (length(interpolated_data_points) == 0) {
  stop("No interpolated values found.")
}

## Send the interpolated data to ThingsBoard
# url = `https://thingsboard.gruenecho.de:443/api/v1/${process.env.DEVICE_ACCESS_TOKEN}/telemetry`;
url <- paste0(
  "https://thingsboard.gruenecho.de/api/v1/",
  Sys.getenv("DEVICE_ACCESS_TOKEN"),
  "/telemetry"
)
response <- POST(
  url = url,
  body = toJSON(interpolated_data_points, auto_unbox = TRUE),
  encode = "json",
  add_headers(.headers = header),
  verbose()
)

# Check the response
if (http_error(response)) {
  stop(
    "HTTP error: ",
    status_code(response),
    " - ",
    http_status(response)$message
  )
} else {
  print("Data successfully sent to ThingsBoard.")
}

print("Done")