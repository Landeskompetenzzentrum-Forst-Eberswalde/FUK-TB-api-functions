# Load the required libraries
library(httr)
library(jsonlite)

library(dotenv)

#setwd(paste(getwd(), "/R", sep = ""))

load_dot_env(file = "../.env")



#  get_data function
post_request <- function(url, body, header = c()) {
  response <- POST(
    url = url,
    body = body,
    encode = "json",
    add_headers(.headers = header),
    verbose()
  )
  return(content(response, as = "parsed"))
}

response <- post_request("https://thingsboard.gruenecho.de/api/auth/login", list(username = Sys.getenv("USERNAME"), password = Sys.getenv("PASSWORD")))
token <- response$token
print(token)

header <- c(
  'X-Authorization' = paste("Bearer", token, sep = " ")
)


# Now Timestamp
now <- as.numeric(Sys.time()) * 1000
# a year ago Timesamp
a_year_ago <- now - 31536000000


query <- list(
    keys = "B_Temp",
    limit = "10000",
    startTs = round(a_year_ago),
    endTs = round(now)
)
data <- GET('https://thingsboard.gruenecho.de/api/plugins/telemetry/DEVICE/5b68c260-695a-11ef-a966-a56844bfea6c/values/timeseries?', query = query, add_headers(.headers = header), verbose())
res <- content(data, as = "parsed")

# parse value attribute in  res["B_Temp"] to float
b_temp <- lapply(res$B_Temp, function(x) {
  x$value <- as.numeric(x$value)
  return(x)
})

# Convert the list to a data frame
df <- do.call(rbind, lapply(b_temp, as.data.frame))

df$ts <- as.POSIXct(df$ts / 1000, origin = "1970-01-01", tz = "UTC")

# Plot the data
plot(x = df$ts, y = df$value, type = "l", xlab = "Time / Date", ylab = "Temperature (°C)", main = "Temperature Over Time")