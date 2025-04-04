# Load necessary libraries
if (!require("httr")) {
  install.packages("httr")
}
library(httr)

if (!require("dotenv")) {
  install.packages("dotenv")
}
library(dotenv)

# ADD readr
if (!require("readr")) {
  install.packages("readr")
}
library(readr)

load_dot_env(file = paste(getwd(), "/.env", sep = ""))


# GET DATA
# About querying: https://docs.postgrest.org/en/v12/references/api/tables_views.html
response <- GET(
    url = paste(Sys.getenv("ICP_SERVER_URL"), "d_accuracy", sep = ""),
    query = list(
        "or" = "(code.eq.2)"
    ),
    add_headers('apikey' =  Sys.getenv("ANON_KEY")),
    add_headers('Accept-Profile' = 'icp_dictionaries'), # Name of the Schema
    add_headers('Accept' = 'text/csv')
)
#content <- content(response, as = "parsed")
content <- content(response, type = "text/csv")