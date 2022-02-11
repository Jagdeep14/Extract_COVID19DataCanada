library(httr)
library(jsonlite)
library(dplyr)

url <- 'https://api.opencovid.ca/timeseries?stat=cases&loc=prov&date=01-09-2020'
get_link <- GET(url)
content(get_link)
