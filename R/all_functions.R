#' @import httr
#' @import lubridate
#' @import jsonlite
#' @import dplyr
#' @keywords internal
request_fun <- function(){
  url <- 'https://api.opencovid.ca/timeseries'
  request <- GET(url)
  return(request)
}
#'
#' @keywords internal
  request_fun_date_summary <- function(date){
  base_url <- 'https://api.opencovid.ca/summary?date='
  url <- gsub(" ", "", paste(base_url, date))
  request <- GET(url)
  return(request)
}
#'
#' @keywords internal
  request_fun_summary_canada <- function(date){
    base_url <- 'https://api.opencovid.ca/summary?loc=canada&date='
    url <- gsub(" ", "", paste(base_url, date))
    request <- GET(url)
    return(request)
  }
#'
#' @keywords internal
  request_fun_summary <- function() {
    url <- "https://api.opencovid.ca/summary"
    request <- GET(url)
    return(request)
  }
#'
#' @keywords internal
  request_fun_pop <- function(){
    url <- 'https://api.opencovid.ca/other?stat=prov'
    request <- GET(url)
    return(request)
  }
#'
#' @export
active_cases <- function(provinceName = 'Canada'){
  #' @title  Returns a dataframe of active Covid-19 cases in desired province of Canada
  #'
  #' @description Perform data wrangling and cleaning using the API for the Covid-19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a  whole.
  #' The returned data is a data frame and contains the columns including the date, province name, active cases, change in active cases,
  #' cumulative cases, cumulative deaths and cumulative recovered.
  #'
  #' @param provinceName a character/string depicting the name of the province
  #'
  #' @return Data frame for the Covid-19 active cases  corresponding to a particular province
  #'
  #' @examples active_cases("Alberta")

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon", "Canada")

  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name in full form!")
  }

  request <- request_fun()
  json_data <- content(request, as  = "parse")
  all_active <- json_data$active
  active_data <- data.frame()

  for(i in 1:length(all_active)){
    active_data <- rbind(active_data, data.frame(all_active[[i]]))
  }

  active_data <- active_data %>% rename(date = date_active) %>%
    mutate(date, date = dmy(date)) %>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))


  active_data <- active_data[,c(6,7,1:5)]

  if(tolower(provinceName) == "canada"){
    return(active_data)
  } else {
    active_data <- active_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(active_data)
  }
}
#'
#'
#' @export
vaccine_as_per_province <- function(provinceName = 'Canada'){
  #' @title Function for returning data frame for the Covid - 19 vaccination in different provinces in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the date of the vaccination, province name, administered vaccine and cumulative vaccine.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame for the Covid - 19 vaccines corresponding to a particular province
  #'
  #' @examples vaccine_as_per_province('Alberta')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  all_avaccine <- json_data$avaccine
  avaccine_data <- data.frame()
  for(i in 1:length(all_avaccine)){
    avaccine_data <- rbind(avaccine_data, data.frame(all_avaccine[[i]]))
  }

  avaccine_data <- avaccine_data %>%
    rename('administered_vaccine' = 'avaccine', 'date' = 'date_vaccine_administered') %>%
    mutate(date=as.Date(date, format = "%d-%m-%Y")) %>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))

  avaccine_data <- avaccine_data[, c(3, 4, 1, 2)]

  if(tolower(provinceName) == 'canada'){
    return(avaccine_data)
  } else {
    avaccine_data <- avaccine_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(avaccine_data)
  }
}
#'
#' @export
cases_as_per_province <- function(provinceName = 'Canada'){
  #' @title Function for returning data frame for the Covid - 19 cases in different provinces in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the date of the cases, province name, cases and cumulative cases.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame for the Covid - 19 cases corresponding to a particular province
  #'
  #' @examples cases_as_per_province('Alberta')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  cases <- json_data$cases
  cases_data <- data.frame()
  for(i in 1:length(cases)){
    cases_data <- rbind(cases_data, data.frame(cases[[i]]))
  }

  cases_data <- cases_data %>%
    rename('date' = 'date_report') %>%
    mutate(date=as.Date(date, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))

  cases_data <- cases_data[, c(3, 4, 1, 2)]

  if(tolower(provinceName) == 'canada'){
    return(cases_data)
  } else {
    cases_data <- cases_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(cases_data)
  }
}
#'
#' @export
Yearlycases_as_per_province <- function(provinceName = 'Canada',yearpassed='2020'){
  #' @title Function for returning data frame for the Covid - 19 cases in different provinces as per the year specified in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' Also, the data is returned as per the year. By default the year chosen is 2020.
  #' The returned data is a data frame and contains the columns including the date of the cases, province name, cases and cumulative cases for the year 	    #' specified
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #' @param yearpassed a character/ string depicting the year
  #'
  #' @return Data frame for the Covid - 19 cases corresponding to a particular province and year
  #'
  #' @examples Yearlycases_as_per_province('Alberta','2020')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  year_check <- gsub(" ", "", paste(yearpassed, "-01-01"))
  if(is.na(ymd(year_check))){
    stop("Please enter a valid year as string in format yyyy !!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  cases <- json_data$cases
  cases_data <- data.frame()
  for(i in 1:length(cases)){
    cases_data <- rbind(cases_data, data.frame(cases[[i]]))
  }

  cases_data <- cases_data %>%
    rename('date' = 'date_report') %>%
    mutate(date=as.Date(date, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
    mutate(year = format(date, format = "%Y"))

  cases_data <- cases_data[, c(3, 4, 1, 2, 5)]

  if(yearpassed %!in% unique(cases_data$year)){
    stop("There is no data for this year!")
  }

  if(tolower(provinceName) == 'canada'){
    cases_data <- cases_data %>%
      filter(year==yearpassed)
    return(cases_data)
  } else {
    cases_data <- cases_data %>%
      filter(tolower(province) == tolower(provinceName),year==yearpassed)
    return(cases_data)
  }
}
#'
#' @export
cumulativevaccine_as_per_province <- function(provinceName = 'Canada'){
  #' @title Function for returning data frame for the Covid - 19 vaccination which is cumulative in different provinces in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the date of the vaccination, province name,cumulative vaccine and cvaccine.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame for the Covid - 19 cummulative vaccines corresponding to a particular province
  #'
  #' @examples cumulativevaccine_as_per_province('Alberta')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  cvaccine <- json_data$cvaccine
  cvaccine_data <- data.frame()
  for(i in 1:length(cvaccine)){
    cvaccine_data <- rbind(cvaccine_data, data.frame(cvaccine[[i]]))
  }

  cvaccine_data <- cvaccine_data %>%
    rename('date' = 'date_vaccine_completed') %>%
    mutate(date=as.Date(date, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))

  cvaccine_data <- cvaccine_data[, c(3, 4, 1, 2)]

  if(tolower(provinceName) == 'canada'){
    return(cvaccine_data)
  } else {
    cvaccine_data <- cvaccine_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(cvaccine_data)
  }
}
#'
#' @export
vaccine_distribution<- function(provinceName = 'Canada'){
  #' @title Function for returning data frame for the Covid - 19 vaccination distribution in different provinces in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the date distribution, province name,cumulative vaccine and count of distribution of     #' vaccines.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame for the Covid - 19 vaccines distribution corresponding to a particular province
  #'
  #' @examples vaccine_distribution('Alberta')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  dvaccine <- json_data$dvaccine
  dvaccine_data <- data.frame()
  for(i in 1:length(dvaccine)){
    dvaccine_data <- rbind(dvaccine_data, data.frame(dvaccine[[i]]))
  }

  dvaccine_data <- dvaccine_data %>%
    rename('distribution_date' = 'date_vaccine_distributed') %>%
    mutate(distribution_date=as.Date(distribution_date, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))

  dvaccine_data <- dvaccine_data[, c(2, 4, 1, 3)]

  if(tolower(provinceName) == 'canada'){
    return(dvaccine_data)
  } else {
    dvaccine_data <- dvaccine_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(dvaccine_data)
  }
}
#'
#' @export
yearly_vaccine_distribution<- function(provinceName = 'Canada',distyear='2020'){
  #' @title Function for returning data frame for the Covid - 19 vaccination distribution in different provinces in Canada according to the year.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the date distribution, province name,cumulative vaccine and count of distribution of     #' vaccines.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #' @param distyear a character/ string showing the year
  #'
  #' @return Data frame for the Covid - 19 vaccines distribution corresponding to a particular province and year
  #'
  #' @examples yearly_vaccine_distribution('Alberta','2020')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  year_check <- gsub(" ", "", paste(distyear, "-01-01"))
  if(is.na(ymd(year_check))){
    stop("Please enter a valid year as string in format yyyy !!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  dvaccine <- json_data$dvaccine
  dvaccine_data <- data.frame()
  for(i in 1:length(dvaccine)){
    dvaccine_data <- rbind(dvaccine_data, data.frame(dvaccine[[i]]))
  }

  dvaccine_data <- dvaccine_data %>%
    rename('distribution_date' = 'date_vaccine_distributed') %>%
    mutate(distribution_date=as.Date(distribution_date, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
    mutate(year = format(distribution_date, format = "%Y"))

  dvaccine_data <- dvaccine_data[, c(2, 4, 1, 3, 5)]

  if(distyear %!in% unique(dvaccine_data$year)){
    stop("There is no data for this year!")
  }

  if(tolower(provinceName) == 'canada'){
    dvaccine_data <- dvaccine_data %>%
      filter(year==distyear)
    return(dvaccine_data)
  } else {
    dvaccine_data <- dvaccine_data %>%
      filter(tolower(province) == tolower(provinceName), year == distyear)
    return(dvaccine_data)
  }
}
#'
#' @export
mortality <- function(provinceName = 'Canada'){
  #' @title Function for returning data frame for the Covid - 19 mortality rate in different provinces in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the date of the death, province name, cumulative deaths and deaths.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame for the deaths because of COVID -19 corresponding to a particular province
  #'
  #' @examples mortality('Alberta')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  mortaility <- json_data$mortality
  mortaility_data <- data.frame()
  for(i in 1:length(mortaility)){
    mortaility_data <- rbind(mortaility_data, data.frame(mortaility[[i]]))
  }

  mortaility_data <- mortaility_data %>%
    rename('death_date' = 'date_death_report') %>%
    mutate(death_date=as.Date(death_date, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))

  mortaility_data <- mortaility_data[, c(2, 4, 1, 3)]

  if(tolower(provinceName) == 'canada'){
    return(mortaility_data)
  } else {
    mortaility_data <- mortaility_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(mortaility_data)
  }
}
#'
#' @export
yearly_deaths<- function(provinceName = 'Canada', dyear='2020'){
  #' @title Function for returning data frame for the Covid - 19 deaths in different provinces in Canada according to the year.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the date of the death, province name, cumulative deaths and deaths.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #' @param dyear a character/ string showing the year
  #'
  #' @return Data frame for the Covid - 19 deaths corresponding to a particular province and year
  #'
  #' @examples yearly_deaths('Alberta','2020')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }
  year_check <- gsub(" ", "", paste(dyear, "-01-01"))
  if(is.na(ymd(year_check))){
    stop("Please enter a valid year as string in format yyyy !!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  mortaility <- json_data$mortality
  mortaility_data <- data.frame()
  for(i in 1:length(mortaility)){
    mortaility_data <- rbind(mortaility_data, data.frame(mortaility[[i]]))
  }

  mortaility_data <- mortaility_data %>%
    rename('death_date' = 'date_death_report') %>%
    mutate(death_date=as.Date(death_date, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
    mutate(year = format(death_date, format = "%Y"))

  mortaility_data <- mortaility_data[, c(2, 4, 1, 3, 5)]

  if(dyear %!in% unique(mortaility_data$year)){
    stop("There is no data for this year!")
  }

  if(tolower(provinceName) == 'canada'){
    mortaility_data <- mortaility_data %>%
      filter(year == dyear)
    return(mortaility_data)
  } else {
    mortaility_data <- mortaility_data %>%
      filter(tolower(province) == tolower(provinceName), year == dyear)
    return(mortaility_data)
  }
}
#'
#' @export
recovered <- function(provinceName = 'Canada'){
  #' @title Function for returning data frame for the Covid - 19 recovery rate in different provinces in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the recovery date, province, cumulative recovered, and count of recovery.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame for the recovered cases after COVID -19 corresponding to a particular province
  #'
  #' @examples recovered('Alberta')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  recovered <- json_data$recovered
  recovered_data <- data.frame()
  for(i in 1:length(recovered)){
    recovered_data <- rbind(recovered_data, data.frame(recovered[[i]]))

  }

  recovered_data <- recovered_data %>%
    mutate(date_recovered=as.Date(date_recovered, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))

  recovered_data <- recovered_data[, c(2, 3, 1, 4)]

  if(tolower(provinceName) == 'canada'){
    return(recovered_data)
  } else {
    recovered_data <- recovered_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(recovered_data)
  }
}
#'
#' @export
yearly_recovered<- function(provinceName = 'Canada', ryear='2020'){
  #' @title Function for returning data frame for the Covid - 19 recovered cases in different provinces in Canada according to the year.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the recovery date, province, cumulative recovered, and count of recovery.
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #' @param ryear a character/ string showing the year
  #'
  #' @return Data frame for the Covid - 19 recovery corresponding to a particular province and year
  #'
  #' @examples yearly_recovered('Alberta','2020')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }
  year_check <- gsub(" ", "", paste(ryear, "-01-01"))
  if(is.na(ymd(year_check))){
    stop("Please enter a valid year as string in format yyyy !!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  recovered <- json_data$recovered
  recovered_data <- data.frame()
  for(i in 1:length(recovered)){
    recovered_data <- rbind(recovered_data, data.frame(recovered[[i]]))

  }

  recovered_data <- recovered_data %>%
    mutate(date_recovered=as.Date(date_recovered, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
    mutate(year = format(date_recovered, format = "%Y"))

  recovered_data <- recovered_data[, c(2, 3, 1, 4,5)]
  if(ryear %!in% unique(recovered_data$year)){
    stop("There is no data for this year!")
  }

  if(tolower(provinceName) == 'canada'){
    recovered_data <- recovered_data %>%
      filter(year == ryear)
    return(recovered_data)
  } else {
    recovered_data <- recovered_data %>%
      filter(tolower(province) == tolower(provinceName), year == ryear)
    return(recovered_data)
  }
}
#'
#' @export
testing <- function(provinceName = 'Canada'){
  #' @title Function for returning data frame for the Covid - 19 testing rate in different provinces in Canada.
  #'
  #' @description Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada.
  #' It processes the API and returns the data corresponding to one province
  #' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
  #' The returned data is a data frame and contains the columns including the testing date, province, cumulative testing, and count of testing
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame for the recovered cases after COVID -19 corresponding to a particular province
  #'
  #' @examples testing('Alberta')

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name that too in its full form!")
  }

  request <- request_fun()

  json_data <- content(request, as  = "parse")
  testing <- json_data$testing
  testing_data <- data.frame()
  for(i in 1:length(testing)){
    testing_data <- rbind(testing_data, data.frame(testing[[i]]))
  }

  testing_data <- testing_data %>%
    mutate(date_testing=as.Date(date_testing, format = "%d-%m-%Y"))%>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))

  testing_data <- testing_data[, c(2, 3, 1, 4)]

  if(tolower(provinceName) == 'canada'){
    return(testing_data)
  } else {
    testing_data <- testing_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(testing_data)
  }
}
#'
#' @export
summary_of_cases <- function(provinceName = "Canada") {
  #' @title Covid-19 particular day\'s individual summary for every Canadian province
  #'
  #' @description Summary for all the provinces relating to Covid-19 active cases, avaccine, cvaccine and dvaccine. Moreover, is discloses   #' the count of people who have recovered and who were tested.
  #' It processes the API and returns the json data corresponding to entered province. exceptional user input have been handled. If
  #' Canada is passed as an argument the output displays the data for all the Canadian provinces.The returned data is a data frame and,
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame with all the columns for the entire summary for all the provinces individually would be returned. The columns       #' includes the date, province name, active_cases, active_cases_change, avaccine, cases,cumulative_avaccine,
  #' cumulative_cases,cumulative_cvaccine, cumulative_deaths,   cumulative_dvaccine, cumulative_recovered, cumulative_testing, cvaccine,
  #' deaths, dvaccine, recovered, testing and testing_info.
  #'
  #' @examples summary_of_cases("Ontario")

  prov <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon", "Canada")

  `%!in%` <- Negate(`%in%`)
  if (tolower(provinceName) %!in% tolower(prov)) {
    stop("Please enter a valid province name in full form!")
  }

  request <- request_fun_summary()
  json_data <- content(request, as = "parse")
  active_data <- data.frame()

  for (i in 1:length(json_data[[1]])) {
    active_data <- rbind(active_data, data.frame(json_data[[1]][[i]]))
  }

  active_data <- active_data %>%
    mutate(date, date = dmy(date)) %>%
    mutate(
      province = replace(province, province %in% c("BC"), "British Columbia"),
      province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
      province = replace(province, province %in% c("NWT"), "Northwest Territories"),
      province = replace(province, province %in% c("PEI"), "Prince Edward Island")
    )


  active_data <- active_data[, c(13, 16, 1:12, 14:15, 17:19)]
  if (tolower(provinceName) == "canada") {
    return(active_data)
  } else {
    active_data <- active_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(active_data)
  }
}
#'
#' @export
summary_of_peaktime <- function(provinceName = "Canada", date = '01-09-2020') {
  #' @title Covid-19 peak time which is September 2020 individual summary for every Canadian province
  #'
  #' @description Summary for all the provinces relating to Covid-19 peak time in Canada which is the month of September 2020 or on
  #' desired date: the
  #' active cases, avaccine, cvaccine and dvaccine. Moreover, is discloses the count of people who have recovered and who were tested on
  #' September 01, 2020.
  #' It processes the API and returns the json data corresponding to entered province. If
  #' 'Canada' is passed as an argument the output displays the data for all the Canadian provinces.The returned data is a data frame and,
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #' @param date desired date - to get the summary up to this date
  #'
  #' @return Data frame with all the columns for the entire summary for all the provinces individually would be returned. The columns
  #' includes the date, province name, active_cases, active_cases_change, avaccine, cases,cumulative_avaccine,
  #' cumulative_cases,cumulative_cvaccine, cumulative_deaths,   cumulative_dvaccine, cumulative_recovered, cumulative_testing, cvaccine,
  #' deaths, dvaccine, recovered, testing and testing_info.
  #'
  #' @examples summary_of_peaktime("Ontario")

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon", "Canada")

  `%!in%` <- Negate(`%in%`)
  if (tolower(provinceName) %!in% tolower(prov)) {
    stop("Please enter a valid province name in full form!")
  }

  if(is.na(dmy(date))){
    stop("Please enter a valid date as string in format dd-mm-yyyy !!")
  }
  request <- request_fun_date_summary(date)
  json_data <- content(request, as = "parse")
  active_data <- data.frame()

  for(i in 1:length(json_data[[1]])){

    active_data <- rbind(active_data, data.frame(json_data[[1]][[i]]))
  }

  active_data <- active_data %>%
    mutate(date, date = dmy(date)) %>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))


  active_data <- active_data[,c(13,16,1:12,14:15,17:19)]

  if(tolower(provinceName) == "canada"){
    return(active_data)
  } else {
    active_data <- active_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(active_data)
  }

}
#'
#' @export
summary_of_canada <- function(date = '01-09-2020'){
  #' @title Covid-19 cases summary on 01 September 2020 or on desired date for entire Canada
  #'
  #' @description Summary for the entire country relating to Covid-19 peak time in Canada which is the month of September 2020 : the
  #' active cases, avaccine, cvaccine and dvaccine. Moreover, is discloses the count of people who have recovered and who were tested on
  #' September 01, 2020.
  #' It processes the API and returns the json data corresponding to the summary from all the data for all the Canadian provinces.The
  #' returned data is a data frame and,
  #'
  #' @param date desired date - to get the summary up to this date
  #'
  #'
  #' @return Data frame with all the columns for the entire summary for all the provinces individually would be returned. The columns
  #' includes the date, province name, active_cases, active_cases_change, avaccine, cases,cumulative_avaccine,
  #' cumulative_cases,cumulative_cvaccine, cumulative_deaths,   cumulative_dvaccine, cumulative_recovered, cumulative_testing, cvaccine,
  #' deaths, dvaccine, recovered, testing and testing_info.
  #'
  #' @examples summary_of_canada()

  if(is.na(dmy(date))){
    stop("Please enter a valid date as string in format dd-mm-yyyy !!")
  }

  request <- request_fun_summary_canada(date)
  json_data <- content(request, as  = "parse")
  all_active <- json_data$active
  active_data <- data.frame()

  for(i in 1:length(json_data[[1]])){
    active_data <- rbind(active_data, data.frame(json_data[[1]][[i]]))
  }

  active_data <- active_data %>%
    mutate(date, date = dmy(date))

  active_data <- active_data[,c(13,16,1:12,14:15,17:19)]
  return(active_data)
}
#'
#' @export
province_population <- function(provinceName = 'Canada'){
  #' @title Total population segregated for every Canadian province.
  #'
  #' @description The total population count for every province is individual mentioned.
  #' It processes the API and returns the json data corresponding to entered province.
  #' If 'Canada' is passed as an argument the output displays the data for all the Canadian
  #' provinces.The returned data is a data frame and,
  #'
  #' @param provinceName a character/ string depicting the name of the province
  #'
  #' @return Data frame with all the columns for the entire summary for all the provinces individually would be
  #' returned. The columns includes the date, province name, active_cases, active_cases_change, avaccine,
  #' cases,cumulative_avaccine, cumulative_cases,cumulative_cvaccine, cumulative_deaths,   cumulative_dvaccine,
  #' cumulative_recovered, cumulative_testing, cvaccine, deaths, dvaccine, recovered, testing and testing_info.
  #'
  #' @examples province_population("Ontario")

  prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon", "Canada")

  `%!in%` <- Negate(`%in%`)
  if(tolower(provinceName) %!in% tolower(prov)){
    stop("Please enter a valid province name in full form!")
  }

  request <- request_fun_pop()
  json_data <- content(request, as  = "parse")
  all_active <- json_data$active
  active_data <- data.frame()


  for(i in 1:length(json_data[[1]])){

    active_data <- rbind(active_data, data.frame(json_data[[1]][[i]]))
  }


  active_data <- active_data %>%
    mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
           province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
           province = replace(province, province %in% c("NWT"), "Northwest Territories"),
           province = replace(province, province %in% c("PEI"), "Prince Edward Island"))


  active_data <- active_data[,c(2,1)]
  if(tolower(provinceName) == "canada"){
    return(active_data)
  } else {
    active_data <- active_data %>%
      filter(tolower(province) == tolower(provinceName))
    return(active_data)
  }

}
