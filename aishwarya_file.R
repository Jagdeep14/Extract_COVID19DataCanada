library(httr)
library(jsonlite)
library(dplyr)
library(docstring)


request_fun <- function(){
	#' Function to make request to API and returns the response
	url <- 'https://api.opencovid.ca/timeseries'
	request <- GET(url)
	return(request)
}


vaccine_as_per_province <- function(provinceName = 'Canada'){
	#' Function for returning data frame for the Covid - 19 vaccination in different provinces in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the date of the vaccination, province name, administered vaccine and cumulative vaccine.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the Covid - 19 vaccines corresponding to a particular province
	#' 
	#' @examples 
	#' vaccine_as_per_province('Alberta')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	all_avaccine <- json_data$avaccine
	avaccine_data <- data.frame()
	for(i in 1:length(all_avaccine)){
		avaccine_data <- rbind(avaccine_data, data.frame(all_avaccine[[i]]))
	}
	
	# Data Cleaning
	avaccine_data <- avaccine_data %>% 
		rename('administered_vaccine' = 'avaccine', 'date' = 'date_vaccine_administered') %>%
		mutate(date=as.Date(date, format = "%d-%m-%Y")) %>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island"))
	
	avaccine_data <- avaccine_data[, c(3, 4, 1, 2)]
	
	# Data Wrangling
	
	if(tolower(provinceName) == 'canada'){
		return(avaccine_data)
	} else {
		avaccine_data <- avaccine_data %>% 
			filter(tolower(province) == tolower(provinceName))
		return(avaccine_data)
	}
}


cases_as_per_province <- function(provinceName = 'Canada'){
	#' Function for returning data frame for the Covid - 19 cases in different provinces in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the date of the cases, province name, cases and cumulative cases.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the Covid - 19 cases corresponding to a particular province
	#' 
	#' @examples 
	#' cases_as_per_province('Alberta')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	cases <- json_data$cases
	cases_data <- data.frame()
	for(i in 1:length(cases)){
		cases_data <- rbind(cases_data, data.frame(cases[[i]]))
	}
	
	# Data Cleaning
	cases_data <- cases_data %>% 
		rename('date' = 'date_report') %>%
		mutate(date=as.Date(date, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island"))
	
	cases_data <- cases_data[, c(3, 4, 1, 2)]
	
	# Data Wrangling
	
	if(tolower(provinceName) == 'canada'){
		return(cases_data)
	} else {
		cases_data <- cases_data %>% 
			filter(tolower(province) == tolower(provinceName))
		return(cases_data)
	}
}



Yearlycases_as_per_province <- function(provinceName = 'Canada',yearpassed='2020'){
	
	#' Function for returning data frame for the Covid - 19 cases in different provinces as per the year specified in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' Also, the data is returned as per the year. By default the year chosen is 2020.
	#' The returned data is a data frame and contains the columns including the date of the cases, province name, cases and cumulative cases for the year 	    #' specified
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the Covid - 19 cases corresponding to a particular province and year
	#' 
	#' @examples 
	#' Yearlycases_as_per_province('Alberta','2020')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	cases <- json_data$cases
	cases_data <- data.frame()
	for(i in 1:length(cases)){
		cases_data <- rbind(cases_data, data.frame(cases[[i]]))
	}
	
	# Data Cleaning
	cases_data <- cases_data %>% 
		rename('date' = 'date_report') %>%
		mutate(date=as.Date(date, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
		mutate(year = format(date, format = "%Y")) 
	
	cases_data <- cases_data[, c(3, 4, 1, 2, 5)]
	
	# Data Wrangling
	
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