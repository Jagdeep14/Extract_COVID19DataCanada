library(httr)
library(jsonlite)
library(dplyr)
library(docstring)

# Function for requesting using the API
request_fun <- function(){
	#' Function to make request to API and returns the response
	url <- 'https://api.opencovid.ca/timeseries'
	request <- GET(url)
	return(request)
}


# 1. Function for getting information about the vaccine as per the province
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


#1. TESTING of the function 

test_that("testing of vaccine_as_per_province", {
	expected <- vaccine_as_per_province("Alberta")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "Alberta")
	expect_error(vaccine_as_per_province("BC"), "Please enter a valid province name that too in its full form!")
})


# 2. Function for getting the information about the cases as per the province
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


# 2. TESTING the function

test_that("testing of cases_as_per_province", {
	expected <- cases_as_per_province("British Columbia")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "British Columbia")
	expect_error(cases_as_per_province("BC"), "Please enter a valid province name that too in its full form!")
})

# 3. Function for getting the information as per the province and year	
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
	#' @param yearpassed a character/ string depicting the year
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


# 3. TESTING the function

test_that("testing of Yearlycases_as_per_province", {
	expected <- Yearlycases_as_per_province("British Columbia","2021")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "British Columbia")
	expect_equal(unique(expected$year), "2021")
	expect_error(Yearlycases_as_per_province("BC"), "Please enter a valid province name that too in its full form!")
})



# 4. Function for providing details of the cumulative vaccines as per the province
cumulativevaccine_as_per_province <- function(provinceName = 'Canada'){
	#' Function for returning data frame for the Covid - 19 vaccination which is cumulative in different provinces in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the date of the vaccination, province name,cumulative vaccine and cvaccine.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the Covid - 19 cummulative vaccines corresponding to a particular province
	#' 
	#' @examples 
	#' cumulativevaccine_as_per_province('Alberta')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	cvaccine <- json_data$cvaccine
	cvaccine_data <- data.frame()
	for(i in 1:length(cvaccine)){
		cvaccine_data <- rbind(cvaccine_data, data.frame(cvaccine[[i]]))
	}
	
	# Data Cleaning
	cvaccine_data <- cvaccine_data %>% 
		rename('date' = 'date_vaccine_completed') %>%
		mutate(date=as.Date(date, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island"))
	
	cvaccine_data <- cvaccine_data[, c(3, 4, 1, 2)]
	
	# Data Wrangling
	
	if(tolower(provinceName) == 'canada'){
		return(cvaccine_data)
	} else {
		cvaccine_data <- cvaccine_data %>% 
			filter(tolower(province) == tolower(provinceName))
		return(cvaccine_data)
	}
}


# 4. TESTING the function

test_that("testing of cumulativevaccine_as_per_province", {
	expected <- cumulativevaccine_as_per_province("Alberta")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "Alberta")
	expect_error(cumulativevaccine_as_per_province("BC"), "Please enter a valid province name that too in its full form!")
})



# 5. Function for displaying information for the distribution of the vaccine
vaccine_distribution<- function(provinceName = 'Canada'){
	#' Function for returning data frame for the Covid - 19 vaccination distribution in different provinces in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the date distribution, province name,cumulative vaccine and count of distribution of     #' vaccines.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the Covid - 19 vaccines distribution corresponding to a particular province
	#' 
	#' @examples 
	#' vaccine_distribution('Alberta')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	dvaccine <- json_data$dvaccine
	dvaccine_data <- data.frame()
	for(i in 1:length(dvaccine)){
		dvaccine_data <- rbind(dvaccine_data, data.frame(dvaccine[[i]]))
	}
	
	# Data Cleaning
	dvaccine_data <- dvaccine_data %>% 
		rename('distribution_date' = 'date_vaccine_distributed') %>%
		mutate(distribution_date=as.Date(distribution_date, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island"))
	
	dvaccine_data <- dvaccine_data[, c(2, 4, 1, 3)]
	
	# Data Wrangling
	
	if(tolower(provinceName) == 'canada'){
		return(dvaccine_data)
	} else {
		dvaccine_data <- dvaccine_data %>% 
			filter(tolower(province) == tolower(provinceName))
		return(dvaccine_data)
	}
}


# 5. TESTING the function

test_that("testing of vaccine_distribution", {
	expected <- vaccine_distribution("Alberta")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "Alberta")
	expect_error(vaccine_distribution("BC"), "Please enter a valid province name that too in its full form!")
})


# 6. Function showing the distribution of the vaccines as per the province yearly
yearly_vaccine_distribution<- function(provinceName = 'Canada',distyear='2020'){
	#' Function for returning data frame for the Covid - 19 vaccination distribution in different provinces in Canada according to the year.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the date distribution, province name,cumulative vaccine and count of distribution of     #' vaccines.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' @param distyear a character/ string showing the year		
	#' 
	#' @return Data frame for the Covid - 19 vaccines distribution corresponding to a particular province and year
	#' 
	#' @examples 
	#' yearly_vaccine_distribution('Alberta','2020')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	dvaccine <- json_data$dvaccine
	dvaccine_data <- data.frame()
	for(i in 1:length(dvaccine)){
		dvaccine_data <- rbind(dvaccine_data, data.frame(dvaccine[[i]]))
	}
	
	# Data Cleaning
	dvaccine_data <- dvaccine_data %>% 
		rename('distribution_date' = 'date_vaccine_distributed') %>%
		mutate(distribution_date=as.Date(distribution_date, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
		mutate(year = format(distribution_date, format = "%Y")) 
	
	dvaccine_data <- dvaccine_data[, c(2, 4, 1, 3, 5)]
	
	# Data Wrangling
	
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


# 6. TESTING the fucntion

test_that("testing of yearly_vaccine_distribution", {
	expected <- yearly_vaccine_distribution("Alberta")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "Alberta")
	expect_error(yearly_vaccine_distribution("BC"), "Please enter a valid province name that too in its full form!")
})



# 7. Function shows the information of the deaths related to COVID -19 as per the specified province
mortality <- function(provinceName = 'Canada'){
	#' Function for returning data frame for the Covid - 19 mortality rate in different provinces in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the date of the death, province name, cumulative deaths and deaths.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the deaths because of COVID -19 corresponding to a particular province
	#' 
	#' @examples 
	#' mortality('Alberta')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	mortaility <- json_data$mortality
	mortaility_data <- data.frame()
	for(i in 1:length(mortaility)){
		mortaility_data <- rbind(mortaility_data, data.frame(mortaility[[i]]))
	}
	
	# Data Cleaning
	mortaility_data <- mortaility_data %>% 
		rename('death_date' = 'date_death_report') %>%
		mutate(death_date=as.Date(death_date, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island"))
	
	mortaility_data <- mortaility_data[, c(2, 4, 1, 3)]
	
	# Data Wrangling
	
	if(tolower(provinceName) == 'canada'){
		return(mortaility_data)
	} else {
		mortaility_data <- mortaility_data %>% 
			filter(tolower(province) == tolower(provinceName))
		return(mortaility_data)
	}
}


# 7. TSETING the fucntion 
test_that("testing of mortality", {
	expected <- mortality("Alberta")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "Alberta")
	expect_error(mortality("BC"), "Please enter a valid province name that too in its full form!")
})


# 8. Function showing the information of the deaths related to COVID -19 as per the year passed
yearly_deaths<- function(provinceName = 'Canada', dyear='2020'){
	#' Function for returning data frame for the Covid - 19 deaths in different provinces in Canada according to the year.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the date of the death, province name, cumulative deaths and deaths.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' @param dyear a character/ string showing the year		
	#' 
	#' @return Data frame for the Covid - 19 deaths corresponding to a particular province and year
	#' 
	#' @examples 
	#' yearly_deaths('Alberta','2020')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	mortaility <- json_data$mortality
	mortaility_data <- data.frame()
	for(i in 1:length(mortaility)){
		mortaility_data <- rbind(mortaility_data, data.frame(mortaility[[i]]))
	}
	
	# Data Cleaning
	mortaility_data <- mortaility_data %>% 
		rename('death_date' = 'date_death_report') %>%
		mutate(death_date=as.Date(death_date, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
		mutate(year = format(death_date, format = "%Y")) 
	
	mortaility_data <- mortaility_data[, c(2, 4, 1, 3, 5)]
	
	# Data Wrangling
	
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


# 8. TESTING the function
test_that("testing of yearly_deaths", {
	expected <- yearly_deaths("Alberta")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "Alberta")
	expect_error(yearly_deaths("BC"), "Please enter a valid province name that too in its full form!")
})

# 9. Function showing the data for the recovery cases after COVID- 19
recovered <- function(provinceName = 'Canada'){
	#' Function for returning data frame for the Covid - 19 recovery rate in different provinces in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the recovery date, province, cumulative recovered, and count of recovery.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the recovered cases after COVID -19 corresponding to a particular province
	#' 
	#' @examples 
	#' recovered('Alberta')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	recovered <- json_data$recovered
	recovered_data <- data.frame()
	for(i in 1:length(recovered)){
		recovered_data <- rbind(recovered_data, data.frame(recovered[[i]]))
		
	}
	
	# Data Cleaning
	recovered_data <- recovered_data %>% 
		mutate(date_recovered=as.Date(date_recovered, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island"))
	
	recovered_data <- recovered_data[, c(2, 3, 1, 4)]
	
	# Data Wrangling
	
	if(tolower(provinceName) == 'canada'){
		return(recovered_data)
	} else {
		recovered_data <- recovered_data %>% 
			filter(tolower(province) == tolower(provinceName))
		return(recovered_data)
	}
}


# 9. TESTING the function

test_that("testing of recovered", {
	expected <- recovered("Alberta")
	expect_s3_class(expected, "data.frame")
	expect_equal(unique(expected$province), "Alberta")
	expect_error(recovered("BC"), "Please enter a valid province name that too in its full form!")
})

# Function showing recovered cases yearly
yearly_recovered<- function(provinceName = 'Canada', ryear='2020'){
	#' Function for returning data frame for the Covid - 19 recovered cases in different provinces in Canada according to the year.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the recovery date, province, cumulative recovered, and count of recovery.
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' @param ryear a character/ string showing the year		
	#' 
	#' @return Data frame for the Covid - 19 recovery corresponding to a particular province and year
	#' 
	#' @examples 
	#' yearly_recovered('Alberta','2020')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	recovered <- json_data$recovered
	recovered_data <- data.frame()
	for(i in 1:length(recovered)){
		recovered_data <- rbind(recovered_data, data.frame(recovered[[i]]))
		
	}
	# Data Cleaning
	recovered_data <- recovered_data %>% 
		mutate(date_recovered=as.Date(date_recovered, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island")) %>%
		mutate(year = format(date_recovered, format = "%Y")) 
	
	recovered_data <- recovered_data[, c(2, 3, 1, 4,5)]
	
	# Data Wrangling
	
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

# Function for the testing rate of different provinces in Canada
testing <- function(provinceName = 'Canada'){
	#' Function for returning data frame for the Covid - 19 testing rate in different provinces in Canada.
	#' 
	#' Performed data wrangling and cleaning using the API for the Covid - 19 cases in Canada. 
	#' It processes the API and returns the data corresponding to one province 
	#' which is passed on as the argument. If user passes empty argument, so by default Canada is used which returns the data of whole Canada as a whole.
	#' The returned data is a data frame and contains the columns including the testing date, province, cumulative testing, and count of testing
	#'    
	#' @param provinceName a character/ string depicting the name of the province
	#' 
	#' @return Data frame for the recovered cases after COVID -19 corresponding to a particular province
	#' 
	#' @examples 
	#' testing('Alberta')
	
	prov = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Nunavut", "Northwest Territories", "Ontario", "Prince Edward 	                Island", "Quebec", "Saskatchewan", "Yukon", "Canada")
	`%!in%` <- Negate(`%in%`)
	if(tolower(provinceName) %!in% tolower(prov)){
		stop("Please enter a valid province name that too in its full form!")
	}
	
	# Fetching the data using the URL
	request <- request_fun()
	
	# Reading the data as per JSON and then saving that in data frame
	json_data <- content(request, as  = "parse")
	testing <- json_data$testing
	testing_data <- data.frame()
	for(i in 1:length(testing)){
		testing_data <- rbind(testing_data, data.frame(testing[[i]]))
	}
	
	# Data Cleaning
	testing_data <- testing_data %>% 
		mutate(date_testing=as.Date(date_testing, format = "%d-%m-%Y"))%>%
		mutate(province = replace(province, province %in% c("BC"), "British Columbia"),
					 province = replace(province, province %in% c("NL"), "Newfoundland and Labrador"),
					 province = replace(province, province %in% c("NWT"), "Northwest Territories"),
					 province = replace(province, province %in% c("PEI"), "Prince Edward Island"))
	
	testing_data <- testing_data[, c(2, 3, 1, 4)]
	
	# Data Wrangling
	
	if(tolower(provinceName) == 'canada'){
		return(testing_data)
	} else {
		testing_data <- testing_data %>% 
			filter(tolower(province) == tolower(provinceName))
		return(testing_data)
	}
}