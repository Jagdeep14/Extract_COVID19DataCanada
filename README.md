# Package - COVID19data

This R-package `COVID19data` contains 16 wrapper functions to get Covid-19 data of Canada. 
These functions process API [https://opencovid.ca/api/#welcome](https://opencovid.ca/api/#welcome) to get data. 
Some functions provide daily data and some of them provide summary of data for all the provinces. 
This package contains four internal functions to request API which are not available to user. 
The list of 16 functions available to user is given below:


```
1. active_cases(provinceName = 'Canada')
2. vaccine_as_per_province(provinceName = 'Canada')
3. cases_as_per_province(provinceName = 'Canada')
4. Yearlycases_as_per_province(provinceName = 'Canada',yearpassed='2020')
5. cumulativevaccine_as_per_province(provinceName = 'Canada')
6. vaccine_distribution(provinceName = 'Canada')
7. yearly_vaccine_distribution(provinceName = 'Canada',distyear='2020')
8. mortality(provinceName = 'Canada')
9. yearly_deaths(provinceName = 'Canada', dyear='2020')
10. recovered(provinceName = 'Canada')
11. yearly_recovered(provinceName = 'Canada', ryear='2020')
12. testing(provinceName = 'Canada')
13. summary_of_cases(provinceName = "Canada")
14. summary_of_peaktime(provinceName = "Canada", date = '01-09-2020')
15. summary_of_canada(date = '01-09-2020')
16. province_population(provinceName = 'Canada')

``` 

The detailed description of these functions as given below.

### To install package

First, user need to install `install.packages("devtools")` and then install the package from github 
using command `install_github('aishwaryasharma10/COVID19data', build_vignettes=TRUE)`. 
Make the optional argument `build_vignettes` true if you want to build vignettes for this package 
and use `browseVignettes("COVID19data")` to see the vignette .