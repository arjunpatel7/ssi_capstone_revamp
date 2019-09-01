library(tidyverse)
library(mosaic)
library(broom)
library(GGally)
library(knitr)
library(anchors)
library(gdata)
library(readxl)
library(sp)
library(maps)
library(rgeos)
library(maptools)
library(stringr)
library(gridExtra)
library(tidycensus)

#### Clean Internet Speed Data
setwd("~/Desktop/Personal Projects/ssi_capstone_revamp/")

# Grabbed this function from a stack overflow post, located here. 
# https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county

# Comments from original post:

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county=function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}


clean_234_df = function(fpath, year){
  #These are error due to recorder or devices, so I don't want them
  drop_codes = c("ERROR: QUIT BY USER",
                "connect_error1",
                "connect_error2",
                "connect_error3")
  
  #These are errors due to lack of service, so I'll set these to zero
  #as per CALSPEED technique
  
  zero_codes = c("no effective service",
                 "timeout")
  
  df = read_excel(fpath, sheet = "Test Results")
  if(year == 2012 | year == 2013){
    colkeep = c("NORMALIZED_LAT", "NORMALIZED_LONG", "Census 2010 Designation", 
                          "Provider", "wTCP_DOWN1", "wTCP_DOWN2", 
                          "eTCP_DOWN1", "eTCP_DOWN2")
    df = df[df$`Census 2010 Designation` != "Tribal",]
    df = df[!(is.na(df$NORMALIZED_LAT) | is.na(df$NORMALIZED_LONG)),]
    latlong_col = c("NORMALIZED_LONG", "NORMALIZED_LAT")
  } else {
    #2014 categories and filtering
    colkeep = c("Normalized_LAT", "Normalized_LONG", "Census 2012 Designation", 
                "Provider", "wTCP_DOWN1", "wTCP_DOWN2", 
                "eTCP_DOWN1", "eTCP_DOWN2")
    df = df[df$`Census 2012 Designation` != "Tribal",]
    latlong_col = c("Normalized_LONG", "Normalized_LAT")
    df = df[!(is.na(df$Normalized_LAT) | is.na(df$Normalized_LONG)),]
  }
  
  
  df = df[, colkeep]
  
  #clean columns
  #zero out
  
  df$wTCP_DOWN1 = ifelse(df$wTCP_DOWN1 %in% zero_codes, 0, df$wTCP_DOWN1)
  df$wTCP_DOWN2 = ifelse(df$wTCP_DOWN2 %in% zero_codes, 0, df$wTCP_DOWN2)
  
  df$eTCP_DOWN1 = ifelse(df$eTCP_DOWN1 %in% zero_codes, 0, df$eTCP_DOWN1)
  df$eTCP_DOWN2 = ifelse(df$eTCP_DOWN2 %in% zero_codes, 0, df$eTCP_DOWN2)
  
  
  #null out
  df = df[!df$wTCP_DOWN1 %in% drop_codes, ]
  df = df[!df$wTCP_DOWN2 %in% drop_codes, ] 
  df = df[!df$eTCP_DOWN2 %in% drop_codes, ] 
  df = df[!df$eTCP_DOWN1 %in% drop_codes, ]
  
  
  #We notice that the desired independent variables are read as characters
  #WE should change the type of these columns as follows
  df$wTCP_DOWN1 = as.numeric(df$wTCP_DOWN1)
  df$eTCP_DOWN1 = as.numeric(df$eTCP_DOWN1)
  df$eTCP_DOWN2 = as.numeric(df$eTCP_DOWN2)
  df$wTCP_DOWN2 = as.numeric(df$wTCP_DOWN2)
  
  head(df)
  df = df %>% mutate(wtcp_down = (wTCP_DOWN1 + wTCP_DOWN2)/2,
                               etcp_down = (eTCP_DOWN1+ eTCP_DOWN2)/2)
  

  #clean latlong of NAs
  latlong = df[, latlong_col]
  countys = latlong2county(latlong)
  #countys
  #Slap the column back onto the dataframe
  df$county = countys

  
  
  #Clean the columns so that only the name of the county remains
  #unique(df$county)
  df = df[str_detect(df$county, "california"),]
  df$county = gsub("california,", "", df$county)
  df$year = year
  df = dplyr::select(df, county, year, wtcp_down, etcp_down)
  return(df)
}





#read in other dfs, and apply the cleaning to them
#analyze and check for discrepanies (column names, etc)




clean_567_df = function(fpath, year){
  #These are error due to recorder or devices, so I don't want them
  drop_codes = c("ERROR: QUIT BY USER",
                 "connect_error1",
                 "connect_error2",
                 "connect_error3")
  
  #These are errors due to lack of service, so I'll set these to zero
  #as per CALSPEED technique
  
  zero_codes = c("no effective service",
                 "timeout", 
                 "bad_output")
  if (year == 2017){
    df = read_excel(fpath, sheet = "Fall 2017 Results")
  }else{
    df = read_excel(fpath, sheet = "Test Results")
    
  }
  colkeep = c("Latitude", "Longitude", "Type", 
              "Provider", "wTCPDown1", "wTCPDown2", 
              "eTCPDown1", "eTCPDown2")
  df = df[df$Type != "Tribal",]
  df = df[!(is.na(df$Latitude) | is.na(df$Longitude)),]
  df$Latitude = as.numeric(df$Latitude)
  df$Longitude= as.numeric(df$Longitude)
  latlong_col = c("Longitude", "Latitude")
  
  
  df = df[, colkeep]
  
  #clean columns
  #zero out
  
  df$wTCPDown1 = ifelse(df$wTCPDown1 %in% zero_codes, 0, df$wTCPDown1)
  df$wTCPDown2 = ifelse(df$wTCPDown2 %in% zero_codes, 0, df$wTCPDown2)
  
  df$eTCPDown1 = ifelse(df$eTCPDown1 %in% zero_codes, 0, df$eTCPDown1)
  df$eTCPDown2 = ifelse(df$eTCPDown2 %in% zero_codes, 0, df$eTCPDown2)
  
  
  #null out
  df = df[!df$wTCPDown1 %in% drop_codes, ]
  df = df[!df$wTCPDown2 %in% drop_codes, ] 
  df = df[!df$eTCPDown2 %in% drop_codes, ] 
  df = df[!df$eTCPDown1 %in% drop_codes, ]
  
  
  #We notice that the desired independent variables are read as characters
  #WE should change the type of these columns as follows
  df$wTCPDown1 = as.numeric(df$wTCPDown1)
  df$eTCPDown1 = as.numeric(df$eTCPDown1)
  df$eTCPDown2 = as.numeric(df$eTCPDown2)
  df$wTCPDown2 = as.numeric(df$wTCPDown2)
  
  head(df)
  df = df %>% mutate(wtcp_down = (wTCPDown1 + wTCPDown2)/2,
                     etcp_down = (eTCPDown1+ eTCPDown2)/2)
  
  
  #clean latlong of NAs
  latlong = df[, latlong_col]
  countys = latlong2county(latlong)
  #countys
  #Slap the column back onto the dataframe
  df$county = countys
  
  
  #Clean the columns so that only the name of the county remains
  #unique(df$county)
  df = df[str_detect(df$county, "california"),]
  df$county = gsub("california,", "", df$county)
  df$year = year
  df = dplyr::select(df, county, year, wtcp_down, etcp_down)
  return(df)

}


df_2012 = na.omit(clean_234_df("calspeed2012.xlsx", year = 2012))
df_2013 = na.omit(clean_234_df("calspeed2013.xlsx", year = 2013))
df_2014 = na.omit(clean_234_df("calspeed2014.xlsx", year = 2014))

df_2015 = na.omit(clean_567_df("calspeed2015.xlsx", year = 2015))
df_2016 = na.omit(clean_567_df("calspeed2016.xlsx", year = 2016))
df_2017 = na.omit(clean_567_df("calspeed2017.xlsx", year = 2017))

#Bind and group_by manipulations


mobile_internet_df = bind_rows(df_2012, df_2013, df_2014, df_2015, df_2016, df_2017)

mobile_internet_df = mobile_internet_df %>% group_by(county, year) %>%
  summarise(wtcp_down_mean = mean(wtcp_down), 
            etcp_down_mean = mean(etcp_down),
            wtcp_down_med = median(wtcp_down),
            etcp_down_med = median(etcp_down))


#Clean hate crime Data

ret_hc_data = function(){
  cali_df = read_csv("HATE_2001-2018_0.csv")
  length(unique(cali_df$County))
  cali_df = cali_df[, c(1:7)]
  cali_df = cali_df %>% filter(between(ClosedYear, 2012, 2017))
  length(unique(cali_df$County))
  
  cali_df = cali_df %>% group_by(County, ClosedYear) %>% summarise(totalv = sum(TotalNumberOfVictims),
                                                                   totalind =sum(TotalNumberOfIndividualVictims))
  #cali_df$County = as.numeric(cali_df$County)
  cali_df = cali_df %>% complete(ClosedYear = 2012:2017)
  
  
  #need to fill in counties that don't exist. How can I do that? 
  #read in codes
  county_codes = read_excel("code_mapping_county.xlsx", sheet= "Sheet1")
  county_codes = county_codes[, c("CntyCode", "County")]
  
  #sanity check
  length(unique(county_codes$CntyCode)) == length(unique(county_codes$County))
  
  #lowercase and clean the county column
  county_codes$County = str_to_lower(county_codes$County)
  county_codes$County = gsub(" county", "", county_codes$County)
  county_codes= county_codes %>% group_by(County) %>% summarise(code = max(CntyCode))
  #merge em
  length(unique(county_codes$code))
  
  #I contacted OpenJustice and they told me that only counties that report at least one hate crime
  #are in the dataset. This means counties that aren't or those that are missing values for certain years
  #didn't have any hate crimes to report. We can set those to NA here:
  
  cali_df = merge(cali_df, county_codes, by.x = "County", by.y = "code", all.x = TRUE, all.y = TRUE)
  #check to see this worked, length should be 58.
  print(length(unique(cali_df$County)))
  
  mis_dates = cali_df %>% group_by(County.y) %>% filter(n() < 6) %>%ungroup()
  
  drop_county = mis_dates$County.y
  cali_df = cali_df %>%filter(!County.y %in% drop_county)
  mis_dates$ClosedYear = 2012
  mis_dates$totalv = 0
  mis_dates$totalind = 0
  mis_dates = mis_dates %>% complete(ClosedYear = 2012:2017, County.y = County.y)
  mis_dates$totalv = 0
  mis_dates$totalind = 0
  
  #manually cleaning the remaining counties
  mis_dates[mis_dates$County.y == "inyo",]$County = 14 
  mis_dates[mis_dates$County.y == "lassen",]$County = 18 
  mis_dates[mis_dates$County.y == "madera",]$County = 20 
  mis_dates[mis_dates$County.y == "mariposa",]$County = 22 
  mis_dates[mis_dates$County.y == "modoc",]$County =  25
  mis_dates[mis_dates$County.y == "sierra",]$County =  46 
  mis_dates[mis_dates$County.y == "siskiyou",]$County =  47 
  mis_dates[mis_dates$County.y == "trinity",]$County = 53
  
  #now, merge them back. 
  cali_df = bind_rows(cali_df, mis_dates)
  cali_df[is.na(cali_df)] = 0
  #sanity check
  nrow(cali_df %>% group_by(County.y) %>% filter(n() < 6)) ==0


  #population?
  #https://factfinder.census.gov/faces/nav/jsf/pages/guided_search.xhtml
  county_pops = read_csv("population_counts.csv")
  #Prepare dataframe for merging
  county_pops = county_pops %>% rename(county = `GEO.display-label`,
                                       pop2012 = respop72012,
                                       pop2013 = respop72013,
                                       pop2014 = respop72014,
                                       pop2015 = respop72015,
                                       pop2016 = respop72016,
                                       pop2017 = respop72017) 
  county_pops = dplyr::select(county_pops, county, pop2012,
                              pop2013, pop2014, pop2015, pop2016, pop2017)
  county_pops$county = gsub(" County,", "", county_pops$county)
  county_pops$county = gsub(" California", "", county_pops$county)
  county_pops$county = str_to_lower(county_pops$county)
  
  #Tricky part. Have to move from long format to tidy format
  county_pops_collapsed = county_pops %>% gather(pYear,pop, pop2012:pop2017)
  county_pops_collapsed$pYear = as.numeric(gsub("pop", "", county_pops_collapsed$pYear))
  #merge on pyear, ClosedYear, and county names.
  
  cali_hc_df = merge(cali_df, county_pops_collapsed, by.x = c("County.y", "ClosedYear"),
                     by.y = c("county", "pYear"))
  
  #usually, per 100,000 is the rate used for crimes
  cali_hc_df$hc_capita = ((cali_hc_df$totalind) /(cali_hc_df$pop)) * 100000 
  return(cali_hc_df)
}

cali_hc_df = ret_hc_data()

#Merge with internet data, and summarize in a fashion that is appropriate
df = merge(cali_hc_df, mobile_internet_df, by.x = c("ClosedYear","County.y"),
           by.y = c("year", "county"))

#ANALYSIS
df$ClosedYear = as.factor(df$ClosedYear) 
length(unique(df$County.y))


###Control variable input and cleaning

#white population

w_props = read_csv("ACS_17_1YR_CP05/ACS_17_1YR_CP05.csv")

#Desired Columns, using the given coding for the columns


col_keep = c("GEO.display-label", "HC01_VC54", "HC02_VC54",
             "HC04_VC54", "HC06_VC54", "HC08_VC54")
w_props = w_props[, col_keep]
w_props = w_props %>% dplyr::select(county = "GEO.display-label", w2017 = "HC01_VC54",
                             w2016 = "HC02_VC54",
                             w2015 = "HC04_VC54",
                             w2014 = "HC06_VC54",
                             w2013 = "HC08_VC54")
w_props$county = gsub("County, California", "", w_props$county)
w_props$county = str_to_lower(w_props$county)

#unemployment
unemp_df = read_excel("Unemployment.xls", sheet = "Unemployment Med HH Inc", skip = 7)
unemp_df = unemp_df %>% filter(State == "CA", Area_name != "California")
unemp_df$Area_name = gsub(" County, CA", "", unemp_df$Area_name)
unemp_df$Area_name = str_to_lower(unemp_df$Area_name)
unemp_df = unemp_df %>% dplyr::select(county = Area_name, unemp2012 = Unemployment_rate_2012,
                                              unemp2013 = Unemployment_rate_2013,
                                              unemp2014 = Unemployment_rate_2014,
                                              unemp2015 = Unemployment_rate_2015,
                                              unemp2016 = Unemployment_rate_2016,
                                              unemp2017 = Unemployment_rate_2017)


temp_df = merge(df, unemp_df, by.x = "County.y", by.y = "county")


#unemployment

#population in 2010, POC, white, etc
#using tidycensus for the data

#REMEMBER TO REMOVE THIS FOR PRIVACY REASONS
census_api_key(key = "04c634ac703cfc40cceef49e4b8466ad06ccebe0")

cali_pop_data = get_estimates(geography = "county", 
                              product = "characteristics", 
                              breakdown = c("RACE"),  
                              breakdown_labels = TRUE, 
                              state = "CA")
#
length(unique(cali_pop_data$NAME))
cali_pop_data = cali_pop_data %>% spread(key = RACE, value = value)
#old school for loop, cause I don't know yet how to do this in a vectorized fashion
for(x in colnames(cali_pop_data)[4:14]){
  #iterate over each column and normalize by total population
  cali_pop_data[, x] = cali_pop_data[, x]/(cali_pop_data$`All races`)
}



#cali_pop_data[, c(2:14)] = (cali_pop_data[, c(2:14)])/cali_pop_data$`All races`
##ARTICLE IDEA: present a hypothetical analysis of hate crime and encouch it in 
## a description of the flimsyness of hate crime statistics
## and statistics as deception



#make sure that all counties are accounted for. If not, label with zeros across the board

#make sure to note missing county data: Sierra reported no hate crimes betewen 2012-2017

#Create maps, bivariate, and maybe create maps to test spatial hypotheses?


#What is the distribution of internet speeds?

#Rescalled to mbs?

#Does hate crime increase over time?

#Does internet speed increase over time?

#Is internet speed predictive of hate crime?

#plot data over time, and w.r.t each county?

#Model development

#Add context variables for analysis after model development + reaffirm null

plot(hc_capita ~ wtcp_down_mean, data = df %>% filter(hc_capita != 0)) 


#linear model development
#model generation

control_vars = read_csv("ca_county_agency_contextual_indicators_2009-2014_05-03-2016.csv")
control_vars = control_vars %>% filter(agency_code == "All Combined") 
control_vars$county = str_to_lower(control_vars$county)
length(unique(control_vars$county))
shortened_model = merge(control_vars, df, by.x = c("year", "county"), by.y = c("ClosedYear",
                                                                               "County.y"))

cor(shortened_model[shortened_model$hc_capita != 0, c("less_than_high_school", "per_capita_income",
                        "poverty_rate", "unemployment_rate",
                        "hc_capita",
                        "wtcp_down_mean")])
shortened_model$wtcp_down_mean = shortened_model$wtcp_down_mean / 1000

shortened_model

m1 = lm(hc_capita ~ wtcp_down_mean, data = shortened_model %>% filter(hc_capita !=0,
                                                                      county != "los angeles"))
summary(m1)

plot(cooks.distance(m1))


m2 = lm(hc_capita ~ wtcp_down_mean+ poverty_rate + less_than_high_school + 
          per_capita_income + unemployment_rate, data = shortened_model %>% filter(hc_capita !=0))
summary(m2) 

m1 <- lm(hc_capita ~ wtcp_down_mean, data = df %>% filter(hc_capita != 0))
summary(m1)

m2 <- lm(hc_capita ~ etcp_down_mean, data = df)
summary(m2)




m_fixed_eff <- lm(hc_capita ~ wtcp_down_mean + as.factor(ClosedYear) + as.factor(County.y), data = df)
summary(m_fixed_eff)

m3 <- lm(hc_capita ~ wtcp_down_mean + as.factor(ClosedYear), data = df)

m4 <- lm(hc_capita ~ wtcp_down_mean + as.factor(County.y), data = df)

summary(m3)
summary(m4)


library(maptools)
library(gpclib)  # may be needed, may not be

# MAP
cali_map <- readShapeSpatial("CA_Counties/CA_Counties_TIGER2016.shp")
plot(cali_map)
cali_map = fortify(cali_map)
cali_map$NAME = str_to_lower(cali_map$NAME)
cali_map$region = cali_map$NAME

df %>% filter(ClosedYear == 2017) %>% summarise(max(wtcp_down_mean))

p1 = ggplot(df, frame = ClosedYear) + geom_map(data = df %>% filter(), aes(map_id = County.y, fill = wtcp_down_mean/1000), 
                      map = cali_map) + expand_limits(x = cali_map$long, y = cali_map$lat) + 
  scale_fill_gradient(low = "white", high = "sky blue", limits = c(0, 20))


p2 = ggplot() + geom_map(data = df %>% filter(ClosedYear == 2017), aes(map_id = County.y, fill = hc_capita), 
                         map = cali_map) + expand_limits(x = cali_map$long, y = cali_map$lat) + 
  scale_fill_gradient(low = "white", high = "red", limits = c(0, 11))

grid.arrange(p1, p2, nrow = 1, top = "Mobile Internet Download Speed and Hate Crime per 100,000 persons in California, 2017")







#summary statistics, distributions, etc

cor(drop_na(df)[,c("ClosedYear", "hc_capita","wtcp_down_mean","wtcp_down_med")])

ggplot(df %>% filter(), aes(y = hc_capita, x = wtcp_down_mean/1000, color = ClosedYear)) +geom_point() + scale_color_brewer(palette = "Set2")


ggparcoord(df, columns = c(7:11),
           groupColumn = 1,
           scale = "std") +scale_color_brewer(palette = "Set2") 
#redo
ggplot(df, aes(x = reorder(County.y, ClosedYear), y = wtcp_down_mean)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + xlab("California Counties") +
  ylab("Download speed, in kilobytes/sec")+ ggtitle("Distribution of Download Speeds in California Counties") +
  geom_boxplot() + theme(plot.title = element_text(hjust = 0.5))

#boxplots, ggplots, pretty as they can be

#create linear models for estimation. Deal with year by year or county vars



#create geomap?

#Model Generation

#lm models

#conclusion


