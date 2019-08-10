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
library(maptools)
library(stringr)

#### Clean Internet Speed Data


# Grabbed this function from a stack overflow post
# god bless
# https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county


# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county=function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  
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
  return(df)
}


df_2012 = clean_234_df("calspeed2012.xlsx", year = 2012)
df_2013 = clean_234_df("calspeed2013.xlsx", year = 2013)
df_2014 = clean_234_df("calspeed2014.xlsx", year = 2014)

df_2015 = clean_567_df("calspeed2015.xlsx", year = 2015)
df_2016 = clean_567_df("calspeed2016.xlsx", year = 2016)
df_2017 = clean_567_df("calspeed2017.xlsx", year = 2017)

#Bind and group_by manipulations



#Clean hate crime Data

#read in data
cali_df = read_csv("HATE_2001-2018_0.csv")
cali_df = cali_df[, c(1:7)]
cali_df = cali_df %>% filter(ClosedYear >= 2012,
                             ClosedYear <= 2017)
cali_df = cali_df %>% group_by(County, ClosedYear) %>% summarise(totalv = sum(TotalNumberOfVictims),
                                                           totalind =sum(TotalNumberOfIndividualVictims))
#read in codes

county_codes = read_excel("code_mapping_county.xlsx", sheet= "Sheet1")
county_codes = county_codes[, c("CntyCode", "County")]

#sanity check
count(unique(county_codes$CntyCode)) == count(unique(county_codes$County))

#lowercase and clean the county column
county_codes$County = str_to_lower(county_codes$County)
county_codes$County = gsub(" county", "", county_codes$County)

#merge em

cali_df = merge(cali_df, county_codes, by.x = "County", by.y = "CntyCode")


#population?
#https://factfinder.census.gov/faces/nav/jsf/pages/guided_search.xhtml



#scale it



#merge with internet data, done






#ANALYSIS

#summary statistics, distributions, etc

#boxplots, ggplots, pretty as they can be

#create geomap?

#Model Generation

#lm models

#conclusion


