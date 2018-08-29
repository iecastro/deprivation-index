library(tidycensus)
library(tidyverse)
library(psych)

options(tigris_class = "sf", tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")

### function 
countyND <- function(arg1,arg2){vars <- c("B17001_002", "B17001_001", "B06009_002" , "B06009_001",
                                         "B09008_011", "B09008_001","B08124_002", "B08124_001", "B25014_005", 
                                         "B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013",  
                                         "B25014_001", "B19058_002", "B19058_001","C23002C_021", "C23002D_008", 
                                         "C23002C_017", "C23002D_003","B19001_002", "B19001_003", "B19001_004", 
                                         "B19001_005", "B19001_006", "B19001_001")
acs_data <- get_acs(geography = "tract", variables =vars,state = arg1,  
                    county = arg2,output = "wide") %>%
  mutate(pct_poverty = B17001_002E/B17001_001E,
         pct_noHS = B06009_002E / B06009_001E,
         pct_FHH = B09008_011E / B09008_001E,
         pct_mgmt = B08124_002E /  B08124_001E, 
         pct_crowd =  (B25014_005E +B25014_006E+ B25014_007E + 
                         B25014_011E + B25014_012E + B25014_013E) / B25014_001E,
         pct_pubassist = B19058_002E/B19058_001E,
         pct_unempl = (C23002C_021E + C23002D_008E)  / (C23002C_017E + C23002D_003E),
         pct_under30K =( B19001_002E+B19001_003E+B19001_004E+B19001_005E +
                           B19001_006E) / B19001_001E)
values  <-  acs_data %>% select(pct_poverty,pct_noHS,pct_FHH,pct_mgmt,pct_crowd,
                                pct_pubassist, pct_unempl,pct_under30K) %>% as.matrix()
values[is.nan(values)] <- 0
ND <- principal(values,nfactors = 1)          
NDI <- cbind(acs_data,ND$scores) 
NDI <- NDI %>% select(NAME,GEOID,PC1) %>% 
  separate(NAME, into = c("Tract", "County","State"), sep = ",")
}

##### examples 
## arg1 = "State"
## arg2 = "County"

NDI <-countyND("NY","Onondaga") 


ggplot(NDI, aes(PC1)) + geom_histogram() + theme_classic()

NDI$type[as.numeric(NDI$GEOID) < 36067006104] <- "City Tract"
NDI$type[as.numeric(NDI$GEOID) >= 36067006104] <- "County Tract"

ggplot(NDI, aes(reorder(Tract, -PC1), PC1)) + geom_col(aes(fill = type)) + coord_flip() +
  theme(axis.text.x = element_text(size = 8, color = "black"), 
        axis.text.y = element_text(size = 5, color = "black")) +
  scale_fill_viridis_d(option = "cividis") + 
  labs(fill = "", x = "Tract", y = "Deprivation Index")
           

############### thematic maps ################
library(viridis)
library(sf)
library(tigris)

options(tigris_class = "sf", tigris_use_cache = TRUE)

#### get census tracts 
### specify state/county
tractsNY <- tracts(state = "NY",  county = "Onondaga County",
                 cb = TRUE)

Map <- geo_join(tractsNY,NDI, by_sp = "GEOID", by_df = "GEOID")

## County
ggplot() + geom_sf(data = Map, aes(fill = PC1)) +
  theme_minimal() + scale_fill_viridis()+
  labs(fill = "Index", caption = "Source: US Census ACS 2016 estimates")+
  ggtitle(" ", subtitle = "Onondaga County, NY")

## Syracuse
## filter by census tract code
Map %>% filter(as.numeric(TRACTCE) < 6104) %>%
  ggplot() + geom_sf(aes(fill = PC1)) +
  theme_minimal() + scale_fill_viridis()+
  labs(fill = "Index", caption = "Source: US Census ACS 2016 estimates")+
  ggtitle(" ", subtitle = "Syracuse, NY")

### Broward county
NDI2 <- countyND("FL","Broward")

tractsFL <- tracts(state = "FL",  county = "Broward",
                 cb = TRUE)

Map2 <- geo_join(tractsFL,NDI2, by_sp = "GEOID", by_df = "GEOID")

ggplot() + geom_sf(data = Map2, aes(fill = PC1)) +
  theme_minimal() + scale_fill_viridis()+
  labs(fill = "Index", caption = "Source: US Census ACS 2016 estimates")+
  ggtitle(" ", subtitle = "Broward County, FL")


### Virgina Beach
NDI3 <- countyND("VA","Virginia Beach")

tractsVA <- tracts(state = "VA",  county = "Virginia Beach",
                   cb = TRUE)

Map3 <- geo_join(tractsVA,NDI3, by_sp = "GEOID", by_df = "GEOID")

ggplot() + geom_sf(data = Map3, aes(fill = PC1)) +
  theme_minimal() + scale_fill_viridis()+
  labs(fill = "Index", caption = "Source: US Census ACS 2016 estimates")+
  ggtitle(" ", subtitle = "Virginia Beach County, VA")


#####
stateND <- function(arg1){vars <- c("B17001_002", "B17001_001", "B06009_002" , "B06009_001",
                                          "B09008_011", "B09008_001","B08124_002", "B08124_001", "B25014_005", 
                                          "B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013",  
                                          "B25014_001", "B19058_002", "B19058_001","C23002C_021", "C23002D_008", 
                                          "C23002C_017", "C23002D_003","B19001_002", "B19001_003", "B19001_004", 
                                          "B19001_005", "B19001_006", "B19001_001")
acs_data <- get_acs(geography = "tract", variables =vars,state = arg1,  
                    output = "wide") %>%
  mutate(pct_poverty = B17001_002E/B17001_001E,
         pct_noHS = B06009_002E / B06009_001E,
         pct_FHH = B09008_011E / B09008_001E,
         pct_mgmt = B08124_002E /  B08124_001E, 
         pct_crowd =  (B25014_005E +B25014_006E+ B25014_007E + 
                         B25014_011E + B25014_012E + B25014_013E) / B25014_001E,
         pct_pubassist = B19058_002E/B19058_001E,
         pct_unempl = (C23002C_021E + C23002D_008E)  / (C23002C_017E + C23002D_003E),
         pct_under30K =( B19001_002E+B19001_003E+B19001_004E+B19001_005E +
                           B19001_006E) / B19001_001E)
values  <-  acs_data %>% select(pct_poverty,pct_noHS,pct_FHH,pct_mgmt,pct_crowd,
                                pct_pubassist, pct_unempl,pct_under30K) %>% as.matrix()
values[is.nan(values)] <- 0
ND <- principal(values,nfactors = 1)          
NDI <- cbind(acs_data,ND$scores) 
NDI <- NDI %>% select(NAME,GEOID,PC1) %>% 
  separate(NAME, into = c("Tract", "County","State"), sep = ",")
}


 