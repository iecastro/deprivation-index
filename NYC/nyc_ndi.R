library(tidycensus)
library(tidyverse)
library(psych)
library(tigris)
library(sf)

options(tigris_class = "sf")

#-- API vars
vars <- c("B17001_002", "B17001_001", "B06009_002" , "B06009_001","B09008_011", 
          "B09008_001","B08124_002", "B08124_001", "B25014_005",
          "B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013", 
          "B25014_001", "B19058_002", "B19058_001", 
          "B23025_003", "B23025_005",
          "B19001_002", "B19001_003", "B19001_004", "B19001_005", "B19001_006", "B19001_001")

boroughs <- c("New York", "Kings", "Bronx",
              "Richmond", "Queens")

acs_data <- get_acs(geography = "tract", variables =vars,
                    state = "NY",  
                    county = boroughs,
                    output = "wide") %>%
  mutate(pct_poverty = B17001_002E/B17001_001E,
         pct_noHS = B06009_002E / B06009_001E,
         pct_FHH = B09008_011E / B09008_001E,
         pct_mgmt = B08124_002E /  B08124_001E, 
         pct_crowd =  (B25014_005E +B25014_006E+ B25014_007E + 
                         B25014_011E + B25014_012E + B25014_013E) / B25014_001E,
         pct_pubassist = B19058_002E/B19058_001E,
         pct_unempl = B23025_005E / B23025_003E,
         pct_under30K =( B19001_002E+B19001_003E+B19001_004E+B19001_005E +
                           B19001_006E) / B19001_001E)

values  <-  acs_data %>% select(pct_poverty,pct_noHS,pct_FHH,pct_mgmt,pct_crowd,
                                pct_pubassist, pct_unempl,pct_under30K) %>% as.matrix()

values[is.nan(values)] <- 0
ND <- principal(values,nfactors = 1)          
NDI <- cbind(acs_data,ND$scores) 

NDI <- NDI %>% 
  select(NAME,GEOID,PC1,57:64) %>% 
  separate(NAME, 
           into = c("Tract", "County","State"), 
           sep = ",") %>% 
  mutate(County = str_trim(County), 
         State = str_trim(State)) %>%
  rename(NDI = PC1)  %>% 
  as_tibble() 
  

## get geometry
tracts <- tracts(state = "NY",  
                 county = boroughs,
                 cb = TRUE)

boro_bound <-counties(state = "NY",
                      cb = TRUE) %>% 
  filter(NAME %in%
           boroughs)

#--- join to data
geo <- geo_join(tracts, NDI, 
                by_sp = "GEOID", 
                by_df = "GEOID")

#--- handling water tracts ----------

# from https://walkerke.github.io/tidycensus/articles/spatial-data.html
st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

# water tracts
ny_water <- purrr::reduce(
  purrr::map(boroughs, function(x){
    area_water("NY",
               county = x,
               class = "sf")
  }),
  rbind)

#-- set crs
proj <- st_crs(4269)

geo <- geo %>% 
  st_transform(proj)

boro_bound <- 
  boro_bound %>% 
  st_transform(proj)

ny_water <- ny_water %>% 
  st_transform(proj)

#--- remove water tracts
geo <- st_erase(geo,
                ny_water)

#--- visualize
map <- ggplot() + 
  geom_sf(data = geo, aes(fill = NDI), 
          color = NA) +
  #geom_sf(data = boro_bound,
  #        fill = NA,
  #        color = "#FFFFFF") +
  theme_minimal() + 
  theme(axis.text = element_blank(),
        legend.position = "bottom") +
  scico::scale_fill_scico(palette = "bilbao") +
  labs(fill = "NDI", 
       caption = "Source: 2013-2017 5-year ACS estimates")


