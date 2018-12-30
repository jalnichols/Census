library(tidycensus)
library(tidyr)
library(dplyr)
library(stringr)
library(leaflet)
library(sf)
library(ggplot2)

#

#census_api_key("a64beca12279fdec7631cd82f307fa9c17b5962d", install = TRUE)

vars_2016 <- load_variables(2016, "acs5", cache = TRUE)

vars_2015 <- load_variables(2015, "acs5", cache = TRUE)

vars_2010 <- load_variables(2010, "sf1", cache = TRUE)

#

pa_ages <- get_acs(geography = "county subdivision",
                          county = c("Luzerne","Wyoming","Lackawanna"),
                          table = "B01002",
                          summary_var = "B01002_001",
                          state = "PA",
                          geometry = FALSE)

#

options(tigris_class = "sf")

cs_sf <- tigris::county_subdivisions(year = "2016", state = "PA")

#

pa_ages_data <- pa_ages %>%
  
  filter(str_detect(variable, "_001")) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_ages_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c()+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Median age (2016 ACS)")

#

pa_under_18 <- get_acs(geography = "county subdivision",
                   county = c("Luzerne","Wyoming","Lackawanna"),
                   table = "B09001",
                   summary_var = "B09001_001",
                   state = "PA",
                   geometry = FALSE)

pa_population <- get_acs(geography = "county subdivision",
                       county = c("Luzerne","Wyoming","Lackawanna"),
                       table = "B01003",
                       summary_var = "B01003_001",
                       state = "PA",
                       geometry = FALSE)

#

pa_under_18_data <- pa_under_18 %>%
  
  filter(str_detect(variable, "_001")) %>%
  
  select(GEOID, NAME, variable, estimate) %>%
  
  inner_join(
    
    pa_population %>%
      filter(str_detect(variable, "_001")) %>%
      select(GEOID, NAME, total_estimate = estimate), by = c("GEOID","NAME")) %>%
  
  mutate(estimate_no = estimate,
         estimate = estimate / total_estimate) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_under_18_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Population under 18 (2016 ACS)")

#

pa_college <- get_acs(geography = "county subdivision",
                       county = c("Luzerne","Wyoming","Lackawanna"),
                       table = "B14004",
                       summary_var = "B14004_001",
                       state = "PA",
                       geometry = FALSE)

pa_population <- get_acs(geography = "county subdivision",
                         county = c("Luzerne","Wyoming","Lackawanna"),
                         table = "B01003",
                         summary_var = "B01003_001",
                         state = "PA",
                         geometry = FALSE)

#

pa_college_data <- pa_college %>%
  
  filter(str_detect(variable, "_013") | str_detect(variable, "_029")) %>%
  
  group_by(GEOID, NAME) %>%
  
  summarize(estimate = sum(estimate, na.rm = T),
            total_estimate = mean(summary_est, na.rm = T)) %>%
  
  ungroup() %>%
  
  select(GEOID, NAME, estimate, total_estimate) %>%
  
  mutate(estimate_no = estimate,
         estimate = 1 - (estimate / total_estimate)) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_college_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Graduate degree (2016 ACS)")

# C15010_001

#

pa_educ <- get_acs(geography = "county subdivision",
                      county = c("Luzerne","Wyoming","Lackawanna"),
                      table = "B15003",
                      summary_var = "B15003_001",
                      state = "PA",
                      geometry = FALSE)

pa_population <- get_acs(geography = "county subdivision",
                         county = c("Luzerne","Wyoming","Lackawanna"),
                         table = "B01003",
                         summary_var = "B01003_001",
                         state = "PA",
                         geometry = FALSE)

#

pa_educ_data <- pa_educ %>%
  
  mutate(specific = as.numeric(str_replace(variable, "B15003_", ""))) %>%
  
  filter(specific > 22) %>%
  
  group_by(GEOID, NAME) %>%
  
  summarize(estimate = sum(estimate, na.rm = T),
            total_estimate = mean(summary_est, na.rm = T)) %>%
  
  ungroup() %>%
  
  select(GEOID, NAME, estimate, total_estimate) %>%
  
  mutate(estimate_no = estimate,
         estimate = (estimate / total_estimate)) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_educ_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Graduate degree (2016 ACS)")

#

pa_income <- get_acs(geography = "county subdivision",
                   county = c("Luzerne","Wyoming","Lackawanna"),
                   table = "B19001",
                   summary_var = "B19001_001",
                   state = "PA",
                   geometry = FALSE)

pa_population <- get_acs(geography = "county subdivision",
                         county = c("Luzerne","Wyoming","Lackawanna"),
                         table = "B01003",
                         summary_var = "B01003_001",
                         state = "PA",
                         geometry = FALSE)

#

pa_income_data <- pa_income %>%
  
  mutate(specific = as.numeric(str_replace(variable, "B19001_", ""))) %>%
  
  filter(specific > 1 & specific < 6) %>%
  
  group_by(GEOID, NAME) %>%
  
  summarize(estimate = sum(estimate, na.rm = T),
            total_estimate = mean(summary_est, na.rm = T)) %>%
  
  ungroup() %>%
  
  select(GEOID, NAME, estimate, total_estimate) %>%
  
  mutate(estimate_no = estimate,
         estimate = (estimate / total_estimate)) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_income_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Below 25k (2016 ACS)")

#

pa_rent_own <- get_acs(geography = "county subdivision",
                     county = c("Luzerne","Wyoming","Lackawanna"),
                     table = "B25119",
                     summary_var = "B25119_001",
                     state = "PA",
                     geometry = FALSE)

#

pa_rent_own_data <- pa_rent_own %>%
  
  filter(variable != "B25119_001") %>%

  inner_join(
    
    vars_2016, by = c("variable" = "name")
    
  ) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_rent_own_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::dollar)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  facet_wrap(~str_sub(label, 95, nchar(label)))+
  labs(title = "Income by Rental or Owner Occupied (2016 ACS)")

#

pa_med_income <- get_acs(geography = "county subdivision",
                       county = c("Luzerne","Wyoming","Lackawanna"),
                       table = "B19019",
                       summary_var = "B19019_001",
                       state = "PA",
                       geometry = FALSE)

#

pa_med_income_data <- pa_med_income %>%
  
  filter(str_sub(variable, 8, 10) %in% c("002", "003", "004", "005")) %>%
  
  mutate(estimate = estimate - summary_est) %>%
  
  group_by(variable) %>%
  
  mutate(rel_estimate = estimate - median(estimate, na.rm = T)) %>%
  
  ungroup() %>%
  
  inner_join(
    
    vars_2016, by = c("variable" = "name")
    
  ) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_med_income_data, aes(fill = rel_estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::dollar)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  facet_wrap(~str_sub(label, 18, nchar(label)))+
  labs(title = "Median income (relative) by household size (2016 ACS)")

#

pa_work_travel <- get_acs(geography = "county subdivision",
                         county = c("Luzerne","Wyoming","Lackawanna"),
                         table = "B08012",
                         summary_var = "B08012_001",
                         state = "PA",
                         geometry = FALSE)

#

pa_work_travel_data <- pa_work_travel %>%
  
  inner_join(
    
    vars_2016, by = c("variable" = "name")
    
  ) %>%
  
  mutate(specific = as.numeric(str_sub(variable, 8, 10))) %>%
  
  filter(specific > 1 & specific < 10) %>%
  
  group_by(GEOID, NAME) %>%
  
  summarize(estimate = sum(estimate),
            summary_est = mean(summary_est)) %>%
  
  ungroup() %>%
  
  mutate(estimate_spec = estimate,
         estimate = estimate / summary_est) %>%

  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_work_travel_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Commutes under 40 minutes (2016 ACS)")

#

pa_veteran <- get_acs(geography = "county subdivision",
                          county = c("Luzerne","Wyoming","Lackawanna"),
                          table = "B21001",
                          summary_var = "B21001_004",
                          state = "PA",
                          geometry = FALSE)

#

pa_veteran_data <- pa_veteran %>%
  
  inner_join(
    
    vars_2016, by = c("variable" = "name")
    
  ) %>%
  
  mutate(specific = as.numeric(str_sub(variable, 8, 10))) %>%
  
  filter(specific %in% c(5)) %>%
  
  mutate(estimate_spec = estimate,
         estimate = estimate / summary_est) %>%
  
  inner_join(
    
    cs_sf %>%
      select(GEOID, geometry, INTPTLAT, INTPTLON), by = c("GEOID")) %>%
  
  mutate(SHORT_NAME = str_replace(NAME, "Lackawanna County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Luzerne County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "Wyoming County", ""),
         SHORT_NAME = str_replace(SHORT_NAME, "borough", "bor"),
         SHORT_NAME = str_replace(SHORT_NAME, "township", "twp"),
         SHORT_NAME = str_replace(SHORT_NAME, "city", ""),
         SHORT_NAME = str_replace(SHORT_NAME, ", , Pennsylvania", ""))

#

ggplot(pa_veteran_data, aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent)+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Veterans (2016 ACS)")

#
#
#
#
#
#
#
#

pa_veteran_c <- get_acs(geography = "county",
                      table = "B21001",
                      summary_var = "B21001_019",
                      state = "PA",
                      geometry = TRUE)

#

pa_veteran_c_data <- pa_veteran_c %>%
  
  inner_join(
    
    vars_2016, by = c("variable" = "name")
    
  ) %>%
  
  mutate(specific = as.numeric(str_sub(variable, 8, 10))) %>%
  
  filter(specific %in% c(20)) %>%
  
  mutate(estimate_spec = estimate,
         estimate = estimate / summary_est) %>%

  mutate(SHORT_NAME = str_replace(NAME, " County, Pennsylvania", ""))

#

ggplot(filter(pa_veteran_c_data, !GEOID %in% c("02","72","15")), aes(fill = estimate, label = SHORT_NAME))+
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent, name = "Percentage")+
  stat_sf_coordinates(geom = "text", fill = "transparent")+
  labs(title = "Veterans (75+) (2016 ACS)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank())

#
#
#
#
#
#
#
#
#
#

pal <- colorNumeric(palette = "viridis", domain = pa_occup$estimate)

pa_occup %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Median age of housing stock",
            opacity = 1)

#

age <- get_acs(geography = "county", table = "B01001", summary_var = "B01001_001", state = "PA", geometry = TRUE) %>%
  
  select(-moe, -summary_moe) %>%
  
  filter(!variable %in% c("B01001_001","B01001_002","B01001_026")) %>%
  
  inner_join(
    
    vars_2015 %>%
      select(name, label) %>%
      mutate(name = str_replace(name, "E", "")), by = c("variable" = "name")
    
  ) %>%
  
  mutate(chars = nchar(label),
         gender = ifelse(str_detect(label, "Female:!!") == TRUE, "F", "M"),
         group = ifelse(gender == "F", str_sub(label, 10, chars), str_sub(label, 8, chars))) %>%
  
  mutate(group = ifelse(str_detect(group, "Under"), "0 to 4 years",
                        ifelse(str_detect(group, "over"), "85 to 99 years", group))) %>%
  select(-chars, -label) %>%
  
  mutate(age = ifelse(nchar(group) == 12, as.numeric(str_sub(group, 1, 1)), as.numeric(str_sub(group, 1, 2)))) %>%
  mutate(age = floor(age / 5) * 5) %>%
  select(-group) %>%
  
  group_by(NAME, GEOID, gender, age) %>%
  summarize(estimate = sum(estimate),
            summary_est = mean(summary_est)) %>%
  ungroup() %>%
  
  mutate(perc = estimate / summary_est,
         perc = ifelse(gender == "F", perc, perc * -1),
         prop = ifelse(gender == "F",perc, perc * -1)) %>%
  
  mutate(NAME = str_replace_all(NAME, ", Pennsylvania", ""))
  


#

library(ggplot2)

#

ggplot(filter(age, summary_est > 50000), aes(x = age, y = perc, fill = gender))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~NAME)+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()

#

median_age <- get_acs(geography = "tract", variable = "B01002_001", state = "PA", geometry = TRUE) %>%
  
  filter(is.na(estimate) == FALSE) %>%
  
  select(-moe) %>%
  
  #filter(variable %in% c("B01002_002","B01002_003")) %>%
  
  mutate(NAME = str_replace_all(NAME, ", ", "/")) %>%
  
  separate(NAME,  c("tract", "county"), sep = "/") %>%
  
  group_by(county) %>%
  
  mutate(median = median(estimate),
         tracts = n()) %>%
  
  ungroup()

#

ggplot(median_age, aes(x = reorder(county, -median), y = estimate, fill = tracts))+
  geom_boxplot()+
  coord_flip()+
  labs(x = "",
       y = "Average age",
       title = "Age by County in PA (2015 ACS)")+
  viridis::scale_fill_viridis()

#

ggsave("pa-age-county.png", height = 12, width = 6)

#
#
#

# congressional district employment

employment_data <- readr::read_csv("C:/Users/Jake/Downloads/Congressional_District_Employment/BP_2015_00A1_with_ann.csv")

industry = tibble::tibble(industry = c("Blue Collar","Blue Collar","Blue Collar","Blue Collar","Blue Collar","Blue Collar","Retail","Blue Collar",
                                       "White Collar","White Collar","White Collar","White Collar","White Collar","White Collar",
                                       "Services","Services","Services","Services","Services",
                                       "Other"),
                          code = c("11","21","22","23","31-33","42","44-45","48-49",
                                   "51","52","53","54","55","56",
                                   "61","62","71","72","81",
                                   "99"))

#

options(tigris_class = "sf")

cd_sf <- tigris::congressional_districts(year = 2015)

#

e_data <- employment_data %>%
  filter(!(YEAR.id == "Year")) %>%
  rename(cd = `GEO.display-label`,
         code = `NAICS.id`,
         GEOID = `GEO.id2`,
         job_type = `NAICS.display-label`,
         estab = `ESTAB`,
         employees = `EMP`,
         payroll = `PAYANN`) %>%
  select(GEOID, cd, code, job_type, estab, employees, payroll) %>%
  
  inner_join(
    industry, by = c("code")
    
  ) %>%
  
  mutate(cd = str_replace_all(cd, "Congressional District", "CD")) %>%
  
  mutate(estab = as.numeric(estab),
         employees = as.numeric(employees),
         payroll = as.numeric(payroll)) %>%
  
  mutate(payroll = ifelse(is.na(payroll), 0, payroll),
         employees = ifelse(is.na(employees), 0, employees),
         estab = ifelse(is.na(estab), 0, estab)) %>%
  
  group_by(cd) %>%
  
  mutate(cd_emp = sum(employees),
         cd_pay = sum(payroll)) %>%
  
  ungroup() %>%
  
  mutate(prop_emp = employees / cd_emp,
         prop_pay = payroll / cd_pay) %>%
  
  group_by(code) %>%
  
  mutate(perc_emp = percent_rank(prop_emp) * 100,
         perc_pay = percent_rank(prop_pay) * 100) %>%
  
  ungroup() %>%
  
  group_by(code) %>%
  
  mutate(code_avg_pay = sum(payroll) / sum(employees)) %>%
  
  ungroup() %>%
  
  group_by(code) %>%
  
  mutate(code_avg_emp = mean(prop_emp)) %>%
  
  ungroup() %>%
  
  mutate(avg_pay = payroll / employees,
         rel_pay = avg_pay / code_avg_pay,
         rel_emp = prop_emp / code_avg_emp,
         cd_avg_pay = cd_pay / cd_emp)

#



#

emp_data <- cd_sf %>%
  sf::select.sf(GEOID, geometry) %>%
  inner_join(e_data, by = c("GEOID")) %>%
  separate(cd, c("cd","state"), sep = ",") %>%
  mutate(cd = ifelse(nchar(cd) == 21, str_sub(cd,1,4), str_sub(cd, 1, 5)),
         state = str_sub(state, 2, nchar(state)))

#

i_data <- e_data %>%
  group_by(GEOID, industry, cd) %>%
  summarize(estab = sum(estab),
            employees = sum(employees),
            payroll = sum(payroll)) %>%
  ungroup() %>%
  group_by(GEOID) %>%
  mutate(total_emp = sum(employees),
         total_pay = sum(payroll)) %>%
  ungroup() %>%
  mutate(perc_type = employees / total_emp,
         avg_pay = total_pay / total_emp) %>%
  group_by(industry) %>%
  mutate(rel_type = perc_type - mean(perc_type),
         percentile = percent_rank(rel_type)) %>%
  ungroup() %>%
  group_by(GEOID) %>%
  mutate(rk = rank(-rel_type, ties.method = "first")) %>%
  ungroup()

#

industries <- cd_sf %>%
  sf::select.sf(GEOID, geometry) %>%
  inner_join(i_data, by = c("GEOID")) %>%
  separate(cd, c("cd","state"), sep = ",") %>%
  mutate(cd = ifelse(nchar(cd) == 21, str_sub(cd,1,4), str_sub(cd, 1, 5)),
         state = str_sub(state, 2, nchar(state)))

#
#
#
#


pal <- colorNumeric(palette = "viridis", domain = filter(industries, industry == "Retail")$perc_type)

pal <- colorFactor(palette = c("#37B36C","orange","purple","#404040"), domain = filter(industries, rk == 1)$industry)

industries %>%
  filter(!(GEOID %in% c("0200","1501","1502"))) %>%
  filter(state %in% c("Pennsylvania","New York","New Jersey","Delaware","Maryland","District of Columbia","Virginia","West Virginia","Ohio")) %>%
  filter(rk == 1) %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste0(cd,": ",industry),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = ~ percentile,
              color = ~ pal(industry)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ industry,
            title = "leading industry by CD",
            opacity = 1)

#
#
#
#
#
#

median_age <- get_acs(geography = "block group", table = c("B09021"), state = "PA", geometry = TRUE) %>%
  
  filter(is.na(estimate) == FALSE) %>%
  
  select(-moe) %>%
  
  #filter(variable %in% c("B01002_002","B01002_003")) %>%
  
  mutate(NAME = str_replace_all(NAME, ", ", "/")) %>%
  
  separate(NAME,  c("tract", "county"), sep = "/") %>%
  
  group_by(county) %>%
  
  mutate(median = median(estimate),
         tracts = n()) %>%
  
  ungroup()

#
#
#
#
#

counties <- c("Lackawanna County",
              "Luzerne County",
              "Wyoming County")

# Use purrr to loop through the counties and download median income at the
# at the tract level along with tract geometry

variable_choice <- "B27010"

nepa <- purrr::map(counties,
                ~ get_acs(
                  geography = "tract", 
                  table = c(paste0(variable_choice)), 
                  state = "PA",
                  county = .x,
                  geometry = TRUE))

#
#

nepa_data <- rbind(nepa[[1]],
                   nepa[[2]],
                   nepa[[3]]) %>%
  
  inner_join(
    
    vars_2015 %>%
      filter(str_detect(name, variable_choice)) %>%
      filter(!str_sub(name, nchar(name), nchar(name)) == "M") %>%
      mutate(variable = str_replace_all(name, "E", "")) %>%
      select(-name), by = c("variable")) %>%
  
  filter(!(str_detect(concept, paste0(variable_choice,"E")))) %>%
  
  separate(label, c("first","second","third"), sep = "!!") %>%
  
  mutate(fourth = ifelse(is.na(third),ifelse(second == "No health insurance coverage", "no coverage",NA),
                  ifelse(third %in% c("With employer-based health insurance only","With employer-based and direct-purchase coverage",
                                      "With employer-based and Medicare coverage"), "employer", 
                         ifelse(third %in% c("With Medicare coverage only","With Medicare and Medicaid/means-tested public coverage",
                                             "Other public only combinations","With Medicaid/means-tested public coverage only",
                                             "With TRICARE/military health coverage only","With VA Health Care only"), "public",
                                ifelse(third == "With direct-purchase health insurance only", "private", "other"))))) %>%
  
  mutate(highest_level = ifelse(first == "Total:",TRUE,FALSE),
         next_level = ifelse(highest_level == FALSE & is.na(second), TRUE, FALSE),
         final_level = ifelse(is.na(fourth), FALSE, TRUE)) %>%
  
  mutate(total_highest = ifelse(highest_level == TRUE, estimate, NA),
         total_next = ifelse(next_level == TRUE, estimate, NA),
         total_final = ifelse(final_level == TRUE, estimate, NA)) %>%
  
  group_by(GEOID) %>%
  
  mutate(total_highest = mean(total_highest, na.rm = T)) %>%
  
  ungroup() %>%
  
  group_by(first, GEOID) %>%
  
  mutate(total_next = mean(total_next, na.rm = T),
         total_final = sum(total_final, na.rm = T)) %>%
  
  ungroup() %>%
  
  group_by(first, GEOID, fourth) %>%
  
  mutate(total_estimate = sum(estimate)) %>%
  
  ungroup() %>%
  
  mutate(of_highest = estimate / total_highest,
         of_next = estimate / total_next,
         of_final = total_estimate / total_final) %>%
  
  mutate(first = ifelse(first == "Under 18 years:", "0 to 18 years:", first))

#

employer <- nepa_data %>%
  
  filter(!is.na(fourth)) %>%
  
  select(GEOID,  NAME, geometry, fourth, of_final, first) %>%
  
  unique() %>%
  
  spread(fourth, of_final) %>%
  
  gather(fourth, of_final, -GEOID, -geometry, -NAME, -first) %>%
  
  group_by(fourth, first) %>%
  
  mutate(type_average = mean(of_final, na.rm = T)) %>%
  
  ungroup() %>%
  
  mutate(relative = of_final - type_average)

#
#

ggplot()+
  
  geom_sf(data = filter(nepa_data, first == "35 to 64 years:" & !is.na(fourth)), 
          aes(fill = of_final))+
  
  scale_fill_viridis_c(labels = scales::percent, name = "Percentage")+
  
  facet_wrap(~fourth)

#

ggplot()+
  
  geom_sf(data = filter(employer, first == "0 to 18 years:"), 
          aes(fill = relative))+
  
  scale_fill_viridis_c(labels = scales::percent, name = "Percentage")+
  
  facet_wrap(~fourth)
