########## LOS ANGELES CRIME RATES DATA ANALYSIS AND VISUALISATION PROJECT ########## 


###################  ** Setting up ** #################### 


# Dependencies:
## The project uses the sf package which requires the following dependencies: GEOS, GDAL, and PROJ. These are often installed automatically but consider setting them up if any issues are encountered.


# -------------- install packages --------------

install.packages("here")
install.packages("tidyverse")
install.packages("sf")
install.packages("gganimate")
install.packages("gifski")

# -------------- load libraries --------------

library(here)
library(tidyverse)
library(sf)
library(gifski)
library(gganimate)


# -------------- load datasets --------------

rawdata2010s <- read_csv(here("data", "raw", "crime_rawdata_2010s.csv")) 
rawdata2020s <- read_csv(here("data", "raw", "crime_rawdata_2020s.csv"))

# -------------- load geojson file --------------

geo_map1 <- st_read(here("data", "map_data", "geo_data.geojson"))


###################  ** Data Wrangling ** #################### 


# ------------------- initial sanity checks (visual) -------------------

# sanity check - visually looked through data in geo_map and crimedata1_merged, cross referencing to ensure area numbers are describing same locations - found no issues

# sanity check - `DATE.OCC` has false time information - has been inputted as midnight on all entries

# sanity check - lots of missing values in Crm Cd 2-4 columns, and in weapon cd and desc columns, these columns will not be included in visualisation

# ------------------- processing crime data pt 1-------------------

# DR_NO column is character in one raw dataset, numeric in the other. Changing to character as it is an identifier column.
rawdata2020s <- rawdata2020s %>% mutate(DR_NO = as.character(DR_NO))

# data collected are same variables just split into decades. Below I am merging these to create one larger dataset to make the visualisation process simpler later on
crimedata1_merged <- bind_rows(rawdata2010s, rawdata2020s)

# trimming unnecessary columns 
crimedata2_coltrim <- crimedata1_merged %>%
  select(
    `DATE OCC`, 
    `AREA`, 
    `AREA NAME`,
    `Crm Cd Desc`
  )

# renaming remaining columns
crimedata3_colrename <- crimedata2_coltrim %>%
  rename(
    "date_occ" = `DATE OCC`,
    "precinct_num" = `AREA`,
    "precinct_name" = `AREA NAME`,
    "crime_type" = `Crm Cd Desc`
  )

# checking for crime types of interest for the project

unique(crimedata3_colrename$crime_type)


# filtering for homicides. Note: negligent manslaughter is not included here, only intentional homicide will be explored (i.e. criminal homicide and lynching or any attempts to do either)

homicide <- c(
    "CRIMINAL HOMICIDE",
    "LYNCHING"
    )

homicidedata1 <- crimedata3_colrename %>%
  filter(
    crime_type %in% homicide
  )


# -------------- sanity checks pt 2 --------------

# sanity check - dates should only be from Jan 2010 - Oct 2024 (when data was pulled). Noticed range is incorrect due to data classed as character not date. This will be addressed below.
range(homicidedata1$date_occ)

# sanity check - following above check, now checking all data classes. 
# noted all character data classes will need to be changed. date_occ will be changed to date data class, remaining will be changed to factor as they are categorical variables.
columns <- c("date_occ", "precinct_num", "precinct_name", "crime_type")
sapply(homicidedata1[columns], class)

# sanity check - there should only be 21 LAPD precincts. geo_map will require padding allow join with homicide data.
range(homicidedata1$precinct_num) # 01 - 21
range(geo_map1$PREC) # 1 - 21

# -------------- processing crime data pt 2 --------------

# changing categorical from character to factor
homicidedata2_factors <- homicidedata1 %>%
  mutate(
    `precinct_name` = as.factor(`precinct_name`),
    `crime_type` = as.factor(`crime_type`),
    `precinct_num` = as.factor(`precinct_num`)
  )

# changing date from character to date class following sanity checks above
homicidedata3_date <- homicidedata2_factors %>%
  mutate(
    date_occ = as.Date(sub(" .*", "", date_occ), format = "%m/%d/%Y")
  )

# checking range of dates - range is now as expected.
range(homicidedata3_date$date_occ)

# creating summary data - not including 2024 data as we don't have the full year

homicidedata4_summary <- homicidedata3_date %>%
  mutate(year = year(date_occ)) %>%
  filter(year < 2024) %>%
  group_by(precinct_num, precinct_name, year) %>%
  summarise(
    homicide_count = n(),
    .groups = "drop"
  )


#### NOTE TO SELF - SAVE FINAL PROCESSED DATA 

# -------------- processing geojson data --------------


# changing column names for consistent format, ease of interpretability, and to allow join
# removing precinct name to avoid duplicate column in merged data later on
# changing precinct_num from int to factor - the column is not reprenting a numerical value

geo_map2_renamed <- geo_map1 %>%
  rename(
    "precinct_num" = `PREC`,
    "area" = `AREA`,
    "perimeter" = `PERIMETER`
  ) %>%
  select(
    -`APREC`,
    -`OBJECTID`
  )

# the precinct_num values require padding to match homicide data, code below will convert to character, pad, then convert to factor to enable the join which follows

geo_map3_pad <- geo_map2_renamed %>%
  mutate(
    precinct_num = as.character(precinct_num),
    precinct_num = str_pad(geo_map2_renamed$precinct_num, width = 2, side = "left", pad = "0"),
    precinct_num = as.factor(precinct_num)
  )

# -------------- final full dataset --------------

joined_data <- left_join(geo_map3_pad, homicidedata4_summary, by = "precinct_num") %>%
  arrange(year, precinct_num)


# sanity check - checking for missing values following join as it may indicate inconsistencies in data - no concerns

colSums(is.na(joined_data))

###### SAVE THIS PROCESSED DATA INTO THE FOLDER

###################  ** Data Visualisation ** ####################


# ------------- visualisation 1: connected scatterplot ----------------------

# connected scatterplot graph visualisation of trends through the years

homicides_scatter <- homicidedata4_summary %>%
  group_by(year) %>% 
  summarise(total_homicide_count = sum(homicide_count)) %>% 
  ggplot(aes(x = year, y = total_homicide_count)) + 
  geom_line(color = "grey") +
  geom_point(color = "#69b3a2", size = 4) + 
  labs(
    title = "Total Homicide Count by Year (2010-2023)",
    x = "Year",
    y = "Total Homicide Count"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) 

# viewing the plot
homicides_scatter

# saving the plot
ggsave(
  filename = here("plots", "homicides_scatter.png"),
  plot = homicides_scatter
  )

# ------------- visualisation 2: animated choropleth map ----------------------

# Animating the data year by year on choropleth map

p <- ggplot(joined_data) +
  geom_sf(aes(fill = homicide_count)) +
  theme_void() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = 'Year: {frame_time}', fill = 'Homicide Count') +
  transition_time(year)


homicides_choropleth <- animate(p, nframes = length(unique(joined_data$year)), width = 500, height = 375, fps = 1)
homicides_choropleth

# saving the animation
anim_save(
  filename = here("plots", "homicides_choropleth.gif"),
  animation = homicides_choropleth
  )


# ------------- visualisation 3: dumbbell plot ----------------------

# filtering and trimming away unnecessary data
dumbbell_data_filter <- homicidedata4_summary %>%
  filter(year %in% c(2019, 2020))

# reformatting data as wide data
dumbbell_data_wide <- dumbbell_data_filter %>%
  pivot_wider(
    names_from = year,
    values_from = homicide_count,
    names_prefix = "count_"
  ) 

# wrapping subtitle
wrapped_subtitle <- str_wrap(
  "The LAPD reported a sharp rise in homicides in the first year of the pandemic, but to differing degrees across precincts",
  width = 110)

# creating the plot
homicides_dumbbell <- ggplot(dumbbell_data_wide) +
  geom_segment(aes(
    x = count_2019, xend = count_2020,
    y = reorder(precinct_name, count_2020), yend = reorder(precinct_name, count_2020)
  ), linetype = "dotted", color = "#27408B", linewidth = 0.4) + # creating dotted line - colour matches point of interest (2020 count)
  # small orange dots for 2019
  geom_point(aes(x = count_2019, y = reorder(precinct_name, count_2020), color = "2019"), size = 2) +
  # larger blue dots for 2020 - to focus consumer's eye on the point of interest
  geom_point(aes(x = count_2020, y = reorder(precinct_name, count_2020), color = "2020"), size = 3.5) +
  # 
  scale_color_manual(
    name = "Year",  
    values = c("2019" = "#FFB385", "2020" = "#27408B")
  ) +
  labs(
    title = "Los Angeles Police Department Homicide Rates",
    subtitle = wrapped_subtitle,
    x = "Number of Homicides",
    y = "LAPD Precinct"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7, family = "Helvetica"),  # Adjust y-axis label size and font
    axis.text.x = element_text(size = 7, family = "Helvetica"),  # Adjust x-axis label size and font
    plot.title = element_text(size = 15, family = "Helvetica", face = "bold"),  # Title font
    plot.subtitle = element_text(size = 12, family = "Helvetica", color = "grey"),  # Subtitle font
    axis.title = element_text(size = 8, family = "Helvetica", face = "bold"),  # Axis title font
    panel.grid.major.y = element_blank(), # removing horizonal grid lines
    panel.grid.minor = element_blank() # removing minor veritcal lines
  )
homicides_dumbbell

ggsave(
  filename = here("plots", "homicides_dumbbell.png"),
  plot = homicides_dumbbell
  )








###### Animated plot to put into rmd file

```{r animation}

df <- homicidedata5_date %>%
  mutate(year = year(date_occ)) %>%
  filter(year >= 2010 & year <= 2023) %>% 
  group_by(precinct_num, precinct_name, year) %>%
  summarise(
    homicide_count = n(),
    .groups = "drop" 
  ) 

df2 <- left_join(geo_map3_pad, df, by = "precinct_num") %>%
  arrange(year, precinct_num)


p <- ggplot(df2) +
  geom_sf(aes(fill = homicide_count)) +
  theme_void() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = 'Year: {frame_time}', fill = 'Homicide Count') +
  transition_time(year)


la_hom_choropleth <- animate(p, nframes = length(unique(df2$year)), width = 500, height = 375, fps = 1)

# saving the animation
anim_save(here("plots", "homicide_animation.gif", la_hom_choropleth))


```
