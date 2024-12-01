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
rawdata2020s_classchange <- rawdata2020s %>% mutate(DR_NO = as.character(DR_NO))

# data collected are same variables just split into decades. Below I am merging these to create one larger dataset to make the visualisation process simpler later on
crimedata1_merged <- bind_rows(rawdata2010s, rawdata2020s_classchange)

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


# creating homicide variable to filter for homicide only (lynching category was included in this as I aim to focus on intentional and unlawful killings).
homicide <- c(
    "CRIMINAL HOMICIDE",
    "LYNCHING"
    )

# filtering for crimes in homicide object 
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

# creating summary data - filtering out 2024 data as we don't have the full year
homicide_summary_data <- homicidedata3_date %>%
  mutate(year = year(date_occ)) %>%
  filter(year < 2024) %>%
  group_by(precinct_num, precinct_name, year) %>%
  summarise(
    homicide_count = n(),
    .groups = "drop"
  )

# saving summary data
write_csv(homicide_summary_data, here("data", "processed", "homicide_summary_data.csv"))


# -------------- processing geojson data --------------


# changing column names for consistent format, ease of interpretability, and to allow join
# filtering only for geometry and precinct_num columns necessary for join and visualisation
# changing precinct_num from int to factor - the column is not reprenting a numerical value

geo_map2_renamed <- geo_map1 %>%
  rename(
    "precinct_num" = `PREC`,
  ) %>%
  select(
    precinct_num,
    geometry
  )

# the precinct_num values require padding to match homicide data, code below will convert to character, pad, then convert to factor to enable the join which follows

geo_map3_pad <- geo_map2_renamed %>%
  mutate(
    precinct_num = as.character(precinct_num),
    precinct_num = str_pad(geo_map2_renamed$precinct_num, width = 2, side = "left", pad = "0"),
    precinct_num = as.factor(precinct_num)
  )

# -------------- final spatial processed data --------------

# joining data by precinct_num
joined_spatial_data <- left_join(geo_map3_pad, homicide_summary_data, by = "precinct_num") %>%
  arrange(year, precinct_num)


# sanity check - checking for missing values following join as it may indicate inconsistencies in data - no concerns
colSums(is.na(joined_spatial_data))

# saving joined data
st_write(joined_spatial_data, here("data", "processed", "joined_spatial_data.geojson"))

###################  ** Data Visualisation ** ####################


# ------------- visualisation 1: connected scatterplot ----------------------

# connected scatterplot graph visualisation of trends through the years

homicides_scatter <- 
  homicide_summary_data %>%
  group_by(year) %>%
  summarise(total_homicide_count = sum(homicide_count)) %>% # creating a total count of homicides per year across LAPD
  ggplot(aes(x = year, y = total_homicide_count)) + # mapping aesthetics
  geom_line(colour = "grey") + # adding simple line graph layer
  geom_point(color = "#69b3a2", size = 4) + # layering with scatterplot for connected scatter graph
  labs(
    title = "Homicides within LAPD jurisdiction (2010-2023)",
    caption = "Data Source: Los Angeles Open Data (2024)", # citing the data source
    x = "Year",
    y = "Number of Homicides"
  ) + # editing labels
  theme_minimal() + # opting for a minimal theme
  theme(
    plot.title = element_text(size = 15, family = "Helvetica", face = "bold"),  # title font
    axis.title = element_text(size = 10, family = "Helvetica", face = "bold"),  # axis title font
    panel.grid.minor = element_blank(), # removing all minor gridlines
    panel.grid.major.x = element_blank() # removing major x gridlines
  ) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1))  # including all years on x axis

# viewing the plot
homicides_scatter

# saving the plot
ggsave(
  filename = here("plots", "homicides_scatter.png"),
  plot = homicides_scatter
  )

# ------------- visualisation 2: animated choropleth map ----------------------

# animating the data year by year on choropleth map
homicides_choropleth <- 
  animate(
    ggplot(joined_spatial_data) + # using joined spatial dataset
      geom_sf(aes(fill = homicide_count)) + # fill by number of homicides in a polygon each year
      geom_text(
        aes(label = paste("Year:", year)), # dynamic year label
        x = -118.65,  
        y = 33.72,    # manual coordinates for label - chose bottom left due to layout of map 
        size = 5, color = "black", hjust = 0, vjust = 0, check_overlap = TRUE # adjusting format of label
      ) + 
      transition_time(year) + # animating by year
      theme_void() + # minimal theme
      scale_fill_viridis_c() + # colour-blind friendly scale applied
      labs(
        title = "Los Angeles Police Department Homicides", 
        subtitle = "by year and precinct",
        fill = "Number of\nHomicides",
        caption = "Data Source: Los Angeles Geohub (2024); Los Angeles Open Data (2024)"
      ) + # added labels
      theme(
        plot.title = element_text(size = 16, family = "Helvetica", hjust = 0.5),
        plot.subtitle = element_text(size = 14, family = "Helvetica", hjust = 0.5),
        plot.caption = element_text(face = "italic", size = 10)
      ), # adjusting formatting of text in visualisation
    nframes = length(unique(joined_spatial_data$year)), # setting number of years as number of frames
    width = 450, # setting width
    height = 650, # setting height
    fps = 1 # low fps to allow time to look at each choropleth map shown
  )

# view the animation
homicides_choropleth

# saving the animation
anim_save(
  filename = here("plots", "homicides_choropleth.gif"),
  animation = homicides_choropleth
  )


# ------------- visualisation 3: dumbbell plot ----------------------

# filtering and trimming away unnecessary data
dumbbell_data_filter <- homicide_summary_data %>%
  filter(year %in% c(2019, 2020))

# reformatting data
dumbbell_data_reformat <- dumbbell_data_filter %>%
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
homicides_dumbbell <- ggplot(dumbbell_data_reformat) +
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
    caption = "Data Source: Los Angeles Open Data (2024)", # citing the data source
    x = "Number of Homicides",
    y = "LAPD Precinct"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9, family = "Helvetica"),  # y-axis label size and font
    axis.text.x = element_text(size = 9, family = "Helvetica"),  # x-axis label size and font
    plot.title = element_text(size = 16, family = "Helvetica", face = "bold"),  # title font
    plot.subtitle = element_text(size = 13, family = "Helvetica", color = "grey"),  # subtitle font
    axis.title = element_text(size = 10, family = "Helvetica", face = "bold"),  # axis title font
    panel.grid.major.y = element_blank(), # removing horizontal grid lines
    panel.grid.minor = element_blank() # removing minor vertical lines
  )

# viewing the dumbbell plot
homicides_dumbbell

# saving final data
write_csv(dumbbell_data_reformat, here("data", "processed", "dumbbell_data_reformat.csv"))

# saving the final visualisation
ggsave(
  filename = here("plots", "homicides_dumbbell.png"),
  plot = homicides_dumbbell
  )
