# Description ----------- -------------------------------------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Created by Marc-Olivier Beausoleil                                                  
# McGill University                                                                   
# Created Saturday, March 11, 2023
# Why:             
  # Script that calculates the mean median beak measurements and finds pox related information 
# Requires
  # Install the libraries before running the script 
# NOTES:
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(xlsx)

# Import the data  --------------------------------------------------------
finch.data = read.xlsx("data/data.finches.scandens_2019.2020.cleaned.2023.03.11.xlsx",
                       sheetIndex = 1)

# Select the years you are interested in 
selected.yrs = 2019:2020

# Subset the data 
finch.data = finch.data %>%
  # Filter data 
  filter(Species == "scandens",
         Year %in% selected.yrs) %>% 
  # Make beak measurements numeric 
  # Might have warning, because there are NAs 
  mutate(across(starts_with("MedianBeak"), as.numeric),
         Year = as.factor(Year))

# Get every year means
sum.mea = finch.data %>% 
  group_by(Year) %>% # For each year
  summarise(mean.bl = mean(MedianBeakLength, na.rm = TRUE),
            mean.bd = mean(MedianBeakDepth, na.rm = TRUE),
            nb = n()) # Count NB of ID in each year 
sum.mea

# Filter the data 
ggp.beak.meas = finch.data %>% 
  # Plot the data 
  ggplot(mapping = aes(x = MedianBeakLength, 
                       y = MedianBeakDepth, 
                       col = Year)) +
  # Add all the points 
  geom_point() +
  # Get just the means in selected years
  geom_point(data = sum.mea, 
             mapping = aes(x = mean.bl, 
                           y = mean.bd), 
             alpha = .5, 
             size = 10) + 
  scale_color_viridis_d() +
  theme_bw()

ggsave(filename = "output/ggpl.beak.measurements.png", plot = ggp.beak.meas, device = "png",
       width = 8, height = 5)

# Pox information ---------------------------------------------------------
# Correct the notes on pox infection 
finch.data$Pox.Notes[is.na(finch.data$Pox.Notes)] <-"N"
finch.data[finch.data$Pox.Notes %in% c("","n","NA"),"Pox.Notes"] <- "N"
finch.data[finch.data$Pox.Notes %in% c("infected","pox on foot","Pox on toes and tarsus","y","yes", "Yes", "R and L", "L", "L+R","R1", "Yes, both tarsus, developping, bigger tarsus"),"Pox.Notes"] <- "Y"

# Make a table of the pox infections 
table(finch.data$Pox.Notes, finch.data$Year)

# Check for missing toes, claws or (|) broken 
finch.data$Notes[grep(pattern = "miss|brok", x = finch.data$Notes)]
# Show the notes in each year
table(finch.data[grep(pattern = "miss|brok", x = finch.data$Notes), c('Year',"Notes")])

