library(rio); library(here); library(tidyverse); library(knitr); library(janitor); library(magrittr); library(stringr)

election <- import(here("data", "countypres_2000-2016.csv")) %>% 
  clean_names() #load election results

s_capital_97 <- import(here("data", "social-capital-variables-for-1997.xlsx")) %>% 
  clean_names() #load social capital data for 1997

s_capital_05 <- import(here("data", "social-capital-variables-for-2005.xlsx")) %>% 
  clean_names() #load social capital data for 2005

s_capital_09 <- import(here("data", "social-capital-variables-for-2009.xlsx")) %>% 
  clean_names() #load social capital data for 2009

s_capital_14 <- import(here("data", "social-capital-variables-spreadsheet-for-2014.xlsx")) %>% 
  clean_names()#load social capital data for 2014


election %<>%
  select(-office, -version, -candidate, -state) %>% #select relevant variables
  filter(year %in% c(2000, 2008, 2012, 2016)) #select relevant years

colnames(election)[1] <- "year_elctn" #rename year variable for merging

elctn_00 <- election %>% filter(year_elctn == 2000) #create a df for each election of interest so that it's easier to match them with sc dfs
elctn_08 <- election %>% filter(year_elctn == 2008)
elctn_12 <- election %>% filter(year_elctn == 2012)
elctn_16 <- election %>% filter(year_elctn == 2016)

##social capital 1997
s_capital_97[290, 2] <- "District of Columbia, DC" #add DC's code

s_capital_97 %<>% #create county and state variables
  separate(areaname, c("county","state_po"), sep = ', ')

s_capital_97 %<>% #select relevant variables
  select(-fitnes97, -memspt97, -memnec97)

colnames(s_capital_97)[4:18] <- str_sub(colnames(s_capital_97)[4:18], end=-3) #remove the year from the variable names

s_capital_97 %<>% #create year variable before merging
  mutate(year_sc = 1997)

##social capital 2005
s_capital_05[290, 2] <- "District of Columbia, DC" #add DC's code

s_capital_05 %<>% #create county and state variables
  separate(areaname, c("county","state_po"), sep = ', ')

s_capital_05 %<>% #select relevant variables
  select(-fitns05)

colnames(s_capital_05)[4:18] <- str_sub(colnames(s_capital_05)[4:18], end=-3) #remove the year from the variable names

s_capital_05 <- s_capital_05[,c(1:3, 10, 5, 11, 4, 12, 7:8, 6, 9, 15, 14, 13, 16, 17:18)] #reorder the variables

s_capital_05$year_sc <- 2005 #create year variable before merging

##social capital 2009
s_capital_09[290, 2] <- "District of Columbia, DC" #add DC's code

s_capital_09 %<>% #create county and state variables
  separate(areaname, c("county","state_po"), sep = ', ')

s_capital_09 %<>% #select relevant variables
  select(-fitns09)

colnames(s_capital_09)[4:18] <- str_sub(colnames(s_capital_09)[4:18], end=-3) #remove the year from the variable names

s_capital_09 <- s_capital_09[,c(1:3, 10, 5, 11, 4, 12, 7:8, 6, 9, 14:15, 13, 16:18)] #reorder the variables

s_capital_09 %<>% #create year variable before merging
  mutate(year_sc = 2009)

##social capital 2014
s_capital_14[77, 2] <- "Hoonah-Angoon Census Area, AK" #remove the repeated state code

s_capital_14 %<>% #create county and state variables
  separate(county_name, c("county","state_po"), sep = ', ') 

s_capital_14$county <- word(s_capital_14$county, 1) #remove the word 'county' and keep only the name

s_capital_14 %<>% #select relevant variables
  select(-recreational2014)

colnames(s_capital_14)[4:18] <- str_sub(colnames(s_capital_14)[4:18], end=-5) #remove the year from the variable names

s_capital_14 <- s_capital_14[, c(1:3, 10, 5, 11, 4, 12, 7:8, 6, 9, 16, 15, 13, 17, 14, 18)] #reorder the variables

s_capital_14 %<>% #create year variable before merging
  mutate(year_sc = 2014)

colnames(s_capital_14) <- colnames(s_capital_09) #rename variables to make it compatible with previous years' datasets

s_capital <- bind_rows(s_capital_97, s_capital_05, s_capital_09, s_capital_14) #merge social capital data


df_00 <- inner_join(elctn_00, s_capital_97, by = "fips") #merge election 2000 & social capital 1997
df_00 <- df_00[, c(1, 25, 2:7, 10:24)] #remove duplicates and reorder variables
colnames(df_00)[3:4] <- c("state_po", "county") #rename variables

df_08 <- inner_join(elctn_08, s_capital_05, by = "fips") #merge election 2008 & social capital 2005
df_08 <- df_08[, c(1, 25, 2:7, 10:24)] #remove duplicates and reorder variables
colnames(df_08)[3:4] <- c("state_po", "county") #rename variables

df_12 <- inner_join(elctn_12, s_capital_09, by = "fips") #merge election 2012 & social capital 2009
df_12 <- df_12[, c(1, 25, 2:7, 10:24)] #remove duplicates and reorder variables
colnames(df_12)[3:4] <- c("state_po", "county") #rename variables

df_16 <- inner_join(elctn_16, s_capital_14, by = "fips") #merge election 2016 & social capital 2014
df_16 <- df_16[, c(1, 25, 2:7, 10:24)] #remove duplicates and reorder variables
colnames(df_16)[3:4] <- c("state_po", "county") #rename variables

df <- bind_rows(df_00, df_08, df_12, df_16) #merged dataset: contains all election and corresponding social capital data


#install.packages("usmap")
library(usmap)


df2014 <- df %>% 
  filter(year_sc == 2014, party == "democrat") %>% 
    mutate(demratio = candidatevotes / totalvotes)

#US Election Results 2016
plot_usmap(data = df2014, regions = "counties", values = "demratio") +
  scale_fill_continuous(low = "white", high = "blue", name = "Percentage of Democratic Votes (2016)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "bottom")

#Oregon election 016
plot_usmap(data = df2014, regions = "counties", include = c("OR"), values = "demratio") +
  scale_fill_continuous(low = "white", high = "blue", name = "Percentage of Democratic Votes (2016)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "right")

#US Social Capital 2014
plot_usmap(data = df2014, regions = "counties", values = "assn") +
  scale_fill_continuous(low = "white", high = "blue", name = "Social Capital Index (2014)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "bottom")

#Oregon social capital 2014
plot_usmap(data = df2014, regions = "counties", include = c("OR"), values = "assn") +
  scale_fill_continuous(low = "white", high = "blue", name = "Social Capital Index (2014)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "right")












plot_usmap(data = df2014, regions = "counties", values = "demratio") +
  scale_fill_continuous(low = "red", space = "Lab", high = "blue", name = "Percentage of Democratic Votes (2016)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "bottom")



plot_usmap(data = df2014, regions = "counties", values = "demratio") +
  scale_fill_continuous(type = "gradient", name = "Percentage of Democratic Votes (2016)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "bottom")




plot_usmap(data = df2014, regions = "counties", values = "pvote") +
  scale_fill_continuous(low = "white", high = "blue", name = "Voter Turnout (2000)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "bottom")




plot_usmap(data = df2, regions = "counties", values = "sk") +
  scale_fill_continuous(low = "white", high = "blue", name = "Social Capital Index (1997)", label = scales::comma
  ) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) + 
  theme(legend.position = "bottom")



    
    ]



   scale_fill_continuous(name = "Population (2012)", label = scales::comma) + 
   theme(legend.position = "right")

 plot_usmap(regions = "counties")


install.packages("stringr")
library(stringr)

str_pad(df2$fips, 5, pad = "0")
