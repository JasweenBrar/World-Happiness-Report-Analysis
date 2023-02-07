library(dplyr)
library(tibble)

# Read world happiness dataset files
df_2015 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2015.csv")
df_2016 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2016.csv")
df_2017 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2017.csv")
df_2018 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2018.csv")
df_2019 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2019.csv")
df_2020 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2020.csv")
df_2021 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2021.csv")
df_2022 <- read.csv("C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\2022.csv")


# DATA TRANSFORMATION & DATA PRE-PROCESSING

# A quick view of the dataset helps us understand the inconsistencies in the dataset.
# We are going to be tackle these and make our dataset consistent:

# Step 1) 2020 & 2021 datsets do not have columns representing "happiness rank" and "happiness score". 
#    Happiness Score is called "ladder score". 
#    Adding the Happiness Rank by sorting the Ladder Score column and getting the highest score as number one rank.

adding_index1 <- seq(1,nrow(df_2020),by=1)
arrange(df_2020,'Ladder score')
mutate(df_2020,Happiness_Rank = adding_index1)
df_2020['Happiness_Rank'] = adding_index1

adding_index2 = seq(1,nrow(df_2021),by=1)
arrange(df_2021,'Ladder score')
df_2021['Happiness_Rank'] = adding_index2

# Step 2) Some datasets have "Region" column mapping each country to a specific region.
#    We want to add this column across all years. 
#    Here we will store this mapping from 2015 data and later merge it with our consolidated dataset.

regions_df <- select(df_2015,Country,Region)

# Step 3) We have inconsistent column names across the different years.
#    So, let us make them consistent.

# 2015 world happiness report data
df_2015 = select(df_2015,'Country','Happiness.Rank','Happiness.Score','Economy..GDP.per.Capita.','Family', 'Health..Life.Expectancy.', 'Freedom', 'Trust..Government.Corruption.','Generosity')
df_2015 <- df_2015 %>% rename('Happiness.Rank'='Happiness_Rank','Happiness.Score' = 'Happiness_Score','Economy..GDP.per.Capita.' = 'GDP_per_Capita','Family' ='Social_support','Health..Life.Expectancy.' = 'Life_Expectancy','Trust..Government.Corruption.' = 'Trust_in_Government')
mutate(df_2015,year = '2015')

#  2016 world happiness report data
df_2016 = select(df_2016,'Country','Happiness.Rank','Happiness.Score','Economy..GDP.per.Capita.','Family', 'Health..Life.Expectancy.', 'Freedom', 'Trust..Government.Corruption.','Generosity')
df_2016 <- df_2016 %>% rename('Happiness.Rank'='Happiness_Rank','Happiness.Score' = 'Happiness_Score','Economy..GDP.per.Capita.' = 'GDP_per_Capita','Family' ='Social_support','Health..Life.Expectancy.' = 'Life_Expectancy','Trust..Government.Corruption.' = 'Trust_in_Government')
mutate(df_2016,year = '2016')

#  2017 world happiness report data
df_2017 = select(df_2017,'Country','Happiness.Rank','Happiness.Score','Economy..GDP.per.Capita.','Family', 'Health..Life.Expectancy.', 'Freedom', 'Trust..Government.Corruption.','Generosity')
df_2017 <- df_2017 %>% rename('Economy..GDP.per.Capita.'='GDP_per_Capita','Family'='Social_support','Health..Life.Expectancy.'='Life_Expectancy','Trust..Government.Corruption.'='Trust_in_Government','Happiness.Rank'='Happiness_Rank','Happiness.Score':'Happiness_Score')
mutate(df_2017,year = '2017')

#  2018 world happiness report data
df_2018 = select(df_2018,'Country or region','Overall rank','Score','GDP per capita','Social support', 'Healthy life expectancy', 'Freedom to make life choices', 'Perceptions of corruption','Generosity')
df_2018 <- df_2018 %>% rename('GDP per capita'='GDP_per_Capita','Social support'='Social_support','Healthy life expectancy'='Life_Expectancy','Perceptions of corruption'='Trust_in_Government','Overall rank'='Happiness_Rank','Score'='Happiness_Score','Freedom to make life choices'='Freedom','Country or region'='Country')
mutate(df_2018,year = '2018')

#  2019 world happiness report data
df_2019 = select(df_2019,'Country or region','Overall rank','Score','GDP per capita','Social support', 'Healthy life expectancy', 'Freedom to make life choices', 'Perceptions of corruption','Generosity')
df_2019 <- df_2019 %>% rename('GDP per capita'='GDP_per_Capita','Social support'='Social_support','Healthy life expectancy'='Life_Expectancy','Perceptions of corruption'='Trust_in_Government','Overall rank'='Happiness_Rank','Score'='Happiness_Score','Freedom to make life choices'='Freedom','Country or region'='Country')
mutate(df_2019,year = '2019')

#  2020 world happiness report data
df_2020 = select(df_2020,'Country name','Happiness_Rank','Ladder score','Explained by: Log GDP per capita','Explained by: Social support', 'Explained by: Healthy life expectancy', 'Explained by: Freedom to make life choices', 'Explained by: Perceptions of corruption','Explained by: Generosity')
df_2020 <- df_2020 %>% rename('Explained by: Log GDP per capita'='GDP_per_Capita','Explained by: Social support'='Social_support','Explained by: Healthy life expectancy'='Life_Expectancy','Explained by: Perceptions of corruption'='Trust_in_Government','Ladder score'='Happiness_Score','Explained by: Freedom to make life choices'='Freedom','Country name'='Country','Explained by: Generosity'='Generosity')
mutate(df_2020,year = '2020')

#  2021 world happiness report data
df_2021 = select(df_2021,'Country name','Happiness_Rank','Ladder score','Explained by: Log GDP per capita','Explained by: Social support', 'Explained by: Healthy life expectancy', 'Explained by: Freedom to make life choices', 'Explained by: Perceptions of corruption','Explained by: Generosity')
df_2021 <- df_2021 %>% rename('Explained by: Log GDP per capita'='GDP_per_Capita','Explained by: Social support'='Social_support','Explained by: Healthy life expectancy'='Life_Expectancy','Explained by: Perceptions of corruption'='Trust_in_Government','Ladder score'='Happiness_Score','Explained by: Freedom to make life choices'='Freedom','Country name'='Country','Explained by: Generosity'='Generosity')
mutate(df_2021,year = '2021')

#  2022 world happiness report data
df_2022 = select(df_2022,'Country','RANK','Happiness score','Explained by: GDP per capita','Explained by: Social support', 'Explained by: Healthy life expectancy', 'Explained by: Freedom to make life choices', 'Explained by: Perceptions of corruption','Explained by: Generosity')
df_2022 <- df_2022 %>% rename('Explained by: GDP per capita'='GDP_per_Capita','Explained by: Social support'='Social_support','Explained by: Healthy life expectancy'='Life_Expectancy','Explained by: Perceptions of corruption'='Trust_in_Government','Happiness score'='Happiness_Score','Explained by: Freedom to make life choices'='Freedom','Country name'='Country','Explained by: Generosity'='Generosity','RANK'='Happiness_Rank')
mutate(df_2022,year = '2022')

# Step 4) Merging the datasets of all years together into a dataframe

happiness_index_df_final <- append(df_2015,df_2016)
happiness_index_df_final <- append(happiness_index_df_final,df_2017)
happiness_index_df_final <- append(happiness_index_df_final,df_2018)
happiness_index_df_final <- append(happiness_index_df_final,df_2019)
happiness_index_df_final <- append(happiness_index_df_final,df_2020)
happiness_index_df_final <- append(happiness_index_df_final,df_2021)
happiness_index_df_final <- append(happiness_index_df_final,df_2022)

# Step 5) Keep the records of countries having data  for all 8 years and remove other countries
# Number of countries in report for each year is different. 
# We want to create a unified set of countries for which we have data across 8 years to perform analysis. 
# Hence we would not perform analysis on countries with missing/incomplete information.

arrange(happiness_index_df_final,year)
happiness_index_df_final <- group_by(happiness_index_df_final,country) 
happiness_index_df_final = happiness_index_df_final[happiness_index_df_final.Country.isin(country_list)==True]

# Step 6) Further cleaning of data
# There are null values in the "Trust_in_Government" column specifically for UAE. 
# We will impute the missing values for this country with average "Trust_in_Government" value for UAE calculated using available data.
meanTrust<-mean(happiness_index_df_final$'Trust_in_Government', na.rm=TRUE)
happiness_index_df_final$'Trust_in_Government'[is.na(happiness_index_df_final$'Trust_in_Government')] <- meanTrust

# Finally, we write our compiled dataset to a csv file.
write.csv(happiness_index_df_final, "C:\\Users\\91922\\Desktop\\Fifth Sem Material 2022 oddsem\\data science R\\World_Happiness_Report_Analysis\\HappinessIndex.csv", row.names=FALSE)








