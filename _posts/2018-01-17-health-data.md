---
layout: post
title: "Data Exploration with A Shiny App"
date: 2018-01-17
---

Better Hospital Quality for Better Health
-----------------------------------------

I want to do an exploratory analysis to see how health outcome is effected by quality of hospitals. Health outcome in this analysis is defined as prevelance of measures of chronic diseases in the 500 Cities dataset, while quality of hosptials is measured according to the Hospital Value-Based Purchasing program's survey. I also incorporate demographic data to control for any difference in ethnicity, income, or age of populations in these 500 cities.

### Data source

The data sets used here are from www.data.gov/health/, including:

-   **500 Cities: Local Data for Better Health:** includes 2013, 2014 model-based small area estimates (% of prevelence) for 27 measures of chronic disease related to unhealthy behaviors (5), health outcomes (13), and use of preventive services (9) for the 500 largest US cities \[1\].

-   **Hospital Value-Based Purchasing - Total Performance Score (HVBP)** (Fiscal Year 2018): allows you to compare the quality of care at over 4,000 Medicare-certified hospitals across the country based on their Clinical Process of Care domain scores, Patient Experience of Care dimension scores, and Total Performance Scores \[2\].

Besides the two maiin data sets mentioned above, I want to control for demographic variables in the analysis:

-   **Demographics**: from U.S. ACS 5-year 2016 for each city. Variables that might influence health outcome/disease prevelance are median age, proportion of male/female, median income, percent of each race/ethnicity, education attainment.

I download the 2016 5-year Community Survey Data Profile using the package `censusapi` \[3\]. As mentioned, I'm interested in getting median household income (api code: DP03\_0062E), percent male/female (DP05\_0002PE, DP05\_0003PE), median age (DP05\_0017E), educational attainment measured by percent of population with a high school diploma (DP02\_0061PE), percent of each race and ethnicity (DP05\_0032PE, DP05\_0033PE, DP05\_0034PE, DP05\_0039PE, DP05\_0066PE) for every cities in the U.S. (API name and variable names can be looked up with `listCensusApis()`, `listCensusMetadata()` or from [census.gov](https://www.census.gov/data/developers/data-sets/)). Cities-level demographic information can be obtained by using `place` as the geographic unit. The name of a place contains both state and city information which I can extract using regular expression in R.

``` r
# Getting ACS 5 year 2016 data profile
mycensuskey <- "1e6df3fa695ea39a8969965e90e8fd11dc782b3f"
myvintage <- 2016
myapiname <- "acs/acs5/profile"

#percent white, black, native, asian, hispanic (of all race), percent male, percent female, median age, median household income, percent with high school diploma
interest_vars <- c("NAME","DP05_0032PE","DP05_0033PE","DP05_0034PE","DP05_0039PE", "DP05_0066PE", 
                   "DP05_0002PE", "DP05_0003PE",
                   "DP05_0017E",
                   "DP03_0062E",
                   "DP02_0061PE")
interest_vars_names <- c("NAME","state", "place", "percent_white", "percent_black", "percent_native", "percent_asian", "percent_hispanic", "percent_male", "percent_female", "median_age", "median_income", "percent_GED")

#call the census api
Demographics <- getCensus(name = myapiname, vintage = myvintage, key = mycensuskey, vars = interest_vars, region = "place:*", regionin = "state:*")

#create a variable for state and city by splitting the name of each place
Demographics <- Demographics %>%
  `colnames<-`(interest_vars_names) %>%
  mutate(city = trimws(gsub("city,.*|CDP,.*|town,.*", "", NAME))) %>%
  mutate(state.abb = trimws(gsub(".*,", "", NAME)))
```

### Clean and merge data

Let's first take a look at the data.

``` r
# Load 500 Cities data
Health <- read.csv("500_Cities__Local_Data_for_Better_Health__2017_release.csv")

# Load Hospital Value-Based Purchasing - Total Performance Score data
Performance <- read.csv("Hospital_Value-Based_Purchasing__HVBP____Total_Performance_Score.csv")
```

Table 1: Number of variables and data points in each data set

    ## 
    ## ---------------------------------------------------------
    ##          &nbsp;           N. observations   N. variables 
    ## ------------------------ ----------------- --------------
    ##      **500 Cities**           810103             24      
    ## 
    ##  **Hospital Value-Based        2808              16      
    ##       Purchasing**                                       
    ## 
    ##     **Demographics**           29574             15      
    ## ---------------------------------------------------------

Table 1 shows that there are many more data points in the HVBP and Demographics data than in 500 Cities. Specifically, there are measurements of performance for several hospitals in each city. I will condense this data table by creating average hospital performance scores (ranging from column eight to sixteen) for each city.

``` r
Condensed_Performance <- Performance %>%
  mutate_at(names(Performance)[8:16], as.numeric) %>%
  group_by(City, State) %>%
  summarise_at(names(Performance)[8:16], mean)
```

Notice that all three datasets contain information on the city level, thus, I join these tables based on a city's name and state, keeping only cities that have data from all tables. There are many cities in the Demographic data set that don't have health outcome measure. These cities will be automatically removed after joining table. (Since 500 Cities and Demographics both have city and unabbriviated state names, I will join these two first and then the Performance data.)

500 Cities contains crude prevelance (number occurances of disease divided by total population) and age-adjusted prevelance (crude prevelance accounted for different age profiles of local population.) I choose to filter for only age-adjusted prevelance.

``` r
Joined_Data <- Health %>% 
  filter(GeographicLevel=="City", DataValueTypeID=="AgeAdjPrv") %>%
  mutate_at(c("CityName", "StateDesc"), as.character) %>%
  inner_join(Demographics, by=c("CityName"="city", "StateDesc" = "state.abb")) %>%
  mutate(CityName = toupper(CityName)) %>%
  inner_join(Condensed_Performance, by=c("StateAbbr"="State", "CityName"="City"))
```

Table 2: Preview of joined 500 Cities, HVBP, and Demographics Data

    ## 
    ## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ##  Year   StateAbbr   StateDesc    CityName    GeographicLevel   DataSource        Category         UniqueID              Measure               Data_Value_Unit   DataValueTypeID       Data_Value_Type       Data_Value   Low_Confidence_Limit   High_Confidence_Limit   Data_Value_Footnote_Symbol   Data_Value_Footnote   PopulationCount     GeoLocation     CategoryID   MeasureId   CityFIPS   TractFIPS   Short_Question_Text              NAME             state   place   percent_white   percent_black   percent_native   percent_asian   percent_hispanic   percent_male   percent_female   median_age   median_income   percent_GED   Unweighted.Normalized.Clinical.Care.Domain.Score   Weighted.Normalized.Clinical.Care.Domain.Score   Unweighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score   Weighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score   Unweighted.Normalized.Safety.Domain.Score   Weighted.Safety.Domain.Score   Unweighted.Normalized.Efficiency.and.Cost.Reduction.Domain.Score   Weighted.Efficiency.and.Cost.Reduction.Domain.Score   Total.Performance.Score 
    ## ------ ----------- ----------- ------------ ----------------- ------------ --------------------- ---------- -------------------------------- ----------------- ----------------- ------------------------- ------------ ---------------------- ----------------------- ---------------------------- --------------------- ----------------- ----------------- ------------ ----------- ---------- ----------- ---------------------- -------------------------- ------- ------- --------------- --------------- ---------------- --------------- ------------------ -------------- ---------------- ------------ --------------- ------------- -------------------------------------------------- ------------------------------------------------ --------------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------- ------------------------------------------- ------------------------------ ------------------------------------------------------------------ ----------------------------------------------------- -------------------------
    ##  2015      AL        Alabama    BIRMINGHAM        City           BRFSS          Prevention        0107000        Current lack of health              %             AgeAdjPrv      Age-adjusted prevalence      19.8              19.5                   20.2                                                                   212237        (33.5275663773,    PREVENT      ACCESS2     107000       NA         Health Insurance     Birmingham city, Alabama    01     07000       24.3             72              0.2              0.9              3.4              46.9            53.1           35.7          32404          28.6                            23.67                                             12.83                                                                   34.83                                                                                        85.17                                                               62.33                                77.83                                             1.5                                                         3.667                                   666.8          
    ##                                                                                                               insurance among adults aged                                                                                                                                                                                                    -86.7988174678)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ##                                                                                                                    18Ã¢â¬â64 Years                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    ## 
    ##  2015      AL        Alabama    BIRMINGHAM        City           BRFSS        Health Outcomes     0107000     Arthritis among adults aged            %             AgeAdjPrv      Age-adjusted prevalence       31               30.8                   31.1                                                                   212237        (33.5275663773,    HLTHOUT     ARTHRITIS    107000       NA            Arthritis         Birmingham city, Alabama    01     07000       24.3             72              0.2              0.9              3.4              46.9            53.1           35.7          32404          28.6                            23.67                                             12.83                                                                   34.83                                                                                        85.17                                                               62.33                                77.83                                             1.5                                                         3.667                                   666.8          
    ##                                                                                                                        >=18 Years                                                                                                                                                                                                            -86.7988174678)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ## 
    ##  2015      AL        Alabama    BIRMINGHAM        City           BRFSS      Unhealthy Behaviors   0107000     Binge drinking among adults            %             AgeAdjPrv      Age-adjusted prevalence      11.2              11.1                   11.3                                                                   212237        (33.5275663773,     UNHBEH       BINGE      107000       NA          Binge Drinking      Birmingham city, Alabama    01     07000       24.3             72              0.2              0.9              3.4              46.9            53.1           35.7          32404          28.6                            23.67                                             12.83                                                                   34.83                                                                                        85.17                                                               62.33                                77.83                                             1.5                                                         3.667                                   666.8          
    ##                                                                                                                     aged >=18 Years                                                                                                                                                                                                          -86.7988174678)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ## 
    ##  2015      AL        Alabama    BIRMINGHAM        City           BRFSS        Health Outcomes     0107000      High blood pressure among             %             AgeAdjPrv      Age-adjusted prevalence      44.1              43.9                   44.2                                                                   212237        (33.5275663773,    HLTHOUT      BPHIGH      107000       NA       High Blood Pressure    Birmingham city, Alabama    01     07000       24.3             72              0.2              0.9              3.4              46.9            53.1           35.7          32404          28.6                            23.67                                             12.83                                                                   34.83                                                                                        85.17                                                               62.33                                77.83                                             1.5                                                         3.667                                   666.8          
    ##                                                                                                                  adults aged >=18 Years                                                                                                                                                                                                      -86.7988174678)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ## 
    ##  2015      AL        Alabama    BIRMINGHAM        City           BRFSS          Prevention        0107000    Taking medicine for high blood          %             AgeAdjPrv      Age-adjusted prevalence      70.1               70                    70.2                                                                   212237        (33.5275663773,    PREVENT       BPMED      107000       NA       Taking BP Medication   Birmingham city, Alabama    01     07000       24.3             72              0.2              0.9              3.4              46.9            53.1           35.7          32404          28.6                            23.67                                             12.83                                                                   34.83                                                                                        85.17                                                               62.33                                77.83                                             1.5                                                         3.667                                   666.8          
    ##                                                                                                              pressure control among adults                                                                                                                                                                                                   -86.7988174678)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ##                                                                                                                aged >=18 Years with high                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    ##                                                                                                                      blood pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    ## 
    ##  2015      AL        Alabama    BIRMINGHAM        City           BRFSS        Health Outcomes     0107000    Cancer (excluding skin cancer)          %             AgeAdjPrv      Age-adjusted prevalence      5.7               5.6                     5.7                                                                   212237        (33.5275663773,    HLTHOUT      CANCER      107000       NA       Cancer (except skin)   Birmingham city, Alabama    01     07000       24.3             72              0.2              0.9              3.4              46.9            53.1           35.7          32404          28.6                            23.67                                             12.83                                                                   34.83                                                                                        85.17                                                               62.33                                77.83                                             1.5                                                         3.667                                   666.8          
    ##                                                                                                               among adults aged >=18 Years                                                                                                                                                                                                   -86.7988174678)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

This data is now in a standardized format that can be visualized and analyzed.

### Understanding each variable

To better understand individual variables, I first look that their summary statistics such as mean, median, standard deviation, and interquantile range.

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -666666666      42802      50360   -1590976      64502     134188

Notice median household income has negative value of -66666666 which is impossible. This indicates missing value. It turns out that Mesquite city in TX has missing median household income. Since this is only one city, I remove this city from the sample.

``` r
Joined_Data <- Joined_Data %>%
  filter(median_income >= 0)
```

Table 3: Summary statistics for variables of interests

    ## 
    ## 
    ##                  variable                     min    median    mean      max      IQR     standard deviation 
    ## ------------------------------------------- ------- -------- -------- --------- -------- --------------------
    ##                 Prevelance                    1.7     23.4    30.58     89.4      36.2          24.68        
    ##            Prevelance low limit               1.6    23.05    30.23     89.1     35.95          24.57        
    ##            Prevelance high limit              1.7     23.9    30.92     89.8     36.45          24.78        
    ##              Population count                42417   112534   221440   8175133   111967         502015       
    ##                Percent white                 13.6      68     66.09     97.8     23.65           16.8        
    ##                Percent black                  0.3     9.75    15.64     81.5     17.85          16.26        
    ##               Percent native                   0      0.4      0.68      11       0.4            0.88        
    ##                Percent asian                  0.2     3.9      6.81     57.3      5.4            8.16        
    ##              Percent hispanic                 1.3     17.2    23.99     96.3     27.73          20.36        
    ##                Percent male                   45      48.9    48.96     58.3     1.325           1.38        
    ##               Percent female                 41.7     51.1    51.04      55      1.325           1.38        
    ##                 Median age                   22.6     34.6     34.9     69.7      4.6            4.41        
    ##           Median household income            24448   50366    55251    134188    21678          18759        
    ##              Percent with GED                  7      24.9     24.3     41.5      8.8            6.83        
    ##    Unweighted Clinical Care Domain score       1     19.12    20.18      41      12.58           9.11        
    ##     Weighted Clinical Care Domain score        1       27     27.61      55      21.81          14.62        
    ##  Unweighted Care Coordination Domain score     2       24     27.05      91        19           15.66        
    ##   Weighted Care Coordination Domain score      2      124     110.4      163     54.77          39.78        
    ##       Unweighted Safety Domain score           3       63     65.76      149     28.81          23.59        
    ##        Weighted Safety Domain score            2       73     82.79      187     88.42          55.06        
    ##   Unweighted Cost Reduction Domain score       1       2       2.63      10        3             1.9         
    ##    Weighted Cost Reduction Domain score        1      4.5      6.41      19       8.5            5.8         
    ##           Total Performance score              7     718.6    698.2     1579     500.8          347.1

Figure 1 shows that distributions of many demographic variables are skewed right and that there are outliners that should be taken into account in more indepth analysis. Measures of quality of hospitals seem to be normally distributed except for Cost Reduction Domain scores, which is skewed right. I apply log and logit transformation to make the demographic variables' distributions more symmetric. Log transformation did not improve Cost Reduction Domain score, so I leave it as is for now. Figure 4 shows that the demographic variables' distributions improve.

``` r
Log_Joined_Data <- Joined_Data %>%
  mutate_at(c("percent_black", "percent_native", "percent_asian", "percent_hispanic"), car::logit) %>%
  mutate_at(c("median_income"), log)
```

Figure 1: Histograms showing distribution of each variable

<img src="https://PhucHNg.github.io/images/health_data/histogram.png" width="600">

<br> \#\#\#\#A closer look at prevelance for measures of chronic diseases

Since 500 Cities data set records prevelance of 27 measures of chronic diseases related to three categories: health outcomes, prevention, and unhealthy behaviors.`tbls("measureDesp", display="cite")` shows the ID, description, and category of each measure.

Table 4: Health measures' IDs, descriptions and categories

    ## 
    ## 
    ## ID             Measure                                                                                                                                                                                          Category             
    ## -------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ ---------------------
    ## ACCESS2        Current lack of health insurance among adults aged 18-64 Years                                                                                                                                   Prevention           
    ## ARTHRITIS      Arthritis among adults aged >=18 Years                                                                                                                                                           Health Outcomes      
    ## BINGE          Binge drinking among adults aged >=18 Years                                                                                                                                                      Unhealthy Behaviors  
    ## BPHIGH         High blood pressure among adults aged >=18 Years                                                                                                                                                 Health Outcomes      
    ## BPMED          Taking medicine for high blood pressure control among adults aged >=18 Years with high blood pressure                                                                                            Prevention           
    ## CANCER         Cancer (excluding skin cancer) among adults aged >=18 Years                                                                                                                                      Health Outcomes      
    ## CASTHMA        Current asthma among adults aged >=18 Years                                                                                                                                                      Health Outcomes      
    ## CHD            Coronary heart disease among adults aged >=18 Years                                                                                                                                              Health Outcomes      
    ## CHECKUP        Visits to doctor for routine checkup within the past Year among adults aged >=18 Years                                                                                                           Prevention           
    ## CHOLSCREEN     Cholesterol screening among adults aged >=18 Years                                                                                                                                               Prevention           
    ## COLON_SCREEN   Fecal occult blood test, sigmoidoscopy, or colonoscopy among adults aged 50-75 Years                                                                                                             Prevention           
    ## COPD           Chronic obstructive pulmonary disease among adults aged >=18 Years                                                                                                                               Health Outcomes      
    ## COREM          Older adult men aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening                                 Prevention           
    ## COREW          Older adult women aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 Years   Prevention           
    ## CSMOKING       Current smoking among adults aged >=18 Years                                                                                                                                                     Unhealthy Behaviors  
    ## DENTAL         Visits to dentist or dental clinic among adults aged >=18 Years                                                                                                                                  Prevention           
    ## DIABETES       Diagnosed diabetes among adults aged >=18 Years                                                                                                                                                  Health Outcomes      
    ## HIGHCHOL       High cholesterol among adults aged >=18 Years who have been screened in the past 5 Years                                                                                                         Health Outcomes      
    ## KIDNEY         Chronic kidney disease among adults aged >=18 Years                                                                                                                                              Health Outcomes      
    ## LPA            No leisure-time physical activity among adults aged >=18 Years                                                                                                                                   Unhealthy Behaviors  
    ## MAMMOUSE       Mammography use among women aged 50-74 Years                                                                                                                                                     Prevention           
    ## MHLTH          Mental health not good for >=14 days among adults aged >=18 Years                                                                                                                                Health Outcomes      
    ## OBESITY        Obesity among adults aged >=18 Years                                                                                                                                                             Unhealthy Behaviors  
    ## PAPTEST        Papanicolaou smear use among adult women aged 21-65 Years                                                                                                                                        Prevention           
    ## PHLTH          Physical health not good for >=14 days among adults aged >=18 Years                                                                                                                              Health Outcomes      
    ## SLEEP          Sleeping less than 7 hours among adults aged >=18 Years                                                                                                                                          Unhealthy Behaviors  
    ## STROKE         Stroke among adults aged >=18 Years                                                                                                                                                              Health Outcomes      
    ## TEETHLOST      All teeth lost among adults aged >=65 Years                                                                                                                                                      Health Outcomes

Box plots can be good visuals to look at the distributions of health outcome for each category.

Figure 2: Box plots of prevelance of health measure categories <img src="https://PhucHNg.github.io/images/health_data/box-category.png" width="600">

Similar boxplots can be used to see the prevelance of specific health measures. This will provide detailed information for better understanding of health problems and which deserve higher priority. For easier comparison, I create boxplots sorted by measure's median prevelance from lowest to highest.

Figure 3: Box plots of prevelance of health measures

<img src="https://PhucHNg.github.io/images/health_data/box-measure.png" width="750" height="500">

Looking at Figure 2, in general, unhealthy behaviors seem more prevelant than chronic diseases. Figure 3 shows in more details that prevelance also differ a lot by the specific health measure. In the health outcome category, prevelance is highest for high cholesterol and high blood pressure among adults, while lack of sleep has highest prevelance in unhealthy behaviors. In preventation, prevelance is lowest for older adult men and women being up to date on core clinical preventative services. (ACCESS2 is lack of access to health insurance.)

### Noticing relationships

In addition to ranking measures of chronical diseases, I want to identify variables that are associated with better health outcome. Since there are many variables in the data, I create a Shiny app to interactively study the relationships between prevelance for measures of chronic diseases and different predictors, particularly hospital performance scores, using scatter plots with a fitted linear regression. I also include an option to control for a second predictor (i.e. demographic variables) which is shown by the points' color. To check the statistical significance of these relationships, I fit a linear regression for measure *i**t**h*'s prevelance according to following formula passed into `lm()`:

    # With one predictor
    prevelance_i ~ predictor_1

    # With two predictor
    prevelance_i ~ predictor_1*predictor_2

Statistically significant coefficients at the 5% level are highlighted in light yellow.

<iframe src="https://phucnguyen.shinyapps.io/healthviz/" style="border: none; width: 800px; height:1300px;">
</iframe>
Higher hospital total performance score is generally significantly associated with higher prevelance in participation in preventative services, lower prevelance in chronic disease outcomes and unhealthy behavior, though the effects are small. When controlling for a demographic variable strongly associated with better health outcomes such as median household income, some associations between better hospital performance score and better health outcome, especially in prevention, become insignificant. Cost reduction, care experience and clinical care score seem to behave similarly. Safety score generally isn't associated with better health outcomes.

### Conclusion

Data exploration provides a better understanding of the data and suggestions for further analysis:

-   There are invalid data values that need to be removed.
-   Data tables have extra observations that need to be filtered and condensed.
-   Many variables are not normally distributed, which is an assumption of many hypothesis tests and models.
-   Outliers exist and should be taken in account in future analysis.
-   Heath measures with highest prevelances are high cholesterol, lack of core clinical preventative services for older adults, and sleep derivation.
-   Better hospital performance scores, except for the safety score, are associated with better health outcome.
-   Demographic variables are also associated with better health outcome.

### Side notes

-   Thera are many outliers in prevelance for measures in chronic diseases as seen in Figure 3, it would be interesting to look at these states in particular and compare their health care policy.
-   Range of different hospital performance measures are different, thus, making it hard to compare the effects of these on prevelance of measures.

### References

1.  <https://catalog.data.gov/dataset/500-cities-local-data-for-better-health-b32fd>
2.  <https://catalog.data.gov/dataset/hospital-compare-data-17295>
3.  <https://www.computerworld.com/article/3120415/data-analytics/how-to-download-new-census-data-with-r.html>

### Appendix

Below is the code to generate the tables and figures

Figure 1: Histograms showing distribution of each variable

``` r
plots <- list()
for (i in 1:length(col_idx)) {
  plots[[i]]<-ggplot(Joined_Data, aes_string(x=col_names[i])) + geom_histogram(aes(y=..density..), binwidth = sd(Joined_Data[,col_idx[i]])/5) + 
    labs(x=row_names[i])+
    theme(axis.text = element_text(size=7), axis.title=element_text(size=7))
}
plot_grid(plotlist=plots, ncol=4)
```

Figure 2: Box plots of prevelance of health measure categories

``` r
ggplot(Joined_Data, aes(y=Data_Value, x=Category)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(y="Prevelance %")
```

Figure 3: Box plots of prevelance of health measures

``` r
# Sort levels of measure by median prevelance
measure_median <- Joined_Data %>%
  group_by(MeasureId) %>%
  summarise(med = median(Data_Value))
order_idx <- sort(measure_median$med, index.return=TRUE)$ix
sorted_measures <- measure_median$MeasureId[order_idx]
Joined_Data$MeasureId <- factor(Joined_Data$MeasureId, 
                                levels = sorted_measures, ordered = TRUE)
ggplot(Joined_Data, aes(x=MeasureId, y=Data_Value)) + geom_boxplot(aes(colour = Category), outlier.colour = "purple", outlier.shape = 1) +
  labs(y="Prevelance %", x="Measure ID") +
  facet_grid(.~Category, scales='free_x') +
  scale_x_discrete(drop=TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

Shiny App UI

``` r
library(shiny)

labels = c("Hospital total performance score" = "Total.Performance.Score",
           "Hospital cost reduction score" = "Unweighted.Normalized.Efficiency.and.Cost.Reduction.Domain.Score",
           "Hospital safety score" = "Unweighted.Normalized.Safety.Domain.Score",
           "Hospital care experience score" = "Unweighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score",
           "Hospital clinical care score" = "Unweighted.Normalized.Clinical.Care.Domain.Score",
           "Median income" = "median_income",
           "Median age" = "median_age",
           "Educational attainment" = "percent_GED",
           "Percent white pop." = "percent_white",
           "Percent black pop." = "percent_black",
           "Percent asian pop." = "percent_asian",
           "Percent native pop." = "percent_native",
           "Percent hispanic pop." = "percent_hispanic")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  fluidRow(column(10, offset = 1,titlePanel("Predictor of Health Outcomes"))),
  
  fluidRow(
    column(4, offset = 1,
           # Drop down menus
           selectInput("pred1", "Select predictor:",
                       labels, selected = "Hospital total performance score")),
    column(4,
           selectInput("pred2", "Select second predictor (optional):",
                       c("None" = "none", labels), selected = "None"))
  ),
  
  hr(),
    
  # Show plots
  fluidRow(
    column(10, offset = 1,
     h4("Plot of relationship between health indicator prevelance and selected predictor(s)"),
       tabsetPanel(
         tabPanel("Health Outcomes", plotOutput("plot1")),
         tabPanel("Preventions", plotOutput("plot2")),
         tabPanel("Unhealthy Behaviors", plotOutput("plot3"))
       ) 
    )
  ),
  
  # Show table
  fluidRow(
    column(10, offset = 1,
       h4("Table of linear regression coefficients"),
       DT::dataTableOutput("table") 
    )
  )
  
))
```

Shiny App Server

``` r
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

# Load in our data
HealthData <- read.csv("./data/HealthData.csv")
col_idx <- c(7, 13, 14, 15, 18, 21, 28:46)
HealthData <- HealthData[,col_idx]
HealthData["none"] <- rep(0, nrow(HealthData)) 

names_map = list(
  "Total.Performance.Score"="Total performance score",
  "Unweighted.Normalized.Efficiency.and.Cost.Reduction.Domain.Score"="Cost reduction score",
  "Unweighted.Normalized.Safety.Domain.Score"="Safety score",
  "Unweighted.Patient.and.Caregiver.Centered.Experience.of.Care.Care.Coordination.Domain.Score"="Care experience score",
  "Unweighted.Normalized.Clinical.Care.Domain.Score"="Clinical care score",
  "median_income"="Median income",
  "median_age"="Median age",
  "percent_GED"="Educational attainment",
  "percent_white"="Percent white",
  "percent_black"="Percent black",
  "percent_asian"="Percent asian",
  "percent_native"="Percent native",
  "percent_hispanic"="Percent hispanic"
)

# Define server logic
shinyServer(function(input, output) {
   
  # Draw scatter plots of relationships between Health indicators and selected predictors
  output$plot1 <- renderPlot({
    ggplot(HealthData%>% filter(Category=="Health Outcomes"), aes_string(x=input$pred1, y="Data_Value")) + geom_point(aes_string(colour=input$pred2)) +
      geom_smooth(se=FALSE, method = "lm", colour="red") +
      labs(y="Prevelance %", x=names_map[[input$pred1]], colour=names_map[[input$pred2]]) +
      facet_grid(. ~ MeasureId)+
      theme(axis.text.x = element_text(size=7))
  })
  
  output$plot2 <- renderPlot({
    ggplot(HealthData%>% filter(Category=="Prevention"), aes_string(x=input$pred1, y="Data_Value")) + geom_point(aes_string(colour=input$pred2)) +
      geom_smooth(se=FALSE, method = "lm", colour="red") +
      labs(y="Prevelance %", x=names_map[[input$pred1]], colour=names_map[[input$pred2]]) +
      facet_grid(. ~ MeasureId)+
      theme(axis.text.x = element_text(size=7))
  })
  
  output$plot3 <- renderPlot({
    ggplot(HealthData%>% filter(Category=="Unhealthy Behaviors"), aes_string(x=input$pred1, y="Data_Value")) + geom_point(aes_string(colour=input$pred2)) +
      geom_smooth(se=FALSE, method = "lm", colour="red") +
      labs(y="Prevelance %", x=names_map[[input$pred1]], colour=names_map[[input$pred2]]) +
      facet_grid(. ~ MeasureId)+
      theme(axis.text.x = element_text(size=7))
  })
  
  # Model the relationships with linear regression for each health measure
  myModelSummary <- reactive({
    
    result_table <- data.frame()
    
    # If a second predictor is selected, add an interaction term between the predictors
    if(input$pred2 != "none"){
      formula <- as.formula(paste("Data_Value~", input$pred1, "*", input$pred2))
    }else{
      formula <- as.formula(paste("Data_Value~", input$pred1))
    }
    
    measures <- levels(HealthData$MeasureId)
    for(measure in measures){
      dat <- HealthData %>% filter(MeasureId == measure)
      
      mod <- lm(formula, data=dat)
      n <- dim(coef(summary(mod)))[1]
      
      # Create a new row containing coef, std. error, p-value for each predictor
      new_row <- round(as.vector(t(coef(summary(mod))[2:n, c("Estimate", "Pr(>|t|)")])),8)
      result_table <- rbind(result_table, new_row)
    }
    
    # Assign row and column names
    result_table <- cbind(measures, result_table)
    if(input$pred2 != "none"){
      colnames(result_table) <- c("Measure",
                                  paste(names_map[[input$pred1]], " est."), 
                                  paste(names_map[[input$pred1]], " p-val"),
                                  paste(names_map[[input$pred2]], " est."), 
                                  paste(names_map[[input$pred2]], " p-val"), 
                        "Interaction est.", "Interaction p-val")
    }else{
      colnames(result_table) <- c("Measure",paste(names_map[[input$pred1]], " est."), 
                                  paste(names_map[[input$pred1]], " p-val"))
    }
    return(result_table)
  })
  
  # Output a table displaying results of regressions
  output$table <- DT::renderDataTable({
    if(input$pred2 == "none"){
      datatable(myModelSummary(), options = list(pageLength=10))  %>% formatStyle(
        paste(names_map[[input$pred1]], " p-val"),
        backgroundColor = styleInterval(0.05, c('lightyellow', 'white'))
      )
    }else{
      datatable(myModelSummary(), options = list(pageLength=10))  %>% formatStyle(
        paste(names_map[[input$pred1]], " p-val"),
        backgroundColor = styleInterval(0.05, c('lightyellow', 'white'))
      ) %>% formatStyle(
        paste(names_map[[input$pred2]], " p-val"),
        backgroundColor = styleInterval(0.05, c('lightyellow', 'white'))
      ) %>% formatStyle(
        "Interaction p-val",
        backgroundColor = styleInterval(0.05, c('lightyellow', 'white'))
      )
    }
  })
  
})
```
