LAB 5
================
Caroline He
9/25/2021

## Lab description

For this lab we will be, again, dealing with the meteorological dataset
downloaded from the NOAA, the met. In this case, we will use data.table
to answer some questions regarding the met dataset, while at the same
time practice your Git+GitHub skills for this project.

# Part 1: Setup the Git project and the GitHub repository

now start working with the MET data.

## Setup in R

``` r
# Set up R packages
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Download and read in the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

``` r
if (!file.exists("met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("met_all.gz")
```

``` r
# Merge date
met <- merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 
```

# Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and
atmospheric pressure? Look for the three weather stations that best
represent continental US using the quantile() function. Do these three
coincide?

Generate a representative version of each station. We will use the
averages (median could be a good way to represent it, but it will depend
on the case).

``` r
station_averages <- met[,.(
  temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = .(USAFID)]
```

Identify median per variable

``` r
medians <- station_averages[,.(
  temp_50 = quantile(temp, probs = .5, na.rm = TRUE),
  wind.sp_50 = quantile(wind.sp, probs = .5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs = .5, na.rm = TRUE)
)]
medians
```

    ##     temp_50 wind.sp_50 atm.press_50
    ## 1: 23.68406   2.461838     1014.691

Find stations that were the closest to these (hint: which.min())

``` r
station_averages[, temp_dist := abs(temp - medians$temp_50)]
median_temp_station <- station_averages[order(temp_dist)][1] 

station_averages[, wind.sp_dist := abs(wind.sp - medians$wind.sp_50)]
median_wind.sp_station <- station_averages[order(wind.sp_dist)][1] 

station_averages[, atm.press_dist := abs(atm.press - medians$atm.press_50)]
median_atm.press_station <- station_averages[order(atm.press_dist)][1] 

median_temp_station
```

    ##    USAFID     temp  wind.sp atm.press   temp_dist
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

``` r
median_wind.sp_station
```

    ##    USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist
    ## 1: 720929 17.43278 2.461838       NaN  6.251284            0

``` r
median_atm.press_station
```

    ##    USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ## 1: 722238 26.13978 1.472656  1014.691  2.455719    0.9891817   0.0005376377

``` r
station_averages
```

    ##       USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ##    1: 690150 33.18763 3.483560  1010.379 9.5035752  1.021721847      4.3124708
    ##    2: 720110 31.22003 2.138348       NaN 7.5359677  0.323490383            NaN
    ##    3: 720113 23.29317 2.470298       NaN 0.3908894  0.008459788            NaN
    ##    4: 720120 27.01922 2.504692       NaN 3.3351568  0.042854372            NaN
    ##    5: 720137 21.88823 1.979335       NaN 1.7958292  0.482502805            NaN
    ##   ---                                                                         
    ## 1591: 726777 19.15492 4.673878  1014.299 4.5291393  2.212040085      0.3920955
    ## 1592: 726797 18.78980 2.858586  1014.902 4.8942607  0.396747923      0.2106085
    ## 1593: 726798 19.47014 4.445783  1014.072 4.2139153  1.983945197      0.6195467
    ## 1594: 726810 25.03549 3.039794  1011.730 1.3514356  0.577955642      2.9607085
    ## 1595: 726813 23.47809 2.435372  1012.315 0.2059716  0.026465595      2.3759601

The median temperature station was 720458

The median wind speed station was 720929

The median atm.press station was 722238

# Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

First recover states variables by merge

``` r
station_averages <- merge(
  x = station_averages, y = stations, 
  by.x = "USAFID", by.y = "USAF",
  all.x = TRUE, all.y = FALSE)
station_averages
```

    ##       USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ##    1: 690150 33.18763 3.483560  1010.379 9.5035752  1.021721847      4.3124708
    ##    2: 720110 31.22003 2.138348       NaN 7.5359677  0.323490383            NaN
    ##    3: 720113 23.29317 2.470298       NaN 0.3908894  0.008459788            NaN
    ##    4: 720120 27.01922 2.504692       NaN 3.3351568  0.042854372            NaN
    ##    5: 720137 21.88823 1.979335       NaN 1.7958292  0.482502805            NaN
    ##   ---                                                                         
    ## 1591: 726777 19.15492 4.673878  1014.299 4.5291393  2.212040085      0.3920955
    ## 1592: 726797 18.78980 2.858586  1014.902 4.8942607  0.396747923      0.2106085
    ## 1593: 726798 19.47014 4.445783  1014.072 4.2139153  1.983945197      0.6195467
    ## 1594: 726810 25.03549 3.039794  1011.730 1.3514356  0.577955642      2.9607085
    ## 1595: 726813 23.47809 2.435372  1012.315 0.2059716  0.026465595      2.3759601
    ##       CTRY STATE
    ##    1:   US    CA
    ##    2:   US    TX
    ##    3:   US    MI
    ##    4:   US    SC
    ##    5:   US    IL
    ##   ---           
    ## 1591:   US    MT
    ## 1592:   US    MT
    ## 1593:   US    MT
    ## 1594:   US    ID
    ## 1595:   US    ID

Now we can compute the median per state

``` r
station_averages[, temp_50 := quantile(temp, probs = .5, na.rm = TRUE), by = STATE]
station_averages[, wind.sp_50 := quantile(wind.sp, probs = .5, na.rm = TRUE), by = STATE]
station_averages[, atm.press_50 := quantile(atm.press, probs = .5, na.rm = TRUE), by = STATE]
head(station_averages)
```

    ##    USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ## 1: 690150 33.18763 3.483560  1010.379 9.5035752  1.021721847       4.312471
    ## 2: 720110 31.22003 2.138348       NaN 7.5359677  0.323490383            NaN
    ## 3: 720113 23.29317 2.470298       NaN 0.3908894  0.008459788            NaN
    ## 4: 720120 27.01922 2.504692       NaN 3.3351568  0.042854372            NaN
    ## 5: 720137 21.88823 1.979335       NaN 1.7958292  0.482502805            NaN
    ## 6: 720151 27.57686 2.998428       NaN 3.8928051  0.536589737            NaN
    ##    CTRY STATE  temp_50 wind.sp_50 atm.press_50
    ## 1:   US    CA 22.66268   2.565445     1012.557
    ## 2:   US    TX 29.75188   3.413737     1012.460
    ## 3:   US    MI 20.51970   2.273423     1014.927
    ## 4:   US    SC 25.80545   1.696119     1015.281
    ## 5:   US    IL 22.43194   2.237622     1014.760
    ## 6:   US    TX 29.75188   3.413737     1012.460

the euclidean distance is calculated by
$\\sqrt{\\sum\_i(x\_i - y\_i)^2}$.

``` r
#temperature & wind speed
station_averages[,eudist_temp_wind.sp := sqrt(
  (temp - temp_50)^2 + (wind.sp - wind.sp_50)^2
  )]
#temperature & atm.press
station_averages[,eudist_temp_atm.press := sqrt(
  (temp - temp_50)^2 + (atm.press - atm.press_50)^2
  )]
#wind speed & atm.press
station_averages[,eudist_wind.sp_atm.press := sqrt(
  (wind.sp - wind.sp_50)^2 + (atm.press - atm.press_50)^2
  )]
station_averages
```

    ##       USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ##    1: 690150 33.18763 3.483560  1010.379 9.5035752  1.021721847      4.3124708
    ##    2: 720110 31.22003 2.138348       NaN 7.5359677  0.323490383            NaN
    ##    3: 720113 23.29317 2.470298       NaN 0.3908894  0.008459788            NaN
    ##    4: 720120 27.01922 2.504692       NaN 3.3351568  0.042854372            NaN
    ##    5: 720137 21.88823 1.979335       NaN 1.7958292  0.482502805            NaN
    ##   ---                                                                         
    ## 1591: 726777 19.15492 4.673878  1014.299 4.5291393  2.212040085      0.3920955
    ## 1592: 726797 18.78980 2.858586  1014.902 4.8942607  0.396747923      0.2106085
    ## 1593: 726798 19.47014 4.445783  1014.072 4.2139153  1.983945197      0.6195467
    ## 1594: 726810 25.03549 3.039794  1011.730 1.3514356  0.577955642      2.9607085
    ## 1595: 726813 23.47809 2.435372  1012.315 0.2059716  0.026465595      2.3759601
    ##       CTRY STATE  temp_50 wind.sp_50 atm.press_50 eudist_temp_wind.sp
    ##    1:   US    CA 22.66268   2.565445     1012.557          10.5649277
    ##    2:   US    TX 29.75188   3.413737     1012.460           1.9447578
    ##    3:   US    MI 20.51970   2.273423     1014.927           2.7804480
    ##    4:   US    SC 25.80545   1.696119     1015.281           1.4584280
    ##    5:   US    IL 22.43194   2.237622     1014.760           0.6019431
    ##   ---                                                                
    ## 1591:   US    MT 19.15492   4.151737     1014.185           0.5221409
    ## 1592:   US    MT 19.15492   4.151737     1014.185           1.3437090
    ## 1593:   US    MT 19.15492   4.151737     1014.185           0.4310791
    ## 1594:   US    ID 20.56798   2.568944     1012.855           4.4922623
    ## 1595:   US    ID 20.56798   2.568944     1012.855           2.9131751
    ##       eudist_temp_atm.press eudist_wind.sp_atm.press
    ##    1:            10.7480686                2.3641377
    ##    2:                   NaN                      NaN
    ##    3:                   NaN                      NaN
    ##    4:                   NaN                      NaN
    ##    5:                   NaN                      NaN
    ##   ---                                               
    ## 1591:             0.1137256                0.5343825
    ## 1592:             0.8041051                1.4783475
    ## 1593:             0.3351114                0.3152722
    ## 1594:             4.6068486                1.2190288
    ## 1595:             2.9597295                0.5559611

Recover data frame

``` r
#select lat & USAFID variables from data set
met_lat <- met[, list(USAFID, lat)]
met_lat[, n := 1:.N, by = .(USAFID)]
met_lat <- met_lat[n == 1,][, n := NULL]
#merge data frames
station_averages_merge <- merge(
  x = station_averages, y = met_lat,
  by.x = "USAFID", by.y = "USAFID",
  all.x = TRUE,      
  all.y = FALSE
)
station_averages_merge
```

    ##       USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ##    1: 690150 33.18763 3.483560  1010.379 9.5035752  1.021721847      4.3124708
    ##    2: 720110 31.22003 2.138348       NaN 7.5359677  0.323490383            NaN
    ##    3: 720113 23.29317 2.470298       NaN 0.3908894  0.008459788            NaN
    ##    4: 720120 27.01922 2.504692       NaN 3.3351568  0.042854372            NaN
    ##    5: 720137 21.88823 1.979335       NaN 1.7958292  0.482502805            NaN
    ##   ---                                                                         
    ## 1591: 726777 19.15492 4.673878  1014.299 4.5291393  2.212040085      0.3920955
    ## 1592: 726797 18.78980 2.858586  1014.902 4.8942607  0.396747923      0.2106085
    ## 1593: 726798 19.47014 4.445783  1014.072 4.2139153  1.983945197      0.6195467
    ## 1594: 726810 25.03549 3.039794  1011.730 1.3514356  0.577955642      2.9607085
    ## 1595: 726813 23.47809 2.435372  1012.315 0.2059716  0.026465595      2.3759601
    ##       CTRY STATE  temp_50 wind.sp_50 atm.press_50 eudist_temp_wind.sp
    ##    1:   US    CA 22.66268   2.565445     1012.557          10.5649277
    ##    2:   US    TX 29.75188   3.413737     1012.460           1.9447578
    ##    3:   US    MI 20.51970   2.273423     1014.927           2.7804480
    ##    4:   US    SC 25.80545   1.696119     1015.281           1.4584280
    ##    5:   US    IL 22.43194   2.237622     1014.760           0.6019431
    ##   ---                                                                
    ## 1591:   US    MT 19.15492   4.151737     1014.185           0.5221409
    ## 1592:   US    MT 19.15492   4.151737     1014.185           1.3437090
    ## 1593:   US    MT 19.15492   4.151737     1014.185           0.4310791
    ## 1594:   US    ID 20.56798   2.568944     1012.855           4.4922623
    ## 1595:   US    ID 20.56798   2.568944     1012.855           2.9131751
    ##       eudist_temp_atm.press eudist_wind.sp_atm.press    lat
    ##    1:            10.7480686                2.3641377 34.300
    ##    2:                   NaN                      NaN 30.784
    ##    3:                   NaN                      NaN 42.543
    ##    4:                   NaN                      NaN 32.224
    ##    5:                   NaN                      NaN 41.425
    ##   ---                                                      
    ## 1591:             0.1137256                0.5343825 46.358
    ## 1592:             0.8041051                1.4783475 45.788
    ## 1593:             0.3351114                0.3152722 45.698
    ## 1594:             4.6068486                1.2190288 43.567
    ## 1595:             2.9597295                0.5559611 43.650

# Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use leaflet() to visualize all \~100 points in
the same figure, applying different colors for those identified in this
question.

# Question 4: Means of means

Using the quantile() function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.
