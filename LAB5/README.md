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
met <- merge(
  x     = met,
  y     = stations,
  by.x  = "USAFID",
  by.y  = "USAF",
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
```

the euclidean distance is calculated by
$\\sqrt{\\sum\_i(x\_i - y\_i)^2}$.

``` r
#calculation of euclidean distance
station_averages[,eudist := 
  sqrt((temp - temp_50)^2 + (wind.sp - wind.sp_50)^2) + 
  sqrt((temp - temp_50)^2 + (atm.press - atm.press_50)^2) +
  sqrt((wind.sp - wind.sp_50)^2 + (atm.press - atm.press_50)^2)]
#group by state
station_averages <- station_averages[ , .SD[which.min(eudist)], by = STATE]
station_averages
```

    ##     STATE USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist
    ##  1:    CA 722970 22.76040 2.325982  1012.710 0.9236572   0.13585606
    ##  2:    TX 722416 29.75394 3.539980  1012.331 6.0698790   1.07814202
    ##  3:    MI 725395 20.44096 2.357275  1015.245 3.2431039   0.10456335
    ##  4:    SC 723190 25.73726 2.253408  1015.116 2.0532044   0.20843011
    ##  5:    IL 725305 22.36831 2.796224  1014.734 1.3157456   0.33438644
    ##  6:    MO 723495 24.31621 2.550940  1014.296 0.6321463   0.08910173
    ##  7:    AR 723407 25.86949 2.208652  1014.575 2.1854323   0.25318602
    ##  8:    OR 725895 18.79793 2.307326  1014.726 4.8861299   0.15451193
    ##  9:    GA 723160 26.59746 1.684538  1014.985 2.9134022   0.77729997
    ## 10:    MN 726550 19.11831 2.832794  1015.319 4.5657541   0.37095652
    ## 11:    AL 722286 26.35793 1.675828  1014.909 2.6738730   0.78600982
    ## 12:    IN 725327 22.40044 2.547951  1015.145 1.2836192   0.08611334
    ## 13:    NC 723174 24.95288 1.744838  1015.350 1.2688166   0.71699988
    ## 14:    VA 724016 24.29327 1.588105  1014.946 0.6092071   0.87373310
    ## 15:    IA 725480 21.43686 2.764312  1014.814 2.2472026   0.30247433
    ## 16:    PA 725130 21.69177 1.970192  1015.125 1.9922887   0.49164643
    ## 17:    NE 725560 21.80411 3.428358  1014.386 1.8799508   0.96652027
    ## 18:    ID 725867 20.81272 2.702517  1012.802 2.8713440   0.24067862
    ## 19:    WI 726452 19.21728 2.411747  1015.180 4.4667828   0.05009126
    ## 20:    WV 724176 21.94072 1.649151  1015.982 1.7433407   0.81268709
    ## 21:    MD 724057 25.00877 2.033233  1014.497 1.3247127   0.42860531
    ## 22:    AZ 722745 30.31538 3.307632  1010.144 6.6313167   0.84579394
    ## 23:    OK 723545 27.03555 3.852697  1012.711 3.3514940   1.39085916
    ## 24:    WY 726650 19.75554 4.243727  1013.527 3.9285171   1.78188923
    ## 25:    LA 722486 28.16413 1.592840  1014.544 4.4800738   0.86899784
    ## 26:    KY 724240 23.79463 2.450704  1015.375 0.1105710   0.01113371
    ## 27:    FL 722106 27.52774 2.711121  1015.322 3.8436793   0.24928306
    ## 28:    CO 724767 21.97732 2.780364  1014.082 1.7067344   0.31852617
    ## 29:    OH 724298 21.79537 2.771958  1015.248 1.8886908   0.31011974
    ## 30:    NJ 724090 23.47238 2.148606  1015.095 0.2116829   0.31323188
    ## 31:    NM 723658 24.94447 3.569281  1013.917 1.2604141   1.10744311
    ## 32:    KS 724550 24.14958 3.449278  1013.315 0.4655163   0.98743989
    ## 33:    VT 726115 18.60548 1.101301  1014.985 5.0785811   1.36053682
    ## 34:    MS 722340 26.91166 1.689861  1014.842 3.2275988   0.77197658
    ## 35:    CT 725087 22.57539 2.126514  1014.534 1.1086682   0.33532437
    ## 36:    NV 725805 25.21743 3.101560  1012.461 1.5333745   0.63972235
    ## 37:    UT 725755 24.31031 3.361211  1012.243 0.6262500   0.89937340
    ## 38:    SD 726590 19.95928 3.550722  1014.284 3.7247800   1.08888409
    ## 39:    TN 723346 24.59407 1.493531  1015.144 0.9100071   0.96830647
    ## 40:    NY 725194 20.37207 2.444051  1015.327 3.3119910   0.01778649
    ## 41:    RI 725079 22.27697 2.583469  1014.620 1.4070879   0.12163090
    ## 42:    MA 725064 21.40933 2.786213  1014.721 2.2747284   0.32437547
    ## 43:    DE 724180 24.56026 2.752929  1015.046 0.8761984   0.29109156
    ## 44:    NH 726050 19.86188 1.732752  1014.487 3.8221833   0.72908585
    ## 45:    ME 726077 18.49969 2.337241  1014.475 5.1843701   0.12459727
    ## 46:    MT 726798 19.47014 4.445783  1014.072 4.2139153   1.98394520
    ##     STATE USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist
    ##     atm.press_dist CTRY  temp_50 wind.sp_50 atm.press_50    eudist
    ##  1:     1.98090662   US 22.66268   2.565445     1012.557 0.7243310
    ##  2:     2.36009989   US 29.75188   3.413737     1012.460 0.4352598
    ##  3:     0.55414742   US 20.51970   2.273423     1014.927 0.7727213
    ##  4:     0.42484635   US 25.80545   1.696119     1015.281 1.3216447
    ##  5:     0.04294876   US 22.43194   2.237622     1014.760 1.1899706
    ##  6:     0.39531494   US 23.95109   2.453547     1014.522 1.0538719
    ##  7:     0.11587946   US 26.24296   1.938625     1014.591 1.1051458
    ##  8:     0.03452377   US 17.98061   2.011436     1015.269 2.4698939
    ##  9:     0.29366355   US 26.70404   1.495596     1015.208 0.7574598
    ## 10:     0.62811903   US 19.63017   2.617071     1015.042 1.4893062
    ## 11:     0.21799151   US 26.33664   1.662132     1014.959 0.1315880
    ## 12:     0.45426952   US 22.25059   2.344333     1015.063 0.6431219
    ## 13:     0.65844850   US 24.72953   1.627306     1015.420 0.6239128
    ## 14:     0.25521291   US 24.37799   1.653032     1015.158 0.5552802
    ## 15:     0.12318699   US 21.33461   2.680875     1014.964 0.4839674
    ## 16:     0.43337241   US 21.69177   1.784167     1015.435 0.8593197
    ## 17:     0.30543399   US 21.87354   3.192539     1014.332 0.5753583
    ## 18:     1.88911023   US 20.56798   2.568944     1012.855 0.6728309
    ## 19:     0.48869044   US 18.85524   2.053283     1014.893 1.4299772
    ## 20:     1.29124420   US 21.94446   1.633487     1015.762 0.4571658
    ## 21:     0.19421341   US 24.89883   1.883499     1014.824 0.8908804
    ## 22:     4.54708887   US 30.32372   3.074359     1010.144 0.4750360
    ## 23:     1.97972354   US 27.14427   3.852697     1012.567 0.4333596
    ## 24:     1.16394936   US 19.80699   3.873392     1013.157 1.2720869
    ## 25:     0.14733107   US 27.87430   1.592840     1014.593 0.6331386
    ## 26:     0.68353867   US 23.88844   1.895486     1015.245 1.2934693
    ## 27:     0.63089474   US 27.57325   2.705069     1015.335 0.1076200
    ## 28:     0.60942784   US 21.49638   3.098777     1013.334 2.2782799
    ## 29:     0.55683560   US 22.02062   2.554397     1015.351 0.8016371
    ## 30:     0.40339033   US 23.47238   2.148606     1014.825 0.5394298
    ## 31:     0.77461602   US 24.94447   3.776083     1012.525 3.0048584
    ## 32:     1.37635327   US 24.21220   3.680613     1013.389 0.5798306
    ## 33:     0.29379796   US 18.61379   1.408247     1014.792 0.8626295
    ## 34:     0.15086561   US 26.69258   1.636392     1014.836 0.4984716
    ## 35:     0.15754612   US 22.36880   2.101801     1014.810 0.8315235
    ## 36:     2.23006956   US 24.56293   3.035050     1012.204 1.6263042
    ## 37:     2.44765177   US 24.35182   3.145427     1011.972 0.8411016
    ## 38:     0.40762293   US 20.35662   3.665638     1014.398 0.9890946
    ## 39:     0.45284633   US 24.88657   1.576035     1015.144 0.6789154
    ## 40:     0.63536586   US 20.40674   2.304075     1014.887 1.0463590
    ## 41:     0.07139021   US 22.53551   2.583469     1014.728 0.6474715
    ## 42:     0.02995388   US 21.30662   2.710944     1014.751 0.3154244
    ## 43:     0.35441624   US 24.56026   2.752929     1015.046 0.0000000
    ## 44:     0.20448161   US 19.55054   1.563826     1014.689 0.9887217
    ## 45:     0.21594166   US 18.79016   2.237210     1014.399 0.7332945
    ## 46:     0.61954666   US 19.15492   4.151737     1014.185 1.0814628
    ##     atm.press_dist CTRY  temp_50 wind.sp_50 atm.press_50    eudist

# Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use leaflet() to visualize all \~100 points in
the same figure, applying different colors for those identified in this
question.

``` r
#create mid lat & mid lon
met[, lat_50 := quantile(lat, probs = .5, na.rm = TRUE), by = STATE]
met[, lon_50 := quantile(lon, probs = .5, na.rm = TRUE), by = STATE]
#select columns
middle <- met[, list(USAFID, STATE, lat, lon, lat_50, lon_50)]
```

combine with previous stations

``` r
station_midpoint <- merge(
  x = station_averages, y = middle, 
  by = "USAFID",
  all.x = TRUE, all.y = FALSE)
```

visualize data via leaflet()

# Question 4: Means of means

Using the quantile() function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.
