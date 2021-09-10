LAB3
================
Caroline
9/10/2021

## Library data.table

\#Install if and only if it’s not present

``` r
if (!require(data.table)){
  install.packages("data.table")
}
```

    ## Loading required package: data.table

## Step 1 Download and read the dataset

\#Install if we don’t have it.

``` r
if (!file.exists("met_all.gz")) {
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
              destfile = "met_all.gz", 
              method="libcurl", 
              timeout = 60
              )
}

met <- data.table::fread("met_all.gz")
```

## Step 2 Check the dimensions, headers, footers, rows and columns

Dimensions

``` r
dim(met)
```

    ## [1] 2377343      30

Headers

``` r
head(met)
```

    ##    USAFID  WBAN year month day hour min  lat      lon elev wind.dir wind.dir.qc
    ## 1: 690150 93121 2019     8   1    0  56 34.3 -116.166  696      220           5
    ## 2: 690150 93121 2019     8   1    1  56 34.3 -116.166  696      230           5
    ## 3: 690150 93121 2019     8   1    2  56 34.3 -116.166  696      230           5
    ## 4: 690150 93121 2019     8   1    3  56 34.3 -116.166  696      210           5
    ## 5: 690150 93121 2019     8   1    4  56 34.3 -116.166  696      120           5
    ## 6: 690150 93121 2019     8   1    5  56 34.3 -116.166  696       NA           9
    ##    wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc ceiling.ht.method
    ## 1:              N     5.7          5      22000             5                 9
    ## 2:              N     8.2          5      22000             5                 9
    ## 3:              N     6.7          5      22000             5                 9
    ## 4:              N     5.1          5      22000             5                 9
    ## 5:              N     2.1          5      22000             5                 9
    ## 6:              C     0.0          5      22000             5                 9
    ##    sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp temp.qc dew.point
    ## 1:        N    16093           5       N          5 37.2       5      10.6
    ## 2:        N    16093           5       N          5 35.6       5      10.6
    ## 3:        N    16093           5       N          5 34.4       5       7.2
    ## 4:        N    16093           5       N          5 33.3       5       5.0
    ## 5:        N    16093           5       N          5 32.8       5       5.0
    ## 6:        N    16093           5       N          5 31.1       5       5.6
    ##    dew.point.qc atm.press atm.press.qc       rh
    ## 1:            5    1009.9            5 19.88127
    ## 2:            5    1010.3            5 21.76098
    ## 3:            5    1010.6            5 18.48212
    ## 4:            5    1011.6            5 16.88862
    ## 5:            5    1012.7            5 17.38410
    ## 6:            5    1012.7            5 20.01540

Footers

``` r
tail(met)
```

    ##    USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ## 1: 726813 94195 2019     8  31   18  56 43.650 -116.633  741       NA
    ## 2: 726813 94195 2019     8  31   19  56 43.650 -116.633  741       70
    ## 3: 726813 94195 2019     8  31   20  56 43.650 -116.633  741       NA
    ## 4: 726813 94195 2019     8  31   21  56 43.650 -116.633  741       10
    ## 5: 726813 94195 2019     8  31   22  56 43.642 -116.636  741       10
    ## 6: 726813 94195 2019     8  31   23  56 43.642 -116.636  741       40
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ## 1:           9              C     0.0          5      22000             5
    ## 2:           5              N     2.1          5      22000             5
    ## 3:           9              C     0.0          5      22000             5
    ## 4:           5              N     2.6          5      22000             5
    ## 5:           1              N     2.1          1      22000             1
    ## 6:           1              N     2.1          1      22000             1
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ## 1:                 9        N    16093           5       N          5 30.0
    ## 2:                 9        N    16093           5       N          5 32.2
    ## 3:                 9        N    16093           5       N          5 33.3
    ## 4:                 9        N    14484           5       N          5 35.0
    ## 5:                 9        N    16093           1       9          9 34.4
    ## 6:                 9        N    16093           1       9          9 34.4
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh
    ## 1:       5      11.7            5    1013.6            5 32.32509
    ## 2:       5      12.2            5    1012.8            5 29.40686
    ## 3:       5      12.2            5    1011.6            5 27.60422
    ## 4:       5       9.4            5    1010.8            5 20.76325
    ## 5:       1       9.4            1    1010.1            1 21.48631
    ## 6:       1       9.4            1    1009.6            1 21.48631

Rows

``` r
nrow(met)
```

    ## [1] 2377343

Columns

``` r
ncol(met)
```

    ## [1] 30

## Step 3 Brief look at the variables

``` r
str(met)
```

    ## Classes 'data.table' and 'data.frame':   2377343 obs. of  30 variables:
    ##  $ USAFID           : int  690150 690150 690150 690150 690150 690150 690150 690150 690150 690150 ...
    ##  $ WBAN             : int  93121 93121 93121 93121 93121 93121 93121 93121 93121 93121 ...
    ##  $ year             : int  2019 2019 2019 2019 2019 2019 2019 2019 2019 2019 ...
    ##  $ month            : int  8 8 8 8 8 8 8 8 8 8 ...
    ##  $ day              : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ hour             : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ min              : int  56 56 56 56 56 56 56 56 56 56 ...
    ##  $ lat              : num  34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 34.3 ...
    ##  $ lon              : num  -116 -116 -116 -116 -116 ...
    ##  $ elev             : int  696 696 696 696 696 696 696 696 696 696 ...
    ##  $ wind.dir         : int  220 230 230 210 120 NA 320 10 320 350 ...
    ##  $ wind.dir.qc      : chr  "5" "5" "5" "5" ...
    ##  $ wind.type.code   : chr  "N" "N" "N" "N" ...
    ##  $ wind.sp          : num  5.7 8.2 6.7 5.1 2.1 0 1.5 2.1 2.6 1.5 ...
    ##  $ wind.sp.qc       : chr  "5" "5" "5" "5" ...
    ##  $ ceiling.ht       : int  22000 22000 22000 22000 22000 22000 22000 22000 22000 22000 ...
    ##  $ ceiling.ht.qc    : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ ceiling.ht.method: chr  "9" "9" "9" "9" ...
    ##  $ sky.cond         : chr  "N" "N" "N" "N" ...
    ##  $ vis.dist         : int  16093 16093 16093 16093 16093 16093 16093 16093 16093 16093 ...
    ##  $ vis.dist.qc      : chr  "5" "5" "5" "5" ...
    ##  $ vis.var          : chr  "N" "N" "N" "N" ...
    ##  $ vis.var.qc       : chr  "5" "5" "5" "5" ...
    ##  $ temp             : num  37.2 35.6 34.4 33.3 32.8 31.1 29.4 28.9 27.2 26.7 ...
    ##  $ temp.qc          : chr  "5" "5" "5" "5" ...
    ##  $ dew.point        : num  10.6 10.6 7.2 5 5 5.6 6.1 6.7 7.8 7.8 ...
    ##  $ dew.point.qc     : chr  "5" "5" "5" "5" ...
    ##  $ atm.press        : num  1010 1010 1011 1012 1013 ...
    ##  $ atm.press.qc     : int  5 5 5 5 5 5 5 5 5 5 ...
    ##  $ rh               : num  19.9 21.8 18.5 16.9 17.4 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

## Step 4 Closer look at the key variables

``` r
table(met$year)
```

    ## 
    ##    2019 
    ## 2377343

``` r
table(met$day)
```

    ## 
    ##     1     2     3     4     5     6     7     8     9    10    11    12    13 
    ## 75975 75923 76915 76594 76332 76734 77677 77766 75366 75450 76187 75052 76906 
    ##    14    15    16    17    18    19    20    21    22    23    24    25    26 
    ## 77852 76217 78015 78219 79191 76709 75527 75786 78312 77413 76965 76806 79114 
    ##    27    28    29    30    31 
    ## 79789 77059 71712 74931 74849

``` r
table(met$hour)
```

    ## 
    ##      0      1      2      3      4      5      6      7      8      9     10 
    ##  99434  93482  93770  96703 110504 112128 106235 101985 100310 102915 101880 
    ##     11     12     13     14     15     16     17     18     19     20     21 
    ## 100470 103605  97004  96507  97635  94942  94184 100179  94604  94928  96070 
    ##     22     23 
    ##  94046  93823

``` r
summary(met$temp)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  -40.00   19.60   23.50   23.59   27.80   56.00   60089

``` r
summary(met$elev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -13.0   101.0   252.0   415.8   400.0  9999.0

update NAs in elevation

``` r
met[met$elev==9999.0] <- NA
summary(met$elev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     -13     101     252     413     400    4113     710

remove temperature of -40C

``` r
met <- met[temp>-40]
met2 <- met[order(temp)]
head(met2)
```

    ##    USAFID WBAN year month day hour min    lat    lon elev wind.dir wind.dir.qc
    ## 1: 722817 3068 2019     8   1    0  56 38.767 -104.3 1838      190           5
    ## 2: 722817 3068 2019     8   1    1  56 38.767 -104.3 1838      180           5
    ## 3: 722817 3068 2019     8   3   11  56 38.767 -104.3 1838       NA           9
    ## 4: 722817 3068 2019     8   3   12  56 38.767 -104.3 1838       NA           9
    ## 5: 722817 3068 2019     8   6   21  56 38.767 -104.3 1838      280           5
    ## 6: 722817 3068 2019     8   6   22  56 38.767 -104.3 1838      240           5
    ##    wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc ceiling.ht.method
    ## 1:              N     7.2          5         NA             9                 9
    ## 2:              N     7.7          5         NA             9                 9
    ## 3:              C     0.0          5         NA             9                 9
    ## 4:              C     0.0          5         NA             9                 9
    ## 5:              N     2.6          5         NA             9                 9
    ## 6:              N     7.7          5         NA             9                 9
    ##    sky.cond vis.dist vis.dist.qc vis.var vis.var.qc  temp temp.qc dew.point
    ## 1:        N       NA           9       N          5 -17.2       5        NA
    ## 2:        N       NA           9       N          5 -17.2       5        NA
    ## 3:        N       NA           9       N          5 -17.2       5        NA
    ## 4:        N       NA           9       N          5 -17.2       5        NA
    ## 5:        N       NA           9       N          5 -17.2       5        NA
    ## 6:        N       NA           9       N          5 -17.2       5        NA
    ##    dew.point.qc atm.press atm.press.qc rh
    ## 1:            9        NA            9 NA
    ## 2:            9        NA            9 NA
    ## 3:            9        NA            9 NA
    ## 4:            9        NA            9 NA
    ## 5:            9        NA            9 NA
    ## 6:            9        NA            9 NA

## Step 5 Check the data against an external data source

``` r
#make the temperature reasonable (temp > -15C)
met <- met[temp>-15][order(temp)]
summary(met[, .(lat, lon, wind.sp, temp, elev)])
```

    ##       lat             lon             wind.sp            temp      
    ##  Min.   :24.55   Min.   :-124.29   Min.   : 0.000   Min.   :-3.00  
    ##  1st Qu.:33.98   1st Qu.: -98.02   1st Qu.: 0.000   1st Qu.:19.60  
    ##  Median :38.37   Median : -91.74   Median : 2.100   Median :23.50  
    ##  Mean   :37.97   Mean   : -92.14   Mean   : 2.462   Mean   :23.59  
    ##  3rd Qu.:41.96   3rd Qu.: -82.99   3rd Qu.: 3.600   3rd Qu.:27.80  
    ##  Max.   :48.94   Max.   : -68.31   Max.   :36.000   Max.   :56.00  
    ##                                    NA's   :31582                   
    ##       elev       
    ##  Min.   : -13.0  
    ##  1st Qu.: 101.0  
    ##  Median : 252.0  
    ##  Mean   : 414.3  
    ##  3rd Qu.: 400.0  
    ##  Max.   :4113.0  
    ## 

``` r
head(met)
```

    ##    USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ## 1: 726764 94163 2019     8  27   11  50 44.683 -111.116 2025       NA
    ## 2: 726764 94163 2019     8  27   12  10 44.683 -111.116 2025       NA
    ## 3: 726764 94163 2019     8  27   12  30 44.683 -111.116 2025       NA
    ## 4: 726764 94163 2019     8  27   12  50 44.683 -111.116 2025       NA
    ## 5: 720411   137 2019     8  18   12  35 36.422 -105.290 2554       NA
    ## 6: 726764 94163 2019     8  26   12  30 44.683 -111.116 2025       NA
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ## 1:           9              C       0          5      22000             5
    ## 2:           9              C       0          5      22000             5
    ## 3:           9              C       0          5      22000             5
    ## 4:           9              C       0          5      22000             5
    ## 5:           9              C       0          5      22000             5
    ## 6:           9              C       0          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ## 1:                 9        N    16093           5       N          5 -3.0
    ## 2:                 9        N    16093           5       N          5 -3.0
    ## 3:                 9        N    16093           5       N          5 -3.0
    ## 4:                 9        N    16093           5       N          5 -3.0
    ## 5:                 9        N    16093           5       N          5 -2.4
    ## 6:                 9        N    16093           5       N          5 -2.0
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh
    ## 1:       C      -5.0            C        NA            9 86.26537
    ## 2:       5      -4.0            5        NA            9 92.91083
    ## 3:       5      -4.0            5        NA            9 92.91083
    ## 4:       C      -4.0            C        NA            9 92.91083
    ## 5:       5      -3.7            5        NA            9 90.91475
    ## 6:       5      -3.0            5        NA            9 92.96690

## Step 6 Calculate summary statistics

``` r
#select station with minimum elevation
elev <- met[elev==min(elev)]
summary(elev)
```

    ##      USAFID            WBAN            year          month        day       
    ##  Min.   :722810   Min.   :23199   Min.   :2019   Min.   :8   Min.   :13.00  
    ##  1st Qu.:722810   1st Qu.:23199   1st Qu.:2019   1st Qu.:8   1st Qu.:13.00  
    ##  Median :722810   Median :23199   Median :2019   Median :8   Median :13.00  
    ##  Mean   :722810   Mean   :23199   Mean   :2019   Mean   :8   Mean   :19.62  
    ##  3rd Qu.:722810   3rd Qu.:23199   3rd Qu.:2019   3rd Qu.:8   3rd Qu.:30.25  
    ##  Max.   :722810   Max.   :23199   Max.   :2019   Max.   :8   Max.   :31.00  
    ##                                                                             
    ##       hour            min          lat             lon              elev    
    ##  Min.   : 0.00   Min.   :56   Min.   :32.83   Min.   :-115.7   Min.   :-13  
    ##  1st Qu.: 1.75   1st Qu.:56   1st Qu.:32.83   1st Qu.:-115.7   1st Qu.:-13  
    ##  Median : 4.50   Median :56   Median :32.83   Median :-115.7   Median :-13  
    ##  Mean   :10.00   Mean   :56   Mean   :32.83   Mean   :-115.7   Mean   :-13  
    ##  3rd Qu.:22.25   3rd Qu.:56   3rd Qu.:32.83   3rd Qu.:-115.7   3rd Qu.:-13  
    ##  Max.   :23.00   Max.   :56   Max.   :32.83   Max.   :-115.7   Max.   :-13  
    ##                                                                             
    ##     wind.dir     wind.dir.qc        wind.type.code        wind.sp     
    ##  Min.   : 50.0   Length:8           Length:8           Min.   :2.100  
    ##  1st Qu.:110.0   Class :character   Class :character   1st Qu.:2.600  
    ##  Median :120.0   Mode  :character   Mode  :character   Median :2.850  
    ##  Mean   :125.7                                         Mean   :3.038  
    ##  3rd Qu.:140.0                                         3rd Qu.:3.600  
    ##  Max.   :210.0                                         Max.   :4.100  
    ##  NA's   :1                                                            
    ##   wind.sp.qc          ceiling.ht    ceiling.ht.qc ceiling.ht.method 
    ##  Length:8           Min.   :22000   Min.   :1     Length:8          
    ##  Class :character   1st Qu.:22000   1st Qu.:1     Class :character  
    ##  Mode  :character   Median :22000   Median :1     Mode  :character  
    ##                     Mean   :22000   Mean   :1                       
    ##                     3rd Qu.:22000   3rd Qu.:1                       
    ##                     Max.   :22000   Max.   :1                       
    ##                                                                     
    ##    sky.cond            vis.dist     vis.dist.qc          vis.var         
    ##  Length:8           Min.   :16093   Length:8           Length:8          
    ##  Class :character   1st Qu.:16093   Class :character   Class :character  
    ##  Mode  :character   Median :16093   Mode  :character   Mode  :character  
    ##                     Mean   :16093                                        
    ##                     3rd Qu.:16093                                        
    ##                     Max.   :16093                                        
    ##                                                                          
    ##   vis.var.qc             temp         temp.qc            dew.point    
    ##  Length:8           Min.   :30.60   Length:8           Min.   :5.600  
    ##  Class :character   1st Qu.:34.73   Class :character   1st Qu.:6.700  
    ##  Mode  :character   Median :38.90   Mode  :character   Median :6.950  
    ##                     Mean   :38.41                      Mean   :7.375  
    ##                     3rd Qu.:42.92                      3rd Qu.:8.075  
    ##                     Max.   :43.90                      Max.   :9.400  
    ##                                                                       
    ##  dew.point.qc         atm.press     atm.press.qc       rh       
    ##  Length:8           Min.   :1007   Min.   :1     Min.   :11.23  
    ##  Class :character   1st Qu.:1007   1st Qu.:1     1st Qu.:12.57  
    ##  Mode  :character   Median :1009   Median :1     Median :15.20  
    ##                     Mean   :1009   Mean   :1     Mean   :15.32  
    ##                     3rd Qu.:1010   3rd Qu.:1     3rd Qu.:16.58  
    ##                     Max.   :1011   Max.   :1     Max.   :22.25  
    ## 

correlation between temperature and wind speed

``` r
cor(elev$temp, elev$wind.sp, use="complete")
```

    ## [1] -0.3318348

correlation between temperature and hours

``` r
cor(elev$temp, elev$hour, use="complete")
```

    ## [1] 0.7302148

correlation between wind speed and day

``` r
cor(elev$wind.sp, elev$day, use="complete")
```

    ## [1] -0.7412098

correlation between wind speed and hour

``` r
cor(elev$wind.sp, elev$hour, use="complete")
```

    ## [1] -0.8021375

correlation between temperature and day

``` r
cor(elev$temp, elev$day, use="complete")
```

    ## [1] 0.8284796

## Step 7 Exploratory Graphs