LAB 10
================
Caroline He
11/8/2021

## R Packages

``` r
library(RSQLite)
library(DBI)
library(sqldf)
```

    ## Loading required package: gsubfn

    ## Loading required package: proto

    ## Warning in doTryCatch(return(expr), name, parentenv, handler): unable to load shared object '/Library/Frameworks/R.framework/Resources/modules//R_X11.so':
    ##   dlopen(/Library/Frameworks/R.framework/Resources/modules//R_X11.so, 6): Library not loaded: /opt/X11/lib/libSM.6.dylib
    ##   Referenced from: /Library/Frameworks/R.framework/Versions/4.1/Resources/modules/R_X11.so
    ##   Reason: image not found

    ## Could not load tcltk.  Will use slower R code instead.

## Setup

``` r
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
```

``` r
dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

``` sql
PRAGMA table_info(actor)
```

This is equivalent to use `dbGetQuery`

``` r
dbGetQuery(con, "PRAGMA table_info(actor)")
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0    actor_id INTEGER       0         NA  0
    ## 2   1  first_name    TEXT       0         NA  0
    ## 3   2   last_name    TEXT       0         NA  0
    ## 4   3 last_update    TEXT       0         NA  0

# Exercise 1

#### Retrive the actor ID, first name and last name for all actors using the actor table. Sort by last name and then by first name.

``` r
dbGetQuery(con, "SELECT actor_id, first_name, last_name
FROM actor /* you can add commit using
mutiple lines! */
ORDER by last_name, first_name 
LIMIT 5")
```

    ##   actor_id first_name last_name
    ## 1       58  CHRISTIAN    AKROYD
    ## 2      182     DEBBIE    AKROYD
    ## 3       92    KIRSTEN    AKROYD
    ## 4      118       CUBA     ALLEN
    ## 5      145        KIM     ALLEN

# Exercise 2

#### Retrive the actor ID, first name, and last name for actors whose last name equals ‘WILLIAMS’ or ‘DAVIS’.

``` r
dbGetQuery(con, "SELECT actor_id, first_name, last_name
FROM actor 
WHERE last_name IN ('WILLIAMS', 'DAVIS')")
```

    ##   actor_id first_name last_name
    ## 1        4   JENNIFER     DAVIS
    ## 2       72       SEAN  WILLIAMS
    ## 3      101      SUSAN     DAVIS
    ## 4      110      SUSAN     DAVIS
    ## 5      137     MORGAN  WILLIAMS
    ## 6      172    GROUCHO  WILLIAMS

# Exercise 3

#### Write a query against the rental table that returns the IDs of the customers who rented a film on July 5, 2005 (use the rental.rental\_date column, and you can use the date() function to ignore the time component). Include a single row for each distinct customer ID.

``` r
dbGetQuery(con, "PRAGMA table_info(rental)")
```

    ##   cid         name    type notnull dflt_value pk
    ## 1   0    rental_id INTEGER       0         NA  0
    ## 2   1  rental_date    TEXT       0         NA  0
    ## 3   2 inventory_id INTEGER       0         NA  0
    ## 4   3  customer_id INTEGER       0         NA  0
    ## 5   4  return_date    TEXT       0         NA  0
    ## 6   5     staff_id INTEGER       0         NA  0
    ## 7   6  last_update    TEXT       0         NA  0

``` r
dbGetQuery(con," 
SELECT DISTINCT customer_id 
FROM rental
WHERE date(rental_date) = '2005-07-05' LIMIT 5")
```

    ##   customer_id
    ## 1         565
    ## 2         242
    ## 3          37
    ## 4          60
    ## 5         594

# Exercise 4

### Exercise 4.1

#### Construct a query that retrives all rows from the payment table where the amount is either 1.99, 7.99, 9.99.

``` r
q <- dbSendQuery(con, "SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)")
dbFetch(q, n = 10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16050         269        2         7   1.99 2007-01-24 21:40:19.996577
    ## 2       16056         270        1       193   1.99 2007-01-26 05:10:14.996577
    ## 3       16081         282        2        48   1.99 2007-01-25 04:49:12.996577
    ## 4       16103         294        1       595   1.99 2007-01-28 12:28:20.996577
    ## 5       16133         307        1       614   1.99 2007-01-28 14:01:54.996577
    ## 6       16158         316        1      1065   1.99 2007-01-31 07:23:22.996577
    ## 7       16160         318        1       224   9.99 2007-01-26 08:46:53.996577
    ## 8       16161         319        1        15   9.99 2007-01-24 23:07:48.996577
    ## 9       16180         330        2       967   7.99 2007-01-30 17:40:32.996577
    ## 10      16206         351        1      1137   1.99 2007-01-31 17:48:40.996577

``` r
dbFetch(q, n = 10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16210         354        2       158   1.99 2007-01-25 23:55:37.996577
    ## 2       16240         369        2       913   7.99 2007-01-30 09:33:24.996577
    ## 3       16275         386        1       583   7.99 2007-01-28 10:17:21.996577
    ## 4       16277         387        1       697   7.99 2007-01-29 00:32:30.996577
    ## 5       16289         391        1       891   7.99 2007-01-30 06:11:38.996577
    ## 6       16302         400        2       516   1.99 2007-01-28 01:40:13.996577
    ## 7       16306         401        2       811   1.99 2007-01-29 17:59:08.996577
    ## 8       16307         402        2       801   1.99 2007-01-29 16:04:16.996577
    ## 9       16314         407        1       619   7.99 2007-01-28 14:20:52.996577
    ## 10      16320         411        2       972   1.99 2007-01-30 18:49:33.996577

``` r
dbClearResult(q)
```

### Exercise 4.2

#### Construct a query that retrives all rows from the payment table where the amount is greater then 5

``` r
dbGetQuery(con, "SELECT *
FROM payment
WHERE amount > 5 LIMIT 5")
```

    ##   payment_id customer_id staff_id rental_id amount               payment_date
    ## 1      16052         269        2       678   6.99 2007-01-28 21:44:14.996577
    ## 2      16058         271        1      1096   8.99 2007-01-31 11:59:15.996577
    ## 3      16060         272        1       405   6.99 2007-01-27 12:01:05.996577
    ## 4      16061         272        1      1041   6.99 2007-01-31 04:14:49.996577
    ## 5      16068         274        1       394   5.99 2007-01-27 09:54:37.996577

# Exercise 5

#### Retrive all the payment IDs and their amount from the customers whose last name is ‘DAVIS’.

``` r
dbGetQuery(con, "SELECT p.payment_id, p.amount
FROM payment AS p
INNER JOIN customer AS c ON p.customer_id=c.customer_id
WHERE c.last_name = 'DAVIS'")
```

    ##   payment_id amount
    ## 1      16685   4.99
    ## 2      16686   2.99
    ## 3      16687   0.99

# Exercise 6

### Exercise 6.1

#### Use COUNT(\*) to count the number of rows in rental

``` r
dbGetQuery(con, "
SELECT customer_id, 
COUNT(*) AS 'Rentals'
FROM payment
")
```

    ##   customer_id Rentals
    ## 1         269    1157

### Exercise 6.2

#### Use COUNT(\*) and GROUP BY to count the number of rentals for each customer\_id

``` r
dbGetQuery(con, "
SELECT customer_id, COUNT(*) AS 'Rentals'
FROM rental GROUP BY customer_id LIMIT 10
")
```

    ##    customer_id Rentals
    ## 1            1      32
    ## 2            2      27
    ## 3            3      26
    ## 4            4      22
    ## 5            5      38
    ## 6            6      28
    ## 7            7      33
    ## 8            8      24
    ## 9            9      23
    ## 10          10      25

### Exercise 6.3

#### Repeat the previous query and sort by the count in descending order

``` r
dbGetQuery(con, "
SELECT customer_id, COUNT(*) AS 'Rentals'
FROM rental GROUP BY customer_id
ORDER BY `Rentals` DESC LIMIT 10
")
```

    ##    customer_id Rentals
    ## 1          148      46
    ## 2          526      45
    ## 3          236      42
    ## 4          144      42
    ## 5           75      41
    ## 6          469      40
    ## 7          197      40
    ## 8          468      39
    ## 9          178      39
    ## 10         137      39

### Exercise 6.4

#### Repeat the previous query but use HAVING to only keep the groups with 40 or more.

``` r
dbGetQuery(con, "
SELECT customer_id, COUNT(*) AS 'Rentals'
FROM rental GROUP BY customer_id
HAVING `Rentals` >= 40
ORDER BY `Rentals` DESC
")
```

    ##   customer_id Rentals
    ## 1         148      46
    ## 2         526      45
    ## 3         236      42
    ## 4         144      42
    ## 5          75      41
    ## 6         469      40
    ## 7         197      40

# Exercise 7

#### The following query calculates a number of summary statistics for the payment table using MAX, MIN, AVG and SUM

``` r
dbGetQuery(con, "
SELECT 
  MAX(amount) AS `max`,
  MIN(amount) AS `min`,
  AVG(amount) AS `avg`,
  SUM(amount) AS `sum`
FROM payment")
```

    ##     max  min      avg     sum
    ## 1 11.99 0.99 4.169775 4824.43

### Exercise 7.1

#### Modify the above query to do those calculations for each customer\_id

``` r
dbGetQuery(con, "
SELECT 
  customer_id,
  MAX(amount) AS `max`,
  MIN(amount) AS `min`,
  AVG(amount) AS `avg`,
  SUM(amount) AS `sum`
FROM payment GROUP BY customer_id
LIMIT 5")
```

    ##   customer_id  max  min      avg  sum
    ## 1           1 2.99 0.99 1.990000 3.98
    ## 2           2 4.99 4.99 4.990000 4.99
    ## 3           3 2.99 1.99 2.490000 4.98
    ## 4           5 6.99 0.99 3.323333 9.97
    ## 5           6 4.99 0.99 2.990000 8.97

### Exercise 7.2

#### Modify the above query to only keep the customer\_ids that have more then 5 payments

``` r
dbGetQuery(con, "
SELECT 
  customer_id,
  MAX(amount) AS `max`,
  MIN(amount) AS `min`,
  AVG(amount) AS `avg`,
  SUM(amount) AS `sum`
FROM payment
GROUP BY customer_id
HAVING COUNT(*) >5")
```

    ##    customer_id  max  min      avg   sum
    ## 1           19 9.99 0.99 4.490000 26.94
    ## 2           53 9.99 0.99 4.490000 26.94
    ## 3          109 7.99 0.99 3.990000 27.93
    ## 4          161 5.99 0.99 2.990000 17.94
    ## 5          197 3.99 0.99 2.615000 20.92
    ## 6          207 6.99 0.99 2.990000 17.94
    ## 7          239 7.99 2.99 5.656667 33.94
    ## 8          245 8.99 0.99 4.823333 28.94
    ## 9          251 4.99 1.99 3.323333 19.94
    ## 10         269 6.99 0.99 3.156667 18.94
    ## 11         274 5.99 2.99 4.156667 24.94
    ## 12         371 6.99 0.99 4.323333 25.94
    ## 13         506 8.99 0.99 4.132857 28.93
    ## 14         596 6.99 0.99 3.823333 22.94

# Clean up

``` r
dbDisconnect(con)
```
