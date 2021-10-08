LAB7
================
Caroline He
10/8/2021

# Web scraping and Regular Expressions

## Learning Goals

-   Use a real world API to make queries and process the data.

-   Use regular expressions to parse the information.

-   Practice your GitHub skills.

## Lab description

In this lab, we will be working with the NCBI API to make queries and
extract information using XML and regular expressions. For this lab, we
will be using the httr, xml2, and stringr R packages.

``` r
#r packages
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidytext)
library(httr)
```

### Question 1: How many sars-cov-2 papers?

Build an automatic counter of sars-cov-2 papers using PubMed. You will
need to apply XPath as we did during the lecture to extract the number
of results returned by PubMed in the following web
address:<https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2>

``` r
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

# Finding the counts
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/section[1]/div[2]/div[1]/span")

# Turning it into text
counts <- as.character(counts)

# Extracting the data using regex
stringr::str_extract(counts, "[0-9,]+")
```

    ## [1] "114,592"

``` r
stringr::str_extract(counts, "[[:digit:],]+")
```

    ## [1] "114,592"

### Question 2: Academic publications on COVID19 and Hawaii

You need to query the following The parameters passed to the query are
documented here.

Use the function httr::GET() to make the following query:

1.  Baseline URL:
    <https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi>
2.  Query parameters:

-   db: pubmed
-   term: covid19 hawaii
-   retmax: 1000

``` r
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db     = "pubmed",
    term   = "covid19 hawaii",
    retmax = 1000
    )
)

# Extracting the content of the response of GET
ids <- httr::content(query_ids)
ids
```

    ## {xml_document}
    ## <eSearchResult>
    ## [1] <Count>150</Count>
    ## [2] <RetMax>150</RetMax>
    ## [3] <RetStart>0</RetStart>
    ## [4] <IdList>\n  <Id>34562997</Id>\n  <Id>34559481</Id>\n  <Id>34545941</Id>\n ...
    ## [5] <TranslationSet>\n  <Translation>\n    <From>covid19</From>\n    <To>"cov ...
    ## [6] <TranslationStack>\n  <TermSet>\n    <Term>"covid-19"[MeSH Terms]</Term>\ ...
    ## [7] <QueryTranslation>("covid-19"[MeSH Terms] OR "covid-19"[All Fields] OR "c ...

The query will return an XML object, we can turn it into a character
list to analyze the text directly with as.character(). Another way of
processing the data could be using lists with the function
xml2::as\_list(). We will skip the latter for now.

### Question 3: Get details about the articles

The Ids are wrapped around text in the following way: <Id>… id number
…</Id>. we can use a regular expression that extract that information.

``` r
# Turn the result into a character vector
ids <- as.character(ids)

# Find all the ids 
ids <- stringr::str_extract_all(ids, "<Id>[[:digit:]]+</Id>")[[1]]

# Remove all the leading and trailing <Id> </Id>. Make use of "|"
ids <- stringr::str_remove_all(ids, "<Id>|</Id>")
ids
```

    ##   [1] "34562997" "34559481" "34545941" "34536350" "34532685" "34529634"
    ##   [7] "34499878" "34491990" "34481278" "34473201" "34448649" "34417121"
    ##  [13] "34406840" "34391908" "34367726" "34355196" "34352507" "34334985"
    ##  [19] "34314211" "34308400" "34308322" "34291832" "34287651" "34287159"
    ##  [25] "34283939" "34254888" "34228774" "34226774" "34210370" "34195618"
    ##  [31] "34189029" "34183789" "34183411" "34183191" "34180390" "34140009"
    ##  [37] "34125658" "34108898" "34102878" "34091576" "34062806" "33990619"
    ##  [43] "33982008" "33980567" "33973241" "33971389" "33966879" "33938253"
    ##  [49] "33929934" "33926498" "33900192" "33897904" "33894385" "33889849"
    ##  [55] "33889848" "33859192" "33856881" "33851191" "33826985" "33789080"
    ##  [61] "33781762" "33781585" "33775167" "33770003" "33769536" "33746047"
    ##  [67] "33728687" "33718878" "33717793" "33706209" "33661861" "33661727"
    ##  [73] "33657176" "33655229" "33607081" "33606666" "33606656" "33587873"
    ##  [79] "33495523" "33482708" "33471778" "33464637" "33442699" "33422679"
    ##  [85] "33422626" "33417334" "33407957" "33331197" "33316097" "33308888"
    ##  [91] "33301024" "33276110" "33270782" "33251328" "33244071" "33236896"
    ##  [97] "33229999" "33216726" "33193454" "33186704" "33176077" "33139866"
    ## [103] "33098971" "33096099" "33087192" "33083826" "33043445" "33027604"
    ## [109] "32984015" "32969950" "32921878" "32914097" "32914093" "32912595"
    ## [115] "32907823" "32907673" "32891785" "32888905" "32881116" "32837709"
    ## [121] "32763956" "32763350" "32745072" "32742897" "32692706" "32690354"
    ## [127] "32680824" "32666058" "32649272" "32596689" "32592394" "32584245"
    ## [133] "32501143" "32486844" "32462545" "32432219" "32432218" "32432217"
    ## [139] "32427288" "32420720" "32386898" "32371624" "32371551" "32361738"
    ## [145] "32326959" "32323016" "32314954" "32300051" "32259247" "32151778"

With the ids in hand, we can now try to get the abstracts of the papers.
As before, we will need to coerce the contents (results) to a list
using:

1.  Baseline url:
    <https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi>
2.  uery parameters:

-   db: pubmed
-   id: A character with all the ids separated by comma, e.g.,
    “1232131,546464,13131”
-   retmax: 1000
-   rettype: abstract

Pro-tip: If you want GET() to take some element literal, wrap it around
I() (as you would do in a formula in R). For example, the text “123,456”
is replaced with “123%2C456”. If you don’t want that behavior, you would
need to do the following I(“123,456”).

``` r
publications <- GET(
  url = "https://eutils.ncbi.nlm.nih.gov/",
  path = "entrez/eutils/efetch.fcgi",
  query = list(
    db = "pubmed",
    id = paste(ids, collapse = ","),
    retmax = 1000,
    rettype = "abstract"
    )
)

# Turning the output into character vector
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```
