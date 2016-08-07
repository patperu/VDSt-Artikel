Wohnungsmarktanalysen mit Immoscout24
================
Patrick Hausmann, Arnt von Bodelschwingh
07/08/2016

-   [Funktionen zu Abfrage der API](#funktionen-zu-abfrage-der-api)
-   [Daten für Berlin abfragen](#daten-fur-berlin-abfragen)
-   [Datensatz bereinigen](#datensatz-bereinigen)
-   [Beispieloutput](#beispieloutput)

``` r
library('httr')
library('jsonlite')
library('dplyr')

options(httr_oauth_cache = FALSE,
        scipen = 999,
        stringsAsFactors = FALSE)
knitr::opts_chunk$set(echo = TRUE)
```

Funktionen zu Abfrage der API
=============================

Für die Nutzung der Rest-API ist eine Anmeldung bei Immobilienscout24 notwendig. Die Authentifizierung erfolgt dann mittels der in der Datei `.Renviron` hinterlegten Keys und Tokens.

``` r
immo24_baseurl <- function() "http://rest.immobilienscout24.de/restapi/api/search/v1.0/search/region?realestatetype=apartmentrent&geocodes="

immo24_auth <- function() {

  app <- oauth_app("app", key = Sys.getenv("consumerKey"),
                          secret = Sys.getenv("consumerSecret"))
  sig <- sign_oauth1.0(app, token = Sys.getenv("tokenGenerated"),
                            token_secret = Sys.getenv("tokenSecret"))
  return(sig)
}

get_recs <- function(x) {
  x <- fromJSON(rawToChar(x$content))$resultlist.resultlist$paging
  return(x)
}

get_numpages <- function(geocode) {

  r  <- GET(paste0(url = immo24_baseurl(), geocode), sig)
  stop_for_status(r)
  r <- get_recs(r)

  message("Seiten: ", r$numberOfPages)
  message("Wohnungsangebote: ", r$numberOfHits)
  
  return(r$numberOfPages)

}

get_data <- function(geocode) {

  num_pages <- get_numpages(geocode)

  res <- list()

  for (i in 1:num_pages) {

    r  <- GET(paste0(url = immo24_baseurl(), geocode, "&pagenumber=", i), sig)
    r <- fromJSON(content(r, "text", encoding="UTF-8"))$resultlist.resultlist$resultlistEntries$resultlistEntry

    fin <- r[[1]][, 1:6]
    colnames(fin) <- gsub("@", "", colnames(fin))
    fin <- data.frame(fin, r[[1]]$resultlist.realEstate)
    res[[i]] <- fin
  }

  res_a <- bind_rows(lapply(res, "[", c("title", "privateOffer", 
                                        "livingSpace", "numberOfRooms", 
                                        "balcony", "garden")))
  res_b <- bind_rows(lapply(res, "[[", "price"))
  
  res_df <- data.frame(res_a, res_b)
  
  out <- list(res_raw = res, res_df = res_df)

  return(out)

}
```

Daten für Berlin abfragen
=========================

``` r
sig <- immo24_auth()
dat <- get_data(geocode = "1276003001")
```

    ## Seiten: 214

    ## Wohnungsangebote: 4278

Datensatz bereinigen
====================

``` r
x <- tbl_df(dat$res_df) %>%
     dplyr::select(livingSpace, numberOfRooms, value) %>%
     rename(price = value, 
            qm = livingSpace,
            rooms = numberOfRooms) %>%
     mutate(pqm = price/qm) %>%
     filter(price >= 150 & 
            qm >= 35 & qm <= 250 & 
            rooms <= 10 &
            pqm   >=  3.50 & pqm <= 20) %>%
            sample_n(3)
```

Beispieloutput
==============

``` r
x <- data.frame(qm = c(55.36, 86.34, 96.45), 
                 rooms = c(2,3,3), 
                 price = c(423, 1045, 987),
                 pqm = c(7.640896, 12.10331, 10.23328))
knitr::kable(x, format = "markdown")
```

|     qm|  rooms|  price|        pqm|
|------:|------:|------:|----------:|
|  55.36|      2|    423|   7.640896|
|  86.34|      3|   1045|  12.103310|
|  96.45|      3|    987|  10.233280|