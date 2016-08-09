Wohnungsmarktanalysen mit Immoscout24
================
Patrick Hausmann, Arnt von Bodelschwingh
09/08/2016

-   [Funktionen zu Abfrage der API](#funktionen-zu-abfrage-der-api)
-   [Daten für Berlin abfragen](#daten-fur-berlin-abfragen)
-   [Datensatz bereinigen](#datensatz-bereinigen)
-   [Beispieloutput](#beispieloutput)
-   [Karte der PLZ Gebiete importieren](#karte-der-plz-gebiete-importieren)

``` r
library('httr')
library('jsonlite')
library('dplyr')
library('rgdal')
library('ggplot2')
library('rlist')

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

    r <- GET(paste0(url = immo24_baseurl(), geocode, "&pagenumber=", i), sig)
    r <- fromJSON(content(r, "text", encoding="UTF-8"))$resultlist.resultlist$resultlistEntries$resultlistEntry
    r <- r[[1]]$resultlist.realEstate

    r$realtorLogo <- NULL
    r$realtorLogoForResultList <- NULL
    r$titlePicture <- NULL

    r <- data.frame(rlist::list.flatten(r))
    res[[i]] <- r
  }

  res <- dplyr::bind_rows(res)
  return(res)

}

get_shpfile <- function(url) {

  shp_layer <- strsplit(basename(url), "\\.")[[1]][1]

  tmp <- tempfile()
  pg <- httr::GET(url, write_disk(tmp))
  unzip(tmp, exdir = dirname(tmp))

  shp <- readOGR(dsn = dirname(tmp), 
                 layer = shp_layer, 
                 stringsAsFactors = FALSE, 
                 verbose = FALSE)
  shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))

  unlink(tmp)

  return(shp)

}
```

Daten für Berlin abfragen
=========================

``` r
sig <- immo24_auth()
dat <- get_data(geocode = "1276003001")
```

    ## Seiten: 217

    ## Wohnungsangebote: 4330

Datensatz bereinigen
====================

``` r
x <- tbl_df(dat) %>%
     dplyr::select(address.city,
                   address.postcode, 
                   address.quarter, 
                   address.wgs84Coordinate.latitude, 
                   address.wgs84Coordinate.longitude,
                   livingSpace, 
                   numberOfRooms, 
                   balcony,
                   garden,
                   price.value) %>%
     rename(city = address.city,
            plz  = address.postcode, 
            ortsteil = address.quarter, 
            lat  = address.wgs84Coordinate.latitude, 
            lon  = address.wgs84Coordinate.longitude,
            price = price.value, 
            qm    = livingSpace,
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
x <- data.frame(city = c("Berlin", "Berlin", "Berlin"),
                plz = c("13089", "13158", "13359"),
                ortsteil = c("Heinersdorf (Weißensee)", "Rosenthal (Pankow)", "Wedding (Wedding)"),
                lat = c( 52.51721, 52.58644, NA),
                lon = c(13.4256, 13.3567, NA),
                qm = c(55.36, 86.34, 96.45), 
                rooms = c(2,3,3), 
                balcony = c(TRUE, TRUE, FALSE),
                garden = c(FALSE, FALSE, TRUE),
                price = c(423, 1045, 987),
                pqm = c(7.640896, 12.10331, 10.23328))
knitr::kable(x, format = "markdown")
```

<table style="width:100%;">
<colgroup>
<col width="7%" />
<col width="6%" />
<col width="23%" />
<col width="9%" />
<col width="8%" />
<col width="6%" />
<col width="6%" />
<col width="8%" />
<col width="7%" />
<col width="6%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">city</th>
<th align="left">plz</th>
<th align="left">ortsteil</th>
<th align="right">lat</th>
<th align="right">lon</th>
<th align="right">qm</th>
<th align="right">rooms</th>
<th align="left">balcony</th>
<th align="left">garden</th>
<th align="right">price</th>
<th align="right">pqm</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Berlin</td>
<td align="left">13089</td>
<td align="left">Heinersdorf (Weißensee)</td>
<td align="right">52.51721</td>
<td align="right">13.4256</td>
<td align="right">55.36</td>
<td align="right">2</td>
<td align="left">TRUE</td>
<td align="left">FALSE</td>
<td align="right">423</td>
<td align="right">7.640896</td>
</tr>
<tr class="even">
<td align="left">Berlin</td>
<td align="left">13158</td>
<td align="left">Rosenthal (Pankow)</td>
<td align="right">52.58644</td>
<td align="right">13.3567</td>
<td align="right">86.34</td>
<td align="right">3</td>
<td align="left">TRUE</td>
<td align="left">FALSE</td>
<td align="right">1045</td>
<td align="right">12.103310</td>
</tr>
<tr class="odd">
<td align="left">Berlin</td>
<td align="left">13359</td>
<td align="left">Wedding (Wedding)</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">96.45</td>
<td align="right">3</td>
<td align="left">FALSE</td>
<td align="left">TRUE</td>
<td align="right">987</td>
<td align="right">10.233280</td>
</tr>
</tbody>
</table>

Karte der PLZ Gebiete importieren
=================================

``` r
url <- "https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_PLZ_2015_12.zip"
plz <- get_shpfile(url)

p1 <- ggplot() + geom_polygon(data=plz, aes(x=long, y=lat, group=group), 
                              fill="grey70", colour="white")
p1 <- p1 + coord_quickmap()
p1 <- p1 + ggtitle("PLZ Gebiete in Berlin")
p1
```

![](index_files/figure-markdown_github/PLZ-1.png)
