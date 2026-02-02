## ----include = FALSE----------------------------------------------------------
library(knitr)
library(PxWebApiData)
options(max.print = 44)

# Re-define the comment functions to control line width and minimize excessive line breaks when printing.
comment <- function(x, fun = base::comment) {
     com <- fun(x)
     nchar_name <- min(103, 2 + max(nchar(com)))
     if (length(com)) 
       if (is.null(names(com)))
         names(com) <- paste0("[", seq_along(com), "]")
     for (name in names(com)) {
         cat(strrep(" ", max(0, (nchar_name - nchar(name)))),
             name, 
             "\n", 
             strrep(" ", max(0, (nchar_name - nchar(com[[name]]) - 2))),
             "\"",
             com[[name]],
             "\"",  "\n", sep = "")
     }
}
info <- function(x) comment(x, PxWebApiData::info)
note <- function(x) comment(x, PxWebApiData::note)

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------

url <- paste0(
  "https://data.ssb.no/api/pxwebapi/v2/tables/04861/data?lang=en",
  "&valueCodes[Region]=0301,324*",
  "&valueCodes[ContentsCode]=???????",
  "&valueCodes[Tid]=top(2)"
)

get_api_data(url)


## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
query_url("https://data.ssb.no/api/pxwebapi/v2/tables/04861/data?lang=en", 
          Region = c("0301", "324*"), 
          ContentsCode = "???????", 
          Tid = "top(2)")

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
api_data("https://data.ssb.no/api/pxwebapi/v2/tables/04861/data?lang=en", 
         Region = c("0301", "324*"), 
         ContentsCode = "???????", 
         Tid = "top(2)")

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
api_data_12("https://data.ssb.no/api/pxwebapi/v2/tables/04861/data?lang=en", 
           Region = 14:17, 
           ContentsCode = 2)

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
api_data_2("https://data.ssb.no/api/pxwebapi/v2/tables/04861/data?lang=en", 
          Region = FALSE, 
          ContentsCode = TRUE, 
          Tid = 3i)

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------

obj <- api_data("https://data.ssb.no/api/pxwebapi/v2/tables/04861/data?lang=en", 
                Region = c("Asker", "Hurdal"), 
                ContentsCode = TRUE, 
                Tid = 2i)


## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------

obj[[1]]
obj[[2]]

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
out <- api_data_2("https://data.ssb.no/api/pxwebapi/v2/tables/10172/data?lang=en", 
                   default_query = TRUE)
out[14:20, ]  # 9 rows printed  

## ----comment=NA---------------------------------------------------------------

info(obj)
note(obj)

## ----comment=NA---------------------------------------------------------------
note(out)

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
 api_data_2("https://data.ssb.no/api/pxwebapi/v2/tables/07459/data?lang=en",
            Region = list(codelist = "agg_KommSummer", 
                          valueCodes = c("K-3101", "K-3103"), 
                          outputValues = "aggregated"),
            Kjonn = TRUE,
            Alder = list(codelist = "agg_TodeltGrupperingB", 
                         valueCodes = c("H17", "H18"),
                         outputValues = "aggregated"),
            ContentsCode = 1,
            Tid = 2i)  

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
 url <- query_url("https://data.ssb.no/api/pxwebapi/v2/tables/07459/data?lang=en",
            Region = list(codelist = "agg_KommSummer", 
                          valueCodes = c("K-3101", "K-3103"), 
                          outputValues = "aggregated"),
            Kjonn = TRUE,
            Alder = list(codelist = "agg_TodeltGrupperingB", 
                         valueCodes = c("H17", "H18"),
                         outputValues = "aggregated"),
            ContentsCode = 1,
            Tid = 2i)
 cat(gsub("&", "\n&", url))

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
mf <- meta_frames("https://data.ssb.no/api/pxwebapi/v2/tables/04861/data?lang=en")
print(mf)

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
sapply(mf, attr, "elimination") # elimination info for all variables

## ----eval=TRUE, tidy = FALSE, comment=NA--------------------------------------
attr(mf[["Region"]], "code_lists")

## ----eval=TRUE, tidy = FALSE, comment=NA, encoding = "UTF-8"------------------

url_eurostat <- paste0(   # Here the long url is split into several lines using paste0 
  "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/prc_hicp_mv12r", 
  "?format=JSON&lang=EN&lastTimePeriod=2&coicop=CP00&geo=NO&geo=EU")
url_eurostat
get_api_data_12(url_eurostat)


