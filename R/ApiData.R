#' PX-Web Data by API
#' 
#' A function to read PX-Web data into R via API. The example code reads data from the three national statistical institutes, Statistics Norway, Statistics Sweden and Statistics Finland.
#' 
#' @encoding UTF8
#'
#' @param urlToData url to data or id of SSB data
#' @param ... specification of JSON query for each variable
#' @param getDataByGET When TRUE, readymade dataset by GET - works only for Statistics Norway readymade datasets
#' @param returnMetaData When TRUE, metadata returned  
#' @param returnMetaValues When TRUE, values from metadata returned 
#' @param returnApiQuery When TRUE, JSON query returned 
#' @param defaultJSONquery specification for variables not included in ...
#' @param verbosePrint When TRUE, printing to console
#' @param use_factors Parameter to \code{\link{fromJSONstat}} defining whether dimension categories should be factors or character objects.
#' @param urlType  Parameter defining how url is constructed from id number. Currently two Statistics Norway possibilities: "SSB" (Norwegian) or "SSBen" (English)
#' 
#' @details Each variable is specified by using the variable name as input parameter. The value can be specified as:  
#' TRUE (all), FALSE (eliminated), imaginary value (top), original variable values or variable indices. Reversed 
#' indices can be specified as negative values. Indices outside the range are removed. Variables not specified 
#' is set to the value of defaultJSONquery whose default means the first and the two last elements. 
#' 
#' The value can also be specified as a (unnamed) two-element list corresponding to the two 
#' query elements, filter and values. I addition it possible with a single-element list.
#' Then filter is set to 'all'. See examples. 
#'
#' @return list of two data sets (label and id)
#' @export
#' 
#' @importFrom jsonlite unbox toJSON read_json
#' @importFrom rjstat fromJSONstat 
#' @importFrom httr GET POST verbose content
#'
#' @examples
#' \dontrun{
#' 
#' ##### Readymade dataset by GET - works only for Statistics Norway readymade datasets
#' x <- ApiData("http://data.ssb.no/api/v0/dataset/1066.json?lang=en", getDataByGET = TRUE)
#' x[[1]]  # The label version of the data set
#' x[[2]]  # The id version of the data set
#' 
#' ##### Special output
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", returnMetaData = TRUE)  # meta data
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", returnMetaValues = TRUE)  # meta data values
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", returnApiQuery = TRUE)  # Query using defaults
#' 
#' ##### Ordinary use
#' 
#' # Two specified and the last is default
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", NACE2007 = 0+10i, ContentsCode = TRUE)
#' 
#' # Two specified and the last is default – in Norwegian change en to no in url
#' ApiData("http://data.ssb.no/api/v0/no/table/09941", NACE2007 = 0+10i, ContentsCode = TRUE)
#' 
#' 
#' # Number of residents (bosatte) last year, each region
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = TRUE, 
#'         ContentsCode = "Bosatte", Tid = 0+1i)
#' 
#' # Number of residents (bosatte) each year, total
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = FALSE, 
#'         ContentsCode = "Bosatte", Tid = TRUE)
#' 
#' # Some years
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = FALSE, 
#'         ContentsCode = "Bosatte", Tid = c(1, 5, -1))
#' 
#' # Two selected regions
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = c("0811", "0301"), 
#'         ContentsCode = 1, Tid = c(1, -1))
#' 
#' 
#' 
#' ##### Advanced use using list
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = list("03*"), 
#'         ContentsCode = 1, Tid = 0+5i)
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = list("all", "03*"), 
#'         ContentsCode = 1, Tid = 0+5i)
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = list("filter", c("0811", "0301")), 
#'         ContentsCode = 1, Tid = 0+5i)
#' 
#' ##### Using id instead of url, unnamed input and verbosePrint
#' ApiData(4861, c("0811", "0301"), 1, c(1, -1))
#' ApiData("4861", c("0811", "0301"), 1, c(1, -1),  urlType="SSBen")
#' ApiData("04861", c("0811", "0301"), 1, c(1, -1), verbosePrint = TRUE)
#' ApiData(1066, getDataByGET = TRUE,  urlType="SSB")
#' ApiData(1066, getDataByGET = TRUE,  urlType="SSBen")
#' 
#' ##### Data from SCB and Statfi:
#' ApiData("http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy")
#' ApiData("http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/kuol/statfin_kuol_pxt_010.px")
#'          
#' }
ApiData <- function(urlToData, ..., getDataByGET = FALSE, returnMetaData = FALSE, returnMetaValues = FALSE, returnApiQuery = FALSE, 
                    defaultJSONquery = c(1,-2, -1), verbosePrint = FALSE,
                    use_factors=FALSE, urlType="SSB") {
  integerUrl <- suppressWarnings(as.integer(urlToData))
  if (!is.na(integerUrl)) 
    urlToData <- MakeUrl(integerUrl, urlType = urlType, getDataByGET = getDataByGET) # SSBurl(integerUrl, getDataByGET)
  if (getDataByGET) 
    post <- GET(urlToData) else {
      metaData <- MetaData(urlToData)
      if (returnMetaData) 
        return(metaData)
      varMetaData <- VarMetaData(metaData)
      if (returnMetaValues) 
        return(varMetaData)
      if (verbosePrint) 
        print(varMetaData)
      cat("\n\n")
      # if(returnApiDataCall) # Not in use
      # return(as.call(c(list(as.symbol('ApiData'),urlToData=urlToData),MakeApiQuery(varMetaData,...,returnThezList=TRUE,defaultJSONquery=defaultJSONquery))))
      sporr <- MakeApiQuery(varMetaData, ..., defaultJSONquery = defaultJSONquery)
      if (returnApiQuery) 
        return(sporr)
      if (verbosePrint) 
        post <- POST(urlToData, body = sporr, encode = "json", verbose()) else post <- POST(urlToData, body = sporr, encode = "json")
    }
  #c(fromJSONstat(content(post, "text"), naming = "label"), fromJSONstat(content(post, "text"), naming = "id"))
  c(fromJSONstat(content(post, "text"), naming = "label",use_factors=use_factors), 
    fromJSONstat(content(post, "text"), naming = "id",use_factors=use_factors))
}



#################################



# library(httr) library(rjstat) library(jsonlite)



#' Adding leading zeros
#'
#' @param n  numeric vector
#' @param width width
#'
#' @return Number as string
#' @keywords internal 
#'
Number = function(n,width=3){
  s = "s=sprintf('%0d',n)"
  s = gsub("0",as.character(width),s)
  eval(parse(text=s))
  s = gsub(" ","0",s)
  s
}


MetaData <- function(url) {
  z <- read_json(url)  # Same as fromJSON(content(GET(url),'text'),simplifyVector = FALSE)
  tit <- z[[1]]
  z <- z[[2]]
  for (i in seq_len(length(z))) {
    for (j in seq_len(length(z[[i]]))) z[[i]][[j]] <- unlist(z[[i]][[j]])
  }
  z
}

VarMetaData <- function(metaData) {
  n <- length(metaData)
  nam <- rep("", n)
  elimination <- rep(FALSE, n)
  for (i in 1:length(metaData)) {
    nam[i] <- metaData[[i]]$code
    if (!is.null(metaData[[i]]$elimination)) 
      elimination[i] <- metaData[[i]]$elimination
    metaData[[i]] <- metaData[[i]]$values
  }
  names(metaData) <- nam
  attr(metaData, "elimination") <- elimination
  metaData
}


MakeApiVar <- function(x, values = c(1, -2, -1)) {
  if (is.list(values)) {
    if (length(values) == 1) {
      filt <- "all"
      valu <- values[[1]]
    } else {
      filt <- values[[1]]
      valu <- values[[2]]
    }
  } else {
    if (is.logical(values)) {
      if (!values) 
        return(NULL) else {
          filt <- "all"
          valu <- "*"
        }
      
    } else if (is.complex(values)) {
      filt <- "top"
      valu <- as.character(Im(values))
    } else if (is.numeric(values)) {
      filt <- "item"
      nx <- length(x[[1]])
      values <- values[abs(values) > 0 & abs(values) <= nx]  # Fix outside range
      values[values < 0] <- rev(seq_len(nx))[-values[values < 0]]  # Fix negative
      valu <- x[[1]][unique(values)]
      if (!length(valu)) 
        stop(paste(names(x), "no indices in valid range"))
    } else {
      filt <- "item"
      valu <- values
    }
  }
  list(code = jsonlite::unbox(names(x)), selection = list(filter = unbox(filt), values = valu))
}


Pmatch <- function(x, y, CheckHandling = stop) {
  # as pmatch where NA set to remaing values in y
  a <- pmatch(x, y)
  naa <- is.na(a)
  if (any(naa)) {
    nax <- naa & !is.na(x)
    if (any(nax)) 
      CheckHandling(paste("Non-matching input:", paste(x[nax], collapse = ", ")))
    if (!any(!naa)) 
      return(seq_len(length(y))[seq_len(length(x))])
    nna <- sum(naa)
    a[naa] <- seq_len(length(y))[-a[!naa]][seq_len(nna)]
  }
  a
}



SSBurl <- function(id, readyMade = FALSE) {
  if (readyMade) 
    url <- paste("http://data.ssb.no/api/v0/dataset/", Number(id, 1), ".json", sep = "") 
  else url <- paste("http://data.ssb.no/api/v0/no/table/", Number(id, 5), sep = "")
  url
}

SSBurlen <- function(id, readyMade = FALSE) {
  if (readyMade) 
    url <- paste("http://data.ssb.no/api/v0/dataset/", Number(id, 1), ".json?lang=en", sep = "") 
  else 
    url <- paste("http://data.ssb.no/api/v0/en/table/", Number(id, 5), sep = "")
  url
}


#' MakeUrl from id
#' 
#' @encoding UTF8
#'
#' @param id integer
#' @param urlType  Currently two possibilities: "SSB" (Norwegian) or "SSBen" (English)
#' @param getDataByGET As input to ApiData
#'
#' @return url as string
#' @export
#' @keywords internal
#'
#' @examples
#' MakeUrl(4861)
#' MakeUrl(4861, "SSBen")
#' MakeUrl(1066, getDataByGET = TRUE)
#' MakeUrl(1066, "SSBen", getDataByGET = TRUE)
MakeUrl <- function(id,urlType="SSB",getDataByGET = FALSE){
  if(urlType=="SSB")
    return(SSBurl(id,getDataByGET))
  if(urlType=="SSBen")
    return(SSBurlen(id,getDataByGET))
  stop('urlType must be "SSB" or "SSBen"')
}



MakeApiQuery <- function(varMetaData, ..., defaultJSONquery = c(1, -2, -1), returnThezList = FALSE) {
  x <- list(...)
  namesx <- names(x)
  if (is.null(namesx)) 
    namesx <- rep(NA, length(x)) else namesx[namesx == ""] <- NA
  z <- vector("list", length(varMetaData))
  a <- z
  names(z) <- names(varMetaData)
  pm <- Pmatch(namesx, names(varMetaData))
  for (i in seq_len(length(z))) z[[i]] <- defaultJSONquery
  z[pm] <- x
  elim <- attr(varMetaData, "elimination")
  emptya <- rep(FALSE, length(a))
  if (returnThezList) 
    return(z)
  for (i in seq_len(length(a))) {
    apiVar <- MakeApiVar(varMetaData[i], z[[i]])
    if (is.null(apiVar)) {
      if (!elim[i]) 
        stop(paste(names(z)[i], "cannot be eliminated"))
      emptya[i] <- TRUE
    } else a[[i]] <- apiVar
  }
  b <- list(query = a[!emptya], response = list(format = unbox("json-stat")))
  toJSON(b, auto_unbox = FALSE, pretty = TRUE)
}































