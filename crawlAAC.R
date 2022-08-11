library(rvest)
library(RSelenium)
library(data.table)
library(tidyverse)
library(webshot)
# docker run -d -p 4444:4444 --shm-size="2g" selenium/standalone-chrome:4.1.4-20220427
## To stop:
# docker stop $(docker ps -q)

## Basic settings - set the site of your document repository
## And the first page to start your crawl
dataDir <- "/Users/peterbradley/Documents/GitHub/AAC-Archive/SpringshareBackup/"
url <- "https://ferris.libguides.com/"
page <- read_html(url)

library(RSelenium)
rD <- rsDriver(browser="chrome", chromever="101.0.4951.41")
remDr <- rD[["client"]]

remDr$open()
remDr$navigate(url)

webElem <-NULL
while(is.null(webElem)){
  webElem <- tryCatch({remDr$findElement(using = 'css selector', value = ".panel-heading")},
                      error = function(e){NULL})
  #loop until element with name <value> is found in <webpage url>
}
pagesource <- remDr$getPageSource()
html <- read_html(pagesource[[1]])
split_headers <- function(myStr) {
  strsplit(myStr, ",") -> strList
  lapply(strList, trimws) -> strList
  return(unlist(strList))
}

mk_header_dir <- function(myHeader) {
  myDir <- paste(dataDir, myHeader[1], sep="/")
  if(!dir.exists(myDir)) {
    dir.create(myDir)
  }
}

headers <- html %>% 
  html_nodes(".panel-heading div.bold") %>% 
  html_text()

lapply(headers, split_headers) -> headerList
lapply(headerList, mk_header_dir)-> headerDirList


html %>% html_nodes(".panel") -> panels
## Header list an panels are in the same order
read_guides <- function(n, headerList, panels) {
  myDir <- paste(dataDir, headerList[[n]][1], sep="/")
  myLinks <- panels[[n]] %>%
    html_nodes(".s-lg-gtitle a") %>%
    html_attr("href")
  myTitles <- panels[[n]] %>%
    html_nodes(".s-lg-gtitle a") %>%
    html_text()
  pagesDF <- data.table("link"=myLinks, "title"=myTitles)
  return(pagesDF)
}

lapply(c(1:length(headerList)), read_guides, headerList, panels) -> pageTables

scrape_libguides <- function(myN, pageTables, headerList, dataDir) {
  dataDir <-  paste(dataDir, headerList[[myN]][1], sep="/")
  message("Doing ", dataDir)
  lapply(c(1:nrow(pageTables[[myN]])), scrape_committee, pageTables[[myN]], dataDir)
}


### Do the AAC
url <- "https://ferris.libguides.com/aacinfohub/home"
page <- read_html(url)

aacdir <- paste(dataDir, "/AACInfoHub/", sep="")
if(!dir.exists(aacdir)) {
  dir.create(aacdir)
}
aacnav <- page %>%
    html_nodes(".nav li")
  
committees <- aacnav %>%
  html_nodes("a") %>%
  html_text()
committeelinks <- aacnav %>%
  html_nodes("a") %>%
  html_attr("href")

committeeDF <- data.table("link"=committeelinks, "title"=committees)

download_annual_report <- function(myN, myDF, dataDir) {
  link <- myDF[myN,]$link
  title <- paste(gsub("[[:space:]]", "", myDF[myN,]$title), "docx", sep=".")
  myDir <- paste(dataDir, myDF[myN,]$safe_title, sep="/")
  message("downloading to ", myDir)
  download.file(link, destfile = paste(myDir, title, sep="/"))
}

scrape_committee <- function(myN, myDF, dataDir) {
  title <- myDF[myN,]$title
  link <- myDF[myN,]$link
  message("Doing ", link)
  safe_title <- gsub("&", "and", title)
  safe_title <- gsub("[[:space:]]", "_", safe_title)
  myDir <- paste(dataDir, safe_title, sep="/")
  if(!dir.exists(myDir)) {
    dir.create(myDir)
  }
  page <- read_html(link)
  download.file(link, destfile=paste(myDir, "/", safe_title, ".html", sep=""))
  webshot(link, file=paste(myDir, "/", safe_title, ".png", sep=""))
  links <-  page %>%
    html_nodes("a") %>%
    html_attr("href")
  titles <- page %>%
    html_nodes("a") %>%
    html_text()
  
  page_set <- data.table("title"=titles, "link"=links, "safe_title"=safe_title)
  todo <- page_set %>% 
    filter(grepl("^http.*", link)) %>%
    filter(grepl("content_id", link))

  if(nrow(todo) > 0) {
    lapply(c(1:nrow(todo)), download_annual_report, todo, dataDir)
  }
}

lapply(c(5:nrow(committeeDF)), scrape_committee, committeeDF, aacdir)

dataDir <- "/Users/peterbradley/Documents/GitHub/AAC-Archive/SpringshareBackup/"

lapply(c(1:length(pageTables)), scrape_libguides, pageTables, headerList, dataDir)

rD[["server"]]$stop()