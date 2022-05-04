library(rvest)
library(data.table)
library(tidyverse)
library(webshot)
## Basic settings - set the site of your document repository
## And the first page to start your crawl
dataDir <- "/Users/peterbradley/Documents/GitHub/AAC-Archive/AACDocStore/"
page <- read_html("https://ferris.libguides.com/aacinfohub/")


aacnav <- page %>% 
  html_nodes(".nav li")
committees <- aacnav %>%
  html_nodes("a") %>%
  html_text()
committeelinks <- aacnav %>%
  html_nodes("a") %>%
  html_attr("href")

committeeDF <- data.table("link"=committeelinks, "committee"=committees)

download_annual_report <- function(myN, myDF) {
  link <- myDF[myN,]$link
  title <- paste(gsub("[[:space:]]", "", myDF[myN,]$title), "docx", sep=".")
  myDir <- paste(dataDir, myDF[myN,]$safe_committee, sep="/")
  download.file(link, destfile = paste(myDir, title, sep="/"))
}

scrape_committee <- function(myN, myDF) {
  committee <- myDF[myN,]$committee
  link <- myDF[myN,]$link
  message("Doing ", link)
  safe_committee <- gsub("&", "and", committee)
  safe_committee <- gsub("[[:space:]]", "_", safe_committee)
  myDir <- paste(dataDir, safe_committee, sep="/")
  if(!dir.exists(myDir)) {
    dir.create(myDir)
  }
  page <- read_html(link)
  download.file(link, destfile=paste(myDir, "/", safe_committee, ".html", sep=""))
  webshot(link, file=paste(myDir, "/", safe_committee, ".png", sep=""))
  links <-  page %>%
    html_nodes("a") %>%
    html_attr("href")
  titles <- page %>%
    html_nodes("a") %>%
    html_text()
  
  page_set <- data.table("title"=titles, "link"=links, "safe_committee"=safe_committee)
  todo <- page_set %>% 
    filter(grepl("^http.*", link)) %>%
    filter(grepl("content_id", link))

  if(nrow(todo) > 0) {
    lapply(c(1:nrow(todo)), download_annual_report, todo)
  }
}

lapply(c(5:nrow(committeeDF)), scrape_committee, committeeDF)