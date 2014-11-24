# Parse txt and images from the source 

library(RCurl)
library(XML)
library(stringr)
library(dplyr) # version 0.3 required

# Information source (© copyright 2009-14 By Umesh Rudrappa)
html <- htmlParse("http://www.nutrition-and-you.com/vegetable-nutrition.html")


# Parse vegetable list
xpath.list <- "//h4[contains(., 'Here\r\nis an impressive')]/following::div[@class='desktopOnly'][1]/table[1]//td"

vegetables <- html %>%
  xpathSApply(xpath.list, xmlChildren) %>%
  lapply(function(x){
    while("text" %in% names(x)) 
      x$text <- NULL  # Remove extra "text" nodes
    name <- xmlValue(x[[2]]) %>% str_replace_all("\r\n|\\s+", " ")
    link <- xmlGetAttr(x[[2]], "href")
    if(is.null(link))
      link <- xmlGetAttr(xmlChildren(x[[2]])[[1]], "href")
    image <- xmlGetAttr(x[[1]], "src")
    if(is.null(image))
      image <- xmlGetAttr(xmlChildren(x[[1]])[[1]], "src")
    return(data_frame(Name=name, Link=link, Image=image))
  }) %>% rbind_all()


# Parse vegetable images
apply(vegetables, 1, function(x){
  download.file(x[["Image"]], paste0("image/", x[["Name"]], ".gif"), mode="wb")
  })


# Parse vegetable benefit descriptions
xpath.item <- "//h3[contains(., 'Health')]/following::ul[1]/li"

apply(vegetables, 1, function(x){
  txt <- xpathSApply(htmlParse(x[["Link"]]), xpath.item, xmlValue) %>% 
    str_replace_all("\r\n", " ") %>%
    str_trim()
  fileConn <- file(paste0("text/", x[["Name"]], ".txt"))
  writeLines(txt, fileConn)
  close(fileConn)
})

