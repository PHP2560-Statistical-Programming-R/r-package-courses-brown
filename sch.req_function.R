library(httr)
library(jsonlite)
library(dplyr)

sch.req <- function(conc_name, term = c("fall", "spring", "winter")){
  # read in json and store info to variable
  url <- "https://cab.brown.edu/asoc-api/?output=json&page=asoc.rjs&route=search&term=999999"
  all_courses <- fromJSON(url, flatten=TRUE)[["courses"]]
  all_courses <- all_courses[, c(1:3, 8)]
  
  # get required courses for concentration
  required <- conc.req(conc_name)
  required <- unlist(str_match_all(required[, 1:2],"[A-Z]{4}[:space:][0-9]{4}[A-Z]*"))
  
  all_courses <- mutate(all_courses, term1 = as.numeric(sapply(all_courses$terms, function(x) x[1])), 
         term2 = as.numeric(sapply(all_courses$terms, function(x) x[2])), 
         term3 = as.numeric(sapply(all_courses$terms, function(x) x[3])))
  all_courses <- all_courses[,-4]
  
  if(term == "fall"){
    sem <- 201710
  } else if (term == "spring") {
    sem <- 201720
  } else if (term == "winter") {
    sem <- 201715
  }
  
  current_term <- filter(all_courses, term1 == sem | term2 == sem | term3 == sem)
  current_term[current_term$code %in% required,]

}
