library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

sch.req <- function(conc_name, term = c("fall", "spring", "winter")){
  # read in json and store info to variable
  url <- "https://cab.brown.edu/asoc-api/?output=json&page=asoc.rjs&route=search&term=999999"
  all_courses <- fromJSON(url, flatten=TRUE)[["courses"]]
  all_courses <- all_courses[, c(1:3, 8)]
  
  # get required courses for concentration
  required <- conc.req(conc_name)
  required <- unlist(str_match_all(required[, 1:2],"[A-Z]{3,4}[:space:][0-9]{4}[A-Z]*"))
  required <- as.character(required)
  
  # split up course code into department, course code, and letter
  required <- data.frame(code = required, 
                         subj = str_extract(required, "^[A-Z]{3,4}"),
                         course_code = as.numeric(str_extract(required, "[0-9]{4}")), 
                         letter = str_extract(required, "[A-Z]*$"),
                         stringsAsFactors = FALSE)
  
  # make columns for up to three possible terms listed that the course runs in 
  all_courses <- mutate(all_courses, term1 = as.numeric(sapply(all_courses$terms, function(x) x[1])), 
         term2 = as.numeric(sapply(all_courses$terms, function(x) x[2])), 
         term3 = as.numeric(sapply(all_courses$terms, function(x) x[3])))
  # remove unneeded column
  all_courses <- all_courses[,-4]
  
  # set term number
  if(term == "fall"){
    sem <- 201710
  } else if (term == "spring") {
    sem <- 201720
  } else if (term == "winter") {
    sem <- 201715
  }

  # filter courses by selected semester
  current_term <- filter(all_courses, term1 == sem | term2 == sem | term3 == sem)  
  # split course code into subject, course code, and letter
  current_term <- filter(current_term, !str_detect(current_term$code, 'XLIST'))
  current_term <- mutate(current_term, course_code = str_extract(current_term$code, "[0-9]{4}"))
  current_term <- mutate(current_term, letter = str_extract(current_term$code, "[A-Z]*$"))
  # change type of course code from chr to num
  current_term$course_code <- as.numeric(current_term$course_code)
  
  #filter for rows in current term courses that are in the respective fields of required courses
  filter(current_term, subj %in% required$subj &
           course_code %in% required$course_code &
           # subset only course code and title
           letter %in% required$letter)[,1:2] %>%
  return()
}
