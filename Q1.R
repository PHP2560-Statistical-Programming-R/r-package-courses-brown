# packages needed
library(rvest)
library(stringr)

# First function
conc.aaa <- function(conc_name){
  real_name <- paste("^", conc_name,"$", sep="")
  
  for (i in 1:length(cars2)){
    #if (grepl(real_name, cars2[i], ignore.case = FALSE) == TRUE) {
    if (str_detect(cars2[i], regex(real_name, ignore_case = T)) == TRUE){
      
      a <- conc.list[i,2]
      b <- as.numeric(a)
      
      link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
      link_conc <- link %>% follow_link(b)
      content <- read_html(link_conc)
      link_table <- html_nodes(content, 'table')
      scrape_table <- html_table(link_table)[[1]]
      classes <- scrape_table$X1
      description <- scrape_table$X2
      number_classes <- scrape_table$X3
      return(data_frame(classes, description, number_classes))
    }else {
    print ("error") 
    }
  }
}

table_req <- print(conc.aaa("Computer Science") )

