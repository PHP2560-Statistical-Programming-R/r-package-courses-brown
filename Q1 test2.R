library(rvest)
library(dplyr)
library(stringr)


conc.req <- function(conc_name){ 
  # Compile a list of undergraduate concentrations available at Brown from the website 
  # The list will be updated with the website
  link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
  
  conc_list <- link %>% 
    # css selector for the entire list of concentration
    html_nodes("#textcontainer li") %>% 
    # select only the text
    html_text()  
  
  # Create a data of the concentration and their corresponding order on the website
  conc_table <- data.frame (conc_list, re_order = seq.int(37, 117, 1))
  # conc_list$re_order <- seq.int(37, 117, 1)
  # Convert the list into a string of characters
  conc_string <- sapply(conc_table$conc.list, as.factor)
  
  for (i in 1:length(conc_list)){
    # Use regular expression for exact matching
    real_name <- paste("^", conc_name,"$", sep="")
    
    #if (any(grepl(real_name, conc_list[i], ignore.case = FALSE)) = TRUE) {
    if (str_detect(conc_list[i], regex(real_name, ignore_case = T)) == TRUE) {
      # Find the corresponding order on the website
      a <- conc_table[i,2]
      # Convert a into numeric
      b <- as.numeric(a)
      # Use R for web searching
      link_conc <- link %>% follow_link(b)
      content <- read_html(link_conc)
      link_table <- html_nodes(content, 'table')
      scrape_table <- html_table(link_table)[[1]]
      
      # Rename scrape table and make the three lists into a dataframe
      classes <- scrape_table$X1
      description <- scrape_table$X2
      number_classes <- scrape_table$X3
      print(return (data_frame(classes, description, number_classes)))
      }else {
      print ("error") 
    }
}
}


table_req <- print(conc.req("Economics") )










