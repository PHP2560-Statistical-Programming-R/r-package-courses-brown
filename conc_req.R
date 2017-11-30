library(rvest)
library(dplyr)
library(stringr)


# If the table doesn't appear, then the concentration doesn't list the requirements, refer to...
# Make input case insensitive, look for similar words


# Compile a list of undergraduate concentrations available at Brown from the website, so 
# that if the concentrations are updated on the website, the list is also updated
link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
conc.list <- link %>% 
  html_nodes("#textcontainer li") %>%
  html_text()

conc.list <- data_frame(conc.list)



### conc.re function
# This function takes the concentration of interest as an input and returns a 
# table with all the required classes for that particular concentration 
conc.req <- function(concentration_name) {
  if (grepl(concentration_name, conc.list, ignore.case=TRUE)==TRUE) {
    for (i in 1:length(conc.list)){
      conc_of_interest <- conc.list[i]
    # Pull up the website that has a list of all the undergraduate concentrations
    link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
    # Select the concentration of interest
    link_conc <- link %>% follow_link(conc_of_interest)
    # Read the content of the link
    content <- read_html(link_conc)
    # Scrape the table
    link_table <- html_nodes(content, 'table')
    scrape_table <- html_table(link_table)[[1]]
    # Convert the table into a dataframe  
    classes <- scrape_table$X1
    description <- scrape_table$X2
    number_classes <- scrape_table$X3
    # Return the table
    return(data_frame(classes, description, number_classes))
    }
  }else stop('Please enter a valid concentration name. Refer to the list of undergraduate concentrations offered at Brown at https://bulletin.brown.edu/the-college/concentrations/')
}
  
table_req <- conc.req("music") 



#if (str_detect(concentration_name, pattern= ) == TRUE) {





conc.req <- function(concentration_name) {
  # Pull up the website that has a list of all the undergraduate concentrations
  link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
  # Select the concentration of interest
  link_conc <- link %>% follow_link(concentration_name,2)
  # Read the content of the link
  content <- read_html(link_conc)
  # Scrape the table
  link_table <- html_nodes(content, 'table')
  scrape_table <- html_table(link_table)[[1]]
  # Convert the table into a dataframe  
  classes <- scrape_table$X1
  description <- scrape_table$X2
  number_classes <- scrape_table$X3
  # Return the table
  return(data_frame(classes, description, number_classes))
}

<<<<<<< HEAD
table_req <- conc.req("Economics")

=======
table_req <- conc.req("Music")
 
>>>>>>> a509921cc68afd14b63ff3e37201ea63f6575562
