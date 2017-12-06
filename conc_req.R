library(rvest)
library(dplyr)
library(stringr)


# Compile a list of undergraduate concentrations available at Brown from the website, so 
# that if the concentrations are updated on the website, the list is also updated
link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
conc_list <- link %>% 
  html_nodes("#textcontainer li") %>% # css selector for the entire list of concentration
  html_text() # select only the text 
conc_list <- as.vector(conc_list)

### conc.re function

# This function takes the concentration of interest as an input and returns a 
# table with all the required classes for that particular concentration. The input is not 
# case sensitive. The function will run only if the input matches the concentration name listed 
# on the website. If the department does not display a table, a message will display this. 

conc.req <- function(conc_name) {
  ## Run the function only if the user's input matches the name listed in the concentration 
  ## list, ignoring cases. 
  match <- grepl(pattern=paste("^", conc_name,"$", sep=""), conc_list, ignore.case=TRUE)
  # Index to find the link of the concentraton of interest
  i <- grep(pattern=paste("^", conc_name,"$", sep=""), conc_list, ignore.case=TRUE)
  if (any(match==TRUE) && str_length(conc_name) == str_length(conc_list[i])) {
    # Pull up the website that has a list of all the undergraduate concentrations
    link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
    # Select the concentration of interest
    link_conc <- link %>% follow_link(i+36)
    # Read the content of the link
    content <- read_html(link_conc)
    # Scrape the table
    link_table <- html_nodes(content, 'table')
    # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will 
    # ignore this error and allow the function to keep working
    scrape_table <- tryCatch(html_table(link_table)[[1]], error=function(e) print(NA))
    # Create a table only if the table exists (i.e. if scrape table ≠ NA)
    if (is.na(scrape_table) == FALSE) {
      # Convert the table into a dataframe  
      Course <- scrape_table$X1
      Title <- scrape_table$X2
      Credit <- scrape_table$X3
      test2 <- data_frame(Course, Title, Credit)
    } else {stop('This department does not have a table of requirements')}
  } else {stop('Please enter a valid concentration name. Refer to the list of undergraduate concentrations offered at Brown at https://bulletin.brown.edu/the-college/concentrations/')}
}  

table_req <- conc.req("Africana Studies") 






####Test
link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
conc_list <- link %>% 
  html_nodes("#textcontainer li") %>% # css selector for the entire list of concentration
  html_text() # select only the text 

conc_list <- as.vector(conc_list)



if (str_length("africana studies") == str_length("africana studies")) {
  2+2
}

concentration_name <- "economics"
match <- grepl(pattern=paste("^", concentration_name,"$", sep=""), conc_list, ignore.case=TRUE)
# Index to find the link of the concentraton of interest (line 32)
i <- grep(pattern=paste("^", concentration_name,"$", sep=""), conc_list, ignore.case=TRUE)
if (any(match == TRUE) & str_length(concentration_name) == str_length(conc_list[29])) {
  a <- 2+2
  }else{print("error")}


  # Index to find the link of the concentraton of interest (line 32)
  i <- grep(pattern=paste("^", concentration_name,"$", sep=""), conc_list, ignore.case=TRUE)
  # Pull up the website that has a list of all the undergraduate concentrations
  link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
  # Select the concentration of interest
  link_conc <- link %>% follow_link(conc_list[i])
  # Read the content of the link
  content <- read_html(link_conc)
  # Scrape the table
  link_table <- html_nodes(content, 'table')
  # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will 
  # ignore this error and allow the function to keep working
  scrape_table <- tryCatch(html_table(link_table)[[1]], error=function(e) print(NA))
  # Create a table only if the table exists (i.e. if scrape table ≠ NA)
  if (is.na(scrape_table) == FALSE) {
    # Convert the table into a dataframe  
    classes <- scrape_table$X1
    description <- scrape_table$X2
    number_classes <- scrape_table$X3
    # Return the table
    return(data_frame(classes, description, number_classes))
    # If the input does not match the name listed in the concentration, stop and show message
     } else {stop('This department does not have a table of requirements')}
  } else {stop('Please enter a valid concentration name. Refer to the list of undergraduate concentrations offered at Brown at https://bulletin.brown.edu/the-college/concentrations/')}




 
