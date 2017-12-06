library(rvest)
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

conc.req2 <- function(conc_name1, conc_name2) {
  ## Run the function only if the user's input matches the name listed in the concentration 
  ## list, ignoring cases. 
  match1 <- grepl(pattern=paste("^", conc_name1,"$", sep=""), conc_list, ignore.case=TRUE)
  match2 <- grepl(pattern=paste("^", conc_name2,"$", sep=""), conc_list, ignore.case=TRUE)
  # Index to find the link of the concentraton of interest
  i <- grep(pattern=paste("^", conc_name1,"$", sep=""), conc_list, ignore.case=TRUE)
  b <- grep(pattern=paste("^", conc_name2,"$", sep=""), conc_list, ignore.case=TRUE)
  
  if (any(match1==TRUE) && str_length(conc_name1) == str_length(conc_list[i]) &&
      any(match2==TRUE) && str_length(conc_name2) == str_length(conc_list[b])) {
    # Pull up the website that has a list of all the undergraduate concentrations
    link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
    # Select the concentration of interest
    link_conc1 <- link %>% follow_link(i+36)
    link_conc2 <- link %>% follow_link(b+36)
    
    # Read the content of the link
    content1 <- read_html(link_conc1)
    content2 <- read_html(link_conc2)
    
    # Scrape the table
    link_table1 <- html_nodes(content1, 'table')
    link_table2 <- html_nodes(content2, 'table')
    
    # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will 
    # ignore this error and allow the function to keep working
    
    
    scrape_table1 <- tryCatch(html_table(link_table1)[[1]], error=function(e) print(NA))
    scrape_table2 <- tryCatch(html_table(link_table2)[[1]], error=function(e) print(NA))
    
    # Create a table only if the table exists (i.e. if scrape table ≠ NA)
    if ((is.na(scrape_table1)== FALSE) && ( is.na(scrape_table2) == FALSE)) {
      # Convert the table into a dataframe  
      Course <- scrape_table1$X1
      Title <- scrape_table1$X2
      Credit <- scrape_table1$X3
      table_req1 <- data_frame(Course, Title, Credit)
      
      Course <- scrape_table2$X1
      Title <- scrape_table2$X2
      Credit <- scrape_table2$X3
      table_req2 <- data_frame(Course, Title, Credit)
      
      
    
      total <- rbind(table_req1, table_req2)
      
    } else {stop('One of the concentrations does not have a table presented')}
  } else {stop('Please enter valid concentration names. Refer to the list of undergraduate concentrations offered at Brown at https://bulletin.brown.edu/the-college/concentrations/')}
}  

table_req <- conc.req2("Chemistry", "Economics") 






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
