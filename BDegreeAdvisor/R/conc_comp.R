#' conc_comp Function
#'
#' This function takes two concentrations of interest as inputs and returns a 
#' table with all the required classes for both concentrations.
#' @param conc.name1: a character string for the first concentration name as it appears in the 
#' list of Brown concentrations in the website: 
#' "https://bulletin.brown.edu/the-college/concentrations/". The input is not case sensitive. 
#' **conc.name is a character string, so it needs to go with "". 
#' @param conc.name2: a character string for the second concentration name as it appears in the 
#' list of Brown concentrations in the website: 
#' "https://bulletin.brown.edu/the-college/concentrations/". The input is not case sensitive. 
#' **conc.name is a character string, so it needs to go with "". 
#' @return Table of all the required courses for the concentration inputed.
#' @examples
#' conc_comp("Economics", "Mathematics")
#' conc_comp("mathematics", "Africana Studies")


conc_comp <- function(conc_name1, conc_name2) {
  
  # This function takes the concentration of interest as an input and returns a 
  # table with all the required classes for that particular concentration. The input is not 
  # case sensitive. The function will run only if the input matches the concentration name listed 
  # on the website. If the department does not display a table, a message will display this. 
  
  
  # Compile a list of undergraduate concentrations available at Brown from the website, so 
  # that if the concentrations are updated on the website, the list is also updated
  link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
  conc_list <- link %>% 
    html_nodes("#textcontainer li") %>% # css selector for the entire list of concentration
    html_text() # select only the text 
  conc_list <- as.vector(conc_list)
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
    
    scrape_table1 <- tryCatch(html_table(link_table1)[[1]], error=function(e)  matrix(nrow=2, ncol=2))
    scrape_table2 <- tryCatch(html_table(link_table2)[[1]], error=function(e)  matrix(nrow=2, ncol=2))
    
    # Create a table only if the table exists (i.e. if scrape table ≠ NA)
    if ((is.na(scrape_table1[1,1])== FALSE) && (is.na(scrape_table2[1,1]) == FALSE)) {
      # Convert the table into a dataframe  
      
<<<<<<< HEAD
      classes <- scrape_table1$X1
      class_name <- scrape_table1$X2
      number_classes <- scrape_table1$X3
      
      table_req1 <- data_frame(classes, class_name, number_classes)
      table_req1$number_classes[table_req1$number_classes == ""] <- " "
      
      space1 <- list("-", "-", "-")
      space2 <- list("-", "-", "-")
      name1 <- list("Concentration 1: ", "", "")
      name2 <- list("Concentration 2: ", "", "")
      
      table_req1s <- rbind(name1, table_req1)
      table_req1s <- rbind(table_req1s, space1)
      table_req1s <- rbind(table_req1s,space2)
      
      
      classes <- scrape_table2$X1
      class_name <- scrape_table2$X2
      number_classes <- scrape_table2$X3
      table_req2 <- data_frame(classes, class_name, number_classes)
      table_req2 <- rbind(name2, table_req2)
      
      total <- rbind(table_req1s, table_req2)
      
      
      total$number_classes[total$number_classes == ""] <- " "
      
      table_req_2<-rename(total, "Class Code" = classes, "Class Name" = class_name, "Number of Classes" = number_classes)
      
      
      explain <- list("", "", "If the Class Number cell is empty or has a NA, refer to the category the class belongs to.")
      rbind(table_req_2, explain)
=======
      Course <- scrape_table2$X1
      Title <- scrape_table2$X2
      Credit <- scrape_table2$X3
      table_req2 <- data_frame(Course, Title, Credit)
  
      total <- rbind(table_req1, table_req2)
      return(total)
>>>>>>> c6cdbda180167fc776e9f7bdc2b8921f332d0489
    } else {stop('One of the concentrations does not have a table presented')}
  } else {stop('Please enter valid concentration names. Refer to the list of undergraduate concentrations offered at Brown at https://bulletin.brown.edu/the-college/concentrations/')}
}  

conc_comp("Economics", "Africana Studies")
