#' conc_req Function
#'
#' This function takes the concentration of interest as an input and returns a
#' table with all the required classes for that particular concentration.
#' @param conc.name:concentration name as it appears in the list of Brown concentrations in the
#' website: "https://bulletin.brown.edu/the-college/concentrations/". The input is not case
#' sensitive. **conc.name is a character string, so it needs to go with "".
#' @return Table of all the required courses for the concentration inputed.
#' @examples
#' conc_req("Economics")
#' conc_req("mathematics")
#' conc_req("Africana Studies")



conc_req <- function(conc_name) {
  
  `%>%` <- dplyr::`%>%`
  
  # This function takes the concentration of interest as an input and returns a
  # table with all the required classes for that particular concentration. The input is not
  # case sensitive. The function will run only if the input matches the concentration name listed
  # on the website. If the department does not display a table, a message will display this.

  # Compile a list of undergraduate concentrations available at Brown from the website, so
  # that if the concentrations are updated on the website, the list is also updated
  link <- rvest::html_session("https://bulletin.brown.edu/the-college/concentrations/")
  conc_list <- link %>%
    rvest::html_nodes("#textcontainer li") %>% # css selector for the entire list of concentration
    rvest::html_text() # select only the text
  conc_list <- as.vector(conc_list)


  ## Run the function only if the user's input matches the name listed in the concentration
  ## list, ignoring cases.
  match <- grepl(pattern=paste("^", conc_name,"$", sep=""), conc_list, ignore.case=TRUE)
  # Index to find the link of the concentraton of interest
  i <- grep(pattern=paste("^", conc_name,"$", sep=""), conc_list, ignore.case=TRUE)
  if (any(match==TRUE) && stringr::str_length(conc_name) == stringr::str_length(conc_list[i])) {
    # Pull up the website that has a list of all the undergraduate concentrations
    link <- rvest::html_session("https://bulletin.brown.edu/the-college/concentrations/")
    # Select the concentration of interest
    link_conc <- link %>% rvest::follow_link(i+36)
    # Read the content of the link
    content <- xml2::read_html(link_conc)
    # Scrape the table
    link_table <- rvest::html_nodes(content, 'table')
    # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will
    # ignore this error and allow the function to keep working
    scrape_table <- tryCatch(rvest::html_table(link_table)[[1]], error=function(e) matrix(nrow=2, ncol=2))

    # Create a table only if the table exists (i.e. if scrape table != NA)
    if (is.na(scrape_table[1,1]) == FALSE) {
      # Convert the table into a dataframe
      Class <- scrape_table$X1
      Names <- scrape_table$X2
      Credit <- scrape_table$X3
      tab <- dplyr::data_frame(Class, Names, Credit)
      return(tab)
    } else {stop('This department does not have a table of requirements')}


  } else {stop('Please enter a valid concentration name. Refer to the list of undergraduate concentrations offered at Brown at https://bulletin.brown.edu/the-college/concentrations/')}
}
