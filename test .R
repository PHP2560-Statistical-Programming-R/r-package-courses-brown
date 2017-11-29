install.packages("rvest")
library(rvest)

conc.req <- function(concentration_name) {
  # This function takes the concentration of interest as an input and returns a 
  # table with all the required classes for that particular concentration 
  
  # Pull up the website that has a list of all the undergraduate concentrations
  link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
  # Select the concentration of interest
  link_conc <- link %>% follow_link("concentration_name")
  # Read the content of the link
  content <- read_html(link_conc)
  # Scrape the table
  link_table <- html_nodes(content, 'table')
  scrape_table <- html_table(link_table)[[1]]
  # Convert the table into a dataframe  
  classes <- scrape_table$X1
  description <- scrape_table$X2
  conc_table <- data.frame(classes, description)
  # Return the table
  return(conc_table)
}

conc.req("Anthropology")
