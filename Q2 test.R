class <- c("Applied Mathematics", "Economics", "Data", "sdfdata","Chemistry")  

conc.aaa <- function(ori){
  real_name <- paste("^", ori,"$", sep="")
  
  for (i in 1:length(class)){
    
    if (str_detect(class[i], regex(real_name, ignore_case = T)) == TRUE){
      t <- class[i]
      link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
      link_conc <- link %>% follow_link(41)
      content <- read_html(link_conc)
      link_table <- html_nodes(content, 'table')
      scrape_table <- html_table(link_table)[[1]]
      classes <- scrape_table$X1
      description <- scrape_table$X2
      number_classes <- scrape_table$X3
      return(data_frame(classes, description, number_classes))
    }
  }
}
