
  
  s <- html_session("https://bulletin.brown.edu/the-college/concentrations/") %>% 
    follow_link("Music")
  
  webpage <- read_html(s)
  
  sb_table <- html_nodes(webpage, 'table')
  sb <- html_table(sb_table)[[1]]
  
  a <- data.frame(sb$X1, sb$X2)



