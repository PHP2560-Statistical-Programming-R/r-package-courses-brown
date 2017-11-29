library(rvest)

s <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
s <- s %>% follow_link("Anthropology")

webpage <- read_html(s)

sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table)[[1]]
sb

sb$X1
sb$X2
test <- data.frame(sb$X1, sb$X2)
test

