library(shiny)

# Compile a list of undergraduate concentrations available at Brown from the website, so 
# that if the concentrations are updated on the website, the list is also updated
library(rvest)
link <- html_session("https://bulletin.brown.edu/the-college/concentrations/") 
conc_list <- link %>% 
  html_nodes("#textcontainer li") %>% # css selector for the entire list of concentration
  html_text() # select only the text 
conc_list <- as.vector(conc_list)
names(conc_list) <- conc_list

ui <- navbarPage("BDegreeAdvisor",
                 tabPanel("Concentration Requirement",
                          sidebarLayout(
                            sidebarPanel(
                              # Select concentration
                              selectInput("selected_conc", label = h5("Select a concentration"), 
                                          choices = names(conc_list)), 
                              # Submit button
                              actionButton("submit", label = h5("See table of requirements!"))
                            ),
                            mainPanel(
                              # Display table of concentration requirements 
                              tableOutput("conc_table")
                              )
                            )
                          ),
                 tabPanel("Compare concentration requirements",  sidebarLayout(
                   sidebarPanel(
                     # Select concentration
                     selectInput("selected_conc1", label = h5("Select concentration 1"), 
                                 choices = names(conc_list)), 
                     selectInput("selected_conc2", label = h5("Select concentration 2"), 
                                 choices = names(conc_list)), 
                     # Submit button
                     actionButton("submit2", label = h5("See table of requirements!"))
                     
                   ),
                   mainPanel(
                     # Display table of concentration requirements 
                     tableOutput("conc_comp")
                   )
                 )
                 ),
                 tabPanel("Classes availabe per semester"),
                 tabPanel("Concentration demographics")
)

## Code used to change values of choices in switch function
#numbering <- vector(list, length=length(conc_list))
#for (i in 1:length(conc_list)) {
  #numbering[i] <- paste0(conc_list[[i]], " = ", i)
#}

server <- function(input, output) {
  
  ################################################# Tab 1   #################################################
  conc_name <- eventReactive(input$submit, {
    switch(input$selected_conc,
           "Africana Studies" = 1, 
           "American Studies" = 2, 
           "Anthropology" = 3, 
           "Applied Mathematics" = 4,                         
           "Applied Mathematics-Biology" = 5,
           "Applied Mathematics-Computer Science" = 6,          
           "Applied Mathematics-Economics" = 7,
           "Archaeology and the Ancient World" = 8,  
           "Architecture" = 9,
           "Astronomy" = 10, 
           "Behavioral Decision Sciences" = 11, 
           "Biochemistry & Molecular Biology" = 12, 
           "Biology" = 13, 
           "Biomedical Engineering" = 14, 
           "Biophysics" = 15, 
           "Business, Entrepreneurship and Organizations" = 16,
           "Chemical Physics" = 17,                              
           "Chemistry" = 18,                                    
           "Classics" = 19,                                      
           "Cognitive Neuroscience" = 20,                     
           "Cognitive Science" = 21,                          
           "Comparative Literature" = 22,                       
           "Computational Biology" = 23,                         
           "Computer Science" = 24,                            
           "Computer Science-Economics" = 25,                    
           "Contemplative Studies" = 26,                        
           "Development Studies" = 27,                           
           "East Asian Studies" = 28,                           
           "Economics" = 29,                                     
           "Education Studies" = 30,                            
           "Egyptology and Assyriology" = 31,                    
           "Engineering" = 32,                                  
           "Engineering and Physics" = 33,                      
           "English" = 34,                                     
           "Environmental Studies" = 35,                        
           "Ethnic Studies" = 36,                               
           "French and Francophone Studies" = 37,                
           "Gender and Sexuality Studies" = 38,                 
           "Geological Sciences" = 39, 
           "Geology-Biology" = 40, 
           "Geology-Chemistry" = 41, 
           "Geology-Physics/Mathematics" = 42, 
           "German Studies" = 43, 
           "Health & Human Biology" = 44, 
           "Hispanic Literatures and Culture" = 45, 
           "History" = 46, 
           "History of Art and Architecture" = 47, 
           "Independent Concentration" = 48, 
           "International Relations" = 49, 
           "Italian Studies" = 50,                              
           "Judaic Studies" = 51, 
           "Latin American and Caribbean Studies" = 52, 
           "Linguistics" = 53,                                   
           "Literary Arts" = 54,                                
           "Mathematics" = 55,
           "Mathematics-Computer Science" = 56,                 
           "Mathematics-Economics" = 57, 
           "Medieval Cultures" = 58,                            
           "Middle East Studies" = 59, 
           "Modern Culture and Media" = 60, 
           "Music" = 61, 
           "Neuroscience" = 62, 
           "Philosophy" = 63, 
           "Physics" = 64, 
           "Physics and Philosophy" = 65, 
           "Political Science" = 66,                            
           "Portuguese and Brazilian Studies" = 67, 
           "Psychology" = 68,                                   
           "Public Health" = 69, 
           "Public Policy" = 70,                                
           "Religious Studies" = 71,
           "Renaissance and Early Modern Studies" = 72, 
           "Science and Society" = 73, 
           "Slavic Studies" = 74,                               
           "Social Analysis and Research" = 75,                  
           "Sociology" = 76, 
           "South Asian Studies" = 77, 
           "Statistics" = 78,                                   
           "Theatre Arts and Performance Studies" = 79, 
           "Urban Studies" = 80, 
           "Visual Art" = 81)                                  
  }, ignoreNULL = FALSE) 
  
  output$conc_table <- renderTable({
    library(rvest)
    library(dplyr)
    # Pull up the website that has a list of all the undergraduate concentrations
    link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
    # Select the concentration of interest
    link_conc <- link %>% follow_link(conc_name()+36)      
    # Read the content of the link
    content <- read_html(link_conc)
    # Scrape the table
    link_table <- html_nodes(content, 'table')
    # If the department doesn't display a table, an error "subscript out of bounds" appears. tryCatch will 
    # ignore this error and allow the function to keep working
    scrape_table <- tryCatch(html_table(link_table)[[1]], error=function(e) print(NA))
    # Create a table only if the table exists (i.e. if scrape table ??? NA)
    if (is.na(scrape_table) == FALSE) {
      # Convert the table into a dataframe  
      classes <- scrape_table$X1
      class_name <- scrape_table$X2
      number_classes <- scrape_table$X3
      
      table_req1 <- data_frame(classes, class_name, number_classes)
      table_req1$number_classes[table_req1$number_classes == ""] <- " "
      
      explain <- list("", "", "If the Class Number cell is empty or has a NA, refer to the category the class belongs to.")
      table_req2 <- rbind(table_req1, explain)
      table_req1re<- rename(table_req2, "Class Code" = classes, "Class Name" = class_name, "Number of Classes" = number_classes)
    } else {stop('This department does not have a table of requirements')}
  })
  
 
  ################################################# Tab 2   ################################################# 
  conc_name1 <- eventReactive(input$submit2, {
    switch(input$selected_conc1,
           "Africana Studies" = 1, 
           "American Studies" = 2, 
           "Anthropology" = 3, 
           "Applied Mathematics" = 4,                         
           "Applied Mathematics-Biology" = 5,
           "Applied Mathematics-Computer Science" = 6,          
           "Applied Mathematics-Economics" = 7,
           "Archaeology and the Ancient World" = 8,  
           "Architecture" = 9,
           "Astronomy" = 10, 
           "Behavioral Decision Sciences" = 11, 
           "Biochemistry & Molecular Biology" = 12, 
           "Biology" = 13, 
           "Biomedical Engineering" = 14, 
           "Biophysics" = 15, 
           "Business, Entrepreneurship and Organizations" = 16,
           "Chemical Physics" = 17,                              
           "Chemistry" = 18,                                    
           "Classics" = 19,                                      
           "Cognitive Neuroscience" = 20,                     
           "Cognitive Science" = 21,                          
           "Comparative Literature" = 22,                       
           "Computational Biology" = 23,                         
           "Computer Science" = 24,                            
           "Computer Science-Economics" = 25,                    
           "Contemplative Studies" = 26,                        
           "Development Studies" = 27,                           
           "East Asian Studies" = 28,                           
           "Economics" = 29,                                     
           "Education Studies" = 30,                            
           "Egyptology and Assyriology" = 31,                    
           "Engineering" = 32,                                  
           "Engineering and Physics" = 33,                      
           "English" = 34,                                     
           "Environmental Studies" = 35,                        
           "Ethnic Studies" = 36,                               
           "French and Francophone Studies" = 37,                
           "Gender and Sexuality Studies" = 38,                 
           "Geological Sciences" = 39, 
           "Geology-Biology" = 40, 
           "Geology-Chemistry" = 41, 
           "Geology-Physics/Mathematics" = 42, 
           "German Studies" = 43, 
           "Health & Human Biology" = 44, 
           "Hispanic Literatures and Culture" = 45, 
           "History" = 46, 
           "History of Art and Architecture" = 47, 
           "Independent Concentration" = 48, 
           "International Relations" = 49, 
           "Italian Studies" = 50,                              
           "Judaic Studies" = 51, 
           "Latin American and Caribbean Studies" = 52, 
           "Linguistics" = 53,                                   
           "Literary Arts" = 54,                                
           "Mathematics" = 55,
           "Mathematics-Computer Science" = 56,                 
           "Mathematics-Economics" = 57, 
           "Medieval Cultures" = 58,                            
           "Middle East Studies" = 59, 
           "Modern Culture and Media" = 60, 
           "Music" = 61, 
           "Neuroscience" = 62, 
           "Philosophy" = 63, 
           "Physics" = 64, 
           "Physics and Philosophy" = 65, 
           "Political Science" = 66,                            
           "Portuguese and Brazilian Studies" = 67, 
           "Psychology" = 68,                                   
           "Public Health" = 69, 
           "Public Policy" = 70,                                
           "Religious Studies" = 71,
           "Renaissance and Early Modern Studies" = 72, 
           "Science and Society" = 73, 
           "Slavic Studies" = 74,                               
           "Social Analysis and Research" = 75,                  
           "Sociology" = 76, 
           "South Asian Studies" = 77, 
           "Statistics" = 78,                                   
           "Theatre Arts and Performance Studies" = 79, 
           "Urban Studies" = 80, 
           "Visual Art" = 81)                                  
  }, ignoreNULL = FALSE)  
  
  conc_name2 <- eventReactive(input$submit2, {
    switch(input$selected_conc2,
           "Africana Studies" = 1, 
           "American Studies" = 2, 
           "Anthropology" = 3, 
           "Applied Mathematics" = 4,                         
           "Applied Mathematics-Biology" = 5,
           "Applied Mathematics-Computer Science" = 6,          
           "Applied Mathematics-Economics" = 7,
           "Archaeology and the Ancient World" = 8,  
           "Architecture" = 9,
           "Astronomy" = 10, 
           "Behavioral Decision Sciences" = 11, 
           "Biochemistry & Molecular Biology" = 12, 
           "Biology" = 13, 
           "Biomedical Engineering" = 14, 
           "Biophysics" = 15, 
           "Business, Entrepreneurship and Organizations" = 16,
           "Chemical Physics" = 17,                              
           "Chemistry" = 18,                                    
           "Classics" = 19,                                      
           "Cognitive Neuroscience" = 20,                     
           "Cognitive Science" = 21,                          
           "Comparative Literature" = 22,                       
           "Computational Biology" = 23,                         
           "Computer Science" = 24,                            
           "Computer Science-Economics" = 25,                    
           "Contemplative Studies" = 26,                        
           "Development Studies" = 27,                           
           "East Asian Studies" = 28,                           
           "Economics" = 29,                                     
           "Education Studies" = 30,                            
           "Egyptology and Assyriology" = 31,                    
           "Engineering" = 32,                                  
           "Engineering and Physics" = 33,                      
           "English" = 34,                                     
           "Environmental Studies" = 35,                        
           "Ethnic Studies" = 36,                               
           "French and Francophone Studies" = 37,                
           "Gender and Sexuality Studies" = 38,                 
           "Geological Sciences" = 39, 
           "Geology-Biology" = 40, 
           "Geology-Chemistry" = 41, 
           "Geology-Physics/Mathematics" = 42, 
           "German Studies" = 43, 
           "Health & Human Biology" = 44, 
           "Hispanic Literatures and Culture" = 45, 
           "History" = 46, 
           "History of Art and Architecture" = 47, 
           "Independent Concentration" = 48, 
           "International Relations" = 49, 
           "Italian Studies" = 50,                              
           "Judaic Studies" = 51, 
           "Latin American and Caribbean Studies" = 52, 
           "Linguistics" = 53,                                   
           "Literary Arts" = 54,                                
           "Mathematics" = 55,
           "Mathematics-Computer Science" = 56,                 
           "Mathematics-Economics" = 57, 
           "Medieval Cultures" = 58,                            
           "Middle East Studies" = 59, 
           "Modern Culture and Media" = 60, 
           "Music" = 61, 
           "Neuroscience" = 62, 
           "Philosophy" = 63, 
           "Physics" = 64, 
           "Physics and Philosophy" = 65, 
           "Political Science" = 66,                            
           "Portuguese and Brazilian Studies" = 67, 
           "Psychology" = 68,                                   
           "Public Health" = 69, 
           "Public Policy" = 70,                                
           "Religious Studies" = 71,
           "Renaissance and Early Modern Studies" = 72, 
           "Science and Society" = 73, 
           "Slavic Studies" = 74,                               
           "Social Analysis and Research" = 75,                  
           "Sociology" = 76, 
           "South Asian Studies" = 77, 
           "Statistics" = 78,                                   
           "Theatre Arts and Performance Studies" = 79, 
           "Urban Studies" = 80, 
           "Visual Art" = 81)                                  
  }, ignoreNULL = FALSE)   
  
 
  
  output$conc_comp <- renderTable({
      library(rvest)
      library(dplyr)
      # Pull up the website that has a list of all the undergraduate concentrations
      link <- html_session("https://bulletin.brown.edu/the-college/concentrations/")
      # Select the concentration of interest
      link_conc1 <- link %>% follow_link(conc_name1()+36)
      link_conc2 <- link %>% follow_link(conc_name2()+36)
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
      # Create a table only if the table exists (i.e. if scrape table ??? NA)
      if ((is.na(scrape_table1)== FALSE) && ( is.na(scrape_table2) == FALSE)) {
        # Convert the table into a dataframe  
        
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
        
      } else {stop('One of the concentrations does not have a table presented')}
  })  
  
}


shinyApp(ui=ui, server=server)  