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
                              selectInput("conc_name", label = h3("Select a concentration"), 
                                          choices = names(conc_list))
                
                            ),
                            mainPanel(
                              # Display a character string with the calculated p-value
                              textOutput("pvalue"), 
                              # Add a horizontal line to separate outputs
                              tags$hr(),
                              # Plot of calculated p-value
                              plotOutput("stdNplot")
                              )
                            )
                          ),
                 tabPanel("Compare concentration requirements"),
                 tabPanel("Classes availabe per semester"),
                 tabPanel("Concentration demographics")
)


server <- function(input, output) {

}


shinyApp(ui=ui, server=server)  