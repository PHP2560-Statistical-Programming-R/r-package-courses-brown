#' indeed Function
#'
#'The purpose of this function is to take the manual work of searching for jobs out of the hand of the user
#'The user can enter the type of job they want and the location they would like to work and the function will
#'pull the most important bits of information about the job and will present them in a compact table for the user
#'to look at all at once.
#'This function takes two arguements. 
#'@param query: The jobtitle or keyword that the user wants to search indeed.com for. The input is not case sensitive
#'**query is a character string and so it needs to be inputted with quotation marks ""
#'@param loc: The location of jobs the user would like to search for. The input is not case sensitive
#'**loc is a character string as well and so the input needs to be encased around quotation marks. ""
#'@return Table of a dataframe that contains the hiring company, the job title, the description, the location, and the job link 
#'for the search results of that specific query
#' @examples 
#' job_finder("data analyst","Providence")
#' job_finder("merchandise planner","07002")
#' job_finder("graphic designer","02912")


job_finder<-function(query,loc){

  `%>%` <- dplyr::`%>%`

# Check to make sure the user entered the query arguement in correctly
  suppressWarnings(
    if(missing(query)){
      stop('Please enter a job title or keyword as a string.', call. = FALSE)
    }
  )
  
# check that the location variable has been inputted
  suppressWarnings(
    if(missing(loc)){
      stop('Please enter a city,state,or zip code as a string.', call. = FALSE)
    }
  )
  
 
# This creates an empty dataframe to store our updating results in
  indeed_job_compiled<-data.frame("Hiring Company"=character(),
                                  "Job Title"=character(),
                                  "Description"=character(),
                                  "Location"=character(),
                                  "Job Link"=character())
                                  
  
# Splitting the url into parts to view 10 pages of search results. I capped the number of pages for size purposes.
  b= seq(from= 10, to= 30, by=10)
  urls=vector(length=length(b)+1)
  
  for (i in 1:length(b)){
    url_part1<-"https://www.indeed.com/jobs?q="
    url_part2<-"&l="
    url_part3<-"&start="  
    
    # paste together a url to webscrape. The url will depend on the user's inputs
    url<-paste0(url_part1,query,url_part2,loc,url_part3,b[i])  
    # Wherever there is a space, replace it with a "+" sign. This is how indeed.com lays out their search pages
    urls[i+1]<-gsub(pattern=" ",replacement = "+",url) 
  }
  # The first page of any query does not have the addition of url_part3, and so we specify this unique url here
  first_page<-paste0(url_part1,query,url_part2,loc)
  urls[1]<-gsub(pattern=" ",replacement = "+",first_page) 
 
# Launch an Html session and web scrape information about each job posting 
 
   for ( j in 1:length(urls)){
  session1 <- rvest::html_session(urls[j])
    
# grab the titles of the jobs
  job_titles <- session1 %>%
      rvest::html_nodes("[data-tn-element=jobTitle]") %>%
      rvest::html_text() 
    
# The name of the Organization that is hiring
    company_names<-session1 %>%
      rvest::html_nodes(".company") %>%
      rvest::html_text()
    
# Where is the job located ?
    location<-session1 %>%
      rvest::html_nodes(".location") %>%
      rvest::html_text()

# Job Description
    description <- session1 %>%
      rvest::html_nodes(".summary") %>%
      rvest::html_text()
    
# Give the user a link to the job so that they can apply to it later
    job_link<- session1%>%
      rvest::html_nodes(css= "[data-tn-element=jobTitle]") %>%
      rvest::html_attr("href")
      job_link<-paste('[Link](https://www.indeed.com',job_link,sep = '')
      job_link<-paste(job_link, ')', sep='')
      
    
  
    
    
    
   
# Pulling it all together !
     # bind together all the results into one final dataframe
    indeed_job_compiled<- rbind(indeed_job_compiled,data.frame(company_names,job_titles,description,location,job_link))
  }
    
 
  print(pander::pandoc.table(indeed_job_compiled))
}
  


 