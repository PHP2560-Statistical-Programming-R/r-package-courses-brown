# Load packages
library(dplyr)
library(rvest)
library(XML)
library(httr)

job_finder<-function(query,loc){
  
  # the name of the job you want
  #query="market research analyst"
  # where you want to work
  #loc="New Jersey"
  # we want to search for jobs on indeed.com
  session<-html_session("https://www.indeed.com")
  
  # we found the location of the form
  form<-html_form(session)[[1]]
  # set the values for the form
  form<-set_values(form,q=query,l=loc)
  # submit the information so that R can search the website from 
  session1<-submit_form(session,form)
  
  
  # Getting the name of the job
  jobtitles<-session1 %>%
    html_nodes(css=".jobtitle") %>%
    html_text()
  
  
  # Scrape Salary information-  Can't get it to work
  salary<-session1 %>%
    html_nodes(css= "#resultsCol li:nth-child(2) a") %>%
    html_attr("href")
  salary<-paste(session$url,salary,sep='')
  
  salaries<-lapply(salary, . %>%
                     read_html() %>%
                     html_nodes(".cmp-sal-salary") %>%
                     html_text())
  salary_final<-unlist(salaries)
  
  
  # Now I want to pull the job titles for each search ( alternative way of doing it, both run)
  job_titles<-session1 %>%
    # grab the titles of the jobs
    html_nodes("[data-tn-element=jobTitle]") %>%
    html_text() 
  
  # The name of the Organization that is hiring
  company_names<-session1 %>%
    html_nodes(".company") %>%
    html_text()
  
  # Where is the job located ?
  location<-session1 %>%
    html_nodes(".location") %>%
    html_text()
  # Job Description
  description <- session1 %>%
    html_nodes(".summary") %>%
    html_text()
  
  # Give the user a link to the job so that they can apply to it later
  job_link<- session1%>%
    html_nodes(css= ".jobtitle") %>%
    html_attr("href")
  job_link<-paste(session$url,job_link,sep='')
  
  #job_link<-paste('[Link](https://www.indeed.com',job_link,sep = '')
  #job_link<-paste(job_link, ')', sep='')
  
  
  
  
  
  # Pulling it all together !
  indeed_job_compiled<-data.frame(company_names,jobtitles,description,location,job_link)
  library(knitr)
  print(kable(indeed_job_compiled,format="html"))
  
}
