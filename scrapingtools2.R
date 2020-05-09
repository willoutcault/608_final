detailscrape <- function(job,city){
  
  time2 = proc.time()
  
  #### Initialize df
  new_df <- data.frame(matrix(ncol = 6, nrow = 0))
  x <- c("Query", "Title", "Details", "Salary", "Skills", "urls")
  colnames(new_df) <- x
  
  append_df <- data.frame(matrix(ncol = 6, nrow = 0))
  x <- c("Query", "Title", "Details", "Salary", "Skills", "urls")
  colnames(append_df) <- x
  
  search_job <- gsub(" ", "+", job)
  search_loc <- sub(",", "", city)
  search_loc <- gsub(" ", "+", search_loc)
  
  #### Reading landingpage
  pages <- list()
 
  #### Counting Loops
  main_page <- html_session(paste("https://www.careerbuilder.com/jobs?keywords=",search_job,"&location=",search_loc, sep = ""))
  main_page <- read_html(main_page)
  count <- main_page %>% 
    html_nodes("div#job-count.col.b.dark-blue-text") %>% 
    html_text()
  count <- as.numeric(as.character(str_remove_all(count, "[^[:digit:]]")))
  loop_count <- round(count/25)
  if (loop_count < 8){
    loop_count <- 8
  }
  
  for (i in 1:loop_count){
    page <- paste("https://www.careerbuilder.com/jobs?keywords=",search_job,"&location=",search_loc,"&page_number=",i, sep = "")
    pages <- append(pages, page)
  }
  
  cluster = makeCluster(2, type = "SOCK")
  registerDoSNOW(cluster)
  read_page_url <- function(pages, job){
    
    details <- list()
    titles <- list()
    salaries <- list()
    urls <- list()
    
    append_df <- data.frame(matrix(ncol = 6, nrow = 0))
    x <- c("Query", "Title", "Details", "Salary", "Skills", "urls")
    colnames(append_df) <- x
    
    library(rvest)
    
    page_url <- read_html(html_session(pages))
    
    ##### URLS
    url <- page_url %>% 
      html_nodes("a.data-results-content.block.job-listing-item") %>% 
      html_attr("href")
    urls <- append(urls, url)
    
    ##### Titles
    title <- page_url %>% 
      html_nodes("div#jobs_collection div.data-results-title.dark-blue-text.b") %>% 
      html_text()
    titles <- append(titles, title)
    
    ##### Details
    detail <- page_url %>% 
      html_nodes("div#jobs_collection div.data-details") %>% 
      html_text()
    details <- append(details, detail)
    
    ##### Salary
    salary <- page_url %>% 
      html_nodes("div#jobs_collection div.data-snapshot div.block:last-child") %>%
      html_text()
    salaries <- append(salaries, salary)
    
    for (i in 1:length(urls)){
      append_df[i,1] <- job
      append_df[i,2] <- titles[[i]]
      append_df[i,3] <- details[[i]]
      append_df[i,4] <- salaries[[i]]
      append_df[i,5] <- ""
      append_df[i,6] <- paste("https://www.careerbuilder.com", urls[[i]], sep = "")
    }
    
    return(append_df)
    
  }
  new_df <- foreach(i = 1:length(pages)) %dopar% read_page_url(pages[[i]],job)
  stopCluster(cluster)
  
  new_df <- do.call("rbind", new_df)
  
  cluster = makeCluster(2, type = "SOCK")
  registerDoSNOW(cluster)
  read_posting_url <- function(df_urls){
    library(rvest)
    page_url <- read_html(html_session(df_urls))
    skills <- list(page_url %>% 
                     html_nodes(".check-bubble") %>% 
                     html_text())
    return(list(skills))    
  }
  ifelse(loop_count > 2, job_posting_count <- 50, job_posting_count <- length(new_df$urls))
  skills_list <- foreach(i = 1:job_posting_count) %dopar% try(read_posting_url(new_df$urls[i]))
  stopCluster(cluster)
  
  for (i in 1:length(skills_list)){
    new_df[i,5] <- skills_list[[i]]
  }
  
  new_df$Skills <- vapply(new_df$Skills, paste, collapse = ", ", character(1L))
  
  new_df$Salary <- str_remove_all(new_df$Salary, "\\$")
  
  new_df$Salary <- str_remove_all(new_df$Salary, ",")
  
  new_df$Salary <- str_remove_all(new_df$Salary, "\\.[:digit:][:digit:]")
  
  
  range <- str_detect(new_df$Salary, "-")
  
  hour <- str_detect(new_df$Salary, "hour")
  
  year <- str_detect(new_df$Salary, "year")
  
  
  mins <- as.numeric(str_extract(new_df$Salary[range&year], "[:digit:]+"))
  new_df$Salary[range&year] <- str_remove(new_df$Salary[range&year], "[:digit:]+")
  maxs <- as.numeric(str_extract(new_df$Salary[range&year], "[:digit:]+"))
  new_df$Salary[range&year] <- (mins+maxs)/2
  
  mins <- as.numeric(str_extract(new_df$Salary[range&hour], "[:digit:]+"))
  new_df$Salary[range&hour] <- str_remove(new_df$Salary[range&hour], "[:digit:]+")
  maxs <- as.numeric(str_extract(new_df$Salary[range&hour], "[:digit:]+"))
  new_df$Salary[range&hour] <- ((mins+maxs)/2)*40*52
  
  
  range <- str_detect(new_df$Salary, "-")
  
  hour <- str_detect(new_df$Salary, "hour")
  
  year <- str_detect(new_df$Salary, "year")
  
  
  new_df$Salary[hour] <- as.numeric(as.character(str_remove_all(new_df$Salary[hour], "[^[:digit:]]")))*52*40
  
  new_df$Salary[year] <- as.numeric(as.character(str_remove_all(new_df$Salary[year], "[^[:digit:]]")))
  
  new_df$Salary <- as.numeric(as.character(str_remove_all(new_df$Salary, "[^[:digit:]]")))
  
  #### DETAILS
  
  new_df <- separate(new_df, Details, c("remove", "Company", "Location", "JobType", "alsoremove"), "\n", fill="right")
  
  drops <- c("remove","alsoremove")
  
  new_df <- new_df[ , !(names(new_df) %in% drops)]
  
  print(proc.time()-time2)
  
  return(new_df)
}

count_skills <- function(df){
  df2 <- separate(df, Skills, c("S1", "S2", "S3", "S4", "S5", "S6"), sep=", ", fill="right")
  skills <- select(df2,S1,S2,S3,S4,S5,S6,Query)
  skills <- gather(skills, "query", "skills", -Query)
  skills <- filter(skills, complete.cases(skills))
  skills <- skills %>% 
    count(skills) %>% 
    filter(skills!="" & !is.na(skills))
  skills <- skills[with(skills,order(-n)),]
  #    skills <- skills[1:10,]
  return(skills)
}

leaflet_points <- function(df,df2, coords){
  
  skills_df <- count_skills_by_loc(df)
  
  locations_df <- df %>% 
    group_by(Location) %>% 
    summarise(Job_Count = n(), Salary = mean(Salary)) 
  
  locations_df$Skills <- skills_df$Skills[cbind(
    match(locations_df$Location, skills_df$Location))]
  
  locations_df$lat <- coords$lat[cbind(
    match(tolower(locations_df$Location), tolower(coords$Location)))]
  
  locations_df$lng <- coords$lng[cbind(
    match(tolower(locations_df$Location), tolower(coords$Location)))]
  
  locations_df <- filter(locations_df, !is.na(lng))
  
  return(locations_df)
}

count_skills_by_loc <- function(df){
    test <- df %>% 
        separate(Skills, c("S1", "S2", "S3", "S4", "S5", "S6"), sep=", ", fill="right") %>% 
        select(S1,S2,S3,S4,S5,S6,Location) %>% 
        group_by(Location) %>% 
        gather("Count", "Skills", -Location)  %>% 
        group_by(Location, Skills) %>% 
        summarise(n = n()) %>% 
        filter((Skills != "") & (!is.na(Skills))) %>% 
        group_by(Location) %>% 
        arrange(desc(n), Location) %>%
        filter(row_number()<4) %>% 
        group_by(Location) %>%
        summarize(list(Skills))
    
    return(test)
}


count_wages <- function(df){
  dswages <- df %>%
    count(Salary) %>%
    filter(Salary != "null" & Salary != " " & Salary != "")
  dswages$salary <- as.numeric(dswages$Salary)
  outliers <- boxplot(dswages$salary, plot=FALSE)$out
  dswages <- dswages[!dswages$salary %in% outliers,]
  return(dswages)
}

count_location <- function(df){
  Locations <- df %>%
    count(Location) %>%
    filter(!is.na(Location) & Location != " " & Location != "" & Location != "Full Time" & Location != "Contractor")
  Locations <- Locations[with(Locations,order(-n)),]
  return(Locations)
}
