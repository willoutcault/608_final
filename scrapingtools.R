skillscrape <- function(job,city){
    
    start_time <- Sys.time()
    
    #### Initialize df
    new_df <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("Query", "Skills")
    colnames(new_df) <- x
    
    append_df <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("Query","Skills")
    colnames(append_df) <- x
    
    search_job <- sub(" ", "+", job)
    search_loc <- sub(",", "", city)
    search_loc <- gsub(" ", "+", search_loc)
    
    #### Reading landingpage
    urls <- list()
    
    #### Counting Loops
    main_page <- read_html(paste("https://www.careerbuilder.com/jobs?keywords=",search_job,"&location=",search_loc, sep = ""))

    ##### URLS
    url <- main_page %>% 
        html_nodes("a.data-results-content.block.job-listing-item") %>% 
        html_attr("href")
    urls <- append(urls, url)
    
    
    #### Append Skills
    for (i in 1:length(urls)){
        job_posting <- read_html(paste("https://www.careerbuilder.com",urls[[i]],sep=""))
        append_df[i,1] <- job
        skills <- list(job_posting %>% 
                           html_nodes(".check-bubble") %>% 
                           html_text()))
        append_df[i,2] <- list(skills)
    }
    closeAllConnections()
    new_df <- rbind(new_df, append_df)
    
    ###### CLEANING
    
    new_df$Skills <- vapply(new_df$Skills, paste, collapse = ", ", character(1L))
    
    end_time <- Sys.time()
    
    print(end_time - start_time)
    
    return(new_df)
}
detailscrape <- function(job,city){
    
    start_time <- Sys.time()
    
    #### Initialize df
    new_df <- data.frame(matrix(ncol = 5, nrow = 0))
    x <- c("Query", "Title", "Details", "Salary", "urls")
    colnames(new_df) <- x
    
    append_df <- data.frame(matrix(ncol = 5, nrow = 0))
    x <- c("Query", "Title", "Details", "Salary", "urls")
    colnames(append_df) <- x
    
    search_job <- gsub(" ", "+", job)
    search_loc <- sub(",", "", city)
    search_loc <- gsub(" ", "+", search_loc)
    
    #### Reading landingpage
    details <- list()
    titles <- list()
    salaries <- list()
    urls <- list()
    
    #### Counting Loops
    main_page <- read_html(paste("https://www.careerbuilder.com/jobs?keywords=",search_job,"&location=",search_loc, sep = ""))
    count <- main_page %>% 
        html_nodes("div#job-count.col.b.dark-blue-text") %>% 
        html_text()
    count <- as.numeric(as.character(str_remove_all(count, "[^[:digit:]]")))
    loop_count <- round(count/25)
    if (loop_count > 10){
        loop_count <- 10
    }
    
    for (i in 1:loop_count){
        
        main_page <- read_html(paste("https://www.careerbuilder.com/jobs?keywords=",search_job,"&location=",search_loc,"&page_number=",i, sep = ""))
        
        ##### URLS
        url <- main_page %>% 
            html_nodes("a.data-results-content.block.job-listing-item") %>% 
            html_attr("href")
        urls <- append(urls, url)
        
        ##### Titles
        title <- main_page %>% 
            html_nodes("div#jobs_collection div.data-results-title.dark-blue-text.b") %>% 
            html_text()
        titles <- append(titles, title)
        
        ##### Details
        detail <- main_page %>% 
            html_nodes("div#jobs_collection div.data-details") %>% 
            html_text()
        details <- append(details, detail)
        
        ##### Salary
        salary <- main_page %>% 
            html_nodes("div#jobs_collection div.data-snapshot div.block:last-child") %>%
            html_text()
        salaries <- append(salaries, salary)
        
        closeAllConnections()
    }
    
    
    #### Append Skills
    
    
    for (i in 1:length(urls)){
        append_df[i,1] <- job
        append_df[i,2] <- titles[[i]]
        append_df[i,3] <- details[[i]]
        append_df[i,4] <- salaries[[i]]
        append_df[i,5] <- urls[[i]]
    }
    
    new_df <- rbind(new_df, append_df)
    
    ###### CLEANING
    
    new_df$Date <- as.factor(Sys.Date())
    
    ###### WAGES
    
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
    
    end_time <- Sys.time()
    
    print(end_time - start_time)
    
    return(new_df)
}
count_skills <- function(df){
    df <- df[1:25,]
    df2 <- separate(df, Skills, c("S1", "S2", "S3", "S4", "S5", "S6"), sep=", ", fill="right")
    skills <- select(df2,S1,S2,S3,S4,S5,S6,Query)
    skills <- gather(skills, "query", "skills", -Query)
    skills <- filter(skills, complete.cases(skills))
    skills <- skills %>% 
        count(skills)
    skills <- skills[with(skills,order(-n)),]
#    skills <- skills[1:10,]
    return(skills)
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

get_lng <- function(hascoords, needscoords){
    needscoords$lng <- hascoords$lng[cbind(
        match(tolower(needscoords$Location), tolower(hascoords$Location)))]
    return(needscoords$lng)
}
get_lat <- function(hascoords, needscoords){
    needscoords$lat <- hascoords$lat[cbind(
        match(tolower(needscoords$Location), tolower(hascoords$Location)))]
    return(needscoords$lat)
}
