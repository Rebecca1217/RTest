# pkgs --------------------------------------------------------------------

library(rvest)
library(data.table)

# paras -------------------------------------------------------------------

n = 6 # pages
suffix <- paste0("news?p=", seq(2, n, 1))
suffix <- c("", suffix)

for (i in 1:n) {
  
  html_address <- c(html_address,
                    paste0("https://news.ycombinator.com/", suffix[i]))
  
}


# function ----------------------------------------------------------------

f_read_html <- function(html_address){
  
  dt_content_bind <- NULL
  for (i in 1:n){
    
    content <- read_html(html_address[i]) # read content from website
    
    title <- content %>%
      html_nodes("a.storylink") %>% # td下面 span class 或者 a class要在class前面加上a. or span.
      # 前面有.就可以，不需要非得有a 或者span 但这样不够稳
      # 也可以写成".class a"把a 或者span写到后面 sometimes not work?
      html_text()
    
    # link_domain <- content %>%
    #   html_nodes("span.sitestr") %>%
    #   html_text()
    
    # link_domain <- content %>%
    #   html_nodes("span.sitebit.comhead") %>%
    #   html_node("span.sitestr") %>%
    #   html_text() 这样不行因为span.sitestr没有的地方span.sitebit.comhead也没有，所以结果一样
    
    
    # link_domain <- content %>%
    #   html_nodes("td.title") %>%
    #   html_node("span.sitestr") %>%
    #   html_text() 这样也不行因为有好几个td.title，所以读出来多的NA，一个编号，一个sitestr
    #   最后多一个是因为a.morelink也在td.title里
    
    #那结构都弄清楚也好处理了
    # 参考https://stackoverflow.com/questions/45723085/scraping-with-rvest-how-to-fill-blank-numbers-in-a-row-to-transform-in-a-data-f
    
    
    link_domain <- content %>%
      html_nodes("td.title") %>%
      html_node("span.sitestr") %>%
      html_text() 
    
    link_domain <- link_domain[-length(link_domain)] # remove the last td.title"MORE" link
    link_domain <- link_domain[c(FALSE, TRUE)] # only keep the index that is even number
    
    testthat::expect_equal(length(title), length(link_domain))
    
    score <- content %>%
      html_nodes("td.subtext") %>%
      html_node("span.score") %>%
      html_text()
    testthat::expect_equal(length(title), length(score))
    
    age <- content %>%
      html_nodes("td.subtext") %>%
      html_node("span.age") %>%
      html_text()
    testthat::expect_equal(length(title), length(age))
    
    dt_content <- data.table(Title = title, Link_Domain = link_domain,
                             Score = score, Age = age)
    dt_content_bind <- rbind(dt_content_bind, dt_content)
    
    # 
    
  }
  
  return(dt_content_bind)
}
  
# testing use
dt <- f_read_html(html_address)
View(dt[grepl("hour", Age)]) 

dt[, Score_Number := as.numeric(stringr::str_extract(Score, "\\d+"))]
setorder(dt, -Score_Number)
