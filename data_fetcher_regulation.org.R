# Data is obtained for the site ----------------------------through api---
#      https://www.regulations.gov/docket/ED-2024-SCC-0040
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


# Load the required library
library(tidyverse)
library(httr)


api      = "https://api.regulations.gov/v4/"
# Replace the api_key with your own api_key. Remember the limit is 1000/hour
# For new api or documentation of api, visit https://open.gsa.gov/api/regulationsgov/
api_key   =
api_key_2 =
api_key_3 =

agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"



# (1) Get objectID for document notice (the document which contain notice)
#----------------------------------------------------------------------
objectID <-
  GET(                           
    url   = str_glue("{api}documents?"),
    query = list(
      `filter[docketId]` = "ED-2024-SCC-0040",
      api_key            = api_key
    ),
    user_agent(agent)
  )                                                    |>
  content(as="parsed", type = "application/json")      |>  
  pluck("data")                                        |> 
  keep(    ~ .x$attributes$documentType == "Notice")   |> 
  map_chr( ~ .x$attributes$objectId)
# ---------------------------------------------------------





# (2) Define the function to fetch comments for a given page and objectId and return the data and total number of pages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fetch_cmnts <-
  function(page_number) {
    # make the API call
    response <- GET(
      url   = str_glue("{api}comments?"),
      query = list(
        `filter[commentOnId]` = objectID,
        `page[size]`          = 250,
        `page[number]`        = page_number,
        `sort`                = "-postedDate",
        api_key               = api_key
      ),
      user_agent(agent)
    )
    
    content_data <- content(response, as = "parsed", type = "application/json")
    
    # Return the data and totalPages information
    list(
      data        = content_data$data,
      total_pages = content_data$meta$totalPages
    )
  }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## (3) Make Function Extracting Comment details for commentID
#............................................................................
fetch_comment_details <- function(cmnt_id) {
  response <- GET(url = str_glue("{api}comments/{cmnt_id}?api_key={api_key}"),
                  user_agent(agent)
  )
  
  # Extract attributes from the response
  attributes <- response |>
    content(as = "parsed", type = "application/json") |>
    map_df(~ .x$attributes)
  
  # Return the entire attributes as a named vector, not just specific columns
  return(attributes)
}
#.........................................................................





#-(4)------
total_pages   <- fetch_cmnts(1)$total_pages   # number of available comments page



#...........................................................................
# (5) Fetch all the comments 
#_==========================================================================
# 
all_comments <- 
  1:total_pages |> 
  map_df(~ {
    # Fetch the data for the current page
    fetch_cmnts(.x)$data |>   # Extract only the data from the response
      map_df(~ {
        c(
          comment_id =.x$id,
          .x$attributes,
          link =.x$links$self
        )
      })
  })



comment_detalis <-
  map_dfr(
    # comment out `|> sample()` and add a comma before that to collect all the comment details
    .x = all_comments$comment_id |> sample(30),
    .f = fetch_comment_details,
    .progress = TRUE  
  ) 

# Write all the data to the storage
write_csv(x = all_comments   , file="all_comments.csv")
write_csv(x = comment_detalis, file="comment_details.csv")
