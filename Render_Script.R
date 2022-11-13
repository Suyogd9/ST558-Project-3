##THIS FILE IS FOR GENERATING REPORTS ONLY##

#Defining a function to translate the provided channel value to a proper title
getTitle <- function(channel) {
  t <- switch(channel,
              'lifestyle' = "Lifestyle",
              'entertainment' = "Entertainment",
              'bus' = "Business",
              'socmed' = "Social Media",
              'tech' = "Technology",
              'world' = "World")
  return(t)
}

#Defining a function to render the report using the parameters of channel and title
#outputs the file with the name associated with the channel
renderReport <- function(c) {
  
  title <- getTitle(c)
  
  rmarkdown::render("Project_3.Rmd",
                    params = list(channel = c,
                                  rep_title = title),
                    output_file = paste0(title, "Analysis")
  )
  
}

renderReport('world')
