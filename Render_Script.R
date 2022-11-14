##THIS SCRIPT IS FOR GENERATING REPORTS ONLY--SEE Project_3.Rmd FOR SOURCE CODE##

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
                    output_format = "github_document",
                    params = list(channel = c,
                                  rep_title = title),
                    output_file = paste0(title, "Analysis.md"))
  
}

#Defining a function run this script for either all of the channels (returns six reports)
#OR for a single specified channel (returns one report)
runScript <- function(mode) {
  
  var_opt <- c('lifestyle', 'entertainment', 'bus', 'socmed', 'tech', 'world')
  
  if (mode == 'all') {
    for (i in var_opt) {
      renderReport(i)
    }
  } 
  
  else if (mode %in% var_opt) {
    renderReport(mode)
  } 
 
   else stop("Invalid input; try again")
  
}

#Calling script function to generate all reports
runScript('all')
