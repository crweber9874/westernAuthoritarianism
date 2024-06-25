# library(httr)
# library(jsonlite)
# library(googleLanguageR)
# library(readr)
# library(readr)
#
# file_data <- read_csv("~/Dropbox/github_repos/westernAuthoritarianism/westernAuthoritarianism/dataActive.csv") # Replace with the actual path
# processed_text <- paste(apply(file_data, 1, paste, collapse = ", "), collapse = "\n")
#
#
#
# gemini <- function(prompt,
#                    temperature=0.5,
#                    max_output_tokens=1024,
#                    api_key=Sys.getenv("GEMINI_API"),
#                    model = "gemini-1.
#                    5-pro-latest") {
#
#   model_query <- paste0(model, ":generateContent")
#
#   response <- POST(
#     url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
#     query = list(key = api_key),
#     content_type_json(),
#     encode = "json",
#     body = list(
#       contents = list(
#         parts = list(
#           list(text = prompt)
#         )),
#       generationConfig = list(
#         temperature = temperature,
#         maxOutputTokens = max_output_tokens
#       )
#     )
#   )
#
#   if(response$status_code>200) {
#     stop(paste("Error - ", content(response)$error$message))
#   }
#
#   candidates <- content(response)$candidates
#   outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
#
#   return(outputs)
#
# }
#
# data <- read_csv("~/Dropbox/github_repos/westernAuthoritarianism/westernAuthoritarianism/dataActive.csv") # Replace with the actual path
# table(data$ideology)
#
# question <- "ideology"
# range_suggestion <- "Strong Liberal to Strong Conservative, ignoring NA's"
# varName <- "ideology"
#
# promptConstructor  = function(question = " ideology",
#                               range_suggestion = " liberal to conservative",
#                               varName = " ideology"){
#            prompt =   paste("There are", length(table(data$ideology) %>% rownames() %>% as.numeric() %>% c()) ,
#                   "levels of this variable called", question,".",
#                   "The eligible numeric values range from",
#                    min(data$ideology, na.rm =TRUE), "to", max(data$ideology, na.rm =TRUE),
#                   ".Now dplyr::recode these eligble values from",
#                    range_suggestion, "where the variables name is",
#                    varName, ".Only include the final suggested R code.
#                   Include an ignore NA statement.")
#   return(prompt)
# }
#
# gemini(promptConstructor()) %>% cat()
