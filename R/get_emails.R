library(gmailr)

pth <- 'C:/Users/mbeck/AppData/Local/gmailr/gmailr/client_secret_874526830294-43165uq3qcgbo0bhel12jnqcm6t1vaoj.apps.googleusercontent.com.json'

gm_auth_configure(path = pth)

gm_oauth_app()

search_query <- "subject:\"Piney Point Update -\""

# Initialize variables for pagination
all_message_ids <- c()
next_page <- NULL
more_messages <- TRUE

# Loop to get all pages of results
while(more_messages) {
  # Get a page of messages using the page token if available
  emails <- gm_messages(search = search_query, page_token = next_page)
  
  # Add these message IDs to our collection
  if(length(gm_id(emails)) > 0) {
    all_message_ids <- c(all_message_ids, gm_id(emails))
    
    # Check if there are more pages
    if(!is.null(emails[1][[1]]$nextPageToken)) {
      next_page <- emails[1][[1]]$nextPageToken
      cat("Retrieved", length(all_message_ids), "emails so far, fetching more...\n")
    } else {
      more_messages <- FALSE
      cat("Retrieved all matching emails: total count =", length(all_message_ids), "\n")
    }
  } else {
    more_messages <- FALSE
    cat("No more messages found.\n")
  }
}

# Function to extract body text from an email
extract_email_body <- function(message_id) {
  # Get the full message
  message <- gm_message(message_id)
  
  # Extract the body
  body_text <- gm_body(message)
  
  # Get the subject for reference
  subject <- gm_subject(message)
  
  # Return as a list
  return(list(
    subject = subject,
    body = body_text,
    date = gm_date(message)
  ))
}


# Apply the function to each email found
email_bodies <- lapply(all_message_ids, extract_email_body)

