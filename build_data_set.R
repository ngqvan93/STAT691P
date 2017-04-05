rm(list = ls())

# This is a modified version of Tangxin Jin's code!
# Modified by John Staudenmayer on Feb 1, 2017
# Modified by Mengyuan Wu on Feb 8, 2017
# Modified by Van Nguyen on Feb 23, 2017

#load library
#install.packages("rvest")
library(rvest)
library(ggmap)
library(gmapsdistance)

CalculateMean <- function(x) {
  # Calculate the mean of a range for rent, square footage, etc.
  #
  #Arg:
  #   x: a vector of ranges
  #
  #Returns:
  #   A vector of average values
  
  temp <- as.numeric(unlist(strsplit(x, " ")))
  return(mean(temp, na.rm = T))
}

# Define the constant url for Amherst
kUrl <- "http://www.rentals.com/Massachusetts/Amherst/?per_page=90"

BuildDataSet <- function(url) {
  # Build the data set of rentals from www.rentals.com.
  #
  # Arg:
  #   url: The URL of result page after searching for a specific town
  #
  # Returns:
  #   A data set consists of all listings with chosen attributes
  
  # Use the given URL to read the HTML file.
  html <- read_html(url)
  
  # Get a list of hrefs (links) of all properties in the result page.
  hrefs <- html %>% 
    html_nodes('#search_results') %>%
    html_nodes('.listing_name') %>%
    html_nodes('a') %>%
    html_attr('href')
  
  # Create an empty data frame to hold the data.
  df <- data.frame(address = character(0), 
                   town = character(0),
                   state = character(0), 
                   zip = character(0),
                   rent = character(0),
                   num.of.bed = character(0),
                   num.of.bath = character(0),
                   sqft = character(0),
                   on.bus.route = character(0),
                   apt.or.house = character(0),
                   utilities = character(0),
                   long = character(0),
                   lat = character(0),
                   dist.amherst = character(0))
  
  # Loop through each property to extract the needed information.
  for (item in hrefs) {
    
    # Get town name
    town <- unlist(strsplit(item, split = "/"))[3] 
    
    link <- ''
    page <- ''
    
    # Check if the property is a house or an apartment.
    
    if (grepl("/lv", item) | grepl("/r", item)) {
      # If the property is a house:
      
      link <- paste("http://www.rentals.com", item, sep = '')
      print(link) # Print the link for debugging purpose
      page <- read_html(link) # Read the HTML page

      # Get the rental price
      rent <- page %>% 
        html_node('#summary_price strong') %>% 
        html_text
      
      # Get the floor plan description
      floorplan <- page %>% 
        html_node('#summary_floorplan') %>% 
        html_text
      floorplan <- unlist(strsplit(floorplan, split = '[|]'))
      num.of.bed <- floorplan[1] # Get the number of bedrooms
      num.of.bath <- floorplan[2] # Get the number of bathrooms
      sqft <- floorplan[3] # Get the square feet
      
      # Get utilities information
      temp <- page %>%
        html_node('#description_single') %>%
        html_text
        temp <- unlist(strsplit(temp, split = '[.]')) # Split the description by sentence
      index <- grep("[Uu]tilities", temp) # Search for "Utilities" in the description
      if (length(index) == 0){
        utilities <- "No"
      } else if (grepl("[Nn]ot", temp[index]) | grepl("[Nn]o", temp[index])){
        # Search if the sentence containing "Utilities" has negation cues
        utilities <- "No"
      } else
        utilities <- "Yes"
        
      # Apartment or house
      apt.or.house <- "House"
    
    } else {
      # If the property is an apartment:
      
      link <- paste("http://www.rentals.com", item, sep = '')
      print(link) # Print the link for debugging purpose
      page <- read_html(link)
      tab <- page %>% 
        html_node(xpath = '//*[@id="page"]/div[2]/div[2]/div[2]/div/div[1]/div[2]/table') %>% 
        html_table(fill = T) # Get the summary table from the posting
    
      # Get the rental price
      rent <- tab$Price 

      # Get the floor plan description
      num.of.bed <- tab$Bd.
      num.of.bath <- tab$Ba.
      sqft <- tab$`Sq. Ft.`

      # Get utilities information
      temp <- page %>%
        html_node(xpath = '//*[@id="property_details"]/div[2]') %>%
        html_node("ul") %>%
        html_nodes("li") %>%
        html_text
      index <- grep("[Uu]tilities", temp) # Search for "Utilities" in the description
      if (length(index) == 0){
        utilities <- "No"
      } else if (grepl("[Nn]ot", temp[index]) | grepl("[Nn]o", temp[index])){
        # Search if the sentence containing "Utilities" has negation cues
        utilities <- "No"
      } else
        utilities <- "Yes"
      
      # Apartment or house
      apt.or.house <- "Apartment"

    }

    # Get the address, state, zip
    address <- page %>% 
      html_node('#summary_address') %>% 
      html_text
    state <- substr(address, nchar(address) - 7, nchar(address) - 6)
    zip <- substr(address, nchar(address) - 4, nchar(address))
    
    # Get public transportation information
    temp <- readLines(link)
    index1 <- grep("[Bb]us route", temp) # Search for "bus route" 
    index2 <- grep('[Pp]ublic [Tt]ransportation', temp) # Search for "public transportation"
    if (length(index1) > 0 | length(index2) > 0) {
      on.bus.route <- "Yes"
    } else {
      on.bus.route <- "No"
    }
    
    # Get the longitude and latitude
    coordinates <- geocode(address)
    lat <- coordinates$lat    			
    long <- coordinates$lon
    
    # Calculate the driving distance from the property to downtown Amherst
    dist.amherst <- gmapsdistance(origin = "42.375379+-72.520528",
                                 destination = paste(lat,long,sep="+"),
                                 mode = "driving")$Distance
    # Create a temporary df
    df_temp <- data.frame(address = address, 
                   town = town,
                   state = state, 
                   zip = zip,
                   rent = rent,
                   num.of.bed = num.of.bed,
                   num.of.bath = num.of.bath,
                   sqft = sqft,
                   on.bus.route = on.bus.route,
                   apt.or.house = apt.or.house,
                   utilities = utilities,
                   long = long,
                   lat = lat,
                   dist.amherst = dist.amherst)
 
    # Combine dataframe
    df <- rbind(df, df_temp)
      
  }

  # Reformat some columns of the data frame
  # Reformat rents
  rents <- df$rent
  rents <- gsub('\\$', '', rents) # Remove dollar signs
  rents <- gsub(',', '', rents) # Remove conmmas
  rents <- gsub('-', ' ', rents) # Replace dashes with spaces
  df$rent <- as.vector(tapply(rents, (1:length(rents)), CalculateMean)) # Replace range of rent with average rent
  df$rent[df$rent == "NaN"] <- NA
  
  # Reformat square footage
  sqft <- df$sqft
  sqft <- gsub(',', '', sqft)
  sqft <- gsub('-', ' ', sqft)
  sqft <- as.vector(tapply(sqft, (1:length(sqft)), CalculateMean))
  sqft[sqft == "NaN"] <- NA
  df$sqft <- sqft
  
  # Reformat number of bedrooms
  br <- df$num.of.bed
  br <- as.numeric(gsub('[^0-9]', '', br)) # get rid of non-numbers
  # "studio" is now NA. Make it "Studio" if we want to treat number of bedrooms as factor
  br[is.na(br)] <- 0.5
  df$num.of.bed <- br
  
  # Reformat number of bathrooms
  bath <- df$num.of.bath
  bath <- gsub(' Bathrooms', '', bath)
  bath <- strsplit(bath, split = '')
  for (i in 1:length(bath)){
    if (length(unlist(bath[i])) >1 ){
      bath[i] <- as.numeric(unlist(bath[i])[1])+0.5
    }
  }
  bath <- as.numeric(unlist(bath))
  df$num.of.bath <- bath

  return(df)
}


df <- BuildDataSet(kUrl)

# Fix the outlier where the property's address is interpreted incorrectly
df$address <- as.character(df$address) 
df$address[131] <- "140 Union Street, Westfield, MA 01085"
df$lat[131] <- geocode(df$address[131])$lat
df$long[131] <- geocode(df$address[131])$lon
df$dist.amherst[131] <- gmapsdistance(origin = "42.375379+-72.520528",
                                      destination = paste(df$lat[131],df$long[131],sep="+"),
                                      mode = "driving")$Distance
