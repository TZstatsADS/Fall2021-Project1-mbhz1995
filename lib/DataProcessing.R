# This r script contains 2 functions

# raw_data_proc() is used to process the raw data. data_file_path is the path
# to the raw data file.
# It selects the data to to be used for analysis;
# get the sentiment scores for each sentence; 
# store the processed data in a csv file; and
# return the path to the processed data

# processed_data_proc() is used to get the top emotion and the 
# emotion score for the top emotion for each sentence.
# It takes in the path to the processed data and return a tibble 
# that contains the processed data with emotion scores

raw_data_proc <- function(data_file_path){
  # Read the original Data
  corpus <- read_csv(data_file_path)
  
  # Data processing:
  # Select the data to be analyzed (sentences) and
  # create a sentence ID (sent.id) for each sentence
  sentence_list <- corpus %>% 
    select(c("title", "author", "school", "sentence_str", 
             "original_publication_date")) %>%
    mutate(sent.id = 1:nrow(sentence_list))
  
  # Get the sentiments from each sentence
  # It took me over an hour to get the results from get_nrc_sentiment
  # Consider using the processed data directly
  emotions <- get_nrc_sentiment(sentence_list$sentence_str)
  sentence_list <- bind_cols(sentence_list, emotions)
  
  processed_data_path <- "../output/sentence_list_emo.csv"
  write_csv(sentence_list, processed_data_path)
  return(processed_data_path)
}

# Get all indexes for maximum in a vector v and
# return a random index
get_max_position <- function(v){
  sample(which(v == max(v)), 1)
}

processed_data_proc <- function(processed_data_file_path){
  # Read the processed data
  sentence_list <- read_csv(processed_data_file_path)
  
  # Get the top emotion of each sentence
  sentence_list$topemo <- sentence_list %>%
    select(anger:trust) %>%
    apply(1, get_max_position)
  
  # Get the emotion score of the top emotion
  sentence_list$topemo.v <- sentence_list %>%
    select(anger:trust) %>%
    apply(1, max)
  
  # If the top emotion has value 0, classify the sentence
  # as not emotionally neutral and set the top emotion to 9
  sentence_list$topemo[sentence_list$topemo.v == 0] <- 9
  
  return(sentence_list)
}

