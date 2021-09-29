# This R script contains functions used to generate plots for each section
# All the functions in this R script should be self-explanatory

# all emotion types
emo_types <- c("anger", "anticipation", "disgust", "fear", "joy",
               "sadness", "surprise", "trust", "neutral")
# Color for each corresponding emotion types
col_use <- c("red", "pink", "green", "black", "yellow",
             "blue", "purple", "brown", "gray")
image_path <- "../figs"

# Generate plots for the first section
get_plot1 <- function(sentence_list){
  
  #Fig 1.1
  plot1.1 <- sentence_list %>% ggplot(aes(x = topemo)) +
    geom_bar(aes(y = (..count..)/sum(..count..)),
             fill = col_use) +
    scale_x_discrete(limits=emo_types) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, .3)) +
    labs(title = "Fig 1.1 Overall emotions for all philosophers",
         x = "Emotion", y = "Percentage of Sentences") +
    theme_linedraw()
  
  #Fig 1.2
  plot1.2 <- sentence_list %>% filter(topemo != 9) %>%
    mutate(topemo = factor(topemo)) %>%
    ggplot() +
    geom_violin(aes(x = topemo, y = topemo.v, fill = topemo)) +
    scale_x_discrete(labels = emo_types[1:8]) +
    scale_fill_manual(values = col_use[1:8]) +
    labs(title = "Fig 1.2 Distribution of emotion scores for each emotion",
         x = "Emotion", y = "Sentiment score") +
    guides(fill = FALSE) +
    theme_linedraw()
  
  ggsave("Fig1.1.jpeg", plot1.1, path = image_path)
  ggsave("Fig1.2.jpeg", plot1.2, path = image_path)
  return(list(plot1.1, plot1.2))
}

get_example2 <- function(sentence_list){
  # Examples in section 2
  # Get the percentage of neutral sentences for each philosophers
  temp <- sentence_list %>% group_by(author) %>%
    summarize(neutral_percentage = sum(topemo == 9) / n()) %>%
    arrange(neutral_percentage)
  
  return(list(head(temp), tail(temp)))
}


get_plot3 <- function(sentence_list){
  sentence_list$topemo <- as.factor(sentence_list$topemo)
  sentence_list$school <- as.factor(sentence_list$school)
  levels(sentence_list$topemo) <- emo_types
  
  #Fig 3.1
  plot3.1 <- sentence_list %>% group_by(school) %>%
    count(topemo) %>% mutate(per = n / sum(n)) %>%
    ggplot(aes(fill = topemo, x = school, y = per)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(values = col_use) +
    labs(title = "Fig2.1 Percentage of emotions within each school",
         x = "School", y = "Percentage") +
    scale_y_continuous(labels = scales::percent) +
    theme_linedraw() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ggsave("Fig3.1.jpeg", plot3.1, path = image_path)
  return(plot3.1)
}
