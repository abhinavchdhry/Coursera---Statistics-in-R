library(dplyr);
library(ggplot2);

setwd("C:/Users/Abhinav Choudhury/Desktop/Study/Final Project");
load("brfss2013");

# Select necessary variables and remove NAs
temp_data <- brfss2013 %>% select(X_state, smokday2) %>% filter(!is.na(X_state)) %>% filter(!is.na(smokday2))

# Group temp_data by (state, smokday2) and summarize first to get a count of each smoker type by state
# Next, summarize again to create a relative frequency table of smokers per state
states_with_rfsmokers <- temp_data %>% group_by(X_state, smokday2) %>% summarize(count=n()) %>% summarize(relfreq=1 - (count[smokday2=="Not at all"]/sum(count)))
states_with_rfsmokers <- as.data.frame(states_with_rfsmokers)

# Sort the states_with_rfsmokers data frame in descending order based on the relfreq
states_with_rfsmokers <- states_with_rfsmokers[order(states_with_rfsmokers$relfreq, decreasing = TRUE), ]

# Print the top 3 and the lowest 3 states in the smoking percentage list
states_with_rfsmokers[1:3,]
num_states <- length(states_with_rfsmokers[,1])  # No. of entries in list
states_with_rfsmokers[num_states:(num_states-2),]

# Plot the resulting data in a vertical bar plot
states_with_rfsmokers %>% ggplot(aes(x=X_state, y=relfreq)) + geom_bar(stat="identity", width=0.75) + coord_flip()
