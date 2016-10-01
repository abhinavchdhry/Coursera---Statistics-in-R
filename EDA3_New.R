# Select necessary variables from the parent dataset and filter out NA values
df1 <- brfss2013 %>% select(numadult, genhlth, income2)
df1 <- df1 %>% filter(!is.na(numadult)) %>% filter(!is.na(genhlth)) %>% filter(!is.na(income2))

# Filter only for numadult = 2, 3, 4, 5
cv <- c("2", "3", "4", "5")
df1 <- df1 %>% filter(numadult %in% cv)

# Group heirarchically by income and health and summarize using count=n()
# Next, mutate the data frame by adding a new variable 'rf' which gives the 
# relative frequency of each genhlth group within each income group
temp <- df1 %>% group_by(income2, genhlth) %>% summarise(count=n()) %>% mutate(rf=count[genhlth]/sum(count))

# Two ways of graphing barplots for the data:

# Barplot based on primary group 'genhlth' and secondary group 'income2'
ggplot(temp, aes(x=income2, y=rf)) + geom_bar(stat="identity") + facet_wrap(~ genhlth) + coord_flip()

# Barplot based on primary group 'income2' and secondary group 'genhlth'  
ggplot(temp, aes(x=genhlth, y=rf)) + geom_bar(stat="identity") + facet_wrap(~ income2) + coord_flip()
