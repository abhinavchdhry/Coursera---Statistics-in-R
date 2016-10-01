df1 <- brfss2013 %>% select(X_pacat1, cvdinfr4) # Data frame 1
df2 <- brfss2013 %>% select(X_pacat1, cvdcrhd4) # Data frame 2
df3 <- brfss2013 %>% select(X_pacat1, cvdstrk3) # Data frame 3

# Remove any NA values from the data sets
df1 <- df1 %>% filter(!is.na(X_pacat1)) %>% filter(!is.na(cvdinfr4))
df2 <- df2 %>% filter(!is.na(X_pacat1)) %>% filter(!is.na(cvdcrhd4))
df3 <- df3 %>% filter(!is.na(X_pacat1)) %>% filter(!is.na(cvdstrk3))

# Group each data frame by X_pacat1 and the corresponding variable
# and then generate summarize using count=n()
df1_summary <- df1 %>% group_by(X_pacat1, cvdinfr4) %>% summarize(count=n())
df2_summary <- df2 %>% group_by(X_pacat1, cvdcrhd4) %>% summarize(count=n())
df3_summary <- df3 %>% group_by(X_pacat1, cvdstrk3) %>% summarize(count=n())

# From the summaries above, for each group of X_pacat1, calculate the proportion
# of the corresponding cvd variable that have value "Yes"
df1_summary <- summarize(df1_summary, relfreq=count[cvdinfr4=="Yes"]/sum(count))
df2_summary <- summarize(df2_summary, relfreq=count[cvdcrhd4=="Yes"]/sum(count))
df3_summary <- summarize(df3_summary, relfreq=count[cvdstrk3=="Yes"]/sum(count))

# At this point, we have for each activity group, the corresponding percentage
# of participants that have been diagnosed with the particular heart problem
# We plot these in bar plots for each data frame
df1_summary %>% ggplot(aes(x=df1_summary$X_pacat1, y=df1_summary$relfreq)) + geom_bar(stat="identity")
df2_summary %>% ggplot(aes(x=df2_summary$X_pacat1, y=df2_summary$relfreq)) + geom_bar(stat="identity")
df3_summary %>% ggplot(aes(x=df3_summary$X_pacat1, y=df3_summary$relfreq)) + geom_bar(stat="identity")