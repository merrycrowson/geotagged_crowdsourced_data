### Code to create a contingency table for the Flickr and Wikipedia data ###
#Output is a contingency table m that is used as input for Pearson’s chi-squared test for independence, and the same table as a dataframe (df)
#that is used for Figure 1 in the paper

colnames(my_data)
table_count <- my_data
table_count <- table_count[,c(1:3, 5, 6)] # ID, reference, sitename, Wiki_T_F, Fli_T_F 
head(table_count)
table_count$Fli_T_F <- as.numeric(as.character(table_count$Fli_T_F))
table_count$wiki_T_F <- as.numeric(as.character(table_count$wiki_T_F))
table_count$Fli_T_F_10 <- 0
table_count$Fli_T_F_10[table_count$Fli_T_F == 1] <- 10
head(table_count)
table_count$cross <- table_count$Fli_T_F_10 + table_count$wiki_T_F 
head(table_count)
table_count$cross <- as.factor(table_count$cross)
summary(table_count)
t <- summary(table_count$cross)
m <- matrix(c(t[4],  t[3],t[2], t[1]) , nrow = 2, ncol = 2)
row.names(m) <- c("Wiki:Y", "Wiki:N")
colnames(m) <- c("Flickr:Y", "Flickr:N")
df <- as.data.frame(m)
