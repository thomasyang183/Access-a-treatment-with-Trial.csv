library(readr)
Trial <- read_csv("~/Google Drive/STA138/Project/Trial.csv")

the.table = table(Trial)

table1=table(Trial$Success) #the number of success treatment overall
table2=table(Trial$Success, Trial$Group)  #the number of success treatment, comparing only groups
table3=table(Trial$Success, Trial$Year)   #the number of success treatment, comparing only year


#1-sample proportions test
alpha = 0.05

#estimate probability of success overall
prop.test(table1["Yes"],sum(table1), conf.level = 1-alpha)

#estimate probability of success, A
prop.test(table2["Yes","A"],sum(table2[,"A"]), conf.level = 1-alpha)


#estimate probability of success, B
prop.test(table2["Yes","B"],sum(table2[,"B"]), conf.level = 1-alpha)

#estimate probability of success, 1
prop.test(table3["Yes","One"],sum(table3[,"One"]), conf.level = 1-alpha)

#estimate probability of success, 2
prop.test(table3["Yes","Two"],sum(table3[,"Two"]), conf.level = 1-alpha)


#mosaicplot
mosaicplot(table1, main = "Moasic Plots for success treatments overall")
mosaicplot(table2, main = "Trial Year vs Success Rate")
mosaicplot(table3, main = "Trial Group vs Success Rate")



