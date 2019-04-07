# *** DATA VISUALIZATION ***
# *** EXPLORATORY DATA ANALYSIS WITH GGPLOT2 ***
# *** Group E | O2 ***

library(ggplot2)
library(countrycode)
library(data.table)
library(forcats)
library(gridExtra)
library(RColorBrewer)
library(maptools)
library(rworldmap)
library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(plyr)
library(jcolors)

#### Loading and Preparing the Data ####
# reading the dataset
madrid <- read.csv(file = file.path("C:/Users/andre/Desktop/Files/MBD Term 2/Data Visualization/Assignments/Assignment 2 - Madrid Transactions/madrid_transactions.csv"),header=TRUE,sep=",",dec=".")

# changing non spanish category nammes
for (ii in 1:length(madrid$category)) {
  
  if(madrid$category[ii]=="Hogar y reformas"){
    madrid$category[ii]=="Home & Reforms"
  }
  
  if(madrid$category[ii]=="AutomociÃ³n"){
    madrid$category[ii]=="Automotive"
  }
  
  if(madrid$category[ii]=="Agencias de viajes"){
    madrid$category[ii]=="Travel Agencies"
  }
  
}

levels(madrid$category)[2] <- "Travel Agencies"
levels(madrid$category)[3] <- "Automotive"
levels(madrid$category)[11] <- "Home & Reforms"


madrid_dt <- as.data.table(madrid)

# filtering for top 20 countries
top_countries <- madrid_dt[,list(sum_purchase=sum(amount)),by=madrid_dt$customer_country][order(sum_purchase,decreasing = TRUE)]
top_20 <- head(top_countries,20)[,madrid_dt]
top_countries <- madrid_dt[customer_country %in% top_20]

# create datatable and transform to get data table with countries
countries <- madrid_dt[,list(total_amount = sum(amount),transactions=.N),by=customer_country][,list(customer_country,total_amount,transactions,avg=round(total_amount/transactions,digits = 2))]
countries <- unique(countries)
countries20 <- countries[customer_country %in% top_20]

#### _____________________________________________________________ ####
#### Basic Table ####

countries20$percent <- round((countries20$total_amount / sum(madrid$amount)),digits = 2)
countries20 <- countries20[order(total_amount,decreasing = TRUE)]
for (ii in 1:20) {
  countries20$index[ii] <- ii
}

countries20$country_name <- countrycode(sourcevar = countries20$customer_country, origin = "genc2c",destination = "country.name")


countries20$accumPerc <- countries20$percent[1]

for (ii in 2:20) {
  countries20$accumPerc[ii] <- countries20$accumPerc[ii-1] + countries20$percent[ii]
}

focus_group <- countries20[,list(Index=index,Country = country_name,"Total spending (â‚¬)" = round(total_amount,digits = 0),"Average spending (â‚¬)"=round(avg,digits = 0),"Number of transactions" = transactions,"% of spending"=percent*100,"Accumulated % of spending"=accumPerc*100)]
write.csv(focus_group, file = "top20Countries1.csv")

#### World Map ####
# preparing the data table
madrid$country_name <- countrycode(sourcevar = madrid[, "customer_country"], origin = "genc2c",destination = "country.name")
for (ii in 1:length(madrid$customer_country)) {
  if(madrid$customer_country[ii]=="AN"){
    madrid$country_name[ii] <- "Netherlands Antilles"
  }
}

dt_countries <- as.data.table(madrid)

total_spending_country <- dt_countries[,list(total=sum(amount)), by=country_name]
map.world = map_data(map='world')
total_spending_country$region = as.character(total_spending_country$country_name)
df <- as.data.frame(total_spending_country)

#recoding countries
df <- df %>%  mutate(region = recode(region, `United States` = 'USA'
                                     , `United Kingdom` = 'UK'
                                     , `Congo, Democratic Republic of the` = 'Democratic Republic of the Congo'
                                     , `Trinidad and Tobago` = 'Trinidad'
                                     , `Sudan and South Sudan` = 'Sudan'
                                     , `Congo, Republic of the` = 'Republic of Congo'
)
)

map.world = merge(df,map.world, by='region',all.y=TRUE)
map.world = map.world[order(map.world$order), ]

map_1 <- ggplot() + geom_map(data = map.world,map = map.world, aes(
  map_id = region,
  x=long,
  y=lat,
  fill=total
)) + scale_fill_gradientn(colours = c('dodgerblue4','dodgerblue2','azure3','brown','firebrick4')
                          ,values = scales::rescale(c(10000,50000, 100000, 150000, 300000))
                          ,labels = comma
                          ,breaks = c(10000, 50000, 100000, 150000, 300000)
) + labs(fill = 'Average Spending (???)'
         ,title = 'Total Spending by Country (???)', size = 2
         ,caption = "Source: © Group E | O2"
         ,x = NULL
         ,y = NULL) + theme(text = element_text(family = 'Gill Sans', color = 'gray0')
                            ,plot.title = element_text(size = 28)
                            ,plot.subtitle = element_text(size = 14)
                            ,axis.ticks = element_blank()
                            ,axis.text = element_blank()
                            ,panel.grid = element_blank()
                            ,panel.background = element_rect(fill = 'white')
                            ,plot.background = element_rect(fill = 'white')
                            ,legend.position = c(.18,.36)
                            ,legend.background = element_blank()
                            ,legend.key = element_blank()
         ) + theme(plot.title = element_text(size=22))
map_1

#### Plot 1 ####
# Scatterplot for Transactions vs avg amount spent

getPalette = colorRampPalette(brewer.pal(8, "Spectral"))
colourCount <- 20

plot1 <- ggplot(countries20, aes(x=transactions, y=avg,col="black")) + 
  geom_point(aes(size=total_amount)) + 
  xlim(c(0, 2000)) + 
  ylim(c(0, 520)) + 
  labs(subtitle="for top 20 spending countries (including total amount spent)", 
       y="Avg Amount (???)", 
       x="Transactions", 
       title="Average amount spent vs number of transactions", 
       caption = "Source: © Group E | O2") + 
  geom_text(aes(label=customer_country),hjust=0, vjust=0,col="black") +
  scale_fill_manual(values = getPalette(colourCount),limits=top_20) + 
  guides(col=FALSE) + theme_minimal()

plot1

#### Plot 2 ####
# Total spending per top 20 countries

getPalette = colorRampPalette(brewer.pal(8, "Spectral"))
colourCount <- 15

plot2 <- ggplot(top_countries[category %in% c("Fashion & Shoes","Bars & restaurants")], aes(x=factor(customer_country), y=amount)) + 
  geom_col(aes(fill = category)) + 
  labs(subtitle="per Category", 
       y="Amount spent (???)",
       x="Top 20 Countries", 
       title="Amount Spent for Top 20 Countries", 
       caption = "Source: © Group E | O2",
       fill = "Countries") + 
  scale_x_discrete(limits=c("IN","FI","CL","AR","CA","NO",
                            "NL","IL","TW","CH","SE","IT",
                            "KR","BR","RU","JP","GB","FR","US","CN")) +
  coord_flip() + scale_fill_manual(values = c("brown3","dodgerblue4")) + theme_minimal() + theme(text = element_text(size=18))

plot2

#### Plot 3 ####
# two datasets for each day
thursday <- top_countries[top_countries$weekday==madrid[1,8]]
friday <- top_countries[top_countries$weekday==madrid[2,8]]

# total amount spend per daytime and category
thursday$name <- "thursday"
friday$name <- "friday"
d <- rbind(thursday, friday)
d$weekday = factor(d$weekday, levels=c('thursday ','friday   '))
levels(d$weekday)[1] <- "Thursday"
levels(d$weekday)[2] <- "Friday"

plot3 <- ggplot(d[category %in% c("Fashion & Shoes","Bars & restaurants")], aes(daytime, amount)) + 
  stat_summary(fun.y = "sum", geom = "bar", aes(fill=category)) + 
  facet_grid(cols = vars(weekday)) +
  labs(subtitle="for each day (Thursday & Friday)", 
     y="Total Amount (???)", 
     x="Daytime", 
     title="Total spending per day and category for top 20 countries", 
     caption = "Source: © Group E | O2") +
  scale_x_discrete(limits=c("Dawn","Morning","Mid morning","Afternoon","Evening","Night","Midnight")) +
  scale_fill_manual(values = c("brown3","dodgerblue4")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 23, hjust = 1, vjust = 0.5)) + theme(text = element_text(size=18)) +
  theme(panel.spacing = unit(2, "lines"))

plot3


#### Plot 4 ####
# prepare data table
total_spending <- madrid_dt[,list(total=sum(amount)), by=category]
total_spending$avg_z <- round((total_spending$total - mean(total_spending$total))/sd(total_spending$total), 2)  # compute normalized avg
total_spending$avg_type <- ifelse(total_spending$avg_z < 0, "below", "above")  # above / below avg flag
total_spending <- total_spending[order(total_spending$avg_z), ]  # sort
total_spending$category <- factor(total_spending$category, levels = total_spending$category)  # convert to factor to retain sorted order in plot.

category_count <- madrid_dt[,list(count=.N),by=category]
category_count <- unique(category_count)

# average amount spent normalized and total amount per category
a<-ggplot(total_spending, aes(x=category, y=avg_z, label=avg_z)) + 
  geom_bar(stat='identity', aes(fill=avg_type), width=.8)  +
  scale_fill_manual(name="Spending", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="dodgerblue4", "below"="brown3")) +
  coord_flip() + ylab("Normalized Average Spending") +
  xlab("") +
  labs(title="Normalized Average Spending",subtitle = "",caption = "Source: © Group E | O2") + theme_minimal() +
  theme(axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + theme(text = element_text(size=18))

category_spent <- madrid_dt[,list(total=sum(amount)),by=category][order(total,decreasing = T)]
category_spent <- unique(category_spent)

b<-ggplot(category_spent, aes(category, total)) + 
  geom_col(position = "dodge",width=.8, fill = "gray60") + 
  scale_x_discrete(limits=c("Travel Agencies","Automotive","Electronics & computing",
                            "Home & Reforms","Personal products","Sports & Games","Food",
                            "Books & Music","Culture & Leisure","Health","Other goods and services",
                            "Transportation", "Bars & restaurants", "Accommodation", "Fashion & Shoes")) +
  coord_flip() + ylab("Amount spent (???)") + xlab("Category") + theme_minimal() +
  labs(title= "Total Amount per Category", subtitle="") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + theme(text = element_text(size=18))

grid.arrange(b, a, ncol=2)


#### Plot 5 ####

# prepare data table
per_hour_spending <- madrid_dt[,list(total=sum(amount),count=.N), by=hour]
per_hour_spending <- per_hour_spending[order(hour)]
myoo <- madrid_dt
myoo$count <- 1
getPalette = colorRampPalette(brewer.pal(8, "RdBu"))
colourCount <- 15

for (ii in 1:length(myoo$hour)) {
  myoo$count[ii] <- per_hour_spending[myoo$hour[ii]+1,count]
}


plot5 <- ggplot(data=myoo, aes(x=hour, y=amount,fill=category)) + 
  geom_col() +
  labs(y="Total Amount Spent (???)", 
       x="Hour of the day", 
       title="Total Hourly Spending", 
       caption = "Source: © Group E | O2") +
  ggtitle("Total Hourly Spending (???)") + 
  scale_fill_manual(values = getPalette(colourCount)) + theme_minimal() +
  theme(text = element_text(size=20))

plot5

#### Plot 6 ####
#Analyzing each Category
y <- top_countries[,list(average=mean(amount)), by=c("customer_country","category")]
y2 <- top_countries[,list(total=sum(amount)), by=c("customer_country","category")]

fashionshoes <- ggplot(y[y$category == 'Fashion & Shoes'], aes(x=customer_country, y= average)) +
  geom_col(fill="brown3") + theme_minimal() +
  scale_x_discrete(limits=c("CN","US","FR","JP","KR","BR","RU","GB","SE",
                            "TW","CH","NO","CL","IL","IT","NL","AR","CA","FI","IN")) +
  labs(title="", x="", y= "Average Spending (???)", fill = "Countries",caption = "Source: © Group E | O2") + 
  theme(text = element_text(size=18)) +
  scale_y_continuous(labels=function(x) format(x, nsmall = 2, scientific = FALSE))

fashionshoes2 <- ggplot(y2[y2$category == 'Fashion & Shoes'], aes(x=customer_country, y= total)) + ylab("Total Spending") +
  geom_col(fill = "dodgerblue4") + theme_minimal() +
  scale_x_discrete(limits=c("CN","US","FR","JP","KR","BR","RU","GB","SE","TW","CH","NO","CL","IL","IT","NL","AR","CA","FI","IN")) +
  labs(title="Spending per Country - Fashion & Shoes", x="", y= "Total Spending (???)", fill = "Countries") + 
  theme(text = element_text(size=18)) 

grid.arrange(fashionshoes2, fashionshoes, nrow=2)

#### Plot 7 #####

barsrestaurants <- ggplot(y[y$category == 'Bars & restaurants'], aes(x=customer_country, y= average)) +
  geom_col(fill="brown3") + theme_minimal()+
  scale_x_discrete(limits=c("US","GB","FR","IT","CN","RU","JP","CA","BR","SE","KR","CH","IL","NL","AR","FI","NO","CL","TW","IN")) +
  labs(title="", x="", y= "Average Spending (???)", fill = "Countries",caption = "Source: © Group E | O2") + 
  theme(text = element_text(size=18)) +
  scale_y_continuous(labels=function(x) format(x, nsmall = 2, scientific = FALSE))

barsrestaurants2 <- ggplot(y2[y2$category == 'Bars & restaurants'], aes(x=customer_country, y= total))  +
  geom_col(fill = "dodgerblue4") + theme_minimal()+
  scale_x_discrete(limits=c("US","GB","FR","IT","CN","RU","JP","CA","BR","SE","KR","CH","IL","NL","AR","FI","NO","CL","TW","IN")) +
  labs(title="Spending per Country - Bars & Restaurants", x="", y= "Total Spending (???)", fill = "Countries") + 
  theme(text = element_text(size=18))

grid.arrange(barsrestaurants2, barsrestaurants, nrow=2)

