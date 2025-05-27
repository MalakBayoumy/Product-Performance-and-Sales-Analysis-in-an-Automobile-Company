#Read the data
library(readxl)
data<- read.csv("Auto Sales Data.csv")

#About the data
#Time frame
data$ORDERDATE <- as.Date(data$ORDERDATE,format='%d/%m/%Y')
min(data$ORDERDATE) #2018-01-06
max(data$ORDERDATE) #2020-05-31

#Dimensions
dim(data) #20 * 2747

#Checking for NAs
sum(is.na(data)) #0

#Checking structure of data
is.data.frame(data) #TRUE
str(data)

#Qualitative Data Analysis --- Nada Ahmed Amr

#Load necessary packages
library(dplyr)
library(vcd)
library(formattable)

#Preparation of 4 qualitative variables

#Status
data$STATUS <- as.factor(data$STATUS)
levels(data$STATUS)
sum(is.na(data$STATUS)) #0

#Country
data$COUNTRY <- as.factor((data$COUNTRY))
levels(data$COUNTRY)
sum(is.na(data$COUNTRY)) #0

#Product line
data$PRODUCTLINE <- as.factor((data$PRODUCTLINE))
levels(data$PRODUCTLINE)
sum(is.na(data$PRODUCTLINE)) #0

#Deal size
data$DEALSIZE <- as.factor(data$DEALSIZE)
levels(data$DEALSIZE)
sum(is.na(data$DEALSIZE)) #0

#MERGING (STATUS/ COUNTRY)

#Merge rows of status variable
status <- data$STATUS
table(status)
status[status=="Disputed"] <- "Cancelled"
status[status=="In Process"] <- "On Hold"
status <- droplevels(status, 2)
status <- recode(status, "Cancelled" = "Cancelled/Disputed",  
                 "On Hold" = "In Process/ On Hold",  
                 "Resolved"= "Resolved",  
                 "Shipped"="Shipped")

#Merge rows of countries --> Region
region <- data$COUNTRY
region <- recode(region, "Austria" = "EU Countries", "Belgium"= "EU Countries",  
                 "Denmark"= "EU Countries", "Finland"= "EU Countries",  
                 "France"= "EU Countries", "Germany"= "EU Countries",  
                 "Ireland"= "EU Countries", "Italy"= "EU Countries",  
                 "Spain"= "EU Countries", "Sweden"= "EU Countries",  
                 "Australia" = "Australia", "Canada" = "North America", "Japan"= "Asian Countries",  
                 "Norway"= "Non-EU Countries in Europe", "Philippines"= "Asian Countries", "Singapore"= "Asian Countries",  
                 "Switzerland"= "Non-EU Countries in Europe", "UK"= "Non-EU Countries in Europe",  
                 "USA"="North America")  
sum(is.na(region)) #0
attach(data)
data<- data.frame(status,region,PRODUCTLINE, DEALSIZE, QUANTITYORDERED, PRICEEACH,SALES,DAYS_SINCE_LASTORDER)

#Tables:

# Sample data frame for categorical variables under study
d <- data.frame(status, region, data$PRODUCTLINE, data$DEALSIZE)

#for loop --> univariate tables
# Create a list to store frequency tables
frequency_tables <- list()
relative_frequency_tables <- list()

# Create univariate frequency & relative frequency tables for each variable and store them in the list
for (var in names(d)){
  frequency_table <- table(d[[var]])
  relative_frequency_table <- prop.table(frequency_table)
  
  # Combine frequency and relative frequency tables
  combined_table <- cbind(Frequency =frequency_table, Relative_Frequency =relative_frequency_table)
  
  # Print formatted table
  p <- as.data.frame(combined_table)
  print(formattable(p))
  
  # Store the tables in the list
  frequency_tables[[var]] <- frequency_table
  relative_frequency_tables[[var]] <- relative_frequency_table
}

#Charts

#Bar chart 1 (status)
levels_1 <- levels(status)
bar_chart1 <- barplot(frequency_tables$status, main= "Bar Chart for Status", col = c("#04364A","#176B87", "#64CCC5", "#DAFFFB"),  
                      xlab = "Status", names = levels_1,  
                      ylab = "Frequency of Status" )

#pie 1 (status)
percentage1 <- round(relative_frequency_tables$status,3)*100
labels_1 <- paste(levels_1, percentage1, "%")
pie1 <- pie(frequency_tables$status, labels = labels_1, main = "Pie Chart for Status",  
            col = c("#04364A","#176B87", "#64CCC5", "#DAFFFB"))

#Bar chart 2 (region)
levels_2 <- levels(region)
bar_chart2 <- barplot(frequency_tables$region, main= "Bar Chart for Region", col = c("#04364A","#014D4E", "#176B87", "#64CCC5", "#DAFFFB"),  
                      xlab = "Region", names = levels_2,  
                      ylab = "Frequency of Region" )
#pie 2 (region)
percentage2 <- round(relative_frequency_tables$region,3)*100
labels_2 <- paste(levels_2, percentage2, "%")
pie2 <- pie(frequency_tables$region, labels = labels_2, main = "Pie Chart for Region",  
            col = c("#04364A", "#014D4E","#176B87", "#64CCC5", "#DAFFFB"))

#Bar chart 3 (product line)
levels_3 <- levels(data$PRODUCTLINE)
bar_chart3 <- barplot(frequency_tables$data.PRODUCTLINE, main= "Bar Chart for Product Line", col = c("#04364A","#176B87","#4682B4", "#008B8B", "#64CCC5", "#DAFFFB"),  
                      xlab = "Product Line", names = levels_3,  
                      ylab = "Frequency of Product Line" )

#pie 3 (product line)
percentage3 <- round(relative_frequency_tables$data.PRODUCTLINE,3)*100
labels_3 <- paste(levels_3, percentage3, "%")
pie3 <- pie(frequency_tables$data.PRODUCTLINE, labels = labels_3, main = "Pie Chart for Product Line",  
            col = c("#04364A","#014D4E", "#176B87","#4682B4", "#008B8B", "#64CCC5", "#DAFFFB"))

#Bar chart 4 (deal size)
levels_4 <- levels(data$DEALSIZE)
bar_chart4 <- barplot(frequency_tables$data.DEALSIZE, main= "Bar Chart for Deal Size", col = c("#04364A","#176B87", "#64CCC5"),  
                      xlab = "Deal Size", names = levels_4,  
                      ylab = "Frequency of Deal Size" )

#pie 4 (deal size)
percentage4 <- round(relative_frequency_tables$data.DEALSIZE,3)*100
labels_4 <- paste(levels_4, percentage4, "%")
pie4 <- pie(frequency_tables$data.DEALSIZE, labels = labels_4, main = "Pie Chart for Deal Size",  
            col = c("#04364A", "#176B87", "#64CCC5"))

#Measures of Central Tendency 
#Mode
library(DescTools)
Mode(status) #Shipped, freq = 2541
Mode(data$PRODUCTLINE) #Classic cars, freq =949
Mode(data$DEALSIZE) #Medium, freq = 1349
#Region and country:
Mode(region) #EU countries, freq = 1147
table(data$COUNTRY) #Highest EU country = France, freq = 314
314/949 #proportion (france)/ EU countries= 33%
314/length(data$COUNTRY) #Proportion(France) in general
Mode(data$COUNTRY) #USA, freq = 928
929/998 #proportion(USA)/North America = just over 93%
929/length(data$COUNTRY) # proportion(USA) in general

#Association & correlation
library(vcd)
library(graphics)
library(formattable)

#for loop for contingency tables 

# Get the response variable name
response_var <- "Product Line"  
#List of explanatory variables and response at the end
explanatory_vars <- setdiff(names(d), response_var) #to ensure code is correct
#Create a list to store contingency tables
contingency_tables <- list()

#Create two-way contingency tables for each explanatory variable and store them in the list
for (var in explanatory_vars) {
  contingency_table <- table(d[[var]],d$data.PRODUCTLINE)
  contingency_tables[[var]] <- contingency_table
  print(contingency_table)
  print(assocstats(contingency_table))
  c <- as.data.frame.matrix(addmargins(contingency_table))
  print(formattable(c, align = c("l")))
}

#Mosaics 

#Mosaic 1
mosaicplot(contingency_tables$status, xlab = "Status",  
           ylab = "Product Line",  
           main = "Mosaic Plot for Product Line & Status ", col=c("#04364A","#014D4E", "#176B87","#4682B4", "#008B8B", "#64CCC5", "#DAFFFB"),  
           cex.names = 0.8, las=2)

#Mosaic 2
mosaicplot(contingency_tables$region, xlab = "Region",  
           ylab = "Product Line",  
           main = "Mosaic Plot for Product Line & Region ", col=c("#04364A","#014D4E", "#176B87","#4682B4", "#008B8B", "#64CCC5", "#DAFFFB"),  
           cex.names = 0.8, las=2)

#Mosaic 3 
mosaicplot(contingency_tables$data.DEALSIZE, xlab = "Deal Size",  
           ylab = "Product Line",  
           main = "Mosaic Plot for Product Line & Deal Size ", col=c("#04364A","#014D4E", "#176B87","#4682B4", "#008B8B", "#64CCC5", "#DAFFFB"),  
           cex.names = 0.8, las=2)


#Contingency table & mosaic plot for product line & country
#for comparisons in the appendix
pc <- table(data$COUNTRY, data$PRODUCTLINE)
pct <- addmargins(pc)
k <- as.data.frame.matrix(pct)
formattable(k)

assocstats(pc)
assocstats(contingency_tables$region) #for comparison

mosaicplot(pc, xlab = "Country",  
           ylab = "Product Line",  
           main = "Mosaic Plot for Product Line & Country ", col=c("#04364A","#014D4E", "#176B87","#4682B4", "#008B8B", "#64CCC5", "#DAFFFB"),  
           cex.names = 0.8, las=2)

##Univariate tables for the merged qualitative variables

#Status
##Tables
#Univariate frequency tables
ft1 <- table(data$STATUS)
ft1_with_totals <- addmargins(ft1)
#Univariate relative frequency tables
relative_f1 <- ft1/length(data$STATUS)
relative_f1_rounded <- round(relative_f1, 3)
r1 <- addmargins(relative_f1_rounded)
#Styled format for the table
rd1 <- as.data.frame(r1)
t1<- as.data.frame(ft1_with_totals)
f1 <- cbind(t1,rd1$Freq)
colnames(f1) <- c("Status", "Frequency of Status", "Proportion")
b1 <- formattable(f1, align=c("l"))

#Country
#Univariate frequency tables
ft2 <- table(data$COUNTRY) #16 countries
ft2_with_totals <- addmargins(ft2)
#Univariate relative frequency tables
relative_f2 <- ft2/length(data$COUNTRY)
relative_f2_rounded <- round(relative_f2, 3)
r2 <- addmargins(relative_f2_rounded)
#Styled format for the table
rd2 <- as.data.frame(r2)
t2<- as.data.frame(ft2_with_totals)
f2 <- cbind(t2,rd2$Freq)
colnames(f2) <- c("Country", "Frequency of Country", "Proportion")
b2 <- formattable(f2, align=c("l"))

################################################################################
################################################################################

#Quantitative analysis --- Rana Osama

attach(data)
#Studying outliers
#quantity ordered 
Q1 <- quantile(QUANTITYORDERED, .25)
Q3 <- quantile(QUANTITYORDERED, .75)
IQR <- IQR(QUANTITYORDERED)
quan_ordered <- QUANTITYORDERED> (Q1 - 3*IQR) & QUANTITYORDERED< (Q3 + 3*IQR)
sum(quan_ordered==FALSE) #1 extreme outliers
(sum(quan_ordered == FALSE)/length(QUANTITYORDERED))*100 #0.036% 
######################################
#price each
Q1p <- quantile(PRICEEACH, .25)
Q3p <- quantile(PRICEEACH, .75)
IQRp <- IQR(PRICEEACH)
price <- PRICEEACH> (Q1p - 3*IQRp) & PRICEEACH< (Q3p + 3*IQRp)
sum(price==FALSE) #0 extreme outliers
(sum(price == FALSE)/length(PRICEEACH))*100 #0% 
#########################################
#sales
Q1s <- quantile(SALES, .25)
Q3s <- quantile(SALES, .75)
IQRs <- IQR(SALES)
sales <- SALES> (Q1s - 3*IQRs) & SALES< (Q3s + 3*IQRs)
sum(sales==FALSE) #7 extreme outliers
(sum(sales == FALSE)/length(SALES))*100 #0.25% 
############################################

#days since last order
Q1d <- quantile(DAYS_SINCE_LASTORDER, .25)
Q3d <- quantile(DAYS_SINCE_LASTORDER, .75)
IQRd <- IQR(DAYS_SINCE_LASTORDER)
days <- DAYS_SINCE_LASTORDER> (Q1d - 3*IQRd) & DAYS_SINCE_LASTORDER< (Q3d + 3*IQRd)
sum(days==FALSE) #0 extreme outliers
(sum(days == FALSE)/length(DAYS_SINCE_LASTORDER))*100 #0% 
############################################

#get descriptive measures
library(DescTools)
library(psych)
library(formattable)
attach(data)
################################
#quantity ordered
summary(QUANTITYORDERED)
IQR(QUANTITYORDERED) #interquartile range =16.
sum(abs(QUANTITYORDERED - mean(QUANTITYORDERED)))/length(QUANTITYORDERED) #MAD = 8.176148
describe(QUANTITYORDERED)
Mode(QUANTITYORDERED) #34, not meaningful.

# table 
breaks1 = seq(0, 100,20)
freq_table1 <- Freq(QUANTITYORDERED, breaks = breaks1)
d1 <- as.data.frame(freq_table1)
colnames(d1) <-c("Interval", "Frequency", "Proportion", "Cumulative Frequency", "Cumulative proportion" )
q1 <- formattable(d1)
q1

#price of each order
summary(PRICEEACH)
IQR(PRICEEACH) #interquartile range =58.355.
sum(abs(PRICEEACH - mean(PRICEEACH)))/length(PRICEEACH) #MAD = 33.89484.
describe(PRICEEACH)
Mode(PRICEEACH) #59.87 and 96.34, not meaningful.

# table 
breaks2 = seq(0, 300,50)
freq_table2 <- Freq(PRICEEACH, breaks = breaks2)
d2 <- as.data.frame(freq_table2)
colnames(d2) <-c("Interval", "Frequency", "Proportion", "Cumulative Frequency", "Cumulative proportion" )
q2 <- formattable(d2)
q2


#sales
summary(SALES)
IQR(SALES) #interquartile range =2298.745.
sum(abs(SALES - mean(SALES)))/length(SALES) #MAD = 1426.213.
describe(SALES)
Mode(SALES) #not meaningful because there are morethan 2 modes.

# table 
breaks3 = seq(450, 14450,2000)
freq_table3 <- Freq(SALES, breaks = breaks3)
d3 <- as.data.frame(freq_table3)
colnames(d3) <-c("Interval", "Frequency", "Proportion", "Cumulative Frequency", "Cumulative proportion" )
q3 <- formattable(d3)
q3

#days since last order
summary(DAYS_SINCE_LASTORDER)
IQR(DAYS_SINCE_LASTORDER) #interquartile range =1359.5.
sum(abs(DAYS_SINCE_LASTORDER - mean(DAYS_SINCE_LASTORDER)))/length(DAYS_SINCE_LASTORDER) #MAD = 699.9633.
describe(DAYS_SINCE_LASTORDER)
Mode(DAYS_SINCE_LASTORDER) # 2207.

# table 
breaks4 = seq(0,4000 ,500)
freq_table4 <- Freq(DAYS_SINCE_LASTORDER, breaks = breaks4)
d4 <- as.data.frame(freq_table4)
colnames(d4) <-c("Interval", "Frequency", "Proportion", "Cumulative Frequency", "Cumulative proportion" )
q4 <- formattable(d4)
q4

#plots
#histograms
#quantity ordered
min_freq1 <- 0
max_freq1 <- 500
ylim1 <- c(min_freq1, max_freq1)
hist(QUANTITYORDERED, main = "Quantity Ordered Distribution", xlab = "Quantity Ordered", ylab = "Frequency", col = "#04364A", ylim = ylim1)

#price of each order
min_freq2 <- 0
max_freq2 <- 600
ylim2 <- c(min_freq2, max_freq2)
hist(PRICEEACH, main = "Price Of Each Order Distribution", xlab = "Price Of Each Order", ylab = "Frequency", col = "#176B87", ylim = ylim2)

#sales
min_freq3 <- 0
max_freq3 <-1000        
ylim3 <- c(min_freq3, max_freq3)
hist(SALES, main = "Sales Distribution", xlab = "Sales", ylab = "Frequency", col = "#64CCC5", ylim = ylim3)

#days since last order
min_freq4 <- 0
max_freq4 <- 500
ylim4 <- c(min_freq4, max_freq4)
hist(DAYS_SINCE_LASTORDER, main = "Days Since Last Order Distribution", xlab = "Days Since Last Order", ylab = "Frequency", col = "#DAFFFB", ylim = ylim4)


#boxplots
# quantity ordered
boxplot(QUANTITYORDERED, main="Distribution of Quantity Ordered", ylab="Quantity Ordered", col = "#04364A")
# price of each order
boxplot(PRICEEACH, main="Distribution of Price Of Each Order", ylab="Price Of Each Order", col = "#176B87")
#sales
boxplot(SALES, main="Distribution of Sales", ylab="Sales", col = "#64CCC5")
#days since last order
boxplot(DAYS_SINCE_LASTORDER, main="Distribution of Days Since Last Order", ylab="Days Since Last Order", col = "#DAFFFB")


#Density Plots
library(ggplot2)
#quantity ordered
ggplot(data, aes(x = QUANTITYORDERED)) +
  geom_density(fill = "#04364A", color = "grey") +
  labs(title = "Density plot of Quantity Ordered", x = " Quantity Ordered", y = "Density") +
  scale_x_continuous(limits = c(0, 100))

#price of each order
ggplot(data, aes(x = PRICEEACH)) +
  geom_density(fill = "#176B87", color = "grey") +
  labs(title = "Density plot of Price Of Each Order", x = " Price Of Each Order", y = "Density") +
  scale_x_continuous(limits = c(0, 300))

#sales
ggplot(data, aes(x = SALES)) +
  geom_density(fill = "#64CCC5", color = "grey") +
  labs(title = "Density plot of Sales", x = " Sales", y = "Density") +
  scale_x_continuous(limits = c(0, 14000))

#days since last order
ggplot(data, aes(x = DAYS_SINCE_LASTORDER)) +
  geom_density(fill = "#DAFFFB", color = "grey") +
  labs(title = "Density plot of Days Since Last Order", x = " Days Since Last Order", y = "Density") +
  scale_x_continuous(limits = c(-1000, 4500))


#scatter plots and correlation matrix
correlation_matrix <- cor(data[,1:3])
correlation_matrix
plot(data[,1:3], main= "scatter plots of each two variables", col= "#176B87")

################################################################################
################################################################################

#Qualitative x Quantitative Data Analysis --- Nada Ahmed Amr

#Sales analysis
library(Hmisc)
library(formattable)
library(broom)
library(stats)
library(plotly)
library(tidyr)
library(dplyr)
my_colors <- c("#04364A","#014D4E", "#176B87","#4682B4", "#008B8B", "#64CCC5", "#DAFFFB")

################################################################################
#Sales & status
#Summary statistics for sales for each status
######DOESN'T RUN AFTER UPDATE##########
m <- by(data$SALES, status, describe)
class(m) #by --> list ---> call each, convert to data frame, format --> bind by row
formatted_tables1 <- lapply(as.list(m), function(x) {
  formatted_data <- formattable::formattable(as.data.frame(x))
  return(formatted_data)
})
combined_table1 <- do.call(rbind, formatted_tables1)[,-1]
##################################################
#####ALTERNATIVE######
result1 <- data %>%
  group_by(status) %>%
  summarize(
    Mean = mean(SALES),
    SD = sd(SALES),
    Min = min(SALES),
    Q1 = quantile(SALES, 0.25),
    Median = median(SALES),
    Q3 = quantile(SALES, 0.75),
    Max = max(SALES)
  )

#Formatting the result using
formatted_result1 <- formattable::formattable(as.data.frame(result1), align = c("l"))
####################
#Box plot sales vs status
boxplot(data$SALES ~ status, main = "Sales by Status Box Plot",  
        ylab = "Sales", col="#64CCC5")

#Pie chart for sales by status
pie_status <- table(status)
pie_chart_status <- plot_ly(labels = names(pie_status), values = pie_status, type = "pie",  
                            marker = list(colors = my_colors)) %>%
  layout(title = "Sales Distribution by Status")

################################################################################
#Sales & region
#Summary statistics for sales within each region
mm <- by(data$SALES, region, describe)
formatted_tables2 <- lapply(as.list(mm), function(x) {
  formatted_data <- formattable::formattable(as.data.frame(x))
  return(formatted_data)
})
combined_table2 <- do.call(rbind, formatted_tables2)[,-1]
####################
result2 <- data %>%
  group_by(region) %>%
  summarize(
    Mean = mean(SALES),
    SD = sd(SALES),
    Min = min(SALES),
    Q1 = quantile(SALES, 0.25),
    Median = median(SALES),
    Q3 = quantile(SALES, 0.75),
    Max = max(SALES)
  )

#Formatting the result 
formatted_result2 <- formattable::formattable(as.data.frame(result2), align = c("l"))
###################
#Box plot sales vs region
boxplot(data$SALES ~ region, main = "Sales by Region Box Plot",  
        ylab = "Sales", xlab = "Region", col="#64CCC5")

#Pie chart for sales by region
pie_region <- table(region)
pie_chart_region <- plot_ly(labels = names(pie_region), values = pie_region, type = "pie",  
                            marker = list(colors = my_colors), textinfo = "label+percent") %>%
  layout(title = "Sales Distribution by Region")

################################################################################
#Sales & product line
#Summary statistics for sales for each product line
mmm <- by(data$SALES, data$PRODUCTLINE, describe)
formatted_tables3 <- lapply(as.list(mmm), function(x) {
  formatted_data <- formattable::formattable(as.data.frame(x))
  return(formatted_data)
})
combined_table3 <- do.call(rbind, formatted_tables3)[,-1]
##################
result3 <- data %>%
  group_by(PRODUCTLINE) %>%
  summarize(
    Mean = mean(SALES),
    SD = sd(SALES),
    Min = min(SALES),
    Q1 = quantile(SALES, 0.25),
    Median = median(SALES),
    Q3 = quantile(SALES, 0.75),
    Max = max(SALES)
  )

#Formatting the result 
formatted_result3 <- formattable::formattable(as.data.frame(result3), align = c("l"))
###################
#Box plot sales vs product line
boxplot(data$SALES ~ data$PRODUCTLINE, main = "Sales by Product Line Box Plot",  
        ylab = "Sales", xlab = "Product Line", col="#64CCC5")

#Pie chart for sales by product line
pie_line <- table(data$PRODUCTLINE)
pie_chart_line <- plot_ly(labels = names(pie_line), values = pie_line, type = "pie",  
                          marker = list(colors = my_colors), textinfo = "label+percent") %>%
  layout(title = "Sales Distribution by Product Line")

################################################################################
#Sales & deal size
#Summary statistics for sales for each product line
mmmm <- by(data$SALES, data$DEALSIZE, describe)
formatted_tables4 <- lapply(as.list(mmmm), function(x) {
  formatted_data <- formattable::formattable(as.data.frame(x))
  return(formatted_data)
})
combined_table4 <- do.call(rbind, formatted_tables4)[,-1]
###################
result4 <- data %>%
  group_by(DEALSIZE) %>%
  summarize(
    Mean = mean(SALES),
    SD = sd(SALES),
    Min = min(SALES),
    Q1 = quantile(SALES, 0.25),
    Median = median(SALES),
    Q3 = quantile(SALES, 0.75),
    Max = max(SALES)
  )

#Formatting the result 
formatted_result4 <- formattable::formattable(as.data.frame(result4), align = c("l"))
###################
#Box plot sales vs deal size
boxplot(data$SALES ~ data$DEALSIZE, main = "Sales by Deal Size Box Plot",  
        ylab = "Sales",xlab = "Deal Size", col="#64CCC5")

#Pie chart for sales by deal size
pie_size <- table(data$DEALSIZE)
pie_chart_size <- plot_ly(labels = names(pie_size), values = pie_size, type = "pie",  
                          marker = list(colors = my_colors), textinfo = "label+percent+value") %>%
  layout(title = "Sales Distribution by Deal Size")

################################################################################
################################################################################

#Regression Analysis --- Malak Mohamed Medhat Bayoumy
is.data.frame(data) # true #
str(data)

#qual var: status,region,product line, deal size # already transformed into factors#
#quant var: sales,quant oredered, price each, days since last order

## Testing The Normality of sales 
summary(data)
par(mfrow=c(1, 3)) 
hist(data$SALES, main= "Histogram of Sales", xlab="Sales", ylab= "Frequency",col = "#04364A")
plot(density(data$SALES),main= "Density Plot of Sales",col="#04364A")
qqnorm(data$SALES,main= "Normal QQ plot of Sales",col="#04364A")
ks.test(data$SALES,"pnorm",  mean(data$SALES), sd(data$SALES)) # reject ho #

## taking log sales to the base e 
data$SALES<- log(data$SALES,base = exp(1))
par(mfrow=c(1, 3)) 
hist(data$SALES, main= "Histogram of Log Sales", xlab=" Log Sales", ylab= "Frequency",col="#014D4E")
plot(density(data$SALES),main= "Density Plot of Log Sales",col="#014D4E")
qqnorm(data$SALES,main= "Normal QQ plot of Log Sales",col="#014D4E")
ks.test(data$SALES,"pnorm",  mean(data$SALES), sd(data$SALES)) # don't reject ho #

#scatter plots for testing multicollinearity between quant var 
correlation_matrix <- cor(data[,5:8])
correlation_matrix
pairs(data[, c(5,6,7,8)], main = "Scatterplot Matrix of the Data",col="#176B87")
library(kableExtra)
library(formattable)
df <- as.data.frame(correlation_matrix)
formattable(df, caption  = " Figure 1:  Correlation Table ")

##
# Chi-square test for qual var to check whether there is an association or not
chisq.test(data$status, data$region)    
chisq.test(data$status, data$PRODUCTLINE) 
chisq.test(data$status, data$DEALSIZE)    ### p-value = 0.5877 ###
chisq.test(data$region, data$PRODUCTLINE)
chisq.test(data$region, data$DEALSIZE)    ### p-value = 0.5148 ###
chisq.test(data$PRODUCTLINE, data$DEALSIZE) 

# Cramér's V for qual var to measure degree of association 

install.packages("lsr")
library(lsr)
cramersV(data$status, data$region)  
cramersV(data$status, data$PRODUCTLINE) 
cramersV(data$status, data$DEALSIZE)  
cramersV(data$region, data$PRODUCTLINE) 
cramersV(data$region, data$DEALSIZE)  
cramersV(data$PRODUCTLINE, data$DEALSIZE)  

##

#spearman's Rank Correlation for both qualitative and quantitative variables

cor(rank(data$status), data$SALES, method = "spearman")
cor(rank(data$status), data$QUANTITYORDERED, method = "spearman")
cor(rank(data$status), data$PRICEEACH, method = "spearman")
cor(rank(data$status), data$DAYS_SINCE_LASTORDER, method = "spearman")

cor(rank(data$region), data$SALES, method = "spearman")
cor(rank(data$region), data$QUANTITYORDERED, method = "spearman")
cor(rank(data$region), data$PRICEEACH, method = "spearman")
cor(rank(data$region), data$DAYS_SINCE_LASTORDER, method = "spearman")


cor(rank(data$PRODUCTLINE), data$SALES, method = "spearman")
cor(rank(data$PRODUCTLINE), data$QUANTITYORDERED, method = "spearman")
cor(rank(data$PRODUCTLINE), data$PRICEEACH, method = "spearman")
cor(rank(data$PRODUCTLINE), data$DAYS_SINCE_LASTORDER, method = "spearman")


cor(rank(data$DEALSIZE), data$SALES, method = "spearman")
cor(rank(data$DEALSIZE), data$QUANTITYORDERED, method = "spearman")
cor(rank(data$DEALSIZE), data$PRICEEACH, method = "spearman")
cor(rank(data$DEALSIZE), data$DAYS_SINCE_LASTORDER, method = "spearman")
#####
# dealing with the outliers #
hist(data$QUANTITYORDERED,main= "Histogram of Quantity Ordered", xlab=" Quantity Ordered", ylab= "Frequency",col="#4682B4") # there are some outliers #
hist(data$PRICEEACH,main= "Histogram of Price Each", xlab=" Price Each ", ylab= "Frequency",col="#4682B4") # no significant outliers #
hist(data$DAYS_SINCE_LASTORDER,main= "Histogram of Days Since Last Order", xlab=" Days Since Last Order", ylab= "Frequency",col="#4682B4") # no significant outliers #

Q1 = quantile(data$QUANTITYORDERED, 0.25); Q3 = quantile(data$QUANTITYORDERED, 0.75); IQR = IQR(data$QUANTITYORDERED)
outlier_ind_quant <- which((data$QUANTITYORDERED< (Q1 - 1.5 * IQR)) | (data$QUANTITYORDERED > (Q3 + 1.5 * IQR)))
clean_data1 <- data[-outlier_ind_quant, ]

hist(clean_data1$QUANTITYORDERED, main = ("Histogram Of Quantity Ordered"), xlab=" Quantity Ordered", ylab= "Frequency",col="#4682B4") # no significant outliers #


# splitting the data # 
# Install and load the caTools package
install.packages("caTools")
library(caTools)
# Set a seed for reproducibility
set.seed(123)
# Create a binary split of 70% training and 30% testing
split <- sample.split(clean_data1$SALES, SplitRatio = 0.7)
# Subset the data into training and testing sets
train_data <- subset(clean_data1, split == TRUE)
test_data <- subset(clean_data1, split == FALSE)


# fitting the full model on trained data
linear_model <- lm(train_data$SALES ~ ., data = train_data)
summary(linear_model) # 95.79% of adj r squared #
anova(linear_model)
car::vif(linear_model)

# quantity ordered is quadratic
quad_model1<-lm(train_data$SALES ~ train_data$status+train_data$PRODUCTLINE+train_data$DEALSIZE+train_data$region+train_data$QUANTITYORDERED+I(train_data$QUANTITYORDERED^2)+train_data$PRICEEACH+train_data$DAYS_SINCE_LASTORDER, data = train_data)
summary(quad_model1) # 96.29 % adj r squarred #
anova(quad_model1)

# quantity ordered and price each are quadratic 
quad_model2<-lm(train_data$SALES ~ train_data$status+train_data$PRODUCTLINE+train_data$DEALSIZE+train_data$region+train_data$QUANTITYORDERED+I(train_data$QUANTITYORDERED^2)+train_data$PRICEEACH+I(train_data$PRICEEACH^2)+train_data$DAYS_SINCE_LASTORDER, data = train_data)
summary(quad_model2) # 99.31# adj r squared 
anova(quad_model2)
car::vif(quad_model2)
#

anova(linear_model,quad_model1) # quad model1 containing quantity ordered as quadratic is better #
anova(quad_model1,quad_model2) # quad model2 containing quantity ordered and price each as quadratic is better #
anova(linear_model,quad_model2)# quad model2 containing quantity ordered and price each as quadratic is better than the linear model #
#####
step<- step(quad_model2,direction = "both")
summary(step)
anova(step)
car::vif(step)
plot(step,col="#04364A")

# Testing Assumptions after fitting the step model #
library(lmtest)
# 1- Normality 
# Histogram 
dev.off()
hist(residuals(step), main="Histogram of The Residuals of the Fitted Model(Step)",col="#176B87")
hist(residuals(step), 
     main="Histogram of The Residuals of the Fitted Model (Step)
     compared to Normal Curve",
     probability = TRUE,
     ylim = range(density(residuals(step))$y),col="#176B87")  # Adjust y-axis limits

lines(density(residuals(step)), col = "blue", lwd = 2) # nearly normal #

# Normal QQ Plot 
qqnorm(residuals(step),col="#176B87") #normality of residuals
qqline(residuals(step), col = "black")  # Points are mostly close to the normal line 

# 2) homoscedasticity assumptions using Residuals vs. Fitted Plot
plot(step, which = 1,col="#04364A")#Random scatter around zero, with consistent spread across the range of fitted values.

# 3) Autocorrelation using Durbin-Watson test:
plot(step, which = 2,col="#04364A")  # Residuals vs. Order Plot, specifically designed to assess the independence of residuals. Random scatter of points around zero, without any discernible trend or pattern.
dwtest(step) # There is no autocorrelation.

# 4) Multicollinearity using VIF 
library(car)
vif(step) # No problem with multicollinearity.
library(formattable)
tab<- as.data.frame(vif(step))
formattable(tab, caption  = " Figure 2:  VIF Table ")

#Testing Set:
# Predict using the testing set
is.data.frame(test_data) # True 
predictions <- predict(object = step, newdata = test_data)

# Assess model performance (e.g., using mean squared error for regression)
mean((test_data$SALES - predictions)^2) # mse 
mean(abs(test_data$SALES - predictions)) #MAE measures the average absolute difference between actual and predicted values.

################################################################################
################################################################################

#machine learning --- Rana Osama
#knn
install.packages("e1071")
install.packages("caTools")
install.packages("class")
#get optimal k value
library(e1071)
library(class)
library(caTools)
library(dplyr)
train_data$DEALSIZE <-recode(train_data$DEALSIZE, "Small"=0L,"Medium"=1L, 'Large'= 2L)
test_data$DEALSIZE <- recode(test_data$DEALSIZE,"Small"=0L,"Medium"=1L, 'Large'= 2L)
#Feature Scaling
train_scale1 <- scale(train_data[, 4])
test_scale1 <- scale(test_data[, 4])

misClassError <- c()
for(k in 1:10){classify_knn <- knn(train = train_scale1,
                                   test = test_scale1,
                                   cl = train_data$DEALSIZE,
                                   k = k);
misClassError[k] <- mean(classify_knn != test_data$DEALSIZE)}
which.min(misClassError)
min(misClassError) 

classify_knn <- knn(train = train_scale1,
                    test = test_scale1,
                    cl = train_data$DEALSIZE,
                    k = 1)

classify_knn
