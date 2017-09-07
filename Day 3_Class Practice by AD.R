##Integer, character ---datatype
##Vector, matrix ----- object

##Data frame--- snapshot of the data, different kind of objects in a single frame

a = c(1:10) ##vector1

b = c(15:24) ##vector2

df <- data.frame(a,b)

View(df)

df[,1]
df$a <- c(21:30)

##EXERCISE 1

install.packages("data.table")

install.packages("datasets")
library("datasets")
women

meanheight <- mean(women$height)
meanweight <- mean(women$weight)

df_women = subset.default(women$height > meanheight && women$weight > meanweight)

result = women[women$height > meanheight & women$weight < meanweight]
View(result)

##EXERCISE 2
install.packages("XML")
library("XML")
?read_html

install.packages("xml2")
library("xml2")

install.packages("htmltools")
library("htmltools")
install.packages("htmlwidgets")
library("htmlwidgets")

help(readHTMLTable,package = "XML") 


city_link = "http://www.worldatlas.com/articles/thebiggest-cities-in-india.html"
city_file = read_html(city_link)
city_table= html_nodes(city_file, "table")
##city_table_final = 

##Exercise 3

normal_income=rnorm(10,mean=250000, sd=75000)
View(normal_income)

cust_id= c(1:100)
income = rnorm(cust_id, mean=250000, sd=75000)

gender = c(rep("M", 60), rep("F",40))

df_exer3 = c(cust_id,income,gender)
View(df_exer3)
######
gender = c(rep("M", 100))
i <- sample(1:100,100,replace = FALSE)
for(q in 1:100)
  {
  ##print(i[q])
  if(gender[i(q)]=="M" && q <= 40){}
    else 
      {gender[i(q)<- c("F")]
}}
gender
table(gender)



