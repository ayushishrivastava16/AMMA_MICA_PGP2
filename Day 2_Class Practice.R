##Day 2
##data frame
student <- data.frame(Name=c(LETTERS[1:5]),
                      Age = c(23,22,21,25,20),
                      Maths_marks= c(87,86,83,88,89),
                      Science_marks= c(65,81,78,55,93))

Name1 <- c(LETTERS[1:5])
Age1 <- c(23,22,21,25,20)
Maths_marks1 <- c(87,86,83,88,89)
Science_marks1 <- c(65,81,78,55,93)
student1 <- data.frame(Name1,Age1,Maths_marks1,Science_marks1)

##add column

student$total_marks <- student$Maths_marks + student$Science_marks

##check
student$pct_maths_marks <- round(100*student$Maths_marks/student$total_marks.2)

##drop or remove columns

student2 <- student[,c(2:6)]
student2 <- student[,-1]
student2 <- student1[,-c(2,3)]

rm(student1)

student1 <- student
student1[1,2] <- 70 

student1 <- student[,"Name"]

names(student)
names_student <- names(student)

names(student) <- names_student

vect1 <- seq(1,10,by=2)
vect1[3] <-100

v_col <- names(student)
v_col[3] <- 'New_maths'

student3 <- student
names(student3) <- v_col

##Check if column name with  Value and then change it

names(student3[4])

##transformation


student3$log_age <- log(student3$Age)
student3$exp_age <- exp(student3$Age)
student3$inv_age <- 1/(student3$Age)
student3$sqrt_age <- sqrt(student3$Age)
student3$sqr_age <- student3$Age*student3$Age

student3$exp_age <- exp(student3$Age/mean(student$Age))

class(student$Name)
student$Name <- as.character(student$Name)

nn <- c("xx","23",24,"78.6")
nn.num <- as.numeric(nn)
is.na(nn.num)
table(is.na(nn.num))


## Read Data From Facebook- 232

##readHTMLTable

##Conditional selection
##excluding based on criteria


View(student)
student$Age>=23

s1 <- student$Age>=23
student4 <- student[s1,]
student4 <- student[!s1,]
student5 <- student[student$Science_marks>80 & student$Maths_marks>80, c(1:5)]

?sample
sample_index <- sample(1:nrow(student),3,replace = F)

student[sample_index,]

## select both observations and variables

## 2 more data frames

##Combining vertically - appending & combining
##and horizontally - merging

## Class 6 

missing_value_tbl
missing_value_tbl$missing_age <- is.na(missing_value_tbl$Age)
missing_value_tbl$missing_sex <- is.na(missing_value_tbl$Sex)
missing_value_tbl$missing_amt <- is.na(missing_value_tbl$Spend_Amt)


missing_non <- missing_value_tbl[!missing_value_tbl$missing_age | !missing_value_tbl$missing_sex]


##class 8

windows()



