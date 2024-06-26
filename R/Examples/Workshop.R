x <- 'hello world'
y = 5.4
state = FALSE
x
print(x)
cat(state)
typeof(state)
sum <- 0
for (i in 1:100) {
  sum <- sum + i
}



for (i in 1:100) {
  if (i %% 2 == 0){
    sum <- sum + i
  }
}

adder <- function(a, b){return(a+ b)}

n <- 100
divisors <- 0
for (i in 1:n) {
  if (n %% i == 0){
    divisors <- divisors + i
    cat(i, "\n")
  }
}
divisors

?log
help("%%")
x <- c(1, 2, 4, 5)
x_char <- c("hello", "how", "are", "you")
x_logical <- c(FALSE, T, F, F)
c(1, 3, 1.4, FALSE, "hello", 1+2i)
typeof(x_char)

x <- c(1, 4, 2, 5, 6)
y <- c(-1, 7, 2, 4, 5)
x + y
x - y
x %% y
x * y
x > y
c(x, y)
y <- c(1, 2, 3)
x + y
x >= 2
#not zero base
x[0]
x[1]
x[-1] #all except first
x[c(1, 2)]
x[c(4, 2)]
length(x)
x[10]
x[c(T, F, F, T, T)]
x[c(F, T)]
x[1:3]
?seq
seq(1, 10, 3)
?sample
sample(1:100, 10) -> x
x
x %% 2 == 0
x[x %% 2 == 0]

#list
l <- list(1, 4, "hello", F, c(1, 2, 4))
l
typeof(l)
typeof(l[[1]])
typeof(l[[3]])
l[1]
typeof(l[1])
names(l)
names(l) <- c("one", "four", "greeting", "logical", "vector")
l
l[[1]]
l$one
l[["one"]]
typeof(l$one)
list(one = 1, four = 4, vector = c(1, 2, 3))
l$vector[1]
#matrix
?matrix
M <- matrix(1:30, 5, 6, byrow = T)
M[1]
M[30]
M[6]
M[1, 2]
M[c(1, 2), c(3, 4)]
M[1,]
M[,2]
M[c(T, F, T, F, T),]
#dataframe
getwd()
setwd()
mobile <- read.csv("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.csv")
View(mobile)
mobile[1, 2]
mobile[1, 1]
mobile[1,]
mobile[c(T, F)]
mobile[,"season"]
typeof(mobile[,"season"])
mobile[["season"]]
View(mobile[mobile$season == "WINTER",])
?order
x <- sample(1:100, 10)
order(x)
#NA <- Not Avalaible
?is.na
x <- sample(1:100, 10)
is.na(x)
x[11] = NA
is.na(x)
#dplyr package isna
mobile[!is.na(mobile$season),] -> mobile_notna
View(mobile_notna[order(mobile_notna$temp),])
nrow(mobile_notna[order(mobile_notna$temp),])
mobile$area = mobile$temp * mobile$hum
min(mobile$temp)
min(mobile$temp, na.rm = T)
mean(mobile$temp, na.rm = T)

#visualization
library(ggplot2)
#grammer of ghraphics
?mpg
ggplot(data = mpg)+
  geom_point(mapping = aes(y=hwy, x=displ))#geom:geometric object aes:statis scaterplot parakonesh

ggplot(data = mpg)+
  geom_point(mapping = aes(y=hwy, x=displ, color = class))#geom:geometric object aes:aesthetics:zibaigarai  statis scaterplot parakonesh

ggplot(data = mobile)+
  geom_point(mapping = aes(y=cnt, x=temp, color = season))#geom:geometric object aes:statis scaterplot parakonesh

ggplot(data = mpg)+
  geom_point(mapping = aes(y=hwy, x=displ), color = "red")    

ggplot(data = mpg)+
  geom_smooth(mapping = aes(y=hwy, x=displ))+
  geom_point(mapping = aes(y=hwy, x=displ))

ggplot(data = mpg)+
  geom_smooth(mapping = aes(y=hwy, x=displ, color = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv, linetype = drv))

ggplot(data = mobile)+
  geom_smooth(mapping = aes(y=cnt, x=temp, color = season))

# ctrl+L clear console

ggplot(data = mobile)+
  geom_smooth(mapping = aes(y=cnt, x=temp, color = season))+
  geom_point(mapping = aes(y=cnt, x=temp, color = season))

ggplot(data = mobile, mapping = aes(y=cnt, x=temp, color = season))+
  geom_point() +
  geom_smooth(mapping = aes(linetype = season))

ggplot()+geom_bar(data = mobile, mapping = aes(x = season))

ggplot(data = mobile) +
  geom_point(mapping = aes(x = temp, y = cnt, color = temp > 10)).

ggplot(data = mobile) +
  geom_point(mapping = aes(x = temp, y = cnt, color = weathersit))

table(mobile$weathersit)

ggplot(data = mobile) +
  geom_point(mapping = aes(x = weathersit, y = temp))

ggplot(data = mobile) +
  geom_col(mapping = aes(x = weathersit, y = temp))

ggplot(mobile) +
  geom_point(mapping = aes(x = temp, y = cnt, color = season, shape =weathersit))

?diamonds
View(diamonds)

ggplot(mobile) +
  geom_bar(mapping = aes(x = season))

ggplot(mobile) +
  stat_count(aes(x = season))

df1 <- data.frame(cut = c("fair", "good", "very good", "premium", "ideal"),
                          count = c(100, 150, 200, 130, 400))

ggplot(df1) +
  geom_bar(mapping = aes(x = cut, y = count), stat = "identity")

?stat_identity
?geom_col

ggplot(df1) +
  geom_col(aes(x = cut, y = count))

ggplot(data = mobile) +
  geom_col(mapping = aes(x = weathersit, y = temp), stat = "mean")

?stat_identity
