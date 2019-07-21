## Program, from 19.4 Functions, Conditional execution


f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
} ##Check start prefix

f1(c("abc", "abcde", "ad"), "ab")


f2 <- function(x) {
  if (length(x) <= 1) {
    return(NULL)
  }
  x[-length(x)]
}  ##Drop last

f2(1:4)

f3 <- function(x, y) {
  rep(y, length.out = length(x))
} ##Recycle

switch_test <-  function(x, y, op) {
   switch(op,
       plus = x + y,
       minus = x - y,
       times = x * y,
       divide = x / y,
       stop("Unknown op!")
     )
}

op <- 1
switch_test(x,y,op)

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

show_missings(mtcars)

x <- show_missings(mtcars)
class(x)
dim(x)


mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings()



## Chapter 20 Vectors

sample(10) + 100

runif(10) > 0.5

1:10 + 1:2

1:10 + 1:3

tibble(x = 1:4, y = 1:2)

tibble(x = 1:4, y = rep(1:2,2))
tibble(x = 1:4, y = rep(1:2, each = 2))

c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1, 1, 5, 5, 5, 2)]
x[c(-1, -3, -5)]
x[c(-1, 1)]

x <- c(10, 3, NA, 5, 8, 1, NA)

x[!is.na(x)]

x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]


x <- list(1, 2, 3)
x
str(x)

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])
a$a ## ER det samme som
a[["a"]]

x <- 1:10
attr(x, "greeting")

attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"

attributes(x)

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x

attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
typeof(y)
attributes(y)
