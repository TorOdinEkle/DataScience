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

