## Programing part 3, Chapter 21 Iteration

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

median(df$a)
median(df$b)
median(df$c)
median(df$d)

## Bedre med en loop

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

View(mtcars)
output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output

output <- vector("list", ncol(nycflights13::flights))
names(output) <- names(nycflights13::flights)
for (i in names(nycflights13::flights)) {
  output[[i]] <- class(nycflights13::flights[[i]])
}
output

data("iris")
iris_uniq <- vector("double", ncol(iris))
names(iris_uniq) <- names(iris)
for (i in names(iris)) {
  iris_uniq[i] <- length(unique(iris[[i]]))
}
iris_uniq

n <- 10

mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals

matrix(rnorm(n * length(mu), mean = mu), ncol = n)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

## Dette kan bli erstattet av følgende loop

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

## Chapter 21.4 For loops vs. functionals

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output


col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
col_summary(df, mean)

X <- matrix(rnorm(15), nrow = 5)
X

apply(X, 1, mean)


col_summary2 <- function(df, fun) {
  # create an empty vector which will store whether each
  # column is numeric
  numeric_cols <- vector("logical", length(df))
  # test whether each column is numeric
  for (i in seq_along(df)) {
    numeric_cols[[i]] <- is.numeric(df[[i]])
  }
  # find the indexes of the numeric columns
  idxs <- which(numeric_cols)
  # find the number of numeric columns
  n <- sum(numeric_cols)
  # create a vector to hold the results
  out <- vector("double", n)
  # apply the function only to numeric vectors
  for (i in seq_along(idxs)) {
    out[[i]] <- fun(df[[idxs[[i]]]])
  }
  # name the vector
  names(out) <- names(df)[idxs]
  out
}

df <- tibble(
  X1 = c(1, 2, 3),
  X2 = c("A", "B", "C"),
  X3 = c(0, -1, 5),
  X4 = c(TRUE, FALSE, TRUE)
)
col_summary2(df, mean)

## 21.5 Map functions

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

## Enda enklere måte
models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()


## Exercises 21.5.3
map_dbl(mtcars, mean)
map_chr(nycflights13::flights, typeof)

map_int(iris, function(x) length(unique(x)))
map_int(iris, ~ length(unique(.))) ## Mer kompakt måte, levert av PURRR

map(c(-10, 0, 10, 100), ~ rnorm(n = 10, mean = .)) 

map_lgl(diamonds, is.factor)

x <- split(mtcars, mtcars$cyl)
View(x)

map(x, function(df) lm(mpg ~ wt, data = df))
map(x, ~ lm(mpg ~ wt, data = .))
## Tredje alternativ
run_reg <- function(df) {
  lm(mpg ~ wt, data = df)
}
map(x, run_reg)

## 21.6 Dealing with failure

safe_log <- safely(log)
str(safe_log(10))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

## 21.7 Mapping over multiple arguments

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

## Mer kompirmert og bruke map2
map2(mu, sigma, rnorm, n = 5) %>% str()

n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()

## Mer intuitiv måte å bedre å lese output
args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)

params %>% 
  pmap(rnorm)

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

## 21.8 Walk

x <- list(1, "a", 3)

x %>% 
  walk(print)

## 21.9 Other patterns of for loops

iris %>% 
       keep(is.factor) %>% 
       str()

iris %>% 
  discard(is.factor) %>% 
  str()

x <- list(1:5, letters, list(10))

x %>% 
  some(is_character)

x %>% 
  every(is_vector)

x <- sample(10)
x


x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)

x %>% 
  head_while(~ . > 5)

x %>% 
  tail_while(~ . > 5)

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

View(dfs)

dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)

x <- sample(10)
x
x %>% accumulate(`+`)
