# Advanced R - Hadley Wickham

#### 2 - DATA STRUCTURES ####
#### Vectors ####
# check if object is a vector
a <- c(1, 4,"klaudia") #All elements of an atomic vector must be the same type, so when you attempt to combine different types they will be coerced to the most flexible type. Types from least to most flexible are: logical, integer, double, and character.
str(a)
is.vector(a) #is.vector returns TRUE if x is a vector of the specified mode having no attributes other than names. It returns FALSE otherwise.
# A vector can have attributes so it's not a good way to check if something is a vector! Jesli zrobimy liste to nam wyjdzie, ze ona jest wektorem

# use is.atomic(a) 
# Being recursive for a type of object in R means that you can have this object holding its own type as an entry. Atomic is the opposite. Vectors are atomic and lists are recursive as you can easily check.

l <- list(1,4,"Klaudia")
str(l)
is.vector(l)
is.atomic(l) #lista nie jest atomic, czyli nie jest "bazowym" elementem, a tylko takie bazowe elementy moga byc przechowywane w wektorze
is.atomic(a)
is.atomic(NULL) #UWAGA NA TO!
# Wiec wg Wickhama najbezpieczniejsze sa: is.double, is.integer, is.character, is.logical (is.numeric tez moze zaskoczyc)

# There are four common types of atomic vectors: logical, integer, double (often called numeric), and character.

dbl_var <- c(1, 2.5, 4.5)
# With the L suffix, you get an integer rather than a double
int_var <- c(1L, 6L, 10L)
# Use TRUE and FALSE (or T and F) to create logical vectors
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("these are", "some strings")

# Lists are sometimes called recursive vectors, because a list can contain other lists. This makes them fundamentally different from atomic vectors.

x <- list(list(list(list())))
str(x)
is.recursive(x)
is.atomic(x)

vec <- c(1, c(2, c(3, 4)))
is.recursive(vec)
is.atomic(vec)

# You can turn a list into an atomic vector with unlist(). If the elements of a list have different types, unlist() uses the same coercion rules as c().

# Kazdy data frame i model jest lista!

is.list(mtcars)

mod <- lm(mpg ~ wt, data = mtcars)
is.list(mod)

# c() will combine several lists into one.
c(list(1,2), list(3,4))


#### Attributes ####

#  Attributes are a named list. Attributes can be accessed individually with attr() or all at once (as a list) with attributes().
y <- 1:10
attr(y, "my_attribute") <- "This is a vector"
attr(y, "my_attribute")
attributes(y)


# You can create a new vector without names using unname(x), or remove names in place with names(x) <- NULL

#### FACTORS ####

data1 <- read.csv2("missing_number.csv")
str(data1) #pierwsza zmienna wczytala sie jako factor, poniewaz w jednym miejscu byla kropka, druga juz sie wczytala jako integer z pustym polem.

# Jak zamienic pierwsza kolumne na integer?
as.double(data1$Variable1) #INCORRECT!!!!
data1$Variable1 <- as.double(as.character(data1$Variable1)) # factor najpierw zmieniamy na character, a potem na liczbe!
str(data1)

# Jesli wiemy, ze mamy braki danych jako "." to mozemy od razu to uwzglednic przy wczytywaniu
data1.1 <- read.csv2("missing_number.csv", na.strings = ".")
str(data1.1)

# Unfortunately, most data loading functions in R automatically convert character vectors to factors. This is suboptimal, because there’s no way for those functions to know the set of all possible levels or their optimal order. Instead, use the argument stringsAsFactors = FALSE to suppress this behaviour, and then manually convert character vectors to factors using your knowledge of the data.

# While factors look (and often behave) like character vectors, they are actually integers. Be careful when treating them like strings. Some string methods (like gsub() and grepl()) will coerce factors to strings, while others (like nchar()) will throw an error, and still others (like c()) will use the underlying integer values. For this reason, it’s usually best to explicitly convert factors to character vectors if you need string-like behaviour. In early versions of R, there was a memory advantage to using factors instead of character vectors, but this is no longer the case.



# ewentualnie po prostu ustawic strings as factors = F, wtedy bedzie trzeba tylko zmienic na liczbe
data.1.1.1 <- read.csv2("missing_number.csv", stringsAsFactors = F)
str(data.1.1.1)
data.1.1.1$Variable1 <- as.integer(data.1.1.1$Variable1)
str(data.1.1.1)


#### MATRICES/ARRAYS ####
# matrices are two-dimensional, arrays multi-dimensional


#### DATA FRAMES ####
# Beware data.frame()’s default behaviour which turns strings into factors.

df <- data.frame(x = 1:3, 
                 y = c("a", "b", "c"))
str(df)

# Use stringsAsFactors = FALSE to suppress this behaviour:

df <- data.frame(x = 1:3,
                 y = c("a", "b", "c"),
                 stringsAsFactors = FALSE)
str(df)

# checking if an object is data frame

typeof(df) #list - so not appropriate
class(df) #"data.frame" - ok
is.data.frame(df) #TRUE - ok

# When combining column-wise, the number of rows must match, but row names are ignored. When combining row-wise, both the number and names of columns must match. Use plyr::rbind.fill() to combine data frames that don’t have the same columns.

# It’s a common mistake to try and create a data frame by cbind()ing vectors together. This doesn’t work because cbind() will create a matrix unless one of the arguments is already a data frame. Instead use data.frame() directly:

good <- data.frame(a = 1:2, b = c("a", "b"),
                   stringsAsFactors = FALSE)
str(good)

#### 3 - SUBSETTING ####

x <- c(2.1, 4.2, 3.3, 5.4)

# ordered elements
x[ordered(x)]

# Duplicated indices yield duplicated values
x[c(1, 1)]


# all elements but first and second
x[-c(1, 2)]


x[c(TRUE, TRUE, FALSE, FALSE)]

x[x>3]

# If the logical vector is shorter than the vector being subsetted, it will be recycled to be the same length.

x[c(TRUE, FALSE)]
# Equivalent to
x[c(TRUE, FALSE, TRUE, FALSE)]

x[] # returns the original vector


# if the vector is named there are also other options

(y <- setNames(x, letters[1:4])) #setNames() is a convenience function that sets the names on an object and returns the object. It is most useful at the end of a function definition where one is creating the object to be returned and would prefer not to store it under a name just so the names can be assigned.

y[c("d", "c", "a")]

(vals <- outer(1:5, 1:5, FUN = "paste", sep = ","))


# !!!
# Omitting drop = FALSE when subsetting matrices and data frames is one of the most common sources of programming errors. (It will work for your test cases, but then someone will pass in a single column data frame and it will fail in an unexpected and unclear way.)

# Subsetting lists
li <- list(a = "cos", b = 1, c = TRUE)
li[1] #extracts the elemnt of the list with preserving the original structure
li[[1]] #extracts the elemnt of the list WITHOUT preserving the original

# Subsetting factors
z <- factor(c("a", "b"))
z[1]
z[1, drop = T] #omits any unused levels

# Subsetting matrices and data frames!!!
a <- matrix(1:4, nrow = 2)
a
a[1, , drop = FALSE] #keeps the data frame structure
a[1] #drops the data frame structure

df <- data.frame(a = 1:2, b = 1:2)
df[1]
df[,1]
df[,1, drop = F]
df["a"]
df[,"a"]
df[,"a", drop = F]


# $ is a shorthand operator, where x$y is equivalent to x[["y", exact = FALSE]]. It’s often used to access variables in a data frame, as in mtcars$cyl or diamonds$carat.
# One common mistake with $ is to try and use it when you have the name of a column stored in a variable:

var <- "cyl"
mtcars$var
## NULL
# Instead use [[
mtcars[[var]]


x <- list(abc = 1)
x$a #$ does partial matching so watch out!
x[["a"]]

# removing a component from a list
x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)

# adding a NULL component to the list
y <- list(a = 1)
y["b"] <- list(NULL)
str(y)


# character matching
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
unname(lookup[x])


grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
    grade = 3:1,
    desc = c("Excellent", "Good", "Poor"),
    fail = c(F, F, T)
)

id <- match(grades, info$grade)
info[id, ]


# random samples
df <- data.frame(d = rep(1:3, each = 2), a = 6:1, r = letters[1:6])
df

# select 3 random rows
df[sample(nrow(df), 3), ]

# Select 6 bootstrap replicates
df2 <- df[sample(nrow(df), 6, rep = T), ]

# order by x
df2[order(df2$d), ]

# order columns alphabetically!
df2[, order(names(df2))]

# More concise, but less flexible, functions are available for sorting vectors, sort(), and data frames, plyr::arrange()

# Expanding aggregated counts

df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
df

reps <- rep(1:nrow(df), df$n)
df[reps,]

# If you know the columns you don’t want, use set operations to work out which colums to keep:
df[setdiff(names(df), "z")] #all columns but z


# SET OPERATORS
union(c("a", "b", 5), c(3,4,5)) # a, b, 5, 3, 4 - all elemnts of two vectors without duplicates
intersect(c("a", "b", 5), c(3,4,5)) #5 -what the vectors have in common
setdiff(c("a", "b", 5), c(3,4,5)) #"a", "b" - which elements of the first vector are not present in the second vector
setdiff(c(3,4,5), c("a", "b", 5)) # 3,4
setequal(c("a", "b", 5), c(3,4,5)) #FALSE - tests if two vectors are equal
setequal(c("a", "b", 5), c("a", "b",5)) #TRUE


(x <- c(10,9,8,7,6,5,4,3,2,1) < 4) #logical vector
which(x) #shows which elements of x are TRUE


#### FUNCTIONS ####

# to see what inide the functions...
formals(which)
body(which)
environment(which)

rm(a)
j <- function() {
    if (!exists("a")) {
        a <- 1
    } else {
        a <- a + 1
    }
    a
}
j() #it always returns a because it does not save a

f <- function() x + 1 #no a good practice, function is not self-contained, depends on external x
codetools::findGlobals(f)

f <- function(x) x + 1 
codetools::findGlobals(f)

# when it is useful to define simple functions
multiply <- function(x, y) x*y

sapply(1:10, multiply, 3)

sapply(1:10, "*", 3) #the same result

# always take the second element of a list
x <- list(1:3, 4:9, 10:12)
sapply(x, "[", 2)
# equivalent to
sapply(x, function(x) x[2])


# using do.call()

#let's say we have a list of arguments
args <-  list(1:10, na.rm = TRUE)

# if we want to pass this list to a function we can use do.call()
do.call(mean, args)

# my example
my.args <- list(1:6, nrow = 2, ncol = 3)
(my.matrix <- do.call(matrix, my.args))

# You can determine if an argument was supplied or not with the missing() function.
i <- function(a, b) {
    c(missing(a), missing(b))
}
i()
i(a = 1)
i(b = 2)
i(1, 2)

# Hadley usually sets the default value to NULL and uses is.null() to check if the argument was supplied.

# Lazy evaluation - By default, R function arguments are lazy — they’re only evaluated if they’re actually used.

# Using ...
f <- function(...) {
    names(list(...))
}
f(a = 1, b = 2)

# Using ... comes at a price — any misspelled arguments will not raise an error, and any arguments after ... must be fully named. This makes it easy for typos to go unnoticed:
sum(1, 2, NA, na.mr = TRUE) #NA
sum(1, 2, NA, na.rm = TRUE) #3


# Most functions in R are “prefix” operators: the name of the function comes before the arguments. You can also create infix functions where the function name comes in between its arguments, like + or -. All user-created infix functions must start and end with %. R comes with the following infix functions predefined: %%, %*%, %/%, %in%, %o%, %x%.

# For example, we could create a new operator that pastes together strings:
`%+%` <- function(a, b) paste0(a, b)
"new" %+% " string"

10 %% 3 #reszta z dzielenia
10 %*% 3 #mnozenie macierzy
10 %/% 3 #integer division (wynik dzielenia bez reszty)

# we can call function arguments by abbreviations, if they are not ambiguous

# As well as returning a value, functions can set up other triggers to occur when the function is finished using on.exit(). This is often used as a way to guarantee that changes to the global state are restored when the function exits. The code in on.exit() is run regardless of how the function exits, whether with an explicit (early) return, an error, or simply reaching the end of the function body. (FOR EXAMPLE IF WE WANT TO RESTORE THE OLD WORKING DIRECTORY)
# Caution: If you’re using multiple on.exit() calls within a function, make sure to set add = TRUE. Unfortunately, the default in on.exit() is add = FALSE, so that every time you run it, it overwrites existing exit expressions. Because of the way on.exit() is implemented, it’s not possible to create a variant with add = TRUE, so you must be careful when using it.

#### MEMORY ####

# To understand memory usage in R, we will start with pryr::object_size(). This function tells you how many bytes of memory an object occupies:

library(pryr) #pryr package = Useful tools to pry back the covers of R and understand the language at a deeper level.
object_size(1:10)
#> 88 B
object_size(mean)
#> 1.13 kB
object_size(mtcars)
#> 6.74 kB

# checking how much memory integers take
sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
     type = "s")

seq_len(3)

# Every length 0 vector occupies 40 bytes of memory:
object_size(numeric())
bject_size(logical())
bject_size(list())


# While object_size() tells you the size of a single object, pryr::mem_used() tells you the total size of all objects in memory:

mem_used()

# mem_change() builds on top of mem_used() to tell you how memory changes during code execution. Positive numbers represent an increase in the memory used by R, and negative numbers represent a decrease.

mem_change(x <- 1:1e6)
mem_change(NULL) #very little memory, You can ignore anything smaller than a couple kB.

# more informative view on memory usage

#devtools::install_github("hadley/lineprof")
library(lineprof)
source("samplefunction.R")

samplelist <- lineprof(samplefunction("missing_number.csv"))
shine(samplelist) #slaba funkcja?

# Loops

x <- data.frame(matrix(runif(100 * 1e4), ncol = 100))
medians <- vapply(x, median, numeric(1))

for(i in seq_along(medians)) {
    x[, i] <- x[, i] - medians[i]
}

# Why are loops sometimes slow? Every iteration of the loop copies the data frame


#### ENVIRONMENTS ####

#### DEBUGGING ####

# using try()
elements <- list(1:10, c(-1, 10), c(TRUE, FALSE), letters)
results <- lapply(elements, log)
#> Warning in FUN(X[[i]], ...): NaNs produced
#> Error in FUN(X[[i]], ...): non-numeric argument to mathematical function
results <- lapply(elements, function(x) try(log(x)))
#> Warning in log(x): NaNs produced


# this function is not robust to errors
col_means <- function(df) {
    numeric <- sapply(df, is.numeric)
    numeric_cols <- df[, numeric]
    
    data.frame(lapply(numeric_cols, mean))
}

#let's improve it
col_means <- function(df) {
    if (is.data.frame(df) == FALSE) {
        stop("Input must be a data frame!")
    }
    
    numeric <- sapply(df, is.numeric)
    numeric_cols <- df[, numeric, drop = F]
    
    data.frame(lapply(numeric_cols, mean))
}

col_means(mtcars)
col_means(mtcars[, 0])
col_means(mtcars[0, ])
col_means(mtcars[, "mpg", drop = F])
col_means(1:10)
col_means(as.matrix(mtcars))
col_means(as.list(mtcars))

mtcars2 <- mtcars
mtcars2[-1] <- lapply(mtcars2[-1], as.character)
col_means(mtcars2)


lag <- function(x, n = 1L) {
    xlen <- length(x)
    c(rep(NA, n), x[seq_len(xlen - n)])
}

lag("1:10"a
    , n = 2)


#### FUNCTIONAL PORGRAMMING ####

df <- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
names(df) <- letters[1:6]
df

# LET'S CHANGE ALL -99 TO NA

fix_missing <- function(x) {
    x[x == -99] <- NA
    x
}
df[] <- lapply(df, fix_missing) #uzywamy kwadratowego naiwasu aby df pozostalo data framem!

#jesli chcemy to zrobic tylko dla pierwszych pieciu kolumn
df[1:5] <- lapply(df[1:5], fix_missing)


summary <- function(x) {
    c(mean(x, na.rm = TRUE),
      median(x, na.rm = TRUE),
      sd(x, na.rm = TRUE),
      mad(x, na.rm = TRUE),
      IQR(x, na.rm = TRUE))
}
lapply(df, summary)



summary <- function(x) {
    funs <- c(mean, median, sd, mad, IQR)
    lapply(funs, function(f) f(x, na.rm = TRUE))
}


#anonymous functions
lapply(mtcars, function(x) length(unique(x))) #ile unikatowych rekordow w kazdej kolumnie


#lapply

#creating randomised list
l <- replicate(20, runif(sample(1:10, 1)), simplify = FALSE)
l

lapply(l, length) #length of each list element
unlist(lapply(l, length))

# Since data frames are also lists, lapply() is also useful when you want to do something to each column of a data frame
unlist(lapply(mtcars, class)) #str(mtcars)
unlist(lapply(mtcars, median))


mtcars2[] <- lapply(mtcars, function(x) x / mean(x))

# Use both for loops and lapply() to fit linear models to the mtcars using the formulas stored in this list
formulas <- list(
    mpg ~ disp,
    mpg ~ I(1 / disp),
    mpg ~ disp + wt,
    mpg ~ I(1 / disp) + wt
)

models <- lapply(formulas, function(x) lm(x, data = mtcars))

# also possible
models_mtcars <- lapply(formulas, lm, data = mtcars)

for (i in seq_along(formulas)) {
    print(lm(formulas[[i]], data = mtcars))
}


# Fit the model mpg ~ disp to each of the bootstrap replicates of mtcars in the list below by using a for loop and lapply(). Can you do it without an anonymous function?

bootstraps <- lapply(1:10, function(i) {
    rows <- sample(1:nrow(mtcars), rep = TRUE)
    mtcars[rows, ]
}) #lista z probkami bootstrapowymi

## lapply

bootstrap_models_lapply <- lapply(bootstraps, lm, formula = mpg ~ disp)

for (i in seq_along(bootstraps)) {
    print(lm(mpg ~ disp, data = bootstraps[[i]]))
}

# For each model in the previous two exercises, extract  R2  using the function below.
rsq <- function(mod) summary(mod)$r.squared

sapply(bootstrap_models_lapply, rsq)

# it doesn't work because summary doesn't work!


# using sapply and vapply - they ae simplified lapply sapply guesses the output type, in vapply we can determine the output type

sapply(mtcars, is.numeric)

vapply(mtcars, is.numeric, logical(1))
vapply(mtcars, is.numeric, numeric(1))

# sapply is dangerous inside other functions

weighted.mean(c(1,2,3), c(0.5, 0.2, 0.2)) # pierwszy argument to obserwacje, drugi to wagi

xs <- replicate(5, runif(10), simplify = FALSE) #our observations in a list
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE) # our weights

# we want to calculate weighted means
unlist(Map(weighted.mean, xs, ws))

# Map is useful whenever you have two (or more) lists (or data frames) that you need to process in parallel.
unlist(Map(weighted.mean, xs, ws))

# we have trial pulse data for 20 people and we want to compare groups
pulse <- round(rnorm(22, 70, 10 / 3)) + rep(c(0, 5), c(10, 12))
group <- rep(c("A", "B"), c(10, 12))

# sredni puls w grupach
tapply(pulse, group, mean)


split(pulse, group) #grouping into groups


#  Imagine you have a list of numeric vectors, and you want to find the values that occur in every element:

l <- replicate(5, sample(1:10, 15, replace = T), simplify = FALSE)
str(l)

# the easiest, but not the most legible way:
intersect(intersect(intersect(intersect(l[[1]], l[[2]]),
                              l[[3]]), l[[4]]), l[[5]])

# better with Reduce
Reduce(intersect, l)


# other functions

where <- function(f, x) {
    vapply(x, f, logical(1))
}

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
where(is.factor, df)

str(Filter(is.factor, df))
str(Find(is.factor, df))
str(Position(is.factor, df))
