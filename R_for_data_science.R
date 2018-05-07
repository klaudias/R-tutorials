# dokonczyc stringi i czwiczenia z relational data

# R for Data Science
# http://r4ds.had.co.nz

#### VISUALISATION ####

library(tidyverse)
library(nycflights13)
library(stringr)
library(forcats) # to work with factors
library(lubridate)
library(purrr)
library(modelr)

ggplot2::mpg
?mpg

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy))
# we can see that some data do not follow the linear trend

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, col = class))
# when we also look at class we see that these are 2seaters - they have hihre fuel efficiency than expected

# aletrnatively we can use size or points transparecny instead of colour but it is not advised with discrete variables such as class! 
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Shape is somewhat better but is limited to 6 shapes so SUV is unplotted
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# if we want all points to be green we have to set it outside the aesthetics
ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy), colour = "green") #the same goes with size,  shapes and alpha

# Splitting the plot into facets (useful with categorical variables)

# various plots by a single variable
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_wrap(~ class, nrow = 4)

# plot by two variables
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(. ~ cyl)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(cyl ~ .)

# A geom is the geometrical object that a plot uses to represent data. People often describe plots by the type of geom that the plot uses. For example, bar charts use bar geoms, line charts use line geoms, boxplots use boxplot geoms, and so on. Scatterplots break the trend; they use the point geom. As we see above, you can use different geoms to plot the same data

#smoothing
ggplot(data = mpg) + 
    geom_smooth(mapping = aes(x = displ, y = hwy))

# three smoothed lines for different drives (3 levels)
ggplot(data = mpg) + 
    geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, colour = drv))

# the same but easier to see with colours and points
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, colour = drv)) +
    geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, colour = drv))


# using multiple geoms on the same plot
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(x = displ, y = hwy))

# we can instead use global mapping that will be passed to all geoms!
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point() + 
    geom_smooth()

# but in each mapping we can use "local" additional aesthetics that will not affect the global aesthetics
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point(aes(col = class)) + 
    geom_smooth()

# local settings can overwrite tje global settings - here we will have a smooth line only for subcompact cars
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
    geom_point(mapping = aes(color = class)) + 
    geom_smooth(data = filter(mpg, class == "subcompact"), se = F)

# line chart
# let's prepare data - mean displ per years
mpg %>% 
    group_by(year) %>% 
    summarise(m = mean(displ)) %>% 
    ggplot(aes(year, m)) +
    geom_line() #between 1998 and 2008 the mean engine displacement grew

# Area chart
huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
ggplot(huron, aes(year, level)) +
    geom_area() +
    coord_cartesian(ylim=c(550,600))

# The following chart displays the total number of diamonds in the diamonds dataset, grouped by cut
?diamonds

ggplot(data = diamonds) +
    geom_bar(aes(x = cut)) #the default stat is "count", that is why we do not use y

# if we want to see proportions instead
ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))


# to summarise the y values for each unique x value
ggplot(data = diamonds) + 
    stat_summary(
        mapping = aes(x = cut, y = depth),
        fun.ymin = min,
        fun.ymax = max,
        fun.y = median
    )
#min, median and max depth for each unique cut 

# the same but worse
ggplot(diamonds) +
    geom_pointrange(aes(cut, depth, ymin = depth, ymax = depth))

ggplot(data = diamonds) +
    geom_histogram(aes(carat), bins = 10)

# colorful bar chart
ggplot(data = diamonds) +
    geom_bar(aes(x = cut, fill = cut))

ggplot(data = diamonds) +
    geom_bar(aes(x = cut, fill = clarity)) #automatic stacked bar chart

ggplot(data = diamonds) +
    geom_bar(aes(x = cut, fill = clarity), position = "fill") #each column is the same height

ggplot(data = diamonds) +
    geom_bar(aes(x = cut, fill = clarity), position = "dodge")

# useful adjustment for scatterplots! 
# on this graph we can see 126 points eventhough in the data there are 234 observations.some points may overlap because they are rounded. In this way we cannot see where a point mass is.
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy))

# we can add jitter, that is a bit of noise to the points. That spread the points :)
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
# Adding randomness seems like a strange way to improve your plot, but while it makes your graph less accurate at small scales, it makes your graph more revealing at large scales.

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
    geom_point(position = "jitter")
#or
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
    geom_point() +
    geom_jitter()


ggplot(mpg) +
    geom_boxplot(aes(x = class, y = displ, col = class), show.legend = F)
# or
ggplot(mpg) +
    geom_boxplot(aes(x = class, y = displ, col = class)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# coordinates
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
    geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
    geom_boxplot() +
    coord_flip() #flipping the chart


ggplot(data = diamonds) + 
    geom_bar(
        mapping = aes(x = cut, fill = cut), 
        show.legend = FALSE,
        width = 1 #no break between categories
    ) +
    labs(x = NULL, y = NULL) +
    coord_polar()




ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_point() + 
    geom_jitter() +
    geom_abline() + #adds a reference line to the plot
    coord_fixed() #A fixed scale coordinate system forces a specified ratio between the physical representation of data units on the axes. The ratio represents the number of units on the y-axis equivalent to one unit on the x-axis. The default, ratio = 1, ensures that one unit on the x-axis is the same length as one unit on the y-axis. 


#### DATA TRANSFORMATION ####

library(nycflights13) # dataset on flights departing New York City in 2013

head(flights) #It prints differently because it’s a tibble. Tibbles are data frames, but slightly tweaked to work better in the tidyverse.
# time_hour variable has dttm type (date time)

#In this chapter you are going to learn the five key dplyr functions that allow you to solve the vast majority of your data manipulation challenges:
# 1. Pick observations by their values (filter()).
# 2. Reorder the rows (arrange()).
# 3. Pick variables by their names (select()).
# 4. Create new variables with functions of existing variables (mutate()).
# 5. Collapse many values down to a single summary (summarise()).
# These can all be used in conjunction with group_by() which changes the scope of each function from operating on the entire dataset to operating on it group-by-group. These six functions provide the verbs for a language of data manipulation.

filter(flights, dep_delay > 15)
(jan1 <- filter(flights, month == 1, day == 1))
filter(flights, between(dep_delay, 100, 150)) # shortcut for >= and <=
filter(flights, dep_delay > 0 | is.na(dep_delay)) # we have to ask explicitely for NAs

# Instead of relying on ==, use near() !!!!
1/49 * 49 == 1
near(1 / 49 * 49, 1)

# rearranging the order of rows by given variables in ascending order
arranged.df <- arrange(flights, year, month, day) #If you provide more than one column name, each additional column will be used to break ties in the values of preceding column

arrange(flights, desc(dep_delay), desc(arr_delay))
# missing values are always at the end

# selecting columns
select(flights, year, month, day, dep_delay)
select(flights, year:dep_delay)
select(flights, -(year:dep_delay))

# starts_with, ends_with, contains, matches (reg expr), num_range (like x1, x2, x3...)

rename(flights, tail_num = tailnum) #renaming a variable

select(flights, time_hour, air_time, everything()) # this is useful if we want to move a few columns to the beginning of the data frame!!!


# Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights

select(flights, starts_with("arr_"), starts_with("dep_"))
select(flights, ends_with("_time"), ends_with("_delay"))
select(flights, one_of("dep_time", "dep_delay", "arr_time", "arr_delay"))
select(flights, contains("delay"), contains("time"), -contains("sched"), -contains("hour"), -contains("air"))

# surprising
select(flights, contains("TIME"))

# how to change to be sensitive
select(flights, contains("TIME", ignore.case = F))


# adding new variables with mutate()

mutate(flights,
       scheduled_travel_time = sched_arr_time - sched_dep_time,
       gain = arr_delay - dep_delay,
       travel_time_hours = air_time/60,
       gain_per_hour = gain/travel_time_hours)

# if we only want to keep new variables in a new data frame we should use transmute()

transmute(flights,
          scheduled_travel_time = sched_arr_time - sched_dep_time,
          gain = arr_delay - dep_delay,
          travel_time_hours = air_time/60,
          gain_per_hour = gain/travel_time_hours)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100, #integer division
          minute = dep_time %% 100 #remainder
)

x <- c(3,1,7,6,6,10)
lag(x)
x - lag(x) #computing diffference
diff(x)
x != lag(x) # learn when value changes
diff(x) != 0

cumsum(x)
cumprod(x)
cummin(x)
cummax(x)

min_rank(x) #ranking where the smallest number is best
min_rank(desc(x))

transmute(flights,
          dep_time_minutes_since_midnight = dep_time %/% 100 * 60 + dep_time %% 100
)

# find the 10 most delayed flights
arrange(flights, -arr_delay)[,1:10]

flights %>%
    top_n(10, dep_delay)

# Grouped summaries with summarise()
# It collapses a data frame to a single row

# let's find the average departure delay
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

flights %>% 
    summarise(delay = mean(dep_delay, na.rm = T))

# summarise() is not terribly useful unless we pair it with group_by(). This changes the unit of analysis from the complete dataset to individual groups. Then, when you use the dplyr verbs on a grouped data frame they’ll be automatically applied “by group”. For example, if we applied exactly the same code to a data frame grouped by date, we get the average delay per date:

# avg delay by day
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

flights %>% 
    group_by(year, month, day) %>% 
    summarise(delay = mean(dep_delay, na.rm = T))

# avg delay by month
flights %>% 
    group_by(year, month) %>% 
    summarise(delay = mean(dep_delay, na.rm = T))

# we want to explore the relationship between the distance and average delay for each location

flights2 <- flights %>% 
    group_by(dest) %>% 
    summarise(count = n(),
              delay = mean(arr_delay, na.rm = T),
              avg_distance = mean(distance, na.rm = T)) %>% 
    arrange(avg_distance)

ggplot(data = flights2, aes(x = avg_distance, y = delay)) +
    geom_point(aes(size = count), alpha = (1/3)) +
    geom_smooth(se = F) +
    xlab("Average distance") +
    ylab("Average delay")


# Whenever you do any aggregation, it’s always a good idea to include either a count (n()), or a count of non-missing values (sum(!is.na(x))). That way you can check that you’re not drawing conclusions based on very small amounts of data. For example, let’s look at the planes (identified by their tail number) that have the highest average delays:

not_cancelled <- flights %>% 
    filter(!is.na(arr_time))

delays <- not_cancelled %>% 
    group_by(tailnum) %>% 
    summarise(avg_delay = mean(arr_delay, na.rm = T),
              n = n())

ggplot(data = delays, aes(x = avg_delay)) +
    geom_freqpoly()

ggplot(data = delays, aes(x = avg_delay)) +
    geom_histogram()

# number of flights vs average delay - variation decreases as the sample size increases
ggplot(data = delays, aes(x = n, y = avg_delay)) +
    geom_point(alpha = 1/10)

# When looking at this sort of plot, it’s often useful to filter out the groups with the smallest numbers of observations, so you can see more of the pattern and less of the extreme variation in the smallest groups. 

delays %>% 
    filter(n > 25) %>% 
    ggplot(mapping = aes(x = n, y = avg_delay)) + 
    geom_point(alpha = 1/10)

# another useful summary statistics robust when we have outliers
IQR() #interquartile range
mad() #median absolute deviation


not_cancelled %>% 
    group_by(dest) %>% 
    summarise(mean_distance = mean(distance, na.rm = T),
              distance_sd = sd(distance)) %>% 
    arrange(desc(distance_sd))

# When do the first and last flights leave each day?
not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarise(earliest_flight = min(dep_time),
              latest_flight = max(dep_time)) 

# or
not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarise(earliest_flight = first(dep_time),
              latest_flight = last(dep_time)) 

# n() takes no arguments, and returns the size of the current group
# sum(!is.na(x)) counts the number of non-missing values
# n_distinct(x) counts the number of distinct values

# Which destinations have the most carriers?
not_cancelled %>% 
    group_by(dest) %>% 
    summarise(carriers = n_distinct(carrier)) %>% 
    arrange(desc(carriers))

# Wich carriers have the most destinations from NYC?
not_cancelled %>% 
    group_by(carrier) %>% 
    summarise(n_of_destinations = n_distinct(dest)) %>% 
    arrange(desc(n_of_destinations))

# Counts are so useful that dplyr provides a simple helper if all you want is a coun
not_cancelled %>% 
    count(dest)

# You can optionally provide a weight variable. For example, you could use this to “count” (sum) the total number of miles a plane flew:
not_cancelled %>% 
    count(tailnum, wt = distance)

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)

not_cancelled %>% 
    filter(dep_time < 500) %>% 
    count()

# the same by day
not_cancelled %>% 
    group_by(year, month, day) %>% 
    summarise(left_before_5 = sum(dep_time < 500)) #!!!

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
    group_by(year, month, day) %>%
    summarise(percent_of_delayed_over_an_hour = mean(arr_delay > 60, na.rm = T))

# do 5.6.7 and 5.7.1 excercises

# the worst delays each day
flights %>% 
    group_by(year, month, day) %>%
    filter(rank(desc(arr_delay)) < 10)

# find popular destinations (flights every day)
flights %>%
    group_by(dest) %>% 
    summarise(number_of_flights_yearly = n()) %>% 
    filter(number_of_flights_yearly > 365) %>% 
    arrange(desc(number_of_flights_yearly))


flights %>%
    group_by(dest) %>% 
    filter(n() > 365) 


#### EXPLORATORY DATA ANALYSIS ####

# If you wish to overlay multiple histograms in the same plot, I recommend using geom_freqpoly() instead of geom_histogram(). geom_freqpoly() performs the same calculation as geom_histogram(), but instead of displaying the counts with bars, uses lines instead. It’s much easier to understand overlapping lines than bars.

ggplot(data = diamonds, aes(x = carat, colour = cut)) +
    geom_freqpoly(bins = 20)

# Covariation is the tendency for the values of two or more variables to vary together in a related way.

# in this plot boxplots are "unordered"
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
    geom_boxplot()

# here we order boxplots by the median y values

ggplot(data = mpg) +
    geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

# correlation possible only for numeric values
cor(diamonds[sapply(diamonds, is.numeric)])

# To visualise the covariation between categorical variables, you’ll need to count the number of observations for each combination. One way to do that is to rely on the built-in geom_count()

ggplot(data = diamonds) +
    geom_count(mapping = aes(x = cut, y = color))

# The size of each circle in the plot displays how many observations occurred at each combination of values. Covariation will appear as a strong correlation between specific x values and specific y values.

# Another approach is to compute the count with dplyr:
diamonds %>% 
    count(color, cut)

# Then visualise with geom_tile() and the fill aesthetic

diamonds %>% 
    count(color, cut) %>%  
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

# Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read? How could you improve it?

flights %>% 
    group_by(month, dest) %>% 
    summarise(avg_delay = mean(arr_delay, na.rm = T)) %>% 
    ggplot() +
    geom_tile(aes(x = month, y = dest, fill = avg_delay))

# Covariation of two continuous variables
# scatterplot for large datasets
ggplot(data = diamonds) +
    geom_bin2d(mapping = aes(x = carat, y = price))


# Another option is to bin one continuous variable so it acts like a categorical variable. 

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
    geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# By default, boxplots look roughly the same (apart from number of outliers) regardless of how many observations there are, so it’s difficult to tell that each boxplot summarises a different number of points.Another approach is to display approximately the same number of points in each bin:

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
    geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

#  If you think of variation as a phenomenon that creates uncertainty, covariation is a phenomenon that reduces it. If two variables covary, you can use the values of one variable to make better predictions about the values of the second. If the covariation is due to a causal relationship (a special case), then you can use the value of one variable to control the value of the second.


# we can see the relationship between price and cut once the carat impact is removed

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
    add_residuals(mod) %>% 
    mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
    geom_point(mapping = aes(x = carat, y = resid))

# now you can see what you expect in the relationship between cut and price: relative to their size, better quality diamonds are more expensive.

ggplot(data = diamonds2) + 
    geom_boxplot(mapping = aes(x = cut, y = resid))
ggsave("boxplots.pdf")

#### WRANGLING ####

# tibbles are dataframes in the tidyverse framework
as_tibble(iris)

tibble(
    x = 1:5, 
    y = 1, 
    z = x ^ 2 + y
)

# If you’re already familiar with data.frame(), note that tibble() does much less: it never changes the type of the inputs (e.g. it never converts strings to factors!), it never changes the names of variables, and it never creates row names.

# Another way to create a tibble is with tribble(), short for transposed tibble. tribble() is customised for data entry in code: column headings are defined by formulas (i.e. they start with ~), and entries are separated by commas. This makes it possible to lay out small amounts of data in easy to read form.

tribble(
    ~x, ~y, ~z,
    #--|--|----
    "a", 2, 3.6,
    "b", 1, 8.5
)

# Tibbles have a refined print method that shows only the first 10 rows, and all the columns that fit on screen. This makes it much easier to work with large data. In addition to its name, each column reports its type, a nice feature borrowed from str()

tibble(
    a = lubridate::now() + runif(1e3) * 86400,
    b = lubridate::today() + runif(1e3) * 30,
    c = 1:1e3,
    d = runif(1e3),
    e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>% 
    print(n = 10, width = Inf) #prints ten rows and all columns

# choosing columns from tibble in pipe

df %>% .$x
df %>% .[["x"]]

# Compared to a data.frame, tibbles are more strict: they never do partial matching, and they will generate a warning if the column you are trying to access does not exist.

# Some older functions don’t work with tibbles. If you encounter one of these functions, use as.data.frame() to turn a tibble back to a data.frame

# If you have the name of a variable stored in an object, e.g. var <- "cyl", how can you extract the reference variable from a tibble?

var <- "cyl"
mpg_tibble <- as.tibble(mpg)
mpg_tibble %>% 
    .[[var]]

# excercise
annoying <- tibble(
    `1` = 1:10,
    `2` = `1` * 2 + rnorm(length(`1`))
)

annoying %>% 
    .$'1'

annoying %>%
    rename(one = '1',
           two = '2') %>% 
    ggplot() +
    geom_point(aes(x = one, y = two))

letters <- c(letters[1:10])
letters_tibble <- enframe(letters) #makes a tibble of a vector
is.tibble(letters_tibble)

# reading in data

# in readr package we have read_csv2 instead of read.csv2, data ar read as tibbles, strings are not converted to factors

# Why use readr instead of base functions?
# 1. It is faster
# 2. They produce tibbles, they don’t convert character vectors to factors, use row names, or munge the column names. These are common sources of frustration with the base R functions.

# Parsing
c("TRUE", "FALSE", "NA")
str(parse_logical(c("TRUE", "FALSE", "NA")))

c("2010-01-01", "1979-10-14")
str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")

# parse_double addressed the problem of different decimal marks
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))

# parse_number addreses the problem of unnecessary spaces and text
parse_number("$100") #100
parse_number("20%") #20
parse_number("It cost $123.45") #123.45

# Used in America
parse_number("$123,456,789")

# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# parsing characters
x1 <- "El Ni\xf1o was particularly bad this year"
guess_encoding(charToRaw(x1)) #0.46 that it is Latin-1
parse_character(x1, locale = locale(encoding = "Latin1"))

# parsing factors
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

# parsing dtes and times
parse_datetime("2010-10-01T2010") # from biggest to smallest

# If time is omitted, it will be set to midnight
parse_datetime("20101010")

parse_date("2010-10-01")
parse_date("2010/10/01")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

# we can always set our own format explained in 11.3.4
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%y/%m/%d")
parse_date("22", "%y")
parse_date("22-Jan-1901", "%d-%b-%Y")
parse_date("1-marca-1992", "%d-%B-%Y", locale = locale("pl"))
date_names_lang("pl") #check the polish names and abbreviations

# check default locale settings
default_locale()

# change locale settings
locale(date_names = "pl", decimal_mark = ",", grouping_mark = " ")

# back to default
locale()

# excersise: Generate the correct format string to parse each of the following dates and times:

d1 <- "January 1, 2010"
parse_date(d1, "%B %d, %Y")
d2 <- "2015-Mar-07"
parse_date(d2, "%Y-%b-%d")
d3 <- "06-Jun-2017"
parse_date(d3, "%d-%b-%Y")
d4 <- c("August 19 (2015)", "July 1 (2015)")
parse_date(d4, "%B %d (%Y)")
d5 <- "12/30/14" # Dec 30, 2014
parse_date(d5, "%m/%d/%y")
t1 <- "1705"
parse_time(t1, "%H%M")
t2 <- "11:15:10.12 PM"
parse_time(t2, "%I:%M:%S %p")

# readr functions guess the column types by looking at the first 1000 rows. sometimes it can lead to problems

challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
challenge
# the first column was set to integer (the first 1000 values were integers, then doubles started) the second to character (the dates started after 1000 rows)

# to fix it, we can manually set column types
challenge <- read_csv(
    readr_example("challenge.csv"), 
    col_types = cols(
        x = col_double(),
        y = col_date()
    )
)
tail(challenge)

# Every parse_xyz() function has a corresponding col_xyz() function. You use parse_xyz() when the data is in a character vector in R already; you use col_xyz() when you want to tell readr how to load the data.

# we can write files to csv but this is a little unreliable as we lose information e.g. about column types
write_csv(challenge, "challenge.csv")

# it's better to save as RDS
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

# To get other types of data into R, we recommend starting with the tidyverse packages listed below. They’re certainly not perfect, but they are a good place to start. For rectangular data:
# - haven reads SPSS, Stata, and SAS files.
# - readxl reads excel files (both .xls and .xlsx).
# - DBI, along with a database specific backend (e.g. RMySQL, RSQLite, RPostgreSQL etc) allows you to run SQL queries against a database and return a data frame.


#### TIDY DATA ####
table1 # this is tidy data
table2
table3
table4a
table4b

# excercise
# Compute the rate for table2, and table4a + table4b.
cases <- filter(table2, type == "cases")
colnames(cases)[which(colnames(cases)=="count")] <- "cases"
cases[,which(colnames(cases)=="type")] <- NULL
population <- filter(table2, type == "population")
colnames(population)[which(colnames(population)=="count")] <- "population"
population[,which(colnames(population)=="type")] <- NULL
df <- cbind(cases, population[,which(colnames(population) == "population")])
df$rate <- df$cases/df$population * 10000

# gather()
# A common problem is a dataset where some of the column names are not names of variables, but values of a variable. Take table4a: the column names 1999 and 2000 represent values of the year variable, and each row represents two observations, not one.

table4a %>% 
    gather(`1999`, `2000`, key = "year", value = "cases") # key - variable name for the olumn names of now, value - variable name for the values
table4b %>% 
    gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b) #returns all rows from x and all columns from x and y

# spread() - the opposite of gathering

table2 %>% 
    spread(key = type, value = count) #key - to co ma stac sie zmienna/zmiennymi, value - wartosci tych zmiennych


#tidy the data below
preg <- tribble(
    ~pregnant, ~male, ~female,
    "yes",     NA,    10,
    "no",      20,    12
)

preg %>% gather('male', 'female', key = sex, value = count)

# separate() plls apart one column int multiple columns, by splitting wherever a separator character appears
table3 %>% 
    separate(rate, #the column to separate
             into = c("cases", "population")) #the columns to separate into
# By default, separate() will split values wherever it sees a non-alphanumeric character
# to be more expicit, we can write:
table3 %>% 
    separate(rate, into = c("cases", "population"), sep = "/")
# after separating our cases and population columns are characters (they preserve the type of the original variable) so we can try convert it to better types

table3 %>% 
    separate(rate, into = c("cases", "population"), sep = "/", convert = T)

# another possibility
table3 %>% 
    separate(year, into = c("century", "year"), sep = 2) #if sep is numeric, it is interpreted as a position to split at, the lenth of sep sould be one less than into

# unite() is the inverse of separate(): it combines multiple columns into a single column.

table5 %>% 
    unite(new, century, year)

# we need to use a "" separator, because the default is "_"
table5 %>% 
    unite(new, century, year, sep = "")

# excercise - too many or not enough values
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
    separate(x, c("one", "two", "three"), extra = "drop") #drops if there are too many values

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
    separate(x, c("one", "two", "three"), fill = "right") #fills missing values with values on the right


# how to handle missing values?

stocks <- tibble(
    year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
    qtr    = c(   1,    2,    3,    4,    2,    3,    4),
    return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks

# one explicitly misisng value with NA
# first quarter of 2016 simply not present in the data

stocks %>% 
    complete(year, qtr) #Turns implicit missing values into explicit missing values

# Sometimes when a data source has primarily been used for data entry, missing values indicate that the previous value should be carried forward:
treatment <- tribble(
    ~ person,           ~ treatment, ~response,
    "Derrick Whitmore", 1,           7,
    NA,                 2,           10,
    NA,                 3,           9,
    "Katherine Burke",  1,           4
)

#You can fill in these missing values with fill(). It takes a set of columns where you want missing values to be replaced by the most recent non-missing value (sometimes called last observation carried forward)
treatment %>% 
    fill(person)

# Case study

who
colnames(who)

who_tidy <- who %>% 
    gather(new_sp_m014:newrel_f65, 
           key = key, 
           value = cases) %>%
    mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
    separate(key, into = c("new", "method", "others"), sep = "_") %>% 
    separate(others, into = c("gender", "age"), sep = 1) %>% 
    select(-new, -iso2, -iso3)

# For each country, year, and sex compute the total number of cases of TB. Make an informative visualisation of the data.

who_tidy %>% 
    group_by(country, year, gender) %>% 
    summarise(total_cases = sum(cases, na.rm = T)) 


who_tidy %>%
    group_by(country, year, gender) %>%
    filter(year > 1995, country %in% c("Italy", "Spain")) %>%
    summarise(cases = sum(cases, na.rm = T)) %>%
    ggplot(aes(x = year, y = cases, colour = country)) +
    geom_line() +
    facet_wrap(~gender, nrow = 2)

#### Relational data ####

nycflights13::airlines #carrier names
nycflights13::airports #airports specification
nycflights13::planes
nycflights13::weather
nycflights13::flights #flights table contains all info from the other tables!

# Once you’ve identified the primary keys in your tables, it’s good practice to verify that they do indeed uniquely identify each observation. One way to do that is to count() the primary keys and look for entries where n is greater than one:
planes %>% 
    count(tailnum) %>% 
    filter(n > 1)

weather %>% 
    count(year, month, day, hour, origin) %>% 
    filter(n > 1)

# Add a surrogate key to flights (rownames, there is no primary key in flights even the flight numers aren't unique!)

flights <- flights %>% 
    mutate(key = row_number())

#find primary keys
ggplot2::diamonds
babynames::babynames

#joining tables
# we select only some columns to better see the examples

flights2 <- flights %>% 
    select(year:day, hour, origin, dest, tailnum, carrier)
flights2

# we want to add the full airline name to the flights2 data

flights2 %>% 
    left_join(airlines, by = "carrier")

# JOINS explained - really good visual explanation here: http://r4ds.had.co.nz/relational-data.html

x <- tribble(
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    3, "x3"
)
y <- tribble(
    ~key, ~val_y,
    1, "y1",
    2, "y2",
    4, "y3"
)

x
y

# The simplest type of join is the inner join. An inner join matches pairs of observations whenever their keys are equal

x %>% 
    inner_join(y, by = "key")

# The most important property of an inner join is that unmatched rows are not included in the result. This means that generally inner joins are usually not appropriate for use in analysis because it’s too easy to lose observations.

# An inner join keeps observations that appear in both tables. An outer join keeps observations that appear in at least one of the tables. There are three types of outer joins:
# - A left join keeps all observations in x.
# - A right join keeps all observations in y.
# - A full join keeps all observations in x and y

# The most commonly used join is the left join: you use this whenever you look up additional data from another table, because it preserves the original observations even when there isn’t a match. The left join should be your default join: use it unless you have a strong reason to prefer one of the others.

# case of duplicate keys

# one table has duplicate key
x <- tribble(
    ~key, ~val_x,
    1, "x1",
    2, "x2",
    2, "x3",
    1, "x4"
)
y <- tribble(
    ~key, ~val_y,
    1, "y1",
    2, "y2"
)

x %>% 
    left_join(y, by = "key")


# excercise - Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. 

delays <- flights %>% 
    group_by(dest) %>% 
    summarise(avg_delay = mean(arr_delay, na.rm = T)) %>% 
    arrange(desc(avg_delay)) %>% 
    left_join(airports, by = c("dest" = "faa")) %>% 
    select(dest, avg_delay)

airports %>%
    semi_join(flights, c("faa" = "dest")) %>%
    left_join(delays, c("faa" = "dest")) %>% 
    ggplot(aes(lon, lat, col = avg_delay)) +
    borders("state") +
    geom_point() +
    coord_quickmap()

# Add the location of the origin and destination (i.e. the lat and lon) to flights.

flights_latlon <- flights %>%
    left_join(airports[,c("faa", "lat", "lon")], by = c("dest" = "faa")) %>% 
    rename(dest_lat = lat,
           dest_lon = lon) %>% 
    left_join(airports[,c("faa", "lat", "lon")], by = c("origin" = "faa")) %>% 
    rename(origin_lat = lat,
           origin_lon = lon)

# Is there a relationship between the age of a plane and its delays?

flights %>% 
    group_by(tailnum) %>% 
    summarise(avg_delay = mean(arr_delay, na.rm = T)) %>% 
    left_join(planes[,c("tailnum", "year")], by = "tailnum") %>% 
    mutate(age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(year)) %>%     ggplot(aes(x = avg_delay, y = age)) +
    geom_point() +
    geom_smooth()

# What weather conditions make it more likely to see a delay?
flightss <- flights %>%
    left_join(weather, by = c("year", "month", "day", "origin", "hour")) %>%
    filter(origin == "JFK")

mean(weather$precip)


flights2 <- flights %>%
    left_join(weather, by = c("year", "month", "day", "origin", "hour")) %>%
    filter(origin == "JFK") %>%
    arrange(min_rank(desc(dep_delay))) %>% 
    filter(dep_delay > 60)


flights %>%
    left_join(weather, by = c("year", "month", "day", "origin", "hour")) %>%
    filter(origin == "JFK") %>% 
    mutate(temp_celcius = (temp-32)/1.8,
           temp_intervals = cut(temp_celcius, 10)) %>%     
    group_by(temp_intervals) %>% 
    summarise(avg_delay = mean(dep_delay, na.rm = T))



cor(flightss$dep_delay, flightss$temp, use = "complete.obs")
cor(flightss$dep_delay, flightss$precip, use = "complete.obs")
cor(flightss$dep_delay, flightss$humid, use = "complete.obs")
cor(flightss$dep_delay, flightss$visib, use = "complete.obs")

View(flightss)

flights %>%
    left_join(weather, by = c("year", "month", "day", "origin", "hour")) %>%
    filter(origin == "JFK") %>% 
    mutate(visib_intervals = cut(visib, 5)) %>%
    filter(!is.na(visib_intervals)) %>%
    group_by(visib_intervals) %>% 
    summarise(avg_delay = mean(dep_delay, na.rm = T)) %>% 
    ggplot(aes(x = visib_intervals, y = avg_delay)) +
    geom_col() # the average delay is smaller for higher visibility


flights %>%
    left_join(weather, by = c("year", "month", "day", "origin", "hour")) %>%
    filter(origin == "JFK") %>% 
    mutate(visib_intervals = cut(visib, 5)) %>%
    filter(!is.na(visib_intervals)) %>%
    group_by(visib_intervals) %>% 
    summarise(avg_delay = mean(dep_delay, na.rm = T)) %>% 
    ggplot(aes(x = visib_intervals, y = avg_delay)) +
    geom_col()

flights %>%
    left_join(weather, by = c("year", "month", "day", "origin", "hour")) %>%
    filter(origin == "JFK") %>% 
    ggplot(aes(x = precip, y = dep_delay)) +
    geom_point()


# What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.

june_13 <- flights %>% 
    group_by(year, month, day) %>% 
    summarise(avg_dep_delay = mean(dep_delay, na.rm = T),
              avg_arr_delay = mean(arr_delay, na.rm = T)) %>%
    filter(month == 6, year == 2013)

ggplot(june_13, aes(x = day, y = avg_arr_delay)) +
    geom_line(colour = "black") +
    geom_point(data = filter(june_13, day == 13), colour = "blue", size = 2) +
    geom_text(data = filter(june_13, day == 13), aes(x = day * 1.33, y = avg_arr_delay, label = "Severe weather"))


# dplyr joins are faster than merge

# mutating join allows you to combine variables from two tables
# filtering joins match observations in the same way as mutating joins, but affect the observations, not the variables. There are two types:
# - semi_join(x, y) keeps all observations in x that have a match in y.
# - anti_join(x, y) drops all observations in x that have a match in y.


# semi-joins

# finding top destinations (popularity)
top_dest <- flights %>%
    count(dest, sort = TRUE) %>% 
    rename(number_of_flights = n) %>%
    head(10)
top_dest

# filtering only rows reffering to top destinations from the original dataset
flights %>%
    semi_join(top_dest, by = "dest") #we can omit "by"

# anti-join: diagnosing mismatches
flights %>% 
    anti_join(planes, by = "tailnum") %>% 
    distinct(tailnum) #planes that are not present in the planes table

flights %>% 
    anti_join(planes, by = "tailnum") %>% 
    count(tailnum, sort = T)

# What does it mean for a flight to have a missing tailnum? What do the tail numbers that don’t have a matching record in planes have in common?

# carriers of planes with missing tailnums
missing_tailnums <- flights %>% 
    anti_join(planes, by = "tailnum") %>%  
    count(carrier, sort = T)

# carriers of planes with known tailnums
known_tailnums <- flights %>% 
    semi_join(planes, by = "tailnum") %>%  
    count(carrier, sort = T)

# missing tailnums as percent of all tailnums by airline

flights_per_carrier <- flights %>% 
    count(carrier, sort = T) %>% 
    rename(flights_per_carrier = n) #all flights by carrier

flights_per_carrier_with_missing_tailnum <- flights %>% 
    anti_join(planes, by = "tailnum") %>%  
    count(carrier, sort = T) %>% 
    rename(flights_per_carrier_with_missing_tailnums = n) #all flights with missing tailnums by carrier

summary <- flights_per_carrier %>% 
    left_join(flights_per_carrier_with_missing_tailnum, by = "carrier")

summary$flights_per_carrier_with_missing_tailnums[is.na(summary$flights_per_carrier_with_missing_tailnums)] <- 0 

summary %>% 
    mutate(percent_of_missing_tailnums = flights_per_carrier_with_missing_tailnums/flights_per_carrier) %>% 
    select(carrier, percent_of_missing_tailnums) %>% 
    arrange(desc(percent_of_missing_tailnums)) %>% 
    mutate(percent_of_missing_tailnums = scales::percent(percent_of_missing_tailnums))


# Filter flights to only show flights with planes that have flown at least 100 flights.

planes_over_100_flights <- flights %>%
    count(tailnum, sort = T) %>% 
    filter(n >= 100)

flights %>%
    semi_join(planes_over_100_flights)

# Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.
fueleconomy::vehicles
fueleconomy::common

vehicles %>% 
    semi_join(common)

# Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?

worse_48_hours <- flights %>%
    group_by(year, month, day) %>%
    summarise(total_24 = sum(dep_delay, na.rm = TRUE)+ sum(arr_delay, na.rm = TRUE)) %>%
    mutate(total_48 = total_24 + lag(total_24)) %>%
    arrange(desc(total_48))

# avg weather on worse delays
weather %>% 
    filter(month == 7 & day == c(23, 24)) %>% 
    summarise(avg_temp = mean(temp, na.rm = T),
              avg_humid = mean(humid, na.rm = T),
              avg_wind_speed = mean(wind_speed, na.rm = T),
              avg_visib = mean(visib, na.rm = T))

# avg weather conditions over the year
weather %>% 
    summarise(avg_temp = mean(temp, na.rm = T),
              avg_humid = mean(humid, na.rm = T),
              avg_wind_speed = mean(wind_speed, na.rm = T),
              avg_visib = mean(visib, na.rm = T))


# Be aware that simply checking the number of rows before and after the join is not sufficient to ensure that your join has gone smoothly. If you have an inner join with duplicate keys in both tables, you might get unlucky as the number of dropped rows might exactly equal the number of duplicated rows!


# intersect, unon, setdiff

df1 <- tribble(
    ~x, ~y,
    1,  1,
    2,  1
)
df2 <- tribble(
    ~x, ~y,
    1,  1,
    1,  2
)

intersect(df1, df2) #observations in both x and y
union(df1, df2) #unique observations in x and y
setdiff(df1, df2) #obs in x, but not in y


#### STRINGS ####

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

# That means if you want to include a literal backslash, you’ll need to double it up: "\\".

# all functions in str start with str_

str_length(c("a", "R for data science", NA))

str_c("x", "y", "z") #combining strings
str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_replace_na(x) #turn NA into "NA"

str_c("prefix-", c("a", "b", "c"), "-suffix") #str_c is vectorised

str_c(c("x", "y", "z"), collapse = ", ") #to collapse a vector into a single string

str_c(letters, collapse = ", ") #wszystko polaczone w jeden string
str_c("litera ", letters, sep = ": ") #kazdy string oddzielnie

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
# negative numbers count backwards from end
str_sub(x, -3, -1)

# Note that str_sub() won’t fail if the string is too short: it will just return as much as possible
str_sub("a", 1, 5)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

str_to_upper("i")

x <- c("apple", "eggplant", "banana")

str_sort(x)
str_sort(x, decreasing = T)

# extract the middle character from a string
string <- "banan"

if (str_length(string) %% 2 == 0) {
    str_sub(string, str_length(string)/2, str_length(string)/2 + 1)
} else {
    str_sub(string, str_length(string)/2 + 0.5, str_length(string)/2 + 0.5)
}

str_trim(" Klaudia    ") #trims spaces at the start and end

# Write a function that turns (e.g.) a vector c("a", "b", "c") into the string "a, b, and c". Think carefully about what it should do if given a vector of length 0, 1, or 2.

vector <- c("a", "b", "c", "d")

str_c(str_c(vector[1:length(vector) - 1], collapse = ", "), vector[length(vector)], sep = " and ")



# Matching patterns with regular expressions

x <- c("apple", "banana", "pear")
str_view(x, "an")

# . matches any character
str_view(x, ".a.")

# But if “.” matches any character, how do you match the character “.”? You need to use an “escape” to tell the regular expression you want to match it exactly, not use its special behaviour. Like strings, regexps use the backslash, \, to escape special behaviour. So to match an ., you need the regexp \.. Unfortunately this creates a problem. We use strings to represent regular expressions, and \ is also used as an escape symbol in strings. So to create the regular expression \. we need the string "\\."

# To create the regular expression, we need \\
dot <- "\\."

str_view(c("abc", "a.c", "bef"), "a\\.c")


# If \ is used as an escape character in regular expressions, how do you match a literal \? Well you need to escape it, creating the regular expression \\. To create that regular expression, you need to use a string, which also needs to escape \. That means to match a literal \ you need to write "\\\\" — you need four backslashes to match one!

x <- "a\\b"
x
writeLines(x)

str_view(x, "\\\\")

# How would you match the sequence "'\?
hard <- "\"\'\\"
hard
writeLines(hard)

str_view(hard, "\"\'\\\\")

# By default, regular expressions will match any part of a string. It’s often useful to anchor the regular expression so that it matches from the start or end of the string. You can use:
# ^ to match the start of the string.
# $ to match the end of the string.


x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

# To force a regular expression to only match a complete string, anchor it with both ^ and $

x <- c("apple pie", "apple", "I like apple")
str_view(x, "apple")
str_view(x, "^apple$")


# How would you match the literal string "$^$"
str_view(c("$^$", "ab$^$sfas"), "^\\$\\^\\$$")


# Given the corpus of common words in stringr::words, create regular expressions that find all words that:
# Start with “y”.
stringr::words
str_view(words, "^y", match = T)
# End with “x”
str_view(words, "x$", match = T)
# Are exactly three letters long. (Don’t cheat by using str_length()!)
str_view(words, "^...$", match = T)
# Have seven letters or more.
str_view(words, "^.......", match = T)
# Since this list is long, you might want to use the match argument to str_view() to show only the matching or non-matching words.


# Other useful tools

# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.

# Remember, to create a regular expression containing \d or \s, you’ll need to escape the \ for the string, so you’ll type "\\d" or "\\s".!!!

# You can use alternation to pick between one or more alternative patterns. For example, abc|d..f will match either ‘“abc”’, or "deaf". Note that the precedence for | is low, so that abc|xyz matches abc or xyz not abcyz or abxyz. Like with mathematical expressions, if precedence ever gets confusing, use parentheses to make it clear what you want:

str_view(c("grey", "gray"), "gr(e|a)y")


# Create regular expressions to find all words that:
# Start with a vowel.
str_view(words, "^(o|a|e|i)", match = T)

# That only contain consonants. (Hint: thinking about matching “not”-vowels.)
str_view(stringr::words, "^[^aeiou]+$", match=TRUE)

# End with ed, but not with eed.

str_view(words, "^ed$|[^e]ed$", match = T)
# End with ing or ise.

str_view(words, "ing$|ise$", match = T)


# Empirically verify the rule “i before e except after c”.


# Write a regular expression that matches a word if it’s probably written in British English, not American English
str_view(words, "lou", match = T)

# detect the presence or absence of a pattern in string
x <- c("apple", "banana", "pear")
str_detect(x, "e")

# How many common words start with t?
sum(str_detect(words, "^t"))

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# # Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]") #much easier
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

# select elements that match a pattern

# words ending with x
words[str_detect(words, "x$")]
str_subset(words, "x$")

length(str_subset(words, "x$"))

# position of words ending with x
str_which(words, "x$")

# where are the words containing x and how many x's are there?
str_count(words, "x")


x <- c("apple", "banana", "pear")
str_count(x, "a")
# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))


df <- tibble(
    word = words, 
    i = seq_along(word)
)

df %>% 
    filter(str_detect(words, "x$")) %>% 
    
    
    # Find all words that start or end with x.
    
    str_detect(words, "^x|x$", match = T)
str_subset(words, "^x|x$")

# Find all words that start with a vowel and end with a consonant.

str_view(words, "^[aeoui].*[^aeoui]$", match = T)
str_subset(words, "^[aeoui].*[^aeoui]$")

# Are there any words that contain at least one of each different vowel?

str_view(words, "^[aeoui].*[^aeoui]$", match = T)


# What word has the highest number of vowels? What word has the highest proportion of vowels? 
df %>% 
    mutate(
        n_of_vowels = str_count(word, "[aeiou]"),
        n_of_consonants = str_count(word, "[^aeiou]"),
        proportion_of_vowels = n_of_vowels/length(word)
    ) %>% 
    arrange(desc(n_of_vowels))



# str_extract()

head(sentences)

# let's find all sentences containing a colour

# now define colours
colours <- c("red", "orange", "blue", "yellow", "green")
colours2 <- str_c(colours, collapse = "|")

# find all sentences with colours
sent_with_colours <- str_subset(sentences, colours2)

# find which colours are present in each sentence - only the first matchis returned!
str_extract(sent_with_colours, colours2)

more <- sentences[str_count(sentences, colours2) > 1]
str_view_all(more, colours2)
str_extract(more, colours2)
str_extract_all(more, colours2) # it returns a list
# If you use simplify = TRUE, str_extract_all() will return a matrix with short matches expanded to the same length as the longest:
str_extract_all(more, colours2, simplify = TRUE)

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)


#### Factors ####

month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(c("Jan", "Oct", "Mar"), levels = month_levels)
y1
sort(y1) # a normal vector wouldnot sort this way

y2 <- factor(c("Jam", "Oct", "Mar"), levels = month_levels)
y2 # saves us from typos! if we make a typo, we get NA

# If you want a warning, you can use readr::parse_factor():
y2 <- parse_factor(c("Jam", "Oct", "Mar"), levels = month_levels)

# If you omit the levels, they’ll be taken from the data in alphabetical order:
factor(c("Jan", "Oct", "Mar"))

x1 <- c("M", "M", "F", "Unknown")
f1 <- factor(x1, levels = unique(x1))
f1

head(forcats::gss_cat) #A sample of categorical variables from the General Social survey

# When factors are stored in a tibble, you can’t see their levels so easily. One way to see them is with count():
gss_cat %>%
    count(race)

ggplot(data=gss_cat, aes(x = race)) +
    geom_bar()
# By default, ggplot2 will drop levels that don’t have any values. You can force them to display with:
ggplot(data=gss_cat, aes(x = race)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE)

levels(gss_cat$race) 

# Explore the distribution of rincome (reported income). What makes the default bar chart hard to understand? How could you improve the plot?

gss_cat %>% 
    count(rincome)

ggplot(gss_cat, aes(x = rincome)) +
    geom_bar() +
    coord_flip()

# What is the most common relig in this survey? What’s the most common partyid?
gss_cat %>% 
    count(relig) %>% 
    arrange(desc(relig))

# Which relig does denom (denomination) apply to? How can you find out with a table? How can you find out with a visualisation?

unique(gss_cat[,c('relig', "denom")])

ggplot(gss_cat, aes(x = relig)) +
    geom_bar() +
    coord_flip()

gss_cat %>%
    count(relig, denom) %>%
    arrange(desc(n)) %>% 
    filter(n > 200) %>% 
    ggplot(aes(x = relig, y = denom, size = n)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90))
# wynika z tego, ze wlasciwie moglismy zostawic tylko protestantow na osi x i ich dzielic juz na male podwyznania


gss_cat %>%
    filter(relig == "Protestant") %>% 
    ggplot(aes(x = denom)) +
    geom_bar() +
    coord_flip()


gss_cat %>%
    count(relig, denom) %>%
    arrange(desc(n)) %>% 
    filter(n > 200 & relig == "Protestant") %>% 
    ggplot(aes(x = fct_reorder(denom, n), y = n)) + #moze byc tez desc = T
    geom_col() +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("Denominations among protestants") +
    ylab("Number of people")

# Factor reordering

#  imagine you want to explore the average number of hours spent watching TV per day across religions
gss_cat %>% 
    group_by(relig) %>% 
    summarise(tvhours = mean(tvhours, na.rm = T),
              n = n()) %>% 
    ggplot(aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
    geom_point()

# instead of reordering the factor inside aes, we could have done it earlier

gss_cat %>% 
    group_by(relig) %>% 
    summarise(tvhours = mean(tvhours, na.rm = T),
              n = n()) %>% 
    mutate(relig = fct_reorder(relig, tvhours)) %>% 
    ggplot(aes(x = tvhours, y = relig)) +
    geom_point()

# What if we create a similar plot looking at how average age varies across reported income level?

gss_cat %>% 
    group_by(rincome) %>% 
    summarise(avg_age = mean(age, na.rm = T),
              n = n()) %>% 
    mutate(rincome = fct_reorder(rincome, avg_age)) %>% 
    ggplot(aes(avg_age, rincome)) +
    geom_point()
# bez sensu porzadkowac rincome, poniewaz juz byl uporzadkowany! jedynie ma sens wyciagnac kategorie not applicable na dol itd. z uzyciem relevel

gss_cat %>% 
    group_by(rincome) %>% 
    summarise(avg_age = mean(age, na.rm = T),
              n = n()) %>%  
    ggplot(aes(avg_age, fct_relevel(rincome, "Not applicable"))) +
    geom_point()

# porzadkowanie na wykresie slupkowym na pomoca fct_infreq
gss_cat %>%
    mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
    ggplot(aes(marital)) +
    geom_bar()


# modifying factor levels with fct_recode()

gss_cat %>% count(partyid)

gss_cat %>%
    mutate(partyid = 
               fct_recode(partyid,
                          "Republican, strong"    = "Strong republican",
                          "Republican, weak"      = "Not str republican",
                          "Independent, near rep" = "Ind,near rep",
                          "Independent, near dem" = "Ind,near dem",
                          "Democrat, weak"        = "Not str democrat",
                          "Democrat, strong"      = "Strong democrat"
               )) %>%
    count(partyid)

# To combine groups, you can assign multiple old levels to the same new level:

gss_cat %>%
    mutate(partyid = 
               fct_recode(partyid,
                          "Republican"= "Strong republican",
                          "Republican"= "Not str republican",
                          "Independent" = "Ind,near rep",
                          "Independent" = "Ind,near dem",
                          "Democrat"= "Not str democrat",
                          "Democrat" = "Strong democrat",
                          "Other"  = "No answer",
                          "Other"   = "Don't know",
                          "Other"   = "Other party"
               )) %>%
    count(partyid)

# If you want to collapse a lot of levels, fct_collapse() is a useful variant of fct_recode(). For each new variable, you can provide a vector of old levels:

gss_cat %>%
    mutate(partyid = 
               fct_collapse(partyid,
                            other = c("No answer", "Don't know", "Other party"),
                            rep = c("Strong republican", "Not str republican"),                         ind = c("Ind,near rep", "Independent", "Ind,near dem"),                      dem = c("Not str democrat", "Strong democrat")
               )) %>%
    count(partyid)


# Sometimes you just want to lump together all the small groups to make a plot or table simpler. That’s the job of fct_lump():

gss_cat %>%
    mutate(relig = fct_lump(relig)) %>%
    count(relig)

gss_cat %>%
    mutate(marital = fct_lump(marital)) %>%
    count(marital)

# The default behaviour is to progressively lump together the smallest groups, ensuring that the aggregate is still the smallest group. In this case it’s not very helpful: it is true that the majority of Americans in this survey are Protestant, but we’ve probably over collapsed.

# Instead, we can use the n parameter to specify how many groups (excluding other) we want to keep:

gss_cat %>%
    mutate(relig = fct_lump(relig, n = 10)) %>%
    count(relig, sort = TRUE)

# How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?

# with absolute numbers
gss_cat %>% 
    group_by(year, partyid) %>% 
    count() %>% 
    ggplot(aes(year, n, col = partyid)) +
    geom_line()

# now with proportions
gss_cat %>% 
    group_by(year, partyid) %>% 
    count() %>% 
    mutate(proportion = n/sum(gss_cat$year==year)) %>% 
    ggplot(aes(year, proportion, col = partyid)) +
    geom_line()


# How could you collapse rincome into a small set of categories?

gss_cat %>% 
    mutate(rincome = fct_collapse(
        rincome,
        up_to_25ths = c("$20000 - 24999", "Lt $1000", "$1000 to 2999")
        # itd.
    ))


#### Dates and times ####

today()
now()

# the below functions are very useful, we specify the order of year, month and day
ymd("2017-01-31")
mdy("January 31st, 2017")
mdy("January 31, 2017")
dmy("31-Jan-2017")
dmy("31/Jan/2017")
ymd("20170131")
dmy("1/03/1992")

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

flights %>% 
    select(year, month, day, hour, minute) %>% 
    mutate(departure = ymd_hm(paste(paste(year, month, day, sep = "-"), paste(hour, minute, sep=":"))))

# or much simpler - use make_datetime
flights %>% 
    select(year, month, day, hour, minute) %>% 
    mutate(departure = make_datetime(year, month, day, hour, minute))

as_date(now())

# what can we do with dates and times?
datetime <- ymd_hms("2016-07-08 12:34:56")

year(datetime)

month(datetime)
month(datetime, label = T, abbr = F)

mday(datetime) #day of the month


yday(datetime) #day of the year

wday(datetime, label = T, abbr = F)
wday(datetime, week_start = 1) #ustawiamy, ze tydzien zaczyna sie w poniedzialek(1), a nie jak defaultowo w niedziele(7)


# We can use wday() to see that more flights depart during the week than on the weekend:
flights %>%
    mutate(wday = wday(time_hour, week_start = 1, label = T)) %>% 
    group_by(wday) %>% 
    count() %>% 
    ggplot(aes(wday, n)) +
    geom_col() +
    ylim(0, 60000)

# There’s an interesting pattern if we look at the average departure delay by minute within the hour. It looks like flights leaving in minutes 20-30 and 50-60 have much lower delays than the rest of the hour!

# mine
breaks <- c(0,10,20,30,40,50,60)
flights %>%
    mutate(minute_interval = cut(dep_time %% 100, breaks)) %>% 
    group_by(minute_interval) %>% 
    summarise(avg_arr_delay = mean(arr_delay, na.rm = T)) %>% 
    filter(!is.na(minute_interval)) %>% 
    mutate(minute_interval = fct_recode(minute_interval, 
                                        '0-10 minutes' = '(0,10]',
                                        '10-20 minutes' = '(10,20]',
                                        '20-30 minutes' = '(20,30]',
                                        '30-40 minutes' = '(30,40]',
                                        '40-50 minutes' = '(40,50]',
                                        '50-60 minutes' = '(50,60]'
    )) %>% 
    ggplot(aes(minute_interval, avg_arr_delay)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))

# from the book
flights %>%
    mutate(minute = dep_time %% 100) %>% 
    group_by(minute) %>% 
    summarise(avg_delay = mean(arr_delay, na.rm = T)) %>% 
    ggplot(aes(minute, avg_delay)) +
    geom_line()


# rounding to the nearest unit 
floor_date()
round_date()
ceiling_date()

# przypisywanie dnia do odpowiedniego tygodnia!!! great! 
floor_date(ymd("2018-01-30"), "week", week_start = 1) #first day of week is Monday

# plotting the number of flights per week
flights %>% 
    count(week = floor_date(time_hour, "week", week_start = 1)) %>% 
    ggplot(aes(week, n)) +
    geom_line()

# computing difference between a rounded and unrounded date can be particularly useful
make_datetime_100 <- function(year, month, day, time) {
    make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
    filter(!is.na(dep_time), !is.na(arr_time)) %>% 
    mutate(
        dep_time = make_datetime_100(year, month, day, dep_time),
        arr_time = make_datetime_100(year, month, day, arr_time),
        sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
        sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
    ) %>% 
    select(origin, dest, ends_with("delay"), ends_with("time"))

#distribution of flights across the course of the day for every day of the year
flights_dt %>% 
    mutate(dep_hour = update(dep_time, yday = 1)) %>% 
    ggplot(aes(dep_hour)) +
    geom_freqpoly(binwidth = 300)

# now i want to extract all flights at all times from the whole year
flights %>% 
    mutate(dep_hour = chron::times(paste(hour,minute,"00",sep=":"))) %>% 
    group_by(dep_hour) %>% 
    count()

flights %>% 
    mutate(dep_hour = chron::times(paste(hour,minute,"00",sep=":"))) %>% 
    ggplot(aes(dep_hour)) +
    geom_freqpoly() #niestety nie umie pokazac samego czasu na osi x!


# On what day of the week should you leave if you want to minimise the chance of a delay?

flights %>% 
    mutate(weekday = wday(dep_time, week_start = 1)) %>% 
    group_by(weekday) %>% 
    summarise(avg_delay = mean(arr_delay, na.rm = T)) #Saturday

# durations

# how old am I?
# How old is Hadley?
age <- today() - ymd(19920301)
age
as.duration(age) #always in seconds to avoid ambiguity!

# we can get the duration in seconds of many formats
dseconds(15)
dminutes(15)
dhours(15)
ddays(15)
dweeks(15)
dyears(15)

(tomorrow <- today() + ddays(1))
(last_year <- today() - dyears(1))


#### FUNCTIONS ####


is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0 # test for read permission

is_directory("C:/Users/klaudia.stano/Desktop/Szeregi_czasowe_R/monthly_sales.csv")
is_directory("C:/Users/klaudia.stano/Desktop/Szeregi_czasowe_R")
is_readable("C:/Users/klaudia.stano/Desktop/Szeregi_czasowe_R/monthly_sales.csv")

# if you end up with a very long series of chained if statements, you should consider rewriting. One useful technique is the switch() function. It allows you to evaluate selected code based on position or name.

op <- function(x, y, op) {
    switch(op,
           plus = x + y,
           minus = x - y,
           times = x * y,
           divide = x / y,
           stop("Unknown op!")
    )
}


x <- "three"
y <- 0
switch(x,
       one = {y <- 5},
       two = {y <- 12},
       three = {y <- 432})
y

# w funkcjach warto sprawdzac czy inputy sa dobre, ale czasami stop() za dlugo zajmuje, wiec mozna wtedy uzyc stopifnot() - jest szybsze!

stopifnot(is.logical(na.rm), length(na.rm) == 1)
stopifnot(length(x) == length(w))
# Note that when using stopifnot() you assert what should be true rather than checking for what might be wrong.


#### VECTORS ####

# Functions like is.atomic(), is.numeric() can give unexpected results so it's better to use is_* functions from the purr package.
# Each predicate also comes with a “scalar” version, like is_scalar_atomic(), which checks that the length is 1. This is useful, for example, if you want to check that an argument to your function is a single logical value.

(a <- c("Klaudia", 1, TRUE))
is_character(a)
is_scalar_character(a) #checks if vector is of length 1

# It’s also important to understand what happens when you try and create a vector containing multiple types with c(): the most complex type always wins

typeof(c(TRUE, 1L)) #integer
typeof(c(1L, 1.5)) #double
typeof(c(1.5, "a")) #character

b <- c(1, 2.5, 4)
is.numeric(b)
is.vector(b)
is.list(b)

c <- 7
is.integer(c) #FALSE!
is_integer(c) #FALSE also
c

d <- 7L
is.integer(d) #TRUE


# As well as implicitly coercing the types of vectors to be compatible, R will also implicitly coerce the length of vectors. This is called vector recycling, because the shorter vector is repeated, or recycled, to the same length as the longer vector.
# This is generally most useful when you are mixing vectors and “scalars”. I put scalars in quotes because R doesn’t actually have scalars: instead, a single number is a vector of length 1. Because there are no scalars, most built-in functions are vectorised, meaning that they will operate on a vector of numbers. That’s why, for example, this code works:

sample(10) + 100
runif(10) > 0.5

# It’s intuitive what should happen if you add two vectors of the same length, or a vector and a “scalar”, but what happens if you add two vectors of different lengths?
1:10 + 1:2
1:10 + 1:3 #error, the shortest vector cannot be expanded to 10 places

# in order to avoid mistakes, dplyr will throw an error, you have to repeat the numbers yourself
tibble(x = 1:4, y = 1:2) #error
tibble(x = 1:4, y = rep(1:2, 2))
data.frame(x = 1:4, y = 1:2) #no error!

# vectors can be named during creation
c(x = 1, y = 2, z = 4)

# or after with the purrr set_names() function
set_names(1:3, c("a", "b", "c"))


# lists
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a[4] #it returns a new, smaller list
str(a[4])
a[[4]] # [[]] extracts a single component from a list. it removes te level of hierarchy
str(a[[4]])
# remember the example with pepper shaker :)

# dates
x <- as.Date("1971-01-01")
unclass(x)  #365
# Why? Dates in R are numeric vectors that represent the number of days since 1 January 1970.

# Date-times are numeric vectors with class POSIXct that represent the number of seconds since 1 January 1970.
x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x) #3600

#### ITERATION ####
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

# let's see the medians of all columns

medians <- rep(0, 4)

for (i in 1:ncol(df)) {
    medians[i] <- median(pull(df, i), na.rm = T)
}

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
    output[[i]] <- median(df[[i]])      # 3. body
}
output

# for loop variations

# 1. Modifying an existing object instead of creating a new one
# lets rescale all columns in df

rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}

for (i in seq_along(df)) {
    df[[i]] <- rescale01(df[[i]])
}
df

# 2. Unknown output length
# imagine you want to simulate some random vectors of random lengths. You might be tempted to solve this problem by progressively growing the vector:

#zalozmy, ze chcemy losowac zawsze wektor o sredniej 0

n <- sample(100, 1) #losujemy dlugosc wektora
vector <- rnorm(n, 0) #losujemy wektor

# first try (inefficient) - three vectors with different means and then we paste them
means <- c(0, 1, 2)
output <- double()
for (i in seq_along(means)) {
    n <- sample(100, 1)
    output <- c(output, rnorm(n, means[[i]]))
}
output
str(output)

# it is however better to save the results in a list and then combine into a single vector after the loop is done!

out <- vector(mode = "list", length(means))

for (i in seq_along(means)) {
    n <- sample(100, 1)
    out[[i]] <- rnorm(n, means[[i]])
}

output <- c(out[[1]], out[[2]], out[[3]])

# or simply...

output <- unlist(out)
output

# if we generated a string we should paste everything with collapse = ""
# if we generated rows in a data frame we should use dplyr::bind_rows(output)

# use a while loop to find how many tries it takes to get three heads in a row

flip <- function() {
    sample(c("H", "T"), 1)
}

flip()

flips <- vector("list", length = 50)
flips[1:50] <- 0
#nheads <- vector("numeric")

for (i in 1:50) {
    nheads <- 0
    while (nheads < 3) {
        
        flip()
        flips[[i]] <- flips[[i]] + 1
        
        if (flip() == "H") {
            nheads <- nheads + 1
        }
    }
}

mean(unlist(flips))

# make this function work only for numeric columns

col_summary <- function(df, fun) {
    out <- vector("double", length(df))
    for (i in seq_along(df)) {
        out[i] <- fun(df[[i]])
    }
    out
}

col_summary_num <- function(df, fun) {
    
    df_numeric <- df[,sapply(df, is.numeric)]
    out <- vector("double", length(df_numeric))
    names(out) <- colnames(df_numeric)
    for (i in seq_along(df_numeric)) {
        out[i] <- fun(df_numeric[[i]], na.rm = T)
        names(out[i]) <- colnames(df_numeric)[i]
    }
    out
}

col_summary(arranged, mean)
col_summary_num(arranged, mean)

#  In the rest of the chapter, you’ll learn about and use the purrr package, which provides functions that eliminate the need for many common for loops. The apply family of functions in base R (apply(), lapply(), tapply(), etc) solve a similar problem, but purrr is more consistent and thus is easier to learn.

# The pattern of looping over a vector, doing something to each element and saving the results is so common that the purrr package provides a family of functions to do it for you. There is one function for each type of output:

# map() makes a list.
# map_lgl() makes a logical vector.
# map_int() makes an integer vector.
# map_dbl() makes a double vector.
# map_chr() makes a character vector.

# Each function takes a vector as input, applies a function to each piece, and then returns a new vector that’s the same length (and has the same names) as the input. The type of the vector is determined by the suffix to the map function.

map(list(c(1,2), c(2,3)), mean)

# Once you master these functions, you’ll find it takes much less time to solve iteration problems. But you should never feel bad about using a for loop instead of a map function. The map functions are a step up a tower of abstraction, and it can take a long time to get your head around how they work. The important thing is that you solve the problem that you’re working on, not write the most concise and elegant code (although that’s definitely something you want to strive towards!).

# Some people will tell you to avoid for loops because they are slow. They’re wrong! (Well at least they’re rather out of date, as for loops haven’t been slow for many years). The chief benefits of using functions like map() is not speed, but clarity: they make your code easier to write and to read.

# We can use these functions to perform the same computations as the last for loop. Those summary functions returned doubles, so we need to use map_dbl()

map_dbl(arranged.df, mean, na.rm = T)
# equals
df %>% map_dbl(mean)

# Imagine you want to fit a linear model to each group in a dataset. The following toy example splits the up the mtcars dataset in to three pieces (one for each value of cylinder) and fits the same linear model to each piece:

models <- mtcars %>% 
    split(.$cyl) %>%  #.$ is necessary here, we have now data separated into list elements depending on the variable cyl 
    map(function(df) lm(mpg~wt, data = df))

# or simpler

models <- mtcars %>% 
    split(.$cyl) %>% 
    map(~lm(mpg ~ wt, data = .)) # Here I’ve used . as a pronoun: it refers to the current list element (in the same way that i referred to the current index in the for loop).

models %>% 
    map(base::summary) %>% 
    map_dbl(~.$r.squared)

models %>% 
    map(summary) %>% 
    map_dbl("r.squared")

# You can also use an integer to select elements by position:

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x
x %>% map_dbl(2)

# excercises
# Write code that uses one of the map functions to:
# Compute the mean of every column in mtcars.

mtcars %>% 
    map_dbl(mean)
# Determine the type of each column in nycflights13::flights.

nycflights13::flights %>% 
    map_chr(typeof)
# Compute the number of unique values in each column of iris.
iris %>%
    map(unique) %>% 
    map_dbl(length)

# Generate 10 random normals for each of μ: -10, 0, 10, 100
c(-10, 0, 10, 100) %>% 
    map(rnorm, n = 10)

# How can you create a single vector that for each column in a data frame indicates whether or not it’s a factor?
iris %>% 
    map_lgl(is.factor)


# safely() function - dealing with errors - similar to try()

safe_log <- safely(log)
str(safe_log(10)) #if everything is ok, the error part will be NULL
str(safe_log("a")) #if not, the result part will be NULL, and the error part will contain the errors

x <- list(1, 10, "a")
(y1 <- x %>% map(log)) # this will throw just an error
(y2 <- x %>% map(safely(log))) #this will give us explanations! and show us where the error is present :) t make it even clearer we can transpose it:
y2 <- y2 %>% transpose()

is_ok <- y2$error %>% map_lgl(is_null)
x[!is_ok] # we see what the problem is

y2$result[is_ok] %>% flatten_dbl() #Flatten a list of lists into a simple vector.

# in purr we can also use possibly(), which always succeeds. we provide what should be the output if there is an error
(y3 <- x %>% map_dbl(possibly(log, otherwise = NA)))


# So far we’ve mapped along a single input. But often you have multiple related inputs that you need iterate along in parallel. That’s the job of the map2() and pmap() functions. For example, imagine you want to simulate some random normals with different means.

mu <- list(5, 10, -3) #means
sigma <- list(1, 5, 10) #sds

map2(mu, sigma, rnorm, n = 5) %>% str()

# Note that the arguments that vary for each call come before the function; arguments that are the same for every call come after.

# if we want more inputs than 2 we will use pmap(). For example, if we wanted to differentiate also the number of samples (before it was always n = 5)
n <- list(1, 3, 5)
args1 <- list(n = n, mean = mu, sd = sigma)
args1 %>%
    pmap(rnorm) %>% 
    str()

# walk functions

# alk is an alternative to map that you use when you want to call a function for its side effects, rather than for its return value. You typically do this because you want to render output to the screen or save files to disk - the important thing is the action, not the return value. Here’s a very simple example:

x <- list(1, "a", 3)

x %>% 
    walk(print)

# however, pwalk or walk2 are more useful than simple walk()


# For example, if you had a list of plots and a vector of file names, you could use pwalk() to save each file to the corresponding location on disk:

plots <- mtcars %>% 
    split(.$cyl) %>%  #divide data based on cyl values
    map(~ggplot(., aes(mpg, wt)) + geom_point()) #fit ggplot to each data set
paths <- stringr::str_c(names(plots), ".pdf") #create paths to save plots

pwalk(list(paths, plots), ggsave) #save plots to predefined names

# other useful functions
iris %>% 
    keep(is.factor) %>% #keeps input where TRUE
    str()

iris %>% 
    discard(is.factor) %>% #drops input where TRUE
    str()

x <- list(1:5, letters, list(10))
x

x %>% 
    some(is_character)

x %>% 
    some(is_logical)

x %>% 
    every(is_vector)

x %>% 
    every(is_double)

# detect() finds the first element where the predicate is true; detect_index() returns its position.

x <- sample(10)
x


x %>% 
    detect(~ . > 5)

x %>% 
    detect_index(~ . > 5)


vs <- list(
    c(1, 3, 5, 6, 10),
    c(1, 2, 3, 7, 8, 10),
    c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)

# excerxises
# Implement your own version of every() using a for loop. Compare it with purrr::every(). What does purrr’s version do that your version doesn’t?

x <- list(5, TRUE, "a", 3.4)
x
y <- c(2,3,4,5,6)
x

every_mine <- function(x, fun) {
    
    for (i in seq_along(x)) {
        if (!sapply(x[[i]], fun)) {
            return(FALSE)
        }
    }
    TRUE
}

every_mine(x, is_double)

#### MODELS ####
library(modelr)
options(na.action = na.warn)

# Lets take a look at the simulated dataset sim1, included with the modelr package. It contains two continuous variables, x and y. Let’s plot them to see how they’re related:

sim1_mod <- lm(y ~ x, data = sim1)
intercept <- lm(y ~ x, data = sim1)[[1]][1]
slope <- lm(y ~ x, data = sim1)[[1]][2]

ggplot(sim1, aes(x, y)) + 
    geom_point() +
    geom_abline(aes(intercept = intercept, slope = slope), alpha = 0.5)

# linear models are sensitive to unusual values - we can see it here if we rerun the code a few times
sim1a <- tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2)
)

ggplot(sim1a, aes(x, y)) + 
    geom_point() +
    geom_abline(aes(intercept = lm(y ~ x, data = sim1a)[[1]][1], slope = lm(y ~ x, data = sim1a)[[1]][2]), alpha = 0.5)

# one way to make models more robust is to use a different distance measure, for example a mean absolute distance instead of root-mean-squared distance

# original measure_distance
model1 <- function(a, data) {
    a[1] + data$x * a[2]
}

measure_distance_rmsd <- function(mod, data) {
    diff <- data$y - model1(mod, data)
    sqrt(mean(diff ^ 2))
}

# mean absolute distance
measure_distance_mad <- function(mod, data) {
    diff <- data$y - model1(mod, data)
    mean(abs(diff))
}

best_rmsd <- optim(c(0, 0), measure_distance_rmsd, data = sim1a)
best_rmsd_intercept <- best_rmsd$par[1]
best_rmsd_slope <- best_rmsd$par[2]

best_mad <- optim(c(0, 0), measure_distance_mad, data = sim1a)
best_mad_intercept <- best_mad$par[1]
best_mad_slope <- best_mad$par[2]

ggplot(sim1a, aes(x, y)) + 
    geom_point() +
    geom_abline(aes(intercept = best_rmsd_intercept, slope = best_rmsd_slope), alpha = 0.5, col = "blue") +
    geom_abline(aes(intercept = best_mad_intercept, slope = best_mad_slope), alpha = 0.5, col = "red")

sim1a <- tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2)
)

# w sumie wychodza podobne wyniki i przy rmsd i przy mad

# Predictions

#To visualise the predictions from a model, we start by generating an evenly spaced grid of values that covers the region where our data lies. The easiest way to do that is to use modelr::data_grid(). Its first argument is a data frame, and for each subsequent argument it finds the unique variables and then generates all combinations:

grid <- sim1 %>% 
    data_grid(x) 
grid

# Next we add predictions. We’ll use modelr::add_predictions() which takes a data frame and a model. It adds the predictions from the model to a new column in the data frame:

grid <- grid %>% 
    add_predictions(sim1_mod, var = "pred_lm") 
grid

# Next, we plot the predictions. You might wonder about all this extra work compared to just using geom_abline(). But the advantage of this approach is that it will work with any model in R, from the simplest to the most complex. You’re only limited by your visualisation skills.

ggplot(sim1, aes(x)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

# Residuals

# The flip-side of predictions are residuals. The predictions tells you the pattern that the model has captured, and the residuals tell you what the model has missed. The residuals are just the distances between the observed and predicted values that we computed above.

# We add residuals to the data with add_residuals(), which works much like add_predictions(). Note, however, that we use the original dataset, not a manufactured grid. This is because to compute residuals we need actual y values.

sim1 <- sim1 %>% 
    add_residuals(sim1_mod, var = "resid_lm")
sim1

# visualizing resids
ggplot(sim1, aes(resid)) + 
    geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) + 
    geom_ref_line(h = 0) +
    geom_point() # This looks like random noise, suggesting that our model has done a good job of capturing the patterns in the dataset.

# Excercises

# Instead of using lm() to fit a straight line, you can use loess() to fit a smooth curve. Repeat the process of model fitting, grid generation, predictions, and visualisation on sim1 using loess() instead of lm(). How does the result compare to geom_smooth()?

loess <- loess(y ~ x, data = sim1)

sim1_loess <- modelr::sim1

grid_loess <- sim1_loess %>%
    data_grid(x)
grid_loess

grid_loess <- grid_loess %>% 
    add_predictions(loess, var = "pred_loess")
grid_loess

sim1_loess <- sim1_loess %>% 
    add_predictions(loess, var = "pred_loess") %>% 
    add_residuals(loess, var = "resid_loess")
sim1_loess

ggplot(sim1, aes(x)) + 
    geom_point(aes(y = y)) +
    geom_line(aes(y = pred_lm, col = "1"), data = grid) +
    geom_line(aes(y = pred_loess, col = "2"), data = grid_loess) +
    scale_color_discrete(name = "prediction method", labels = c("lm", "loess"))

rmse_loess <- sqrt(sum((sim1_loess$resid_loess)^2)/length(sim1_loess$resid_loess))
rmse_lm <- sqrt(sum((sim1$resid_lm)^2)/length(sim1$resid_lm))
rmse_loess #troche mniejsze rmse
rmse_lm

mae_loess <- mean(abs(sim1_loess$resid_loess))
mae_lm <- mean(abs(sim1$resid_lm))

mae_loess #znow nieco mniejszy blad mae
mae_lm

ggplot(sim1_loess, aes(resid_loess)) +
    geom_freqpoly(binwidth = 0.5)

ggplot(sim1_loess, aes(x = x, y = resid_loess)) +
    geom_point()


model_matrix(sim1, y ~ x) #regressors in the model

# categorical variables in the model - sim2 dataset

ggplot(sim2) + 
    geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
    data_grid(x) %>% 
    add_predictions(mod2)
grid


# Effectively, a model with a categorical x will predict the mean value for each category. (Why? Because the mean minimises the root-mean-squared distance.) That’s easy to see if we overlay the predictions on top of the original data:

ggplot(sim2, aes(x)) + 
    geom_point(aes(y = y)) +
    geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

# You can’t make predictions about levels that you didn’t observe. Sometimes you’ll do this by accident so it’s good to recognise this error message:

tibble(x = "e") %>% 
    add_predictions(mod2)

# up to 23.4.2


# Strings up to 14.4.3.1

# to do next time and add solutions to joins
# What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?

# You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. Confirm or reject this hypothesis using the tools you’ve learned above.
