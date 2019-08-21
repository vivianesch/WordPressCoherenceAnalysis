# Grouping and Chaining
# break up your dataset into groups of rows based on the values of one or more 
#... variables. 

library(dplyr)

# Function group_by()
cran <- tbl_df(mydf)
rm("mydf")
by_package <- group_by(cran, package)
summarise(by_package, mean(size))


# Compute four values, in the following order, from
# the grouped data:
#
# 1. count = n()
# 2. unique = n_distinct(ip_id)
# 3. countries = n_distinct(country)
# 4. avg_bytes = mean(size)
#
# A few thing to be careful of:
#
# 1. Separate arguments by commas
# 2. Make sure you have a closing parenthesis
# 3. Check your spelling!
# 4. Store the result in pack_sum (for 'package summary')
#
# You should also take a look at ?n and ?n_distinct, so
# that you really understand what is going on.
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))

#We need to know the value of 'count' that splits the data into the top 1% and
#bottom 99% of packages based on total downloads. In statistics, this is called
# the 0.99, or 99%, sample quantile. Use quantile(pack_sum$count, probs = 0.99) to
#determine this number.
quantile(pack_sum$count, probs = 0.99)
top_counts <- filter(pack_sum, count > 679)
View(top_counts)
#desc order
top_counts_sorted <- arrange(top_counts, desc(count))
#let's find the 0.99, or 99%, quantile for the 'unique' variable
quantile(pack_sum$unique, probs = 0.99)
top_unique <- filter(pack_sum, unique > 465)
top_unique_sorted <- arrange(top_unique, desc(unique))

# Chaining allows you to string together multiple function calls 
# in a way that is compact and readable, 
# while still accomplishing the desired result.
# Don't change any of the code below. Just type submit()
# when you think you understand it.

# We've already done this part, but we're repeating it
# here for clarity.

by_package <- group_by(cran, package)
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))

# Here's the new bit, but using the same approach we've
# been using this whole time.

top_countries <- filter(pack_sum, countries > 60)
result1 <- arrange(top_countries, desc(countries), avg_bytes)

# Print the results to the console.
print(result1)

# Don't change any of the code below. Just type submit()
# when you think you understand it. If you find it
# confusing, you're absolutely right!

result2 <-
  arrange(
    filter(
      summarize(
        group_by(cran,
                 package
        ),
        count = n(),
        unique = n_distinct(ip_id),
        countries = n_distinct(country),
        avg_bytes = mean(size)
      ),
      countries > 60
    ),
    desc(countries),
    avg_bytes
  )

print(result2)

# Read the code below, but don't change anything. As
# you read it, you can pronounce the %>% operator as
# the word 'then'.
#
# Type submit() when you think you understand
# everything here.

result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
            unique = n_distinct(ip_id),
            countries = n_distinct(country),
            avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)

# Print result to console
print(result3)


# select() the following columns from cran. Keep in mind
# that when you're using the chaining operator, you don't
# need to specify the name of the data tbl in your call to
# select().
#
# 1. ip_id
# 2. country
# 3. package
# 4. size
#
# The call to print() at the end of the chain is optional,
# but necessary if you want your results printed to the
# console. Note that since there are no additional arguments
# to print(), you can leave off the parentheses after
# the function name. This is a convenient feature of the %>%
# operator.

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20)
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb))
  print






