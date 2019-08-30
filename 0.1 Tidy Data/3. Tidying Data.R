# tidy data tidyr
#| The author of tidyr, Hadley Wickham, discusses his philosophy of tidy data in
# his 'Tidy Data' paper: http://vita.had.co.nz/papers/tidy-data.pdf


# Tidy data is formatted in a standard way that facilitates exploration and
# analysis and works seamlessly with other tidy data tools. Specifically, tidy
# data satisfies three conditions:
#
# 1) Each variable forms a column
# 2) Each observation forms a row
# 3) Each type of observational unit forms a table

# Exemples: 
# 1: Column headers are values, not variable names
# 2. Variables are stored in both rows and columns
# 3: A single observational unit is stored in multiple tables
# 4: Multiple types of observational units are stored in the same table
# 5: Multiple variables are stored in one column



#Read
library(readr)
TicketW <- read_csv("TicketW.csv")

library(tidyr)
# The first problem is when you have column headers 
# that are values, not variable names.
students
#gather arguments (Dataset, key and value, -exeption colunm) 
gather(students, sex, count, -grade)

# The second messy data case we'll look at is when multiple variables 
# are stored in one column.
students2
res <- gather(students2, sex_class, count, -grade)
res
separate(data = res, col = sex_class, into = c("sex", "class"))

# Repeat your calls to gather() and separate(), but this time
# use the %>% operator to chain the commands together without
# storing an intermediate result.
#
# If this is your first time seeing the %>% operator, check
# out ?chain, which will bring up the relevant documentation.
# You can also look at the Examples section at the bottom
# of ?gather and ?separate.
#
# The main idea is that the result to the left of %>%
# takes the place of the first argument of the function to
# the right. Therefore, you OMIT THE FIRST ARGUMENT to each
# function.
#
students2 %>%
  gather(sex_class, count, -grade) %>%
  separate(sex_class, c("sex", "class")) %>%
  print

#A third symptom of messy data is when variables are stored in both rows and columns.
students3
# This script builds on the previous one by appending
# a call to spread(), which will allow us to turn the
# values of the test column, midterm and final, into
# column headers (i.e. variables).
#
# You only need to specify two arguments to spread().
# Can you figure out what they are? (Hint: You don't
# have to specify the data argument since we're using
# the %>% operator.
#
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  print
# We want the values in the class columns to be
# 1, 2, ..., 5 and not class1, class2, ..., class5.
#
# Use the mutate() function from dplyr along with
# parse_number(). Hint: You can "overwrite" a column
# with mutate() by assigning a new value to the existing
# column instead of creating a new column.
#
# Check out ?mutate and/or ?parse_number if you need
# a refresher.
#
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = parse_number(class)) %>%
  print
# Complete the chained command below so that we are
# selecting the id, name, and sex column from students4
# and storing the result in student_info.
#
student_info <- students4 %>%
  select(id,name, sex) %>%
  print

# Add a call to unique() below, which will remove
# duplicate rows from student_info.
#
# Like with the call to the print() function below,
# you can omit the parentheses after the function name.
# This is a nice feature of %>% that applies when
# there are no additional arguments to specify.
#
student_info <- students4 %>%
  select(id, name, sex) %>%
  unique() %>%
  print
# select() the id, class, midterm, and final columns
# (in that order) and store the result in gradebook.
#
gradebook <- students4 %>%
  select(id, class, midterm, final) %>%
  print

#mess data - two separeted data
#put the colunm status into which table
passed <- passed %>% mutate(status = "passed")
failed <- failed %>% mutate(status = "failed")
#put together the two table
bind_rows(passed, failed)

# The SAT is a popular college-readiness exam in the United States that consists of three sections:
#critical reading, mathematics, and writing. Students can earn up to 800 points on each section.
# This dataset presents the total number of students, for each combination of exam section and sex,
# within each of six score ranges. It comes from the 'Total Group Report 2013', which can be found
# here:
# http://research.collegeboard.org/programs/sat/data/cb-seniors-2013

# Accomplish the following three goals:
#
# 1. select() all columns that do NOT contain the word "total",
# since if we have the male and female data, we can always
# recreate the total count in a separate column, if we want it.
# Hint: Use the contains() function, which you'll
# find detailed in 'Special functions' section of ?select.
#
# 2. gather() all columns EXCEPT score_range, using
# key = part_sex and value = count.
#
# 3. separate() part_sex into two separate variables (columns),
# called "part" and "sex", respectively. You may need to check
# the 'Examples' section of ?separate to remember how the 'into'
# argument should be phrased.
#
sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  print

# Append two more function calls to accomplish the following:
#
# 1. Use group_by() (from dplyr) to group the data by part and
# sex, in that order.
#
# 2. Use mutate to add two new columns, whose values will be
# automatically computed group-by-group:
#
#   * total = sum(count)
#   * prop = count / total
#
sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) %>%
  mutate(total = sum(count),
         prop = count / total
  ) %>% print




