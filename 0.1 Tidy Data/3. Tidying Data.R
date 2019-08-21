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

library(tidyr)
# The first problem is when you have column headers 
# that are values, not variable names.
students
#gather arguments (Dataset, key and value, -exeption colunm) 
gather(students, sex, count, -grade)

# The second messy data case we'll look at is when multiple variables 
# are stored in one column.
students2


