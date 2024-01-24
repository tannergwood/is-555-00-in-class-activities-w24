library(tidyverse)

raw <- read_csv('https://www.dropbox.com/scl/fi/ug8tbxsdd2qtsfqwnnox1/dollar_store.csv?rlkey=fu36g6uhfpx8u644d1rpsq11i&dl=1')

ds <- janitor::clean_names(raw)

#str_detect() - searches 
#str_remove() - 
#str_replace() - 
#str_extract()

ds %>% 
  select(unit_size) %>% 
  mutate(unit_size_c = parse_number(unit_size)) %>% 
  mutate(unit_type = str_extract(unit_size, 'each|ounce|pound')) %>% 
  mutate(unit_type = str_replace(unit_type, "Unit Size not available", "N/A")) %>% 
  count(unit_type)

ds %>% 
  select(product_info) %>% 
  mutate(product_info = str_remove_all(product_info, "Brand:|Product:"))

ds %>% 
  select(description) %>% 
  mutate(description = str_remove_all(description, "\\[|\\]|\\'")) %>% 
  mutate(description = na_if(description, ""))

separate_wider_delim --- MOST COMMON, exploits structure in some column
separate_wider_position()
separate_wider_regex()

ds %>% 
  select(product_info) %>% 
  separate_wider_delim(product_info, 
                       delim = ' - ',
                       names = c("brand", "product"),
                       too_many = 'merge')

#parse_number casts it as a number, as.numeric() also does it but its kinda the old way
#parse_number offers some error handling and warning
# Convert to number formats: price, star_rating, review_count, stock_status, unit_size
#   Goals:   (1) Don't lose information
#            (2) Missing where appropriate, but only where appropriate
ds %>% glimpse


# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Convert date columns to date and/or date-time columns, then calculate how many
# days each product sat on the shelf before its first sale.

library(lubridate)

mdy
mdy_h()
mdy_hm
mdy_hms
ds %>% 
  select(date_added, first_sold_day, first_sold_time) %>% 
  mutate(date_added_c = dmy(date_added)) %>% 
  mutate(first_sold_dt = mdy_hms(str_c(first_sold_day, first_sold_time)))


# date is always spit out and displayed yyyy-mm-dd in the console, but it will be stored 
# how you specified
