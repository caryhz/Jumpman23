# Postmates Script
library(readr)
library(tidyverse)
library(lubridate)
library(ggmap)

# db <- read_csv("Desktop/Reference/Personal/Jumpman23/analyze_me.csv")

dim(db)

# rows 5983   cols 18

names(db)

# [1] "delivery_id"                         "customer_id"                        
# [3] "jumpman_id"                          "vehicle_type"                       
# [5] "pickup_place"                        "place_category"                     
# [7] "item_name"                           "item_quantity"                      
# [9] "item_category_name"                  "how_long_it_took_to_order"          
# [11] "pickup_lat"                          "pickup_lon"                         
# [13] "dropoff_lat"                         "dropoff_lon"                        
# [15] "when_the_delivery_started"           "when_the_Jumpman_arrived_at_pickup" 
# [17] "when_the_Jumpman_left_pickup"        "when_the_Jumpman_arrived_at_dropoff"


# general themes
# summary stats - how many orders, how many customers, how many deliveries we have, how many locations
# delivery by day performance - tableau
# how many orders per customer
# how many orders per category
# how many orders per place category


# delivery_id is not unique. there are 5983 records and 5214 unique delivery ids
# some delivery IDs were used multiple times. 656 delivery IDs were used more than once - each row is an item


db %>% 
  summarise(num_deliveries = n_distinct(delivery_id),
            num_jumpman = n_distinct(jumpman_id),
            total_rows = n(),
            num_customers = n_distinct(customer_id),
            min_num_items = sum(item_quantity, na.rm = T),
            max_num_places = n_distinct(pickup_place, na.rm = T)
            )

# summary stats 
# num_deliveries num_jumpman total_rows num_customers num_items num_places
# <int>       <int>      <int>         <int>     <dbl>      <int>
#   1           5214         578       5983          3192      5933        898

db %>% 
  group_by(pickup_place) %>% 
  count() %>% 
  arrange(desc(n))

# Shake Shack is most popular place

# Data Integrity Investigation


# Blue Ribbon Chicken name looks like a repeat
db %>% 
  filter(str_detect(pickup_place,"Blue Ribbon")) -> blue_db

# view(blue_db)

# Creates a db with all names containing 'blue ribbon'
# It's a common phrase within merchant names but Blue Ribbon Chicken is input incorrectly

blue_db %>% 
  filter(str_detect(pickup_place, "Sushi")) %>% 
  count()

blue_db %>% 
  filter(str_detect(pickup_place, "Chicken")) %>% 
  select(pickup_place)

# these have the same coordinates but different names

blue_db %>% 
  filter(str_detect(pickup_place, "Chicken")) %>% 
  select(pickup_place) %>% 
  n_distinct()


length(db$delivery_id)

n_distinct(db$delivery_id)

# Number of rows per unique delivery - deliveries are broken out by item ordered
db %>% 
  group_by(delivery_id) %>% 
  count() %>% 
  arrange(desc(n))

# item quantity

db %>% 
  group_by(item_quantity) %>% 
  count()


db %>% 
  summarise(sum_quant = sum(item_quantity,na.rm = T))

#5933 known quantity of items
# if we count NAs as 1 item, there's been 7163 items delivered 

db %>% 
  mutate(item_quantity2 = case_when(item_quantity >=1 ~ item_quantity,
                                    TRUE ~ 1)) -> db
sum(db$item_quantity2)
# 7163 minimum items ordered

# date range
summary(db$when_the_delivery_started)


# day of week investigation
db %>% 
  mutate(wday = wday(when_the_delivery_started, label = TRUE)) %>% 
  ggplot(aes(x = wday)) + theme_minimal() + 
  geom_bar()


# db %>% 
#   mutate(record_count = 1) -> db

# db$when_the_delivery_started2 <- as.Date(as.POSIXct(db$when_the_delivery_started,tz="EST"))

### PLOTTING

# pivoted counts of delivery methods for the unique deliveries - car/truck/van/etc
delivery_method <- read_csv("Desktop/Reference/Personal/Jumpman23/jumpman delivery.csv")

# ggplot(delivery_method, aes(x = reorder(DeliveryMethod, -DeliveryCount), y = DeliveryCount)) + geom_bar(stat = 'identity')
# 
# ggplot(delivery_method, aes(x = reorder(DeliveryMethod, -DeliveryCount), y = DeliveryCount)) + geom_bar(stat = 'identity')

delivery_method %>% 
  mutate(percent = DeliveryCount/sum(DeliveryCount)) -> delivery_method


ggplot(delivery_method, aes(x = reorder(DeliveryMethod, -percent), y = percent, fill = 3)) + 
  geom_bar(stat = 'identity') + scale_y_continuous(labels = scales::percent, limits = c(0,.8)) + 
  theme_minimal() + geom_text(aes(label=scales::percent(delivery_method$percent)),vjust=-0.2, size = 7) + 
  ylab("% of Deliveries") + xlab("") + 
  labs(title="Jumpman Delivery Method", 
       subtitle="Bikes are by far the most popular") + 
  theme(legend.position = "none", axis.text.x = element_text(size = 14), axis.title.y = element_text(size = 15),
        title = element_text(size = 20))



# Maps 

# register_google(key = "", write = TRUE)

# full NYC map
ny_map <- qmap("empire state building, new york", zoom = 12)

# central NY zoomed in
ny_man <- qmap("madison square park, new york", zoom = 13)

# brooklyn zoom map
brooklyn <- qmap("williamsburg, new york", zoom = 12)


# overview of all delivery pick up locations
ny_map + geom_point(aes(x = pickup_lon,y = pickup_lat, colour = 3, alpha = .1), data = db)

#overview of all delivery drop offs
ny_map + geom_point(aes(x = dropoff_lon,y = dropoff_lat, colour = 3, alpha = .1), data = db)

# PICK UP
ny_man + stat_density2d(
  aes(x = pickup_lon, y = pickup_lat, fill = ..level.., alpha = ..level..),
  size = 4, bins = 15, data = db,
  geom = "polygon"
)

## DROPOFF
ny_man + stat_density2d(
  aes(x = dropoff_lon, y = dropoff_lat, fill = ..level.., alpha = ..level..),
  size = 4, bins = 10, data = db,
  geom = "polygon"
)



# doesn't tell much but it could be interesting to show where each category is on the map
# facet grid by place cat -- top 3, bottom 3


# Brooklyn Breakdown
brooklyn + geom_point(aes(x = pickup_lon,y = pickup_lat, alpha = .1, colour = 3), data = db) + theme(legend.position = 'none')

brooklyn + geom_point(aes(x = dropoff_lon,y = dropoff_lat, alpha = .1, colour = 3), data = db) + theme(legend.position = 'none')



# Dropoff locations of top 3 most popular
db %>% 
  group_by(place_category) %>% 
  count() %>% 
  arrange(desc(n))

# top 3 
# italian, burger, japanese
top_3  <- c("Italian", "Burger", "Japanese")

# plot of top 3 drop-offs

ny_map + stat_density2d(
  aes(x = dropoff_lon, y = dropoff_lat, fill = ..level.., alpha = ..level..),
  size = 4, bins = 10, data = subset(db, place_category %in% top_3),
  geom = "polygon"
) + facet_wrap(~ place_category)


db %>% 
  group_by(place_category) %>% 
  summarise(num_items = sum(item_quantity)) %>% 
  arrange(desc(num_items))


## association of missing item quantity by place category

# db %>% 
#   group_by(place_category) %>% 
#   filter(is.na(item_quantity)) %>% 
#   count() %>% 
#   arrange(desc(n))



# db %>% 
#   filter(is.na(place_category)) %>% 
#   select(pickup_place) %>% 
#   top_n(50)


# db %>% 
#   filter(!is.na(item_quantity)) %>% 
#   group_by(delivery_id) -> grouped_quantity
# 
# grouped_quantity %>% 
#   summarize(mean = sum(item_quantity)) -> pivoted
#   
# max(pivoted$mean)
# # distribution of known item quantities
# ggplot(pivoted, aes(x = mean)) + geom_histogram()
