x <- dbReadTable(con, "SYSTEM_SERVER_USAGE")
x$time <- anytime(x$timestamp)
x$hour <- substr(x$time, 12,13)
x$minute <- substr(x$time, 15,16)
x$second <- substr(x$time, 18,19)
x$year <- substr(x$time, 1,4)
x$month <- substr(x$time, 6,7)
x$day <- substr(x$time, 9,10)

x <- x   %>% 
  tbl_df() %>% 
  select(time, year, month, day, hour, minute, logged_user) 

y <- x %>%
  group_by(year, month, day, hour) %>% 
  summarise(mean_user = mean(logged_user), min_user = min(logged_user), max_user = max(logged_user))
y$min_user <- (y$min_user * -1  + y$max_user)

cols <- c("year", "month", "day", "hour")
y$time <- apply(y[ , cols ], 1 ,paste ,collapse = "" )
#y <- y[ , !( names( y ) %in% cols ) ]

y