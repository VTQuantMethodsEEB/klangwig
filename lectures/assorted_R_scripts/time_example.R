library(lubridate)
#https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html

somestuff = c("2010-12-13 15:30:30", "2010-12-13 16:00:00")

example.time <- ymd_hms(somestuff)

#turn your date column into a real date
xl$date = as.Date(xl$date, "%m/%d/%Y")
#paste date and time column together for calculations
xl$date.time = paste(xl$date, xl$time)
#create a time2 column that gives you just time in an identifiable unit
xl$time2 = hms(xl$time)
#turn date and time together into something that R recognizes for interval calculations
xl$date.time <- ymd_hms(xl$date.time)

head(xl)

xl = xl %>% 
  group_by(pit_tag,date)%>% #create mini dataframes of each tagged bird on a day
  arrange(date.time)%>% #sort those minidataframes by time (midnight to 11:59 PM)
  mutate(delay = interval(lag(date.time), date.time)) #calculate second time - first time
#this gave me some warning messages but appeared to be okay
