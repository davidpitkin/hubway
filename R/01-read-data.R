##=============================================================
##
## http://hubwaydatachallenge.org/
##
## Patrick Hausmann <patrick.hausmann@covimo.de>
##
## 12-10-2012
##=============================================================

round_time <- function(x, xdiv) {
             # http://stackoverflow.com/a/10862241
             m <- strptime("1970-01-01", "%Y-%m-%d", tz="GMT")
             m <- format(m + round(as.numeric(x)/xdiv)*xdiv,"%H:%M")
             return(m)
}

x <- read.csv(file = file.path("data", "trips.csv"))

x <- within(x, { 
               start_date = strptime(start_date, "%Y-%m-%d %H:%M:%S", tz = "GMT")
               end_date   = strptime(end_date,   "%Y-%m-%d %H:%M:%S", tz = "GMT")    
               age = 2012 - birth_date
               ag1 = cut(age, 
                          breaks=c(17, 20, 30, 40, 50, 60, 70, 80), 
                          right = FALSE, 
                          include.lowest = TRUE)
                same_day = format(end_date, "%d-%m") == 
                                  format(start_date, "%d-%m")
                mon  = format(start_date, "%m")
                day  = format(start_date, "%d")
                wday = weekdays(start_date)
                start_s = round_time(start_date, 900)
                end_s   = round_time(end_date,   900)
                }
                )

str(x)

save(x, file = file.path("rdata", "hubway_bike.rdata"))

#
# Fini
#