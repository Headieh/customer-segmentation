#LOAD DATA - Expedia Search Data

df <- read.csv('data/train.csv',stringsAsFactors = FALSE)

COLORS <- c("white", "black")
CUSTOM_COLORS <- colorRampPalette(colors = COLORS)
CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10, "Set3"))

#EXPLORE DATA
cat('number of observations in data:', nrow(df), '\n')
cat('number of columns in data:', ncol(df), '\n')
cat('columns:\n')
print(names(df))


head(df)
df[,'date_time'] = as.Date(df[,'date_time'],
                           format = c("%m/%d/%y %H:%M")
                           )

df[,c("srch_ci", "srch_co")]<- data.frame(lapply(
  df[,c("srch_ci", "srch_co")],
  function(x) as.Date(x, format="%m/%d/%y")
  ))

range(df[,'date_time'])

#LOGIC CHECKS:
cat('LOGIC CHECKS\n')
#check in date is after booking date
df$duration = df$srch_co - df$srch_ci
range(df$duration)
#here we see that there are NAs in check in or check out date
#bookings are not valid unless theres a check in and out date- 
#lets make sure no bookings were actually made with invalid check in and check out times

any_book = sum(df[is.na(df$srch_co) & is.na(df$srch_ci),"is_booking"])==0
if(any_book){#no bookings made with invalid data - good lets remove bad data
  df = df[!is.na(df$srch_co) & !is.na(df$srch_ci),]
  cat('bookings with invalid check in/out times removed - no booking made\n')
} else{
  cat('bookings made with invalid check in/out times - look into it\n')
}


df$duration = as.numeric(df$srch_co - df$srch_ci)
range(df$duration)
#now we see there are negative durations meaning there are check in dates after the check out 
any_book = sum(df[df$duration<=0,"is_booking"])==0
if(any_book){#no bookings made with invalid data - good lets remove bad data
  df = df[df$duration>0,]
  plot(table(df$duration), col=CUSTOM_COLORS_PLOT(10), 
       main="Duration Frequency", 
       lwd=9,
       cex  = 3,
       pch  = 1:25, 
       sub=paste("# of Obs", nrow(df)),
       ylim=c(0, 30000),xlim=c(0,max(df$duration+1)), ylab="Frequency of Durations", xlab='Number of Days')
  #lets remove some of these outliers
  table(df[df$duration < quantile(df$duration, 0.99984), c("duration","is_booking")])
  df = df[df$duration < quantile(df$duration, 0.99984),]
  plot(table(df$duration), col=CUSTOM_COLORS_PLOT(10), 
       main="Duration Frequency", 
       lwd=9,
       cex  = 3,
       pch  = 1:25, 
       sub=paste("# of Obs", nrow(df)),
       ylim=c(0, 30000),xlim=c(0,max(df$duration+1)), ylab="Frequency of Durations", xlab='Number of Days')
  cat('bookings with invalid durations removed - no booking made\n')
} else{
  cat('bookings made with invalid durations - look into it\n')
  }
cat('min duration',min(df$duration),'days, max duration',max(df$duration),'days in searchs\n')


#check out date is after check in date
df$notice = as.numeric(df$srch_ci - df$date_time)
table(df$notice)
#here we see that there are negative days notice which mean the booking date 
#happen after the check in date
df[df$notice==-1,c("is_booking", "srch_ci", "date_time")] 
#without knowing the specific check in times of each hotel it is safe to assume -1 days notice makes sense
#assume youre trying to check in at 1am, date_time would be a day after searched check in date because it is not say 4pm of the present day yet
any_book = sum(df[-1>df$notice,"is_booking"])==0


if (any_book){
  df = df[-1<=df$notice,]
  df$notice = df$notice+1 #adjust so there are no negatives
  
  plot(table(df$notice), col=CUSTOM_COLORS_PLOT(10), 
       main="Notice Frequency", 
       lwd=9,
       cex  = 3,
       pch  = 1:25, 
       sub=paste("# of Obs", nrow(df)),
       ylim=c(0, 5000),xlim=c(0,max(df$notice+1)), ylab="Frequency of Notice", xlab='Number of Days')
  
  
  
  cat('bookings with invalid notice times removed - no booking made\n')
}else{
  cat('bookings made with invalid notice times - look into it\n')
}
range(df$notice)
cat('min notice',min(df$notice),'days before trip, max notices',max(df$notice),'days before trip\n')

#number of guests is more than 0 adults
range(df$srch_adults_cnt)
table(df[df$srch_adults_cnt==0 & df$srch_children_cnt==0,"is_booking"])
#lets say the 0 adult and 0 children searches are just due to lack of
#attention to detail when filling out the search form - 
#lets keep these the datapoints, they have booking and non-booking data

#lets remove the bookings with srch_children_cnt>0 and adults==0
table(df[df$srch_adults_cnt==0 & df$srch_children_cnt>0,"is_booking"])
any_book = nrow(df[df$srch_adults_cnt==0 & df$srch_children_cnt>0,])>0

if (any_book){
  df = df[!(df$srch_adults_cnt==0 & df$srch_children_cnt>0),]
  cat('bookings with invalid children/adult ratios - observations removed\n')
}

cat('Children v Adults\n')
print(table(df$srch_adults_cnt, df$srch_children_cnt, dnn = c("adults", "children")))



range(df$orig_destination_distance, na.rm = TRUE)
cat(length(df$orig_destination_distance<5 & !is.na(df$orig_destination_distance)), 'observations have destination distances less than 5 mi\n')
#further investigation of above shows that there are infact bookings associated with NA distances
#and distances smaller than 5 mi so we will keep for now - maybe they were having issues at home?
table(df[ is.na(df$orig_destination_distance),'is_booking'])

table(df[(df$orig_destination_distance<5 & !is.na(df$orig_destination_distance)),c('is_booking','channel')])

#explore more
describe(df)
table(df[,'site_name'])
length(unique(df[,'user_location_city']))
table(df[,'is_booking'], df[,'srch_rm_cnt'])

bookTable = table(ifelse(df$is_booking==1,'booking','no booking'))
percentage <- round(bookTable/sum(bookTable) * 100)
labels <- paste0(row.names(bookTable), " (", percentage, "%) ")
sub = paste0("Total Obs:",nrow(df)," Total bookings:", sum(df$is_booking))
pie(bookTable, labels = labels, col = CUSTOM_COLORS_PLOT(10), main = paste0("Total Observations\n",sub))

channelTable = table(df$channel)
plot = plot(channelTable, col=CUSTOM_COLORS_PLOT(10), 
            main="Channel Sample", 
            lwd=9,
            cex  = 3,
            pch  = 1:25, 
            sub=paste("# of Obs", nrow(df)),
            ylim=c(0, 60000), ylab="Frequency per Channel", xlab='channel')
text(x=plot, y=channelTable+2500, labels=channelTable, cex=0.75)

cat( 'bookings on mobile\n')
print(table(ifelse(df$is_mobile==1,'mobile','desktop'), ifelse(df$is_booking==1,'booking','no booking')))
cat( '\n# of rooms vs # of people\n')
print(table(df$srch_rm_cnt, df$srch_adults_cnt+df$srch_children_cnt, dnn = c("rooms", "people")))
