#2 sample 2 tailed t test 
#to report underperforming and overperforming segments

#group1 - channel specific
t_data = df %>%
  group_by(channel) %>%
  summarise(cbrate = mean(is_booking),
            cnbooking = length(is_booking))

t_data = as.data.frame(t_data)

#overall booking rate
t_data$obrate = mean(df$is_booking)
t_data$onbooking=length(df$is_booking)

#group2 - the rest of the data not in group1
t_data$rnbooking=t_data$onbooking-t_data$cnbooking
t_data$rbrate=(t_data$onbooking*t_data$obrate - t_data$cbrate*t_data$cnbooking)/t_data$rnbooking

#H0: μ=μ0 channel had no statistical effect on booking rates
#H1: μ≠μ0 channel has an effect on booking rate
#2 tailed, 2 sample T Test

#stats
#Z=(p1hat -p2hat)/sqrt(phat(1-phat)((1/n1)+(1/n2)))
#n=number of bookings
#p= booking rate
c=.95
t_data$zscore = (t_data$cbrate - t_data$rbrate)/
  sqrt(t_data$obrate*(1-t_data$obrate)*((1/t_data$cnbooking)+(1/t_data$rnbooking)))
t_data$prob = 2 * pnorm( abs(t_data$zscore), lower.tail=FALSE)
t_data$significance=ifelse(((1-c)> t_data$prob), 
                           "significant - reject null",
                           "not significant - cannot reject null")
t_data$uop = ifelse((t_data$cbrate - t_data$rbrate)>0,# or can look at if z score is +/-
                    'over performer',
                    'under performer')

print(t_data[,c('channel','significance','uop')])
