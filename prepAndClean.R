# below imputes NA values in steps column by finding average value
# for that interval over the course of all observations.

# x <- fread('activity.csv', colClasses=c('double', 'character', 'integer'))
# 
# means <- x[,mean(steps, na.rm=T), by=interval]
# 
# setnames(means, c('i', 's'))
# 
# x[,r:=1:.N]
# 
# x[,steps:={
#     if (is.na(steps)) {
#         means[i==interval,s]
#     } else {
#         steps
#     }
# }, by=r]
# 
# x[,datetime:={
#     t <- str_pad(interval, 4, pad='0')
#     t <- paste(substr(t,0,2), substr(t,3,4), '00', sep=':')
#     as.POSIXct(paste(date, t, sep=' '))
# }, by=r]
# 
# x[,is.wday:={
#     t <- str_pad(interval, 4, pad='0')
#     t <- paste(substr(t,0,2), substr(t,3,4), '00', sep=':')
#     t <- strptime(paste(date, t, sep=' '), '%F %T', tz='GMT')
#     (t$wday != 0 & t$wday != 6)
# }, by=r]

par(mfrow=c(2,1), las=2)
plot(x[is.wday==T,mean(steps), by=interval], type='l', xaxs='i')
plot(x[is.wday==F,mean(steps), by=interval], type='l', xaxs='i')

plot(x[,mean(steps),by=interval], type='n')
lines(x[is.wday==T,mean(steps), by=interval], type='l', col='red', lwd=2)
lines(x[is.wday==F,mean(steps), by=interval], type='l', col='blue', lwd=2)
legend('topright', lwd=1, col=c('red', 'blue'),
       legend=c('Weekday', 'Weekend'))
