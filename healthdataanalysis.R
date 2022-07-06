daily_activity_df=read_csv('dailyActivity_merged.csv')
weight_log_df=read_csv('weightLogInfo_merged.csv')
hourly_steps_df=read_csv('hourlySteps_merged.csv')
hourly_intensities_df<-read_csv('hourlyIntensities_merged.csv')
hourly_calories_df<-read_csv('hourlyCalories_merged.csv')
sleep_day_df<-read_csv('sleepDay_merged.csv')

daily_activity_df<-mutate(daily_activity_df,total_active_minutes=VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes)

hourly_data_df<-inner_join(hourly_calories_df,hourly_intensities_df,by=c('Id'='Id','Date'='Date','Hour'='Hour'))
hourly_data_df<-inner_join(hourly_data_df,hourly_steps_df,by=c('Id'='Id','Date'='Date','Hour'='Hour'))

activity_by_weekdays_df<-daily_activity_df %>% 
  group_by(Weekday) %>% 
  summarise(avg_steps=mean(TotalSteps),avg_Distance=mean(TotalDistance),avg_sedentary_minutes=mean(SedentaryMinutes),avg_calories=mean(Calories))

sleep_activity_merged_df<-inner_join(daily_activity_df,sleep_day_df,by=c('Id'='Id','ActivityDate'='SleepDay','Weekday'='Weekday'))

hourly_summary_df<-hourly_data_df %>% 
  group_by(Hour) %>% 
  summarize(avg_calories=mean(Calories),avg_intensity=mean(TotalIntensity),avg_steps=mean(StepTotal))

minmax<-function(x){
  (x-min(x))/(max(x)-min(x))
}

hourly_summary_df[c('avg_steps','avg_calories','avg_intensity')]<-as.data.frame(lapply(hourly_summary_df[c('avg_steps','avg_calories','avg_intensity')], minmax))

molten_hourly_summary_df<-melt(hourly_summary_df,id="Hour")


glimpse(daily_activity_df)
glimpse(weight_log_df)
glimpse(hourly_data_df)
glimpse(sleep_day_df)
glimpse(sleep_activity_merged_df)
glimpse(activity_by_weekdays_df)

mean_sleep_minutes<-sleep_day_df %>% 
  group_by(Weekday) %>% 
  summarize(mean=mean(TotalMinutesAsleep))
mean_sleep_minutes$mean=as.integer(mean_sleep_minutes$mean)
mean_sleep_minutes['Weekday']<-factor(x=mean_sleep_minutes$Weekday,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),ordered = TRUE)
mean_sleep_minutes<-arrange(mean_sleep_minutes,Weekday)

mean_active_minutes<-daily_activity_df %>% 
  group_by(Weekday) %>% 
  summarise(mean=mean(total_active_minutes))
mean_active_minutes$mean<-as.integer(mean_active_minutes$mean)
mean_active_minutes['Weekday']<-factor(x=mean_active_minutes$Weekday,levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),ordered = TRUE)
mean_active_minutes<-arrange(mean_active_minutes,Weekday)


sleep_plot<-ggplot(sleep_day_df,aes(x=Weekday,y=TotalMinutesAsleep))+geom_boxplot(aes(fill=Weekday),show.legend = FALSE)
sleep_plot<-sleep_plot+stat_summary(fun.y = 'mean',size=0.1)
sleep_plot<-sleep_plot+geom_text(data=mean_sleep_minutes,aes(label=mean,y=mean-25),size=2,fontface='bold')
sleep_plot<-sleep_plot+theme(legend.position = 'none',plot.title = element_text(face="bold",size = 12))
sleep_plot<-sleep_plot+labs(title="Time asleep on each day",y="Time asleep(min)")
sleep_plot<-sleep_plot+scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))


sleep_activity_plot<-ggplot(sleep_activity_merged_df,aes(x=total_active_minutes,y=TotalMinutesAsleep))+geom_point()+geom_smooth(method='lm')
sleep_activity_plot<-sleep_activity_plot+labs(title="Relation between sleep time and active time",x="Total active time(min)",y="Total time asleep(min)")


activity_plot<-ggplot(data=daily_activity_df,aes(x=Weekday,y=total_active_minutes,fill=Weekday))
activity_plot<-activity_plot+geom_boxplot(show.legend = FALSE)
activity_plot<-activity_plot+stat_summary(fun.y = 'mean',size=0.1)
activity_plot<-activity_plot+geom_text(data=mean_active_minutes,aes(label=mean,y=mean-25),size=2,fontface='bold')
activity_plot<-activity_plot+labs(title='Total activity period on each day',y='Total active time(min)',caption = "Mean is labelled")
activity_plot<-activity_plot+theme(legend.position = "none",plot.title = element_text(face="bold",size=12))


hourly_data_plot<-ggplot(data=molten_hourly_summary_df,aes(x=Hour,y=value,color=variable))+geom_line()
hourly_data_plot<-hourly_data_plot+labs(title="Variation in average calories,intensity and steps in a day",x="Hour of the day",caption = "*Value is scaled")
hourly_data_plot<-hourly_data_plot+theme(legend.title = element_blank(),legend.position =c(0.15,0.75))




