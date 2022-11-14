#packages may use, or not?
library(ggplot2)
library(tidyverse)
library(animation)
library(viridis)
library(data.table)
library(naniar)
library(sf)
library(lubridate)
library(gganimate)
library(transformr)
library(RColorBrewer)
library(magick)
library(randomForest)
library(fastDummies)
#setting working directory
setwd("~/Semester files/GEO 511/final_project-yzhao72-hangtian")


#Plot pedestrian distribution over time
data=read_csv('CCCCCCDep.csv')
#Only use useful part
data=data[,-c(4,7)]
#Create an sf class for plot
data.sf=st_as_sf(data,coords=c('Longitude','Latitude'))
data.sf=st_set_crs(data.sf,'WGS84')
data.sf=st_transform(data.sf,32618)
data.sf=data.sf[!duplicated(data.sf),]
data.sf$logged_Pedestrian=log2(data.sf$PedAve)
data.sf=data.sf%>%
  mutate_if(is.numeric, function(x) ifelse(x<0,0,x))
unique(data.sf$logged_Pedestrian)%>%sort()
data.sf$starttime=as.character(data.sf$starttime)


colnames(data.sf)[1]='intersection_id'

plott=ggplot(data.sf,aes(color=logged_Pedestrian,size=logged_Pedestrian,stroke=1.2))+
  geom_sf()+
  scale_color_distiller(palette = "Spectral")+theme_bw()+
  transition_states(starttime,transition_length=1,state_length = 5)+
  labs(x='Logitude',y='Latitude',title=paste('Time :', '{closest_state}'))

animate(plott)

anim_save(filename = 'Time ~ Pedestrain.gif')














#Random forest modeling
table=read_csv('RunData.csv')%>%as.data.frame()
#Check data availability
check=table%>%
  miss_var_summary()
colnames(table)[1]='intersection_id'

#daily mean
daily_mean=table%>%group_by(intersection_id)%>%summarize(PedAveMean=mean(PedAve))
table=table[,-c(2:4)]
unique_table=unique(table)
model_table=left_join(daily_mean,unique_table,by='intersection_id')
model_table=model_table[,-1]%>%as.data.frame()

#Notdaily mean
model_table=table


#table processing


#If you want involve the time
# unique_time=unique(as.character(table$starttime))
# unique_time_id=c(1:33)
# unique_time_table=cbind(unique_time,unique_time_id)%>%as.data.frame()
# table$starttime=as.character(table$starttime)
# table=left_join(table,unique_time_table,by=c('starttime'='unique_time'))
# table=dummy_cols(table, 
#                  select_columns = "unique_time_id")


#If you want a more general model

colnames(model_table)[20]='Theft_Vehicle'
colnames(model_table)[22]='Housing_Violations'
colnames(model_table)[23]='Policing_Issue'
colnames(model_table)[24]='Pot_Hole'
colnames(model_table)[25]='Road_Walking_Environment'
colnames(model_table)[26]='Traffic_Problems'
colnames(model_table)[27]='Garbage_Problems'
colnames(model_table)[28]='Junk_Service_Problems'
colnames(model_table)[29]='Neighbourhood_Environmental_Issues'
colnames(model_table)[30]='Other_Questions'
colnames(model_table)[31]='starttime'


model_table=model_table[,-c(1,2,6:30)]
colnames(model_table)[6]='Assault'
colnames(model_table)[7]='Breaking_Entering'
colnames(model_table)[8]='Homicide'
colnames(model_table)[9]='Other_Sexual_Offense'
colnames(model_table)[10]='Robbery'
colnames(model_table)[11]='Sexual_Assault'
colnames(model_table)[12]='Sexual_Offense'
colnames(model_table)[14]='Theft_of_Vehicle'
colnames(model_table)[15]='Accident_Property_damage_only'
colnames(model_table)[16]='Accident_skyway'
colnames(model_table)[17]='Accident_injury'
model_table_notime=model_table[,-4]

set.seed(1)
train_rows=sample(nrow(model_table_notime),dim(model_table_notime)[1]*0.8)
train_set=model_table[train_rows,]
test_set=model_table[-train_rows,]
test_x=test_set[,-1]
test_y=test_set[,1]

PedAve.rf <- randomForest(PedAve ~ ., data = train_set,
                         importance = TRUE,ntree=400,xtest=test_x,ytest=test_y,
                         keep.forest=TRUE)

print(PedAve.rf)
plot(PedAve.rf)


test_y_pred=predict(PedAve.rf,test_set)

plot(test_set$PedAve, test_y_pred, main = 'test sample',
     xlab = 'Observed', ylab = 'Predict')
abline(1, 1)





#30 most important attributes
varImpPlot(PedAve.rf, n.var = min(30, nrow(PedAve.rf$importance)),
           main = 'Top 30 - variable importance')

?randomForest
