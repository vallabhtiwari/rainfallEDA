library(plotly)

list.files("/home/Data")
rain_data <- read.csv("/home/Data/Sub_Division_IMD_2017.csv")
head(rain_data,10)

summary(rain_data)
nrow(rain_data)
ncol(rain_data)

rain_data <- na.omit(rain_data)
summary(rain_data)
nrow(rain_data)
ncol(rain_data)

head(rain_data)
#=============================================================================================================
#1.Average rainfall year wise in all subdivisions for last 50 years
columns = colnames(rain_data)
rf_last_50_years = subset(rain_data,rain_data$YEAR>1967)
avg_rf_last_50_years = aggregate(x= rf_last_50_years$ANNUAL,
                                       by = list(rf_last_50_years$YEAR),FUN = mean)
colnames(avg_rf_last_50_years) <- c(columns[2],"Mean")

max_avg <- avg_rf_last_50_years[which.max(avg_rf_last_50_years$Mean), ]
min_avg <- avg_rf_last_50_years[which.min(avg_rf_last_50_years$Mean), ]
annot_max <- list(x = max_avg$YEAR,y = max_avg$Mean,text = max_avg$YEAR,
          xref = "x",yref = "y",showarrow = TRUE,
          arrowhead = 10,ax = 20,ay = -60)
annot_min <- list(x = min_avg$YEAR,y = min_avg$Mean,text = min_avg$YEAR,
              xref = "x",yref = "y",showarrow = TRUE,
              arrowhead = 10,ax = 10,ay = -80)
fig <- plot_ly(x = avg_rf_last_50_years$YEAR,
               y =  avg_rf_last_50_years$Mean,
               hovertemplate = paste('Year: %{x}<br>Rainfall: %{y:.2f}'),
               type = "bar",marker = list(color = c('rgba(99, 150, 230, 0.8)')),
               width = 650, height = 400)
fig <- fig %>% layout(title = 'Average rainfall in India (1968-2017)',
                      xaxis = list(title = 'Year'),
                      yaxis = list(title = 'Rainfall in mm'),
                      annotations=list(annot_min,annot_max))
fig
print("Max rainfall past 50 years:")
print(max_avg)
print("Min rainfall past 50 years:")
print(min_avg)
print("Average rainfall in India in the past 50 years")
mean(avg_rf_last_50_years$Mean)
print("Average rainfall before 1990 :")
mean(subset(avg_rf_last_50_years$Mean,avg_rf_last_50_years$YEAR<1990))
print("Average rainfall after 1990 :")
mean(subset(avg_rf_last_50_years$Mean,avg_rf_last_50_years$YEAR>1990))
#Observations:

#function to shorten subdivision names

#=============================================================================================================
#2.Average anual rainfall subdivision wise in all subdivisions for last 50 years
avg_rf_subdiv_wise = aggregate(x= rf_last_50_years$ANNUAL,
                                       by = list(rf_last_50_years$SUBDIVISION),FUN = mean)
colnames(avg_rf_subdiv_wise) <- c(columns[1],"Mean")

max_avg <- avg_rf_subdiv_wise[which.max(avg_rf_subdiv_wise$Mean), ]
min_avg <- avg_rf_subdiv_wise[which.min(avg_rf_subdiv_wise$Mean), ]
annot_max <- list(x = max_avg$SUBDIVISION,y = max_avg$Mean,text = max_avg$SUBDIVISION,
                  xref = "x",yref = "y",showarrow = TRUE,
                  arrowhead = 10,ax = 20,ay = -20)
annot_min <- list(x = min_avg$SUBDIVISION,y = min_avg$Mean,text = min_avg$SUBDIVISION,
                  xref = "x",yref = "y",showarrow = TRUE,
                  arrowhead = 10,ax = 10,ay = -80)

fig <- plot_ly(x = avg_rf_subdiv_wise$SUBDIVISION,
               y =  avg_rf_subdiv_wise$Mean,
               hovertemplate = paste('%{x}<br>Rainfall: %{y:.2f}'),
               type = "bar",marker = list(color = c('rgba(244, 54, 52, 0.8)')),
               width = 650, height = 400)
fig <- fig %>% layout(title = 'Average anual rainfall in subdivisions ',
                      xaxis = list(title = 'Subdivision',showticklabels=FALSE),
                      yaxis = list(title = 'Rainfall in mm'),
                      annotations=list(annot_min,annot_max))

fig
print("Max rainfall in subdivision:")
print(max_avg)
print("Min rainfall in subdivision:")
print(min_avg)
#Observations:

#=============================================================================================================
#3.Average rainfall season wise in all subdivisions
X <- c("JF","MAM","JJAS","OND")
JF_mean <- mean(rf_last_50_years$JF)
MAM_mean <- mean(rf_last_50_years$MAM)
JJAS_mean <- mean(rf_last_50_years$JJAS)
OND_mean <- mean(rf_last_50_years$OND)
Y <- c(JF_mean,MAM_mean,JJAS_mean,OND_mean)

fig <- plot_ly(x = X,y =  Y,type = "bar",
               hovertemplate = paste('%{x}<br>Rainfall: %{y:.2f}'),
               texttemplate = '%{y:.2f}mm', textposition = 'outside',
               marker = list(color = c('rgba(209,79,47,0.8)', 'rgba(204,204,204,1)',
                                       'rgba(96,209,47,0.8)','rgba(204,204,204,1)')),
               width = 650, height = 400)

fig <- fig %>% layout(title = 'Average seasonal rainfall in India',
                      xaxis = list(title = 'Seasons'),
                      yaxis = list(title = 'Rainfall in mm'))
fig
#Observations:

#=============================================================================================================
#4.Participation of seasons in subdivisions
avg_rf_subdiv_wise_JF = aggregate(x= rf_last_50_years$JF,
                                  by = list(rf_last_50_years$SUBDIVISION),FUN = mean)
avg_rf_subdiv_wise_MAM = aggregate(x= rf_last_50_years$MAM,
                                  by = list(rf_last_50_years$SUBDIVISION),FUN = mean)
avg_rf_subdiv_wise_JJAS = aggregate(x= rf_last_50_years$JJAS,
                                  by = list(rf_last_50_years$SUBDIVISION),FUN = mean)
avg_rf_subdiv_wise_OND = aggregate(x= rf_last_50_years$OND,
                                  by = list(rf_last_50_years$SUBDIVISION),FUN = mean)
fig <- plot_ly(x = avg_rf_subdiv_wise_JF$Group.1,
               y =  avg_rf_subdiv_wise_JF$x,
               hovertemplate = paste('%{x}<br>Rainfall: %{y:.2f}'),
               type = "bar",name="JF",width = 650, height = 400)
fig <- fig %>% add_trace(y = avg_rf_subdiv_wise_MAM$x, name = 'MAM')
fig <- fig %>% add_trace(y = avg_rf_subdiv_wise_JJAS$x, name = 'JJAS')
fig <- fig %>% add_trace(y = avg_rf_subdiv_wise_OND$x, name = 'OND')

TN = subset(avg_rf_subdiv_wise,avg_rf_subdiv_wise$SUBDIVISION=="Tamil Nadu" )
annot_TN <- list(x = TN$SUBDIVISION ,y = TN$Mean,text = TN$SUBDIVISION,
              xref = "x",yref = "y",showarrow = TRUE,xanchor='left',
              arrowhead = 10,ax = 10,ay = -80,bgcolor='rgba(221, 210, 216, 0.8)')

fig <- fig %>% layout(title = 'Participation of seasons in subdivisions of India',
                      xaxis = list(title = 'Subdivisions',showticklabels=FALSE),
                      yaxis = list(title = 'Rainfall in mm'), barmode = 'stack',
                      legend=list(title=list(text='<b> Seasons </b>')),
                      annotations=list(annot_min,annot_max,annot_TN))

fig
#Observations:

#=============================================================================================================
#5.Average seasonal(JF) rainfall subdivision wise in all subdivisions
colnames(avg_rf_subdiv_wise_JF) <- c(columns[1],"Mean")

max_avg <- avg_rf_subdiv_wise_JF[which.max(avg_rf_subdiv_wise_JF$Mean), ]
min_avg <- avg_rf_subdiv_wise_JF[which.min(avg_rf_subdiv_wise_JF$Mean), ]
annot_max <- list(x = max_avg$SUBDIVISION,y = max_avg$Mean,text = max_avg$SUBDIVISION,
                  xref = "x",yref = "y",showarrow = TRUE,
                  arrowhead = 10,ax = 20,ay = -20)
annot_min <- list(x = min_avg$SUBDIVISION,y = min_avg$Mean,text = min_avg$SUBDIVISION,
                  xref = "x",yref = "y",showarrow = TRUE,
                  arrowhead = 10,ax = 10,ay = -80)

fig <- plot_ly(x = avg_rf_subdiv_wise_JF$SUBDIVISION,
               y =  avg_rf_subdiv_wise_JF$Mean,
               hovertemplate = paste('%{x}<br>Rainfall: %{y:.2f}'),
               type = "bar",marker = list(color = c('rgba(223, 70, 113, 0.8)')),
               width = 650, height = 400)
fig <- fig %>% layout(title = 'Average rainfall in Jan Feb',
                      xaxis = list(title = 'Subdivisions',showticklabels=FALSE),
                      yaxis = list(title = 'Rainfall in mm'),
                      annotations=list(annot_min,annot_max))
fig
#Observations:

#=============================================================================================================
#6.Average seasonal(JJAS) rainfall subdivision wise in all subdivisions
colnames(avg_rf_subdiv_wise_JJAS) <- c(columns[1],"Mean")
max_avg <- avg_rf_subdiv_wise_JJAS[which.max(avg_rf_subdiv_wise_JJAS$Mean), ]
min_avg <- avg_rf_subdiv_wise_JJAS[which.min(avg_rf_subdiv_wise_JJAS$Mean), ]
annot_max <- list(x = max_avg$SUBDIVISION,y = max_avg$Mean,text = max_avg$SUBDIVISION,
                  xref = "x",yref = "y",showarrow = TRUE,
                  arrowhead = 10,ax = 20,ay = -20)
annot_min <- list(x = min_avg$SUBDIVISION,y = min_avg$Mean,text = min_avg$SUBDIVISION,
                  xref = "x",yref = "y",showarrow = TRUE,
                  arrowhead = 10,ax = 10,ay = -80)
fig <- plot_ly(x = avg_rf_subdiv_wise_JJAS$SUBDIVISION,
               y =  avg_rf_subdiv_wise_JJAS$Mean,
               hovertemplate = paste('%{x}<br>Rainfall: %{y:.2f}'),
               type = "bar",marker = list(color = c('rgba(100, 241, 45, 0.8)')),
               width = 650, height = 400)
fig <- fig %>% layout(title = 'Average rainfall in Jun Jul Aug Sep',
                      xaxis = list(title = 'Subdivisions',showticklabels=FALSE),
                      yaxis = list(title = 'Rainfall in mm'),
                      annotations=list(annot_min,annot_max))
fig
#Observations:

#=============================================================================================================
#7.Average monthly rainfall in all months
avg_month_rf = sapply(rf_last_50_years[,3:14], mean)
fig <- plot_ly(x = columns[3:14],y = avg_month_rf,
               hovertemplate = paste('%{x}<br>Rainfall: %{y:.2f}'),
               texttemplate = '%{y:.2f}', textposition = 'outside',
               type = "bar",marker = list(color = c('rgba(66, 204, 219, 0.8)')),
               width = 650, height = 400)
fig <- fig %>% layout(title = 'Average monthly rainfall in India ',
                      xaxis = list(title = 'Month',categoryorder = "array",
                                   categoryarray = columns[3:14]),
                      yaxis = list(title = 'Rainfall in mm')
                      )
fig
print("Max rainfall in month:")
print(avg_month_rf[which.max(avg_month_rf)])
print("Min rainfall in month:")
print(avg_month_rf[which.min(avg_month_rf)])
#Observations:


#=============================================================================================================
#8.Box plot for monthly rainfall in Indian subdivisions
fig <- plot_ly(x=columns[3],
               y=rf_last_50_years[3:14]$JAN,
               type='box',name="JAN",width = 650, height = 400)
t = 4
for( c in rf_last_50_years[4:14]){
  fig <- fig %>% add_trace(x=columns[t],y=c,
                           type='box',name=columns[t])
  t = t+1
}
fig <- fig %>% layout(title = 'Monthly rainfall in India ',
                      xaxis = list(title = 'Month',categoryorder = "array",
                                   categoryarray = columns[3:14]),
                      yaxis = list(title = 'Rainfall in mm'),
                      legend=list(title=list(text='<b> Months </b>')))
fig

#=============================================================================================================
#Studying Coastal Karnataka(max avg rf) and West Rajasthan(min avg rf) separately
rf_last_50_years_CK = subset(rf_last_50_years,rf_last_50_years$SUBDIVISION=="Coastal Karnataka")
head(rf_last_50_years_CK)

rf_last_50_years_WR = subset(rf_last_50_years,rf_last_50_years$SUBDIVISION=="West Rajasthan")
head(rf_last_50_years_WR)

#=============================================================================================================
#9.Monthly rainfall in Coastal Karnataka
fig <- plot_ly(x=columns[3],
               y=rf_last_50_years_CK[3:14]$JAN,
               type='box',name="JAN",width = 650, height = 400)
t = 4
for( c in rf_last_50_years_CK[4:14]){
  fig <- fig %>% add_trace(x=columns[t],y=c,
                           type='box',name=columns[t])
  t = t+1
}
fig <- fig %>% layout(title = 'Monthly rainfall in Costal Karnataka',
                      xaxis = list(title = 'Month',categoryorder = "array",
                                   categoryarray = columns[3:14]),
                      yaxis = list(title = 'Rainfall in mm'),
                      legend=list(title=list(text='<b> Months </b>')))

fig <- fig %>% add_lines(y = min(rf_last_50_years_CK[3:14]$AUG),
                  x = columns[6:12],inherit = FALSE,showlegend = FALSE,
                  hovertemplate = paste('Min(JJA): %{y:.2f}'),
                  line = list(color = "rgba(218, 38, 47, 0.8)",
                  width = 2,dash = 'dash'))

fig
#=============================================================================================================
#10.Monthly rainfall in West Rajasthan
fig <- plot_ly(x=columns[3],
               y=rf_last_50_years_WR[3:14]$JAN,
               type='box',name="JAN",width = 650, height = 400)
t = 4
for( c in rf_last_50_years_WR[4:14]){
  fig <- fig %>% add_trace(x=columns[t],y=c,
                           type='box',name=columns[t])
  t = t+1
}
fig <- fig %>% layout(title = 'Monthly rainfall in West Rajasthan',
                      xaxis = list(title = 'Month',categoryorder = "array",
                                   categoryarray = columns[3:14]),
                      yaxis = list(title = 'Rainfall in mm'),
                      legend=list(title=list(text='<b> Months </b>')))

fig <- fig %>% add_lines(y = max(rf_last_50_years_WR[3:14]$JUL),
                         x = columns[6:12],inherit = FALSE,showlegend = FALSE,
                         hovertemplate = paste('Max(JJA): %{y:.2f}'),
                         line = list(color = "rgba(218, 38, 47, 0.8)",
                                     width = 2,dash = 'dash'))

fig

#=============================================================================================================
#11.Tamil Nadu
rf_last_50_years_TN = subset(rf_last_50_years,rf_last_50_years$SUBDIVISION=="Tamil Nadu")
head(rf_last_50_years_TN)

fig <- plot_ly(x=columns[3],
               y=rf_last_50_years_TN[3:14]$JAN,
               type='box',name="JAN",width = 650, height = 400)
t = 4
for( c in rf_last_50_years_TN[4:14]){
  fig <- fig %>% add_trace(x=columns[t],y=c,
                           type='box',name=columns[t])
  t = t+1
}
fig <- fig %>% layout(title = 'Monthly rainfall in Tamil Nadu',
                      xaxis = list(title = 'Month',categoryorder = "array",
                                   categoryarray = columns[3:14]),
                      yaxis = list(title = 'Rainfall in mm'),
                      legend=list(title=list(text='<b> Months </b>')))

fig

