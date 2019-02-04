library(dplyr)
library(ggplot2)
library(scales)

# Uncomment the below line and set the appropriate 
# setwd("F:/Data Analysis/311")
data = read.csv("311-dataset.csv")

# Check the structure of the data, e.g. factor levels.
str(data)

# Data cleanup
# Status column has In-Progress and In-progress values. Combining both to include under same status
data$Status[data$Status == "In-progress "] = 'In-Progress'

data$Creation.Date = as.Date(data$Creation.Date)
data$CreationMonth = as.Date(cut(data$Creation.Date, breaks = "months"))
data$DayOfWeek = weekdays(data$Creation.Date)

#Divsion wise plot
ggplot(data, aes(x=Division)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#City of Toronto has neglibile count. Ignore City of Toronto division records
data = data[data$Division != "City of Toronto", ]

#Divsion wise plot (Ignoring CofT)
ggplot(data, aes(x=Division)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Month wise complaint plot
ggplot(data, aes(x=CreationMonth)) + geom_bar() + scale_x_date(breaks = "1 month", labels=date_format("%Y-%b"))

# Day wise plot
data$DayOfWeek = factor(data$DayOfWeek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data, aes(x=DayOfWeek)) + geom_bar()

#Service type plot
ggplot(data, aes(x=Service.Request.Type)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Section Unit wise plot (can do for individual division?)
ggplot(data, aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Divsion wise plot
ggplot(data[data$Division == "Municipal Licensing & Standards",], aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot(data[data$Division == "Toronto Water",], aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot(data[data$Division == "Transportation Services",], aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot(data[data$Division == "Solid Waste Management Services",], aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Ward wise plot
ggplot(data, aes(x=Ward)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#InProgress
data_inprogress = data[data$Status == "In-Progress",]
ggplot(data_inprogress, aes(x=Ward)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#Plot days still in progress
data_inprogress$Days = as.numeric(as.Date(as.character("2019-01-01")) - data_inprogress$Creation.Date)
ggplot(data_inprogress, aes(x=Days)) + geom_bar() 


#Division wise in progress
ggplot(data_inprogress, aes(x=Division)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(data_inprogress, aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

data_inprogress_6monthsormore = data_inprogress[data_inprogress$Days > 180,]
ggplot(data_inprogress_6monthsormore, aes(x=Division)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot(data_inprogress_6monthsormore, aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

data_initiated = data[data$Status == "Initiated",]
data_initiated$Days = as.numeric(as.Date(as.character("2019-01-01")) - data_initiated$Creation.Date)
ggplot(data_initiated, aes(x=Days)) + geom_bar() 


ggplot(data_initiated, aes(Division)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot(data_initiated, aes(x=Section)) + geom_bar() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# TOP 20 Service Types
data_top20ServiceType = top_n(count(data, Service.Request.Type, sort = TRUE), 20)
ggplot(data_top20ServiceType, aes(x=Service.Request.Type, y=n)) + geom_bar(stat = "identity") + xlab("Service Type") + ylab("Count")  + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
