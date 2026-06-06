#IMT Mines Albi - Internship in Business Intelligence and Data Science.
#Elioz call center profile analysis for attended calls.
#This study aims to analyse the amount of arriving calls to the Elioz call center within different periods of time.
#Besides, a first stage on the prediction of calls has to be developed for the center's future resource allocation.  

#1.Installing the required packages of the project.
#install.packages('readr')
#install.packages("lubridate")
#install.packages("plyr")
#install.packages("nnfor")
#install.packages("tsfknn")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("lubridate")
#install.packages("shiny")
#install.packages('utils')
#install.packages('tidyverse')

library(tidyverse)
library(readr)
library(lubridate)
library(plyr)
library(nnfor)  
library(tsfknn)
library(tseries)
library(forecast)
library(lubridate)
library(shiny)

#2.Reading the data, and adjusting its structure for the further analysis.
datafilename <- "se_CDR-2019-08-01.csv"

calls <- read_csv2(file=datafilename,col_types="cccccccccccccccccccccccccccccccc")

#check detail about the downloaded dataframe
str(calls)

print(head(calls,50))


#check the important columns of the database.
df = data.frame(calls['presentationTime'], calls['sessionStartTime'],calls['sessionEndTime'])
print(head(df,50))

str(df)

#Explore the main important columns of our data frame.
df_important_columns = data.frame(calls['customerId'],calls['presentationTime'],calls['sessionStartTime'],calls['sessionEndTime'])
print(head(df_important_columns,100))

#Check clients that have done the calls.
length(unique(df_important_columns[,'customerId']))

#ids = sort(as.integer(unique(df_important_columns[,'customerId'])),decreasing = FALSE)
#print(ids)

message("Nombre brut d'appels: ", nrow(calls))

#2.1.Normalization of data. 
#Treatment of columns in which empty hits are found. 
#Correction of presentation time for the cases in which this time does not exist. 
message("Normalisation des dates plausibles...")
message("... cdrId")
### cdrId se présente avec deux formats de date : "YYYY-MM-DD HH:MM:SS..." ou "YYYYMMDDHHMMSS..."
### On reformate les deux sous la forme "YYYY-MM-DD HH:MM:SS"
### Les dates correctes (non nulles) sont reconnues par l'année qui débute par "20.."
calls[[ "cdrId" ]] <- gsub("(20\\d{2}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}).*", "\\1", calls[[ "cdrId" ]])
calls[[ "cdrId" ]] <- gsub("(20\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2}).*", "\\1-\\2-\\3 \\4:\\5:\\6", calls[[ "cdrId" ]])
calls[[ "cdrId" ]]
### Les autres dates sont au format "DD/MM/YYYY HH:MM:SS..."
### On les reformate sous la forme "YYYY-MM-DD HH:MM:SS"
### Les dates correctes (non nulles) sont reconnues par l'année qui débute par "20.."
for (dateField in c("sessionStartTime", "sessionEndTime", "presentationTime")){
    message("... ", dateField)
    calls[[ dateField ]] <- gsub("(\\d{2})/(\\d{2})/(20\\d{2}) (\\d{2}:\\d{2}:\\d{2}).*", "\\3-\\2-\\1 \\4", calls[[ dateField ]])
}
### À partir d'ici toutes les dates correctes sont des 'str' au format "YYYY-MM-DD HH:MM:SS"
message("------------------------------------------------------------")
message("Indexation des dates plausibles correctes")
message("------------------------------------------------------------")
for (date_field_name in c("cdrId", "sessionStartTime", "sessionEndTime", "presentationTime")){
    calls[[ paste0(date_field_name, "_ok") ]] <- grepl("^20\\d{2}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", calls[[ date_field_name ]])
    message("...nombre d'appels avec ", date_field_name, " ok: ", sum(calls[[ paste0(date_field_name, "_ok") ]]))
}
calls[[ "all_ok" ]] <- calls[[ "cdrId_ok" ]] & calls[[ "sessionStartTime_ok" ]] & calls[[ "sessionEndTime_ok" ]] & calls[[ "presentationTime_ok" ]]
message("Nombre d'appels avec toutes les dates ok: ", sum(calls[[ "all_ok" ]]))
message("------------------------------------------------------------")
message("Affichage de quelques appels avec dates NON plausibles")
message("------------------------------------------------------------")

show_calls_incorrect_dates <- function(calls_subset) {
    print(
        mapply(
            format,
            head(
                calls_subset[c("cdrId", "presentationTime", "sessionStartTime", "sessionEndTime")],
                n = 10
            ),
            justify=c("left", "left", "left", "left"),
            quote=FALSE
        )
    )
}

for (date_field_name in c("presentationTime", "sessionStartTime", "sessionEndTime")){
    date_field_nameOk <- paste0(date_field_name, "_ok")
    message("... non plausibles pour ", date_field_name)
    show_calls_incorrect_dates(subset(calls, ! calls[[ date_field_nameOk ]]))
}

message("------------------------------------------------------------")
message("--- Données extraites ---")
message("------------------------------------------------------------")

# date_file_to_extract <- "cdrId"
date_file_to_extract <- "sessionStartTime"
date_file_to_extract_ok <- paste0(date_file_to_extract, "_ok")
date_file_to_extract_ok
message("Extraction du sous-ensemble d'appels avec dates ok (pour ", date_file_to_extract, ")")
# dates_ok <- subset(calls, index_cdrId_ok & index_sessionStartTime_ok & index_sessionEndTime_ok)
dates_ok <- subset(calls, calls[[ date_file_to_extract_ok ]])
dates_ok
message("Nombre d'appels avec dates ok: ", nrow(dates_ok))
message("Conversion des dates (str to POSIXct)")
dates_ok[[ date_file_to_extract ]] <- as.POSIXct(dates_ok[[ date_file_to_extract ]])

message("------------------------------------------------------------")
message("--- Filtrage des données entre deux dates ---")
message("------------------------------------------------------------")

dates_ok <- subset(dates_ok,
                   dates_ok[[ date_file_to_extract]] >= as.POSIXct("2016-01-01")
                   & dates_ok[[ date_file_to_extract]] <= as.POSIXct("2018-12-31")
                   )

print(head(dates_ok,20))

message("------------------------------------------------------------")
message("--- Ajout des colonnes supplémentaires ---")
message("------------------------------------------------------------")

message("Dates minimum et maximum et nombre de jours")
datemin <- min(dates_ok[[ date_file_to_extract ]])
datemax <- max(dates_ok[[ date_file_to_extract ]])
nbdays <- length(seq(from=datemin, to=datemax, by='day')) - 1
message("Min: ", datemin, ", Max: ", datemax, ", Distance en jours: ", nbdays)

message("Ajout du jour de la semaine (depuis ", date_file_to_extract, ")")
dates_ok[[ "week_day" ]] <- factor(as.POSIXlt(dates_ok[[ date_file_to_extract ]])$wday)

levels(dates_ok[[ "week_day"]]) = c("Dim", "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam")

message("Ajout de l'année (depuis ", date_file_to_extract, ")")
dates_ok[[ "year" ]] <- as.POSIXlt(dates_ok[[ date_file_to_extract ]])$year + 1900

message("Ajout du mois (depuis ", date_file_to_extract, ")")
dates_ok[[ "month" ]] <- factor(as.POSIXlt(dates_ok[[ date_file_to_extract ]])$mon)

levels(dates_ok[[ "month" ]])= c("Jan.", "Fev.", "Mars", "Avr.", "Mai", "Juin", "Jui.", "Aou.", "Sep.", "Oct.", "Nov.", "Dec.")

print(head(dates_ok))
print(str(dates_ok))

#3.Exploring approaches to filter the data and count the calls.
#It adds to the data frame a column with the correspondent year and month of each call.
message("Ajout de année-mois (depuis ", date_file_to_extract, ")")
dates_ok[[ "ym" ]] <- as.Date(cut(as.Date(as.POSIXlt(dates_ok[[ date_file_to_extract ]])), breaks = "month"))
dates_ok[[ "ym" ]] 

#It adds to the data frame a column with the correspondent year and semester of each call.
year <-  year(as.POSIXlt(dates_ok[[ date_file_to_extract ]]))
semester <- semester(as.POSIXlt(dates_ok[[ date_file_to_extract ]]))
monthNumber <- month(as.POSIXlt(dates_ok[[ date_file_to_extract ]]))
weekDayNumber <- as.numeric(format(dates_ok[[ date_file_to_extract ]], format = "%u"))

#it creates the column year in the data frame of information.
dates_ok[[ "year" ]] <- year
y1 <- dates_ok[[ "year" ]]

#it creates the column semester in the data frame of information.
dates_ok[[ "semester" ]] <- semester
s1 <- dates_ok[[ "semester" ]]

#it creates the column month_number in the data frame of information.
dates_ok[[ "month_number" ]] <- monthNumber
m1 <- dates_ok[[ "month_number" ]]

#it creates the column weekday_number in the data frame of information.
dates_ok[[ "weekday_number" ]]<- weekDayNumber
wd1 <-dates_ok[[ "weekday_number" ]]

#Concatenate years and semesters from each call to generate that filter.
ys <- paste(year,semester,sep ="-")
#it creates the column ys in the data frame of information.
#This column for example, it will help to filter the calls in regards to the year and the semester.
dates_ok[[ "ys" ]] <- ys

#It adds to the data frame a column with the correspondent week number of the call.
weekNumber <- week(as.POSIXlt(dates_ok[[ date_file_to_extract ]]))

#it creates the column wn(week number) in the data frame of information.
dates_ok[[ "wn" ]] <- weekNumber
w1 <- dates_ok[[ "wn" ]]

#It adds to the data frame a column with the correspondent year and week of the call.
#it creates the column yw in the data frame of information.
yw <- paste(year,weekNumber,sep ="-")
dates_ok[[ "yw" ]] <- yw

messsage("Ajout de année-mois-semaine (depuis ", date_file_to_extract, ")")

#It adds to the data frame a column with the correspondent year,month,and week of each call.
dates_ok[[ "ymw" ]] <- as.Date(cut(as.Date(as.POSIXlt(dates_ok[[ date_file_to_extract ]])), breaks = "week"))
dates_ok[[ "ymw" ]]

message("Ajout de la date (depuis ", date_file_to_extract, ")")

#It adds to the data frame a column with the correspondent date (ymd) of each call.
dates_ok[[ "date" ]] <- as.Date(as.POSIXlt(dates_ok[[ date_file_to_extract ]]))
date <-dates_ok[[ "date" ]]

#It adds to the data frame a column with the correspondent half hour of each call.
message("Ajout de la période de la journée par demi-heure (depuis ", date_file_to_extract, ")")
dates_ok[[ "half_hour" ]] <-
    paste(
        sprintf("%02d", as.POSIXlt(dates_ok[[ date_file_to_extract ]])$hour),
        ifelse(as.POSIXlt(dates_ok[[ date_file_to_extract ]])$min < 30, "00", "30"),
        sep=':'
    )

hw <-dates_ok[[ "half_hour" ]]

message("Ajout de la periode de la journée par heure (depuis ", date_file_to_extract, ")")
dates_ok[[ "hour" ]] <- sprintf("%02d", as.POSIXlt(dates_ok[[ date_file_to_extract ]])$hour)


hour <-dates_ok[[ "hour" ]]
hour <- paste(hour,"00",sep =":")
dates_ok[[ "hour" ]] <- hour
hour <-dates_ok[[ "hour" ]]

#It adds to the data frame a column with the the year, month, day, and the half-hour hour.
ymdmh <- paste(date,hw,sep ="-")
dates_ok[["ymd-mh"]] <- ymdmh
aaaaaa <- dates_ok[["ymd-mh"]]

#it completes the date of the half our in which a call falls.
hwdateComplete <- paste(aaaaaa,"00",sep = ":")
hwdateComplete
dates_ok[["ymd-mhcomplete"]] <-hwdateComplete

#It adds to the data frame a column with the correspondent date and the hour division.
ymdhour <- paste(date,hour,sep ="-")
dates_ok[["ymd-hour"]] <- ymdhour
aaaaaa <- dates_ok[["ymd-hour"]]

#It adds to the data frame a column with the ymdweek division.
ymdweek <- paste(y1,w1,sep ="-")
dates_ok[["ymd-w"]] <- ymdweek
aaaaaa <- dates_ok[["ymd-w"]]

#It adds to the data frame a column with the ymdmonth division.
ymdmonth <- paste(y1,m1,sep ="-")
dates_ok[["ymd-m"]] <- ymdmonth
aaaaaa <- dates_ok[["ymd-m"]]

#It adds to the data frame a column with the ymdsemester division.
ymdsemester <- paste(y1,s1,sep ="-")
dates_ok[["ymd-s"]] <- ymdsemester
aaaaaa <- dates_ok[["ymd-s"]]

#It adds to the data frame a column with the ymdyear division.
ymdyear <- paste(y1,y1,sep ="-")
dates_ok[["ymd-y"]] <- ymdyear
aaaaaa <- dates_ok[["ymd-y"]]

#4.Exploring approaches to count the calls.
#In this part of the code the function table to count the number of occurences for values that apper in a column. 
message("Déccompte des appels par jour de la semaine")
by_day <- table(dates_ok[[ "week_day" ]])

#4.1.creating bar plot for calls by day.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_day.jpg")
barplot(by_day)
dev.off()

message("Déccompte des appels par years and weeks")
by_yearWeeks <- table(dates_ok[[ "yw"]])

#4.2.creating bar plot for calls by year weeks.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_yearWeeks.jpg")
barplot(by_yearWeeks)
dev.off()

message("Déccompte des appels par years and weeks divided that in hours")
by_yearWeeksHalfHours <- table(dates_ok[[ "yw"]], dates_ok[[ "half_hour"]] )

#4.3.creating bar plot for calls yearWeeksHalfHours.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_yearWeeksHalfHours.jpg")
barplot(by_yearWeeksHalfHours)
dev.off()

message("Décompte des appels par demi-heure")
by_half_hour <- table(dates_ok[[ "half_hour" ]])

#4.4.creating bar plot for calls by_half_hour.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_half_hour.jpg")
barplot(by_half_hour)
dev.off()

#4.5.Calculating the average of calls by half-hours.
message("Moyenne des appels par demi-heure")
by_half_hour_mean <- by_half_hour / nbdays 

#4.5.Calculating the numbers of calls per year.
message("Décompte des appels par année")
by_year <- table(dates_ok[[ "year" ]])

message("Table des appels par demi-heure et par jour")
by_week_day_half_hour <- table(dates_ok[[ "week_day" ]], dates_ok[[ "half_hour" ]])

#4.6.creating bar plot for calls by_week_day_half_hour.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_week_day_half_hour.jpg")
barplot(by_week_day_half_hour)
dev.off()

message("Table des appels par demi-heure et par année")
by_year_half_hour <- table(dates_ok[[ "year" ]], dates_ok[[ "half_hour" ]])


message("Table par date")
by_date <- table(dates_ok[[ "date" ]])

#4.7.creating bar plot for calls by_date.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_date.jpg")
barplot(by_date)
dev.off()

message("Table par mois")
by_month <- table(dates_ok[[ "month" ]])

message("Table par année-mois")
by_ym <- table(dates_ok[[ "ym" ]])

message("Table par semaine")
by_ymw <- table(dates_ok[[ "ymw" ]])

message("Table par mois et par année")
by_month_year <- table(dates_ok[[ "month" ]], dates_ok[[ "year" ]])

#4.8.creating bar plot for calls by_month_year.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_ym.jpg")
barplot(by_month_year)
dev.off()

message("Table dates divide by hal hour")
by_date_h_hour <- table(dates_ok[["ymd-mh"]])

#4.9.creating bar plot for calls by_date_h_hour.
jpeg(file = "/home/juan-david/Documents/data_science/IMT_mines_DAlbi/by_date_h_hour.jpg")
barplot(by_date_h_hour)
dev.off()

#5.Approach to filter the data base.
filteredInfo <- dates_ok
nrow(filteredInfo)

str(filteredInfo)

#Filtering by years.
filteredInfo <- subset(filteredInfo , year %in%  c(2016))
nrow(filteredInfo)

#Filtering by months.
filteredInfo <- subset(filteredInfo , month_number %in% c(1))
nrow(filteredInfo)

#Filtering by weekday number.
filteredInfo <- subset(filteredInfo , weekday_number %in% c(4))
nrow(filteredInfo)

#Filtering by week number.
filteredInfo <- subset(filteredInfo , wn %in%  c(1))
nrow(filteredInfo)

#5.1.Trying some options to obtain the number of calls within periods.
dataFrame <-filteredInfo
columnNameOfDataFrame <- date_field_name
unitsOfAnalysisD <- "30 min"
filterColumn <- "ymd-mh"
unitsOfTime <- "hours"

#Columns names in dataframe database.
str(dataFrame)

#It shows all of the different types of columns available to filter the database and count the calls.
#Parameter is used for calls in half hour periods of time. ---> ymd-mh
#Parameter is used for calls in hours periods of time. ---> date
#Parameter is used for calls in days periods of time. ---> date
#Parameter is used for calls in weeks periods of time. ---> ymd-w
#Parameter is used for calls in months periods of time. ---> ymd-mm
#Parameter is used for calls in semesters periods of time. ---> ymd-s
#Parameter is used for calls in years periods of time. ---> ymd-y

#6.Obtaining the period of analysis after filtering the database.
#if for example we filter by month 11, then the dates should be 01/11/year - 01/12/year

#Obtain the initial date of the period of analysis.
date_field_name <- "sessionStartTime"
dataForDates <- filteredInfo[[ date_field_name ]]
intialDate <- min(dataForDates)
dateInitial1 <- paste0("a",intialDate)
dateInitial<-gsub('a','',dateInitial1 )
substr(dateInitial, 12, 12) <- "0"
substr(dateInitial, 13, 13) <- "0"
substr(dateInitial, 15, 15) <- "0"
substr(dateInitial, 16, 16) <- "0"
substr(dateInitial, 18, 18) <- "0"
substr(dateInitial, 19, 19) <- "1"
initialDate <-dateInitial
substr(initialDate, 19, 19) <- "0"

print(initialDate)

#Obtain the final date of the period of analysis.
finalDate <- max(dataForDates)
datefinal1 <- paste0("a",finalDate)
dateFinal<-gsub('a','',datefinal1 )
substr(dateFinal, 12, 12) <- "0"
substr(dateFinal, 12, 12) <- "0"
substr(dateFinal, 13, 13) <- "0"
substr(dateFinal, 15, 15) <- "0"
substr(dateFinal, 16, 16) <- "0"
substr(dateFinal, 18, 18) <- "0"
substr(dateFinal, 19, 19) <- "1"
FinalDate <- ymd_hms(dateFinal)
addition <- ddays(1)
timeAddition<- ymd_hms(FinalDate) + addition
timeAddition<-gsub(' UTC','',timeAddition)

class(timeAddition)

FinalDate <- timeAddition
substr(FinalDate, 19, 19) <- "0"

print(FinalDate)

print("---The dates of the period of analysis are:---")
initialDate
FinalDate

#7.Exploring an approach to count the calls as a function.
infoInCorrectIntervals <- function(dataFrame,columnNameOfDataFrame,unitsOfAnalysisD, filterColumn,unitsOfTime){
    
    #Obtains initial, and final dates of the data frame
    
    dataForDates <- dataFrame[[ columnNameOfDataFrame ]]
    dataForDates
    
    
    if(length(dataForDates)!= 0){
    
    intialDate <- min(dataForDates)
    intialDate 
    dateInitial1 <- paste0("a",intialDate)
    dateInitial<-gsub('a','',dateInitial1 )
    dateInitial
    
    message("interface 1")
    substr(dateInitial, 12, 12) <- "0"
    message("interface 2")
    substr(dateInitial, 13, 13) <- "0"
    message("interface 3")
    substr(dateInitial, 15, 15) <- "0"
    message("interface 4")
    substr(dateInitial, 16, 16) <- "0"
    message("interface 5")
    substr(dateInitial, 18, 18) <- "0"
    message("interface 6")
    substr(dateInitial, 19, 19) <- "1"
    
    dateInitial
    initialDate <-dateInitial
    message("interface 7")
    substr(initialDate, 19, 19) <- "0"
    message("interface 8")
    initialDate
    
    
    #---------------------------------------------------------------------------------------------------------
    
    finalDate <- max(dataForDates)
    finalDate
    
    finalDate
    datefinal1 <- paste0("a",finalDate)
    dateFinal<-gsub('a','',datefinal1 )
    dateFinal
    
    message("interface 9")
    substr(dateFinal, 12, 12) <- "0"
    message("interface 10")
    substr(dateFinal, 12, 12) <- "0"
    message("interface 11")
    substr(dateFinal, 13, 13) <- "0"
    message("interface 12")
    substr(dateFinal, 15, 15) <- "0"
    message("interface 13")
    substr(dateFinal, 16, 16) <- "0"
    message("interface 14")
    substr(dateFinal, 18, 18) <- "0"
    message("interface 15")
    substr(dateFinal, 19, 19) <- "1"
    message("interface 16")
    ?substr
    
    dateFinal
    FinalDate <- ymd_hms(dateFinal)
  
    FinalDate
    
    addition <- ddays(1)
    
    
    if(unitsOfTime  == "hours"){
        addition <- ddays(1)
    }
    
    if(unitsOfTime  == "days"){
        addition <- ddays(1)
    }
    
    if(unitsOfTime  == "weeks"){
        addition <- ddays(1)
    }
    
    if(unitsOfTime  == "years"){
        addition <- ddays(1)
    }
    
    if(unitsOfTime  == "months"){
        addition <- ddays(1)
    }
    
    timeAddition<- ymd_hms(FinalDate) + addition
    timeAddition
    
    message("interface 17-1")
    timeAddition <- gsub('UTC','',timeAddition)
    class(timeAddition)
    
    #We need to add a certain period of time to the final hour to cover all the analisys.
    FinalDate <- timeAddition
    FinalDate
    message("interface 17-2")
    substr(FinalDate, 19, 19) <- "0"
    message("interface 17-3")
    FinalDate
    
    }
    
    #Creates the sequence between the dates.
    
    TimeWindows <- ""
    
    if(length(dataForDates)!= 0){
    
    if((unitsOfTime != "semesters")&&(unitsOfTime != "years")&&(unitsOfTime != "months")&&(unitsOfTime != "weeks")){
    TimeWindows <- seq(ymd_hms(initialDate), ymd_hms(FinalDate), by = unitsOfAnalysisD)
    length(TimeWindows)
    }
    message("interface 17-3.1.1")
    
    if(unitsOfTime == "days"){
      
      TimeWindows <- paste(TimeWindows,"00:00:00", sep =" ")
    }
    message("interface 17-3.1.2")
    TimeWindows
    TimeWindows <- as.character(TimeWindows)
    TimeWindows
    }
    
    #----------------------------------------------------------------------------------------------
    #-------------------*****Important method part*******------------------
    #The following method applies the function table to count the number of calls in periods easily.
    #Obtains the table relatad with tehe analysed calls from the data frame. 
    
    if(length(dataForDates)!= 0){
      
    by_date_Filtered_Info <- table(dataFrame[[filterColumn]])
    by_date_Filtered_Info
    length(by_date_Filtered_Info)
    
    #Its is important to remenber that in order to explore a data table, the structure of type [[i]] must be used.
    #numbers <- by_date_Filtered_Info[[length(by_date_Filtered_Info)]]
    #length(by_date_Filtered_Info)
    #umbers
    
    message("interface 17-3.1.3")
    
    #***Obtains the names labels for the calls in periods.
    names <- rownames(by_date_Filtered_Info)
    names <- as.character(names)
    length(names)
    
    #It manipulates the names of the table that contains the counting of the calls.
    
    message("interface 17-3.2")
    substr(names, 11, 11) <- " "
    message("interface 17-3.3")
    
    names
    TimeWindows
    class(TimeWindows)
    
    if(unitsOfTime  == "hours"){
    names <- paste(names,"00",sep = ":")
    }
    names
    
    message("interface 18")
    
    if(unitsOfTime  == "days"){
      names <- paste(names,"00:00:00",sep = " ")
    }
    
    names
    }
    
    #--------------------------------------------------------------------------------------------------------
    
    #It calculates the array of calls based on the previous modifications. 
    #It creates an array with a lenght equal to the sequences creates to store the counted calls.
    
    calls <- c(rep(0,(length(TimeWindows))))
    
    if(length(dataForDates)!= 0){
    callsInTable <- c(rep(0,(length(by_date_Filtered_Info))))
    callsInTable 
    }
    
    callsInTable
    message("interface 19")
    
    #az <- by_date_Filtered_Info[[1]]
    
    #aze <- by_date_Filtered_Info[[length(by_date_Filtered_Info)]]
    
    #message("interface 19.1" +as.character(az) + as.character(aze))
    
    
    #The following code assigns to the array callsInTable for dfanalysis (cases in which the function table applies)  data frame the calls within each period.
    #The data frame  dfanalysis is only generated with calls form table function if there is data in base.
    
    if(length(dataForDates)!= 0){
      
    for(i in 1:length(callsInTable)){
        callsInTable[i] <- by_date_Filtered_Info[[i]][1]
    }
    
      
    #At this moment calls in table contains all the information regarding with the counted calls.  
    callsInTable
    length(callsInTable)
    names
    length(names)
  
    #--------------------------------------------------------------------------------------------------------------------
    #This lines obtains the data frame which contains the information related with the calls and the periods of analysis. 
    
    message("interface 20")
    
    #Creation of the first data which contains information of counted calls from the method table.
    
    ####***--------------------------------------------------
    ####***--------------------------------------------------
    ####***--------------------------------------------------
    
    dfanalysis <- data.frame(names,callsInTable)
    dfanalysis$names
    dfanalysis$callsInTable
    length(dfanalysis$callsInTable)
    }
    
    ####***--------------------------------------------------
    ####***--------------------------------------------------
    ####***--------------------------------------------------
    ####***--------------------------------------------------
    
    #If there is not data in the base, the dfanalysis will not exist.
    
    calls <- c(rep(0,length(TimeWindows)))
    calls
    length(TimeWindows)
    
    #---------------------------------------------------------------
    #The following code assigns the calls obtained from the function table to a real calls vector which has all the related periods.
    
    if(length(dataForDates)!= 0){
      
      
    if(unitsOfTime == "hours"){
      
      for(i in 1:length(TimeWindows)){
        
        h <- subset(dfanalysis, names == TimeWindows[i])
        
        if(length(h$callsInTable)==0){
          h <- 0
        }else{
          h <- h$callsInTable
        }
        h
        calls[i] <- h
      }
    
    }
    
    calls
    
    message("interface 21")
    
    if(unitsOfTime == "days"){
    for(i in 1:length(TimeWindows)){
    
    h <- subset(dfanalysis, names == TimeWindows[i])
    
    if(length(h$callsInTable)==0){
        h <- 0
    }else{
        h <- h$callsInTable
    }
    h
    calls[i] <- h
    }
    }
    
    message("interface 22")
    
    
    #The main parameters of the general data frame that is going to be returned.
    TimeWindows
    calls
    }
    
    #The data frame titulated dfcalls is the final data frame which has the information in sequences of intervalos generated in this method. 
    #The data frame coudl contains empty values when no data, or the information in the real intervals of time.
    #Creation of the data frame dfcalls.
    
    dfCalls <- data.frame(TimeWindows,calls)
    dfCalls$TimeWindows
    dfCalls$calls
    
    message("interface 22.1")
    
    if(length(dataForDates) == 0){
      
      #In the case that dfanalysis doesnt contain anything, dfcalls takes its values to be returned after a while.
      dfanalysis <- dfCalls
      #Has the table data    #has the sequence data empty.
    }  
    
    if(length(dataForDates)!= 0){
      
      
    if(unitsOfTime == "hours"){
      return(dfCalls)
    }
    if(unitsOfTime == "days"){
    return(dfCalls)
    }
    
      
    
      message("interface 22.2")
      
    if(unitsOfTime == "weeks"){
      
      namesEdited <- dfanalysis$names
      namesEdited <- as.character(namesEdited)
      namesEdited <- strsplit(namesEdited , "-")
      columnOrder <- c(rep(" ", length(namesEdited)))
      columnOrder1 <- c(rep(" ", length(namesEdited)))
      
      for(i in 1:length(namesEdited )){
        columnOrder[i] <- namesEdited[[i]][1]
      }
      columnOrder <- as.integer(columnOrder)
      
      for(i in 1:length(namesEdited )){
        columnOrder1[i] <- namesEdited[[i]][2]
      }
      columnOrder1 <- as.integer(columnOrder1)
      
      dfanalysis1 <- cbind(dfanalysis,columnOrder,columnOrder1)
      dfanalysis <- dfanalysis1
      dfanalysis <-  dfanalysis[order(dfanalysis$columnOrder,dfanalysis$columnOrder1),]
      dfanalysis$names
      dfanalysis$callsInTable
      return(dfanalysis)
    }
      message("interface 22.3")
    if(unitsOfTime == "months"){
      
      namesEdited <- dfanalysis$names
      namesEdited <- as.character(namesEdited)
      namesEdited <- strsplit(namesEdited , "-")
      columnOrder <- c(rep(" ", length(namesEdited)))
      columnOrder1 <- c(rep(" ", length(namesEdited)))
      
      for(i in 1:length(namesEdited )){
        columnOrder[i] <- namesEdited[[i]][1]
      }
      columnOrder <- as.integer(columnOrder)
      
      for(i in 1:length(namesEdited )){
        columnOrder1[i] <- namesEdited[[i]][2]
      }
      columnOrder1 <- as.integer(columnOrder1)
      
      dfanalysis1 <- cbind(dfanalysis,columnOrder,columnOrder1)
      dfanalysis <- dfanalysis1
      dfanalysis <-  dfanalysis[order(dfanalysis$columnOrder,dfanalysis$columnOrder1),]
      dfanalysis$names
      dfanalysis$callsInTable
      return(dfanalysis)
    }
    if(unitsOfTime == "semesters"){
      
      namesEdited <- dfanalysis$names
      namesEdited <- as.character(namesEdited)
      namesEdited <- strsplit(namesEdited , "-")
      columnOrder <- c(rep(" ", length(namesEdited)))
      columnOrder1 <- c(rep(" ", length(namesEdited)))
      
      for(i in 1:length(namesEdited )){
        columnOrder[i] <- namesEdited[[i]][1]
      }
      columnOrder <- as.integer(columnOrder)
      
      for(i in 1:length(namesEdited )){
        columnOrder1[i] <- namesEdited[[i]][2]
      }
      columnOrder1 <- as.integer(columnOrder1)
      
      dfanalysis1 <- cbind(dfanalysis,columnOrder,columnOrder1)
      dfanalysis <- dfanalysis1
      dfanalysis <-  dfanalysis[order(dfanalysis$columnOrder,dfanalysis$columnOrder1),]
      dfanalysis$names
      dfanalysis$callsInTable
      return(dfanalysis)
    }
    if(unitsOfTime == "years"){
      
      namesEdited <- dfanalysis$names
      namesEdited <- as.character(namesEdited)
      namesEdited <- strsplit(namesEdited , "-")
      columnOrder <- c(rep(" ", length(namesEdited)))
      columnOrder1 <- c(rep(" ", length(namesEdited)))
      
      for(i in 1:length(namesEdited )){
        columnOrder[i] <- namesEdited[[i]][1]
      }
      columnOrder <- as.integer(columnOrder)
      
      for(i in 1:length(namesEdited )){
        columnOrder1[i] <- namesEdited[[i]][2]
      }
      columnOrder1 <- as.integer(columnOrder1)
      
      dfanalysis1 <- cbind(dfanalysis,columnOrder,columnOrder1)
      dfanalysis <- dfanalysis1
      dfanalysis <-  dfanalysis[order(dfanalysis$columnOrder,dfanalysis$columnOrder1),]
      dfanalysis$names
      dfanalysis$callsInTable
      return(dfanalysis)
    }
    } 
    return(dfanalysis)
}


#8.Exploring an approach to correct future predictions on holydays, weekends, and others.
#Create functions that allows to make predictions with methdos such as NN, Knn, and Arima.
#Function that creates a list of periods for a prediction set.
creationOfDateListForFutureForecast <- function(finalSeriesPeriodDate, unitsOfTime, nPrediction){
  
  serie <- c(rep(" ",nPrediction))
  
  if(unitsOfTime == "days"){
    
    serie <- seq(ymd_hms(finalSeriesPeriodDate), by = unitsOfTime, length.out = nPrediction+1)
    serie <- as.character(serie)
    serie <- serie[2:length(serie)]
    serie <- paste(serie,"00:00:00",sep =" ")
    
    return(serie)  
  }
  
  if(unitsOfTime == "hours"){
    
    serie <- seq(ymd_hms(finalSeriesPeriodDate), by = unitsOfTime, length.out = nPrediction+1)
    serie <- as.character(serie)
    serie <- serie[2:length(serie)]

    return(serie)  
  }
  
  if(unitsOfTime == "30 min"){
    
    serie <- seq(ymd_hms(finalSeriesPeriodDate), by = unitsOfTime, length.out = nPrediction+1)
    serie <- as.character(serie)
    serie <- serie[2:length(serie)]
  
    return(serie)  
  }
  
  if(unitsOfTime == "weeks"){
    
    namesEdited <- finalSeriesPeriodDate
    namesEdited <- as.character(namesEdited)
    namesEdited <- strsplit(namesEdited , "-")
   
    part1 <- as.numeric(namesEdited[[1]][1])
    part2 <- as.numeric(namesEdited[[1]][2])
    
    part1 <- as.integer(part1)
    part2 <- as.integer(part2)
    
    columnOrder <- c(rep(0, nPrediction))
    columnOrder1 <- c(rep(0, nPrediction))
    
    if(part2==12){
      columnOrder[1] <- part1 +1
      columnOrder1[1] <- 1
    }else{
      columnOrder[1] <- part1
      columnOrder1[1] <- part2+1
    }
    
    year <- columnOrder[1]
    
    #fOR WEEKS
    for(i in 2:length(columnOrder1)){
      
      
      if((columnOrder1[i-1]+1) < 55){
        
        columnOrder1[i] <- columnOrder1[i-1]+1
        columnOrder[i] <- year 
      }
      if((columnOrder1[i-1]+1) == 55){
        
        columnOrder1[i] <- 1
        year <- year + 1
        columnOrder[i] <- year
      }
    }
    
    serie <- paste(columnOrder,columnOrder1,sep = "-")
    return(serie)
    }
    
  if(unitsOfTime == "months"){
    
    namesEdited <- finalSeriesPeriodDate
    namesEdited <- as.character(namesEdited)
    namesEdited <- strsplit(namesEdited , "-")
    
    part1 <- as.numeric(namesEdited[[1]][1])
    part2 <- as.numeric(namesEdited[[1]][2])
    
    
    part1 <- as.integer(part1)
    part2 <- as.integer(part2)
    
    columnOrder <- c(rep(0, nPrediction))
    columnOrder1 <- c(rep(0, nPrediction))
  
    if(part2==12){
      columnOrder[1] <- part1 +1
      columnOrder1[1] <- 1
    }else{
      columnOrder[1] <- part1
      columnOrder1[1] <- part2+1
    }
    
    year <- columnOrder[1]
    
    #fOR WEEKS
    for(i in 2:length(columnOrder1)){
      
      if((columnOrder1[i-1]+1) < 13){
        
        columnOrder1[i] <- columnOrder1[i-1]+1
        columnOrder[i] <- year 
      }
      if((columnOrder1[i-1]+1) == 13){
        
        columnOrder1[i] <- 1
        year <- year + 1
        columnOrder[i] <- year
      }
    }
    
    serie <- paste(columnOrder,columnOrder1,sep = "-")
    return(serie)
  }
  

  if(unitsOfTime == "semesters"){
    
    namesEdited <- finalSeriesPeriodDate
    namesEdited <- as.character(namesEdited)
    namesEdited <- strsplit(namesEdited , "-")
    
    part1 <- as.numeric(namesEdited[[1]][1])
    part2 <- as.numeric(namesEdited[[1]][2])
    
    part1 <- as.integer(part1)
    part2 <- as.integer(part2)
  
    columnOrder <- c(rep(0, nPrediction))
    columnOrder1 <- c(rep(0, nPrediction))
    
    if(part2==12){
      columnOrder[1] <- part1 +1
      columnOrder1[1] <- 1
    }else{
      columnOrder[1] <- part1
      columnOrder1[1] <- part2+1
    }
    
    year <- columnOrder[1]
    
    #fOR WEEKS
    for(i in 2:length(columnOrder1)){

      if((columnOrder1[i-1]+1) < 3){
        
        columnOrder1[i] <- columnOrder1[i-1]+1
        columnOrder[i] <- year 
      }
      if((columnOrder1[i-1]+1) == 3){
        
        columnOrder1[i] <- 1
        year <- year + 1
        columnOrder[i] <- year
      }
    }
  
    serie <- paste(columnOrder,columnOrder1,sep = "-")
    return(serie)
  }
  
  if(unitsOfTime == "years"){
    
    namesEdited <- finalSeriesPeriodDate
    namesEdited <- as.character(namesEdited)
    namesEdited <- strsplit(namesEdited , "-")
    
    part1 <- as.numeric(namesEdited[[1]][1])
    part2 <- as.numeric(namesEdited[[1]][2])
    
    
    part1 <- as.integer(part1)
    part2 <- as.integer(part2)
    
    columnOrder <- c(rep(0, nPrediction))
    columnOrder1 <- c(rep(0, nPrediction)) 
    
    columnOrder[1] <- part1+1
    columnOrder1[1] <- part2+1
    
    #fOR WEEKS
    for(i in 2:length(columnOrder1)){
      
        columnOrder1[i] <- columnOrder1[i-1]+1
        columnOrder[i] <- columnOrder[i-1]+1
      
    }
    serie <- paste(columnOrder,columnOrder1,sep = "-")
    return(serie)
  }
  
}
  
#It is the dataFrame of trial for the function.
f0 <- c(13,26,47,49,30,29,47,49,2,37)
f1 <- c("2016-2","2016-3","2016-4","2016-5","2016-6","2016-7","2016-8","2016-9","2016-10","2016-11")


w1 <- c("2016-5","2016-6","2016-7","2016-8","2016-9","2016-10","2016-11","2016-12","2017-1","2017-2","2017-3","2017-4","2017-5","2017-6","2017-7","2017-8","2017-9","2017-10")
w2 <- c(123,124,235,345,234,234,252,342,342,532,423,435,576,675,231,234,398,98)

theLabels <-w1
theCalls <- w2

finalDataFrameMonth <- data.frame(theCalls,theLabels)

#This method generatates the forecasting of the values in the future.
calculateFutureForecast <- function(seriesLabels,seriesHistory, methodOfcalculation, nPrediction,unitsOFtimeForecast){

  finalPeriod <- seriesLabels[length(seriesLabels)]

  seriesForecastlabels <- creationOfDateListForFutureForecast(finalPeriod,unitsOFtimeForecast,nPrediction)

  dataFrameOfAnalysis <- data.frame(seriesHistory,seriesLabels)

  dataFrameOfAnalysisArima <- ts(dataFrameOfAnalysis$seriesHistory, start = c(1,1), end = c(1,length(dataFrameOfAnalysis$seriesHistory)), frequency = 1)

  fit_diff_ar <- 0
    
  if(methodOfcalculation == "arima"){
  
  if(unitsOFtimeForecast == "days"){
    fit_diff_ar <- arima(dataFrameOfAnalysisArima,c(1,0,1),seasonal = list(order= c(0,1,2),period=7))  #Zero in d because by parameter we used a diff data set.
  }
  #, period=7
  #arima(df11$appelsInIntervals11,c(1,0,1),seasonal = list(order= c(0,1,2), period=7))
  
  if(unitsOFtimeForecast == "months"){
    fit_diff_ar <- arima(dataFrameOfAnalysisArima,c(1,0,1),seasonal = list(order= c(0,1,2), period=3))  #Zero in d because by parameter we used a diff data set.
  }
  
  if(unitsOFtimeForecast == "years"){
    fit_diff_ar <- arima(dataFrameOfAnalysisArima,c(1,0,1),seasonal = list(order= c(0,1,2), period=1))  #Zero in d because by parameter we used a diff data set.
  }
  
  if(unitsOFtimeForecast == "weeks"){
    fit_diff_ar <- arima(dataFrameOfAnalysisArima,c(1,0,1),seasonal = list(order= c(0,1,2), period=4))  #Zero in d because by parameter we used a diff data set.
  }
  
  if(unitsOFtimeForecast == "hours"){
    fit_diff_ar <- arima(dataFrameOfAnalysisArima,c(1,0,1),seasonal = list(order= c(0,1,2), period=8))  #Zero in d because by parameter we used a diff data set.
  }
  
  if(unitsOFtimeForecast == "30 min"){
    fit_diff_ar <- arima(dataFrameOfAnalysisArima,c(1,0,1),seasonal = list(order= c(0,1,2), period=8))  #Zero in d because by parameter we used a diff data set.
  }
  
  #Calculates the forecast of the calling arrival procedure. h represents the number of periods we wanted to forecast.
  fit_diff_arf <- forecast(fit_diff_ar, h = nPrediction)
  
  a <- forecast(fit_diff_arf, include = 7)
  
  forecastValues <- as.integer(a$mean)
  
  forecastValues
  
  dfOfReturn <- data.frame(forecastValues,seriesForecastlabels)
  
  return(dfOfReturn)
  
  }

#The following line calcualtes the forecast prediction regarding the method of knn

  if(methodOfcalculation == "knn"){
  
  predKnn <- 0
  
  predKnn <- knn_forecasting(dataFrameOfAnalysis$seriesHistory, h = nPrediction, msas = "MIMO")
  
  forecastKnn <- as.integer(predKnn$prediction)
  forecastValues <- forecastKnn
  
  dfOfReturn <- data.frame(forecastValues,seriesForecastlabels)
  
  return(dfOfReturn)
  }

#The following calcualtes the forecast prediction regarding the neural networks method.
  if(methodOfcalculation == "nn"){
  
  appelsFornn <- ts(dataFrameOfAnalysis$seriesHistory)
  prednn <- elm(appelsFornn)

  predicted_nn <- forecast(prednn , h = nPrediction)
  
  predicted_nn$mean 
  
  predicted_nn <- as.integer(predicted_nn$mean)
  forecastValues <- predicted_nn
  
  dfOfReturn <- data.frame(forecastValues,seriesForecastlabels)
  
  return(dfOfReturn)
  }

}

#9.Testing the methods of prediction over created samples.
#Obtaining a data frame with the forecasting developed within selected parameters.

#9.1.Arima forecasting.
#arimaDays <- calculateFutureForecast(finalDataFramedays$theLabels,finalDataFramedays$theCalls,"arima", 4, "days")
#arimaWeeks <- calculateFutureForecast(finalDataFrameWeeks$theLabels,finalDataFrameWeeks$theCalls,"arima", 4, "weeks")
#arimaMonths <- calculateFutureForecast(finalDataFrameMonths$theLabels,finalDataFrameMonths$theCalls,"arima", 4, "months")

#9.2.Knn forecasting.
#KnnDays <- calculateFutureForecast(finalDataFramedays$theLabels,finalDataFramedays$theCalls,"knn", 4, "days")
#KnnWeeks <- calculateFutureForecast(finalDataFrameWeeks$theLabels,finalDataFrameWeeks$theCalls,"knn", 4, "weeks")
#KnnMonths <- calculateFutureForecast(finalDataFrameMonths$theLabels,finalDataFrameMonths$theCalls,"knn", 4, "months")
#KnnMonths

#9.3.nn forecasting.
#nnDays <- calculateFutureForecast(finalDataFramedays$theLabels,finalDataFramedays$theCalls,"nn", 4, "days")
#nnWeeks <- calculateFutureForecast(finalDataFrameWeeks$theLabels,finalDataFrameWeeks$theCalls,"nn", 4, "weeks")
#nnMonths <- calculateFutureForecast(finalDataFrameMonths$theLabels,finalDataFrameMonths$theCalls,"nn", 4, "months")

#9.4.Testing again over another data set.
#appelsInIntervals <- c(142,90,73,0,1,91,72,54,60,40,0,0,29,0,71,98,53,3,0,93,122,69,99,60,0,0,124,112,106,104,86,0,0,85,171,162,144,99,1,1,121,146,112,106,104,86,0,0,85,171,162,144,99,1,1,121,146)
#appelsInIntervals1 <- c(142,90,73,0,1,91,72,54,60,40,0,0,29,0,71,98,53,3,0,93,122,69,99,60,0,0,124,112,106,104,86,0,0,85,171,162,144,99,1,1,121,146)
#length(appelsInIntervals1)
#nPredictionForecast <- 15

#df3 <- data.frame(appelsInIntervals1)
#appelsFornn <- as.ts(df3$appelsInIntervals1)

#if(nPredictionForecast !=0){
  prednn <- elm(appelsFornn)
  predicted_nn <- forecast(prednn, h = nPredictionForecast)
#}

#predicted_nn = predicted_nn$mean
#print(predicted_nn)


#predicted_nn<- as.integer(predicted_nn)
#maximunValue <- max(appelsInIntervals)

#plot(appelsInIntervals,type = "l", main="Calling prediction with Neural Networks model", xlab="time period", ylab="number of calls", sub="t",xlim=c(0,length(appelsInIntervals)), ylim= c(0,maximunValue+10), lwd = 2.5,col='black') #Include shows from the whole data the last n-values of the graph. 
#lines(c((length(df3$appelsInIntervals1)+1):(length(appelsInIntervals))), predicted_nn,col='Blue',lwd = 2.5)

#9.5.Implementing a function that takes a serie and correct its the predicted values.
deletingSaturdayAndSundaysValues <- function(ForecastSerie,dayNames){
  
  serieToModify <- ForecastSerie

  for(i in 1:length(serieToModify)){
    
    if(dayNames[i] == "samedi"){
      
      serieToModify[i] = 0
    }
    
    if(dayNames[i] == "dimanche"){
      
      serieToModify[i] = 0
    }
  }
  
  return(serieToModify)
}


#9.6.This function applies the correction of negative values for the series.
correctionOfNegativeValues <-  function(forecastSerie){
  
  forecastToReturn <- forecastSerie
  for(i in 1:length(forecastSerie)){
    if(forecastSerie[i] < 0){
      forecastToReturn[i] =0
    }
    else{
      
    }
  }
  return(forecastToReturn)
}


#10.User interface implemtation.
#Assumptions for the interface which is development process, and will be modified for future interns,
#The interface is developped in Shiny for the development of R applications.
#Creation of the fluid page.

ui <- fluidPage(
  
  titlePanel("Elioz's calling procces analysis"),
  
  column(4, wellPanel(
    
    textInput("nForecast", label = ("Please select the number of periods for the forecast")),
    
    selectInput("year", label = 'Please select the years of analysis', 
                choices = list("2016" = 2016, "2017" = 2017, "2018" = 2018 , "No filter" = 100), 
                selected = 100,multiple = TRUE),
    
    ####checkboxGroupInput("year", label = 'Please select the years of analysis', 
    ####choices = list("2016" = 2016, "2017" = 2017, "2018" = 2018 , "No filter" = 100), 
    ####selected = 0),
    
    selectInput("month", label = 'Please select the months of analysis', 
                choices = list("January" = 1, "Febrary" = 2, "March" = 3,"April" = 4,"May" = 5,"June" = 6,"July" = 7,"August" = 8,"September" = 9,"October" = 10,"November" = 11, "December" = 12,"No filter" = 100), 
                selected = 100,multiple = TRUE),
    
    
    ####checkboxGroupInput("month", label = 'Please select the months of analysis', 
    ####choices = list("January" = 1, "Febrary" = 2, "March" = 3,"April" = 4,"May" = 5,"June" = 6,"July" = 7,"August" = 8,"September" = 9,"October" = 10,"November" = 11, "December" = 12,"No filter" = 100), 
    ####selected = 0),
    
    selectInput("weekNumber", label = 'Please select the weeks of the analysis', 
                choices = list("1" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5,"6" = 6,"7" = 7,"8" = 8,"9" = 9,"10" = 10,"11" = 11, "12" = 12,"13" = 13,"14" = 14,"15" = 15,"16" = 16,"17" = 17,"18" = 18,"19" = 19,"20" = 20,"21" = 21,"22" = 22,"23" = 23,"24" = 24,"25" = 25,"26" = 26,"27" = 27,"28" = 28,"29" = 29,"30" = 30,"31" = 31,"32" = 32,"33" = 33,"34" = 34,"35" = 35,"36" = 36,"37" = 37,"38" = 38,"39" = 39,"40" = 40,"41" = 41,"42" = 42,"43" = 43,"44" = 44,"45" = 45,"46" = 46,"44" = 44,"45" = 45,"46" = 46,"47" = 47,"48" = 48,"49" = 49,"50" = 50,"51" = 51,"52" = 52,"53" = 53,"54" = 54, "No filter" = 100), 
                selected = 100,multiple = TRUE),
    
    ####checkboxGroupInput("weekNumber", label = 'Please select the weeks of the analysis', 
    #### choices = list("1" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5,"6" = 6,"7" = 7,"8" = 8,"9" = 9,"10" = 10,"11" = 11, "12" = 12,"13" = 13,"14" = 14,"15" = 15,"16" = 16,"17" = 17,"18" = 18,"19" = 19,"20" = 20,"21" = 21,"22" = 22,"23" = 23,"24" = 24,"25" = 25,"26" = 26,"27" = 27,"28" = 28,"29" = 29,"30" = 30,"31" = 31,"32" = 32,"33" = 33,"34" = 34,"35" = 35,"36" = 36,"37" = 37,"38" = 38,"39" = 39,"40" = 40,"41" = 41,"42" = 42,"43" = 43,"44" = 44,"45" = 45,"46" = 46,"44" = 44,"45" = 45,"46" = 46,"47" = 47,"48" = 48,"49" = 49,"50" = 50,"51" = 51,"52" = 52,"53" = 53,"54" = 54, "No filter" = 100),
    ####selected = 0),
    
    selectInput("weekDay", label = 'Please select the days of the analysis' , 
                choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3,"Thursday" = 4,"Friday" = 5,"Saturday" = 6,"Sunday" = 7,"No filter" = 100), 
                selected = 100,multiple = TRUE),
    
    ####checkboxGroupInput("weekDay", label = 'Please select the days of analysis', 
    ####choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3,"Thursday" = 4,"Friday" = 5,"Saturday" = 6,"Sunday" = 7,"No filter" = 100), 
    ####selected = 0),
    
    selectInput("units", label = 'Please select the units to perform the analysis', 
                choices = list("half hours" = 1,"hours" = 2, "days" = 3, "weeks" = 4,"months" = 5,"semesters" = 6,"years" = 7), 
                selected = 0),
    
    selectInput("forecast", label = 'Please select a forecast method', 
                choices = list("Arima" = 1,"Knn" = 2,"Neural networks" = 3, "None" = 4), 
                selected = 4),
    
    actionButton("action", "Run the procedure"),
    
    actionButton("action2", "Reinitialize the procedure")
  )            
  ),
  column(6,
         verbatimTextOutput("dateText"),
         verbatimTextOutput("dateText2"),
         verbatimTextOutput("dateText3"),
         verbatimTextOutput("dateRangeText"),
         verbatimTextOutput("dateRangeText2")
  ),
  mainPanel(
    
    
    #This line writes the textA in the interface.
    verbatimTextOutput("textA"),
    
    #ThiS line writes the textB in the interface.
    verbatimTextOutput("textB"),
    
    #ThiS line writes the textC in the interface.
    verbatimTextOutput("textC"),
    
    #ThiS line writes the textD in the interface.
    verbatimTextOutput("textD"),
    
    #This line depicts the bar plot graph of calls in intervals in the interface.
    plotOutput(outputId = "distPlotbarPlot"),
    
    #ThiS line writes the textForecasting in which the procedure related with the forecastin is mentioned.
    verbatimTextOutput("textIntroductionForecast"),
    
    #This line depicts the graph of calls in intervals in the interface.
    plotOutput(outputId = "distPlot"),
    
    #ThiS line writes the textE in the interface.
    verbatimTextOutput("textE"),
    
    #ThiS line writes the textE in the interface.
    verbatimTextOutput("textF"),
    
    #ThiS line writes the textE in the interface.
    verbatimTextOutput("textG"),
  
    #ThiS line writes the text that describes the results of the future forecast .
    verbatimTextOutput("textDescriptionFutureForecast"),
    
    #This line depicts the graph of calls in intervals in the interface.
    plotOutput(outputId = "distPlot1"),
    
    #This line writes the textE in the interface.
    verbatimTextOutput("totalPredictedCalls"),
    
    #This line writes the introduction for the table that contains the information of all predicted calls in the interface.
    verbatimTextOutput("textI"),
    
    #This line writes the table that contains the information of all predicted calls in the interface.
    tableOutput("predictedCalls")
    
  )
)

#Creation of the server.
server <- function(input, output) {
  
  observeEvent(input$action, {
    output$textA <- renderText("This graph aims to expose the profile of the arrived calls")
    
    #-----------------------------------------------------------------------------------------------------------------
    #It obtains the data to use in the interface from the imported file of elioz
    
    interfaceData <- dates_ok
    interfaceFilteredData <- interfaceData 
    
    #-----------------------------------------------------------------------------------------------------------------
    #These lines control the filters that the user performs when using the interface.
    #Part of the code that catches years for filtering the data based on the user selection.
    
    interfaceYear <-input$year
    
    if((length(interfaceYear) != 0)&&(interfaceYear!= 100)){
      
      interfaceFilteredData <- subset(interfaceFilteredData  , year %in%  interfaceYear)
      
    }
    
    #Part of the code that catches months for filtering the data based on the user selection.
    
    interfaceMonth <-input$month
    
    if((length(interfaceMonth) != 0)&&(interfaceMonth!= 100)){
      
      interfaceFilteredData <- subset(interfaceFilteredData , month_number %in% interfaceMonth)
    }
    
    #Part of the code that catches months for filtering the data based on the user selection.
    
    interfaceWeekNumber <-input$weekNumber
    
    if((length(interfaceWeekNumber) != 0)&&(interfaceWeekNumber!= 100)){
      
      interfaceFilteredData <- subset(interfaceFilteredData , wn %in% interfaceWeekNumber)
    }
    
    #Part of the code that catches weekdays for filtering the data based on the user selection.
    
    interfaceWeekDay <-input$weekDay
    
    if((interfaceWeekDay != 100)&&(length(interfaceWeekDay) != 0)){
      
      interfaceFilteredData <- subset(interfaceFilteredData , weekday_number ==  interfaceWeekDay )
    }
    
    #These lines aim to capture the units for running the procedure in the interface regarding in the options selected by the user.
    interfaceUnits <-input$units
    
    if(interfaceUnits == 1){
      interfaceUnits <- as.character(interfaceUnits)
      interfaceUnits <- "30 min"
    }
    if(interfaceUnits == 2){
      interfaceUnits <- as.character(interfaceUnits)
      interfaceUnits <- "hours"
    }
    if(interfaceUnits == 3){
      interfaceUnits <- as.character(interfaceUnits)
      interfaceUnits <- "days"
    }
    if(interfaceUnits == 4){
      interfaceUnits <- as.character(interfaceUnits)
      interfaceUnits <- "weeks"
    }
    if(interfaceUnits == 5){
      interfaceUnits <- as.character(interfaceUnits)
      interfaceUnits <- "months"
    }
    if(interfaceUnits == 6){
      interfaceUnits <- as.character(interfaceUnits)
      interfaceUnits <- "semesters"
    }
    if(interfaceUnits == 7){
      interfaceUnits <- as.character(interfaceUnits)
      interfaceUnits <- "years"
    }
    
    
    #validate(
      #need(try(nrow(interfaceFilteredData) == 0, "Please select a data set"))
    #)
    
    #It calcualtes the number of calls in the analysis based on the filters that the user apply with the interface.  
    
    n_calls1 <- length(interfaceFilteredData[[date_field_name]])

    
    #It is Used to manipulate the data dates in the file.
  
    dataForDates <- interfaceFilteredData[[ date_field_name ]]
    
    
    intialDate <- min(dataForDates)
    
    dateInitial <- paste0("a",intialDate)
    dateInitial <- gsub('a','',dateInitial )
    
    
    message("First fase done")
    substr(dateInitial, 12, 12) <- "0"
    substr(dateInitial, 13, 13) <- "0"
    substr(dateInitial, 15, 15) <- "0"
    substr(dateInitial, 16, 16) <- "0"
    substr(dateInitial, 18, 18) <- "0"
    substr(dateInitial, 19, 19) <- "1"
    
    
    initialDate <- dateInitial
    substr(initialDate, 19, 19) <- "0"
    
    
    #---------------------------------------------------------------------------------------------------------
    
    finalDate <- max(dataForDates)
    
    
    
    datefinal1 <- paste0("a",finalDate)
    dateFinal<-gsub('a','',datefinal1 )
    
    
    
    substr(dateFinal, 12, 12) <- "0"
    substr(dateFinal, 12, 12) <- "0"
    substr(dateFinal, 13, 13) <- "0"
    substr(dateFinal, 15, 15) <- "0"
    substr(dateFinal, 16, 16) <- "0"
    substr(dateFinal, 18, 18) <- "0"
    substr(dateFinal, 19, 19) <- "1"
    
    #?substr
    
    
    FinalDate <- ymd_hms(dateFinal)
    
    
    
    addition <- ddays(1)
    
    timeAddition<- ymd_hms(FinalDate) + addition
    
    
    timeAddition<-gsub(' UTC','',timeAddition)
    class(timeAddition)
    
    #We need to add a certain period of time to the final hour to cover all the analisys.
    FinalDate <- timeAddition
    
    
    substr(FinalDate, 19, 19) <- "0"
    
    
    #It prints the initial and final dates in the console of the software.
    
    
    
    message("second fase done")
    #Creates the graph regarding in the calculated information.
    #----------------------------------------------------------------------------------------------------------------------
    
    b <- as.character("The first period of analysis is : ") 
    c <- as.character("The final period of analysis is : ")
    e <- as.character(initialDate)
    f <- as.character(FinalDate)
    b <- paste(b,e)
    c <- paste(c,f)
    
    
    #The following lines aim to calcaulte the number of calls in intervals, besides generating the corresponding plot.
    # function parameters dataFrame,columnNameOfDataFrame,unitsOfAnalysisD, filterColumn,unitsOfTime).
    #It shows all of the different types of columns to use in order to perform the calculation of the counting algorithm.
    #This parameter is used for calls in half hour periods of time. ---> ymd-mh
    #This parameter is used for calls in hours periods of time. ---> ymd-hours
    #This parameter is used for calls in days periods of time. ---> date
    #This parameter is used for calls in weeks periods of time. ---> ymd-w
    #This parameter is used for calls in months periods of time. ---> ymd-m
    #This parameter is used for calls in semesters periods of time. ---> ymd-s
    #This parameter is used for calls in years periods of time. ---> ymd-y
    
    #a <- infoInCorrectIntervals(baseOFData,dataFileColumnOFAnalisis,"unitsOfintervalsDivisions","columFilter","unitsOfInformation") for day intervals, and daily counting.
    
    #Obtaining the data frame in relation with the filters that the user chose.
    #Parameters
    
    #1.It obtains the data base to run to method of calculation regarding in the filters.
    
    baseOFData <- interfaceFilteredData 
    
    #2.It selects the column from the data file with the calls to run the method regarding in the filers that the user selects.
    
    dataFileColumnOFAnalisis <- date_file_to_extract
    
    
    #3.It selects the units of interval division regarding in the user preferences.
    
    unitsOfintervalsDivisions <- interfaceUnits 
  
    #4.selects the column that has a filter column to run the method regarding in the filers that the user selects.
    #It has to evaluate the columns that the user selected with the interface.
    
    columFilter <- " "
    
    if(unitsOfintervalsDivisions == "30 min"){
      columFilter <- "ymd-mh"
    }
    if(unitsOfintervalsDivisions == "hours"){
      columFilter <- "ymd-hour"
    }
    if(unitsOfintervalsDivisions == "days"){
      columFilter <- "date"
    }
    if(unitsOfintervalsDivisions == "weeks"){
      columFilter <- "ymd-w"
    }
    if(unitsOfintervalsDivisions == "months"){
      columFilter <- "ymd-m"
    }
    if(unitsOfintervalsDivisions == "semesters"){
      columFilter <- "ymd-s"
    }
    if(unitsOfintervalsDivisions == "years"){
      columFilter <- "ymd-y"
    }
    
    #5.It selects the units of intervals division regarding on the user preferences.
    unitsOfInformation <- unitsOfintervalsDivisions
    
    if(unitsOfintervalsDivisions == "30 min"){
      unitsOfInformation <- "hours"
    }
    
    #It obtains teh data frame with the information after the method of calculation have been done.
    
    message("third fase done")
    a <- " "
    message("3.1")
    a <- infoInCorrectIntervals(baseOFData,dataFileColumnOFAnalisis,unitsOfintervalsDivisions,columFilter,unitsOfInformation)
    message("3.2")
    
    #Select the specific columns regarding in the units of time.
    #a$TimeWindows
    #length(a$TimeWindows)
    #a$calls
    #length(a$calls)
    
    #unitsOfAnalysisPlot <- " "
    theCalls <- ""
    theLabels <- " "
    finalDataFrame <- " "
    unitsOfAnalysisPlot <- unitsOfInformation
    
    message("3.3")
    if((unitsOfAnalysisPlot == "weeks")||(unitsOfAnalysisPlot == "months")||(unitsOfAnalysisPlot == "semesters")||(unitsOfAnalysisPlot == "years")){
      
      theCalls <-a$callsInTable
      theLabels <- a$names
      
      finalDataFrame <- data.frame(theLabels,theCalls)
    }
    message("3.4")
    if((unitsOfAnalysisPlot == "hours")||(unitsOfAnalysisPlot == "days")||(unitsOfAnalysisPlot == "30 min")){
      
      theCalls <-a$calls
      theLabels <- a$TimeWindows
      finalDataFrame <- data.frame(theLabels,theCalls)
    }
    
    #It prints the following the graph with the different time intervals and the calls in it.
    
    message("3.5")
    plotXlab <- paste(" Periods of time",unitsOfAnalysisPlot,sep = " (")
    plotXlab <- paste(plotXlab," ", sep =")" )
    
    
    #plotInterface <- barplot(finalDataFrame$theCalls, main= " CALL CENTER'S CALLS PROFILE  ",names.arg = finalDataFrame$theLabels,
     #       xlab=plotXlab, ylab = "# of calls " , col= "darkblue"
    #)
    
    #----------------------------------------------------------------------------------------------------------------------
    #These lines are used to generate and calculate the forecast of calls for the profile of calls within the interface.
    
    #These lines just catch the writting of the total number of calls for the interface. 
    theTextD1 <- as.character("The number of calls during the period of analysis is : ")
    theTextD2 <- as.character(length(dataForDates))
    theTextD <- paste(theTextD1,theTextD2)
    
    
    #The next sextion aims to develop the prediction of calls regarding in the calculated dataframe that contains all of the information related with the calls.
    #---------------------------------------------------------------------------------------------------------
    #Prediction method using neural networks.
    #Installing the libraries to use neuronal networks approach.
    #https://towardsdatascience.com/neural-networks-to-predict-the-market-c4861b649371
    
    #This line is used to catch the number of periods the user want in order to make the forecast prediction.  
    message("3.6")
    nPredictionForecast <- input$nForecast
    message("3.6.1")
    if((length(nPredictionForecast)==0) |(is.null(nPredictionForecast)==TRUE)|(nPredictionForecast=="")){
      nPredictionForecast <- "0"
    }
    message("3.6.2")
    nPredictionForecast <- as.numeric(nPredictionForecast)
    message("3.6.3")
    #This line creates the array of calls that is going to be used to perform the forecast.
    message("3.7")
    message("fourth fase done")
    appelsInHoursFinal <- finalDataFrame$theCalls
    message("fourth fase done 1")
    timeIntervals <- finalDataFrame$theLabels
    message("fourth fase done 2")
    timeIntervals11 <-  timeIntervals[1:((length(appelsInHoursFinal)-nPredictionForecast ))] 
    message("fourth fase done 3")
    appelsInIntervals11 <-  appelsInHoursFinal[1:((length(appelsInHoursFinal)-nPredictionForecast ))] 
    message("five fase done ")
   
    #-----------------------------------------------------------------------------------------------------------------------
    #This line creates the intervals that are going to be used within the forecast.
    
    df11 <- data.frame(timeIntervals11,appelsInIntervals11)
    message("six fase done")
    #-------------------------------------------------------------------------------------------------------------------
    #These lines aim to perform the forecast calculation regarding the method of neural networks. 
  
    #appelsInIntervals11 <- c(10,2,4,6,4,3,12,23,12,34,12,4,5,6,7,10,2,4,6,4,3,12,23,12,34,12,4,5,6,7)
    #df11 <- as.data.frame(appelsInIntervals11)
    #df11 
    
    appelsFornn12 <- as.ts(df11$appelsInIntervals11) 
    prednn1  <- 0
    
    size <- length(df11$appelsInIntervals11)-nPredictionForecast
    
    yesnn <- 0
    if(nPredictionForecast!= 0 &&(size > 0)&&(nPredictionForecast>=10)){
      ("nPredictionForecast is not null")
      prednn1 <- elm(appelsFornn12 )
      yesnn <- 1
    }
    
    
    predicted_nn1 <- 0
    
    if((nPredictionForecast!= 0)&&(size > 0)&&(nPredictionForecast>=10)){
      
      predicted_nn1 <- forecast(prednn1 , h = nPredictionForecast)
      #These lines calculate the nPrediction predicted values.
      predicted_nn1  <- as.integer(predicted_nn1$mean)
    }else{
      predicted_nn1 <- c(rep(0,nPredictionForecast))
    }
    
    
    message("seven fase done")
    #--------------------------------------------------------------------------------------------------------------------
    #These lines aim to perform the forecast calculation regarding with the Arima model method. 
    #If the number of analysed periods is smaller than 7 + nPredictionForecast, the procedure will not work. since the period parameter of arima is 7
    
    fit_diff_ar1 <- 0
    if(nPredictionForecast != 0){
      fit_diff_ar1 <- arima(df11$appelsInIntervals11,c(1,0,0),seasonal = list(order= c(0,1,2), period=7))  #Zero in d because by parameter we used a diff data set.
    }
    
    #Calculates the forecast of the calling arrival procedure. h represents the number of periods we wanted to forecast.
    
    fit_diff_arf1 <-0
    if(nPredictionForecast != 0){
      fit_diff_arf1 <- forecast(fit_diff_ar1, h =nPredictionForecast )
    }
    
    k1 <- 0
    predicted_arima1 <-0
    if(nPredictionForecast != 0){
      k1 <- forecast(fit_diff_arf1, include = nPredictionForecast)
      predicted_arima1 <- as.integer(k1$mean)
    }
    message("eight fase done")
    #-------------------------------------------------------------------------------------------------------------------------
    #It performs the forecast based on the method of k-means.

    predKnn1 <- 0
    #appelsInIntervals11 <- c(10,2,4,6,4,3,12,23,12,34,12,4,5,6,7)
    #df11 <- as.data.frame(appelsInIntervals11)
    #nPredictionForecast <- 15
    
    #appelsInIntervals11 <-
    
    difference <- length(df11$appelsInIntervals11) - nPredictionForecast
    
    
    yesKnn <- 0
    if(nPredictionForecast !=0&&(difference > 0)&&(nPredictionForecast>=10)){
      predKnn1 <- knn_forecasting(df11$appelsInIntervals11, h = nPredictionForecast, k = 3, msas = "MIMO")
      yesKnn <- 1
      
      }
    
    message("eight.1 fase done")
    forecastKnn1 <- 0
    forecastKnn1 <- c(rep(0,nPredictionForecast))
    
    
    
    if((nPredictionForecast != 0)&&(difference > 0)&&(nPredictionForecast>=10)){
      forecastKnn1 <- as.integer(predKnn1$prediction)
    }
    
    message("eight.2 fase done")
    
    forecastKnn1 <- c(forecastKnn1)
    message("Nine fase done")
    #-------------------------------------------------------------------------------------------------------------------------
    #These lines aims to calculate the effciency function of the forecast obtain with neural networks.
    #Creates the function that evaluates the effiency in the model.
    
    efficiencyForecast1 <- function(vReal,vForecast){
      
      efficiency <- 0
      Error <- 0
      
      for(i in 1:length(vReal)){
        
        if(vReal[i]!=0){
          if(vReal[i]>=vForecast[i]){
            
            Error =  Error + (((vReal[i])^2 - (vForecast[i])^2)/(vReal[i])^2)
          }                  
          
          else{
            Error = Error + (((vForecast[i])^2 - (vReal[i])^2)/(vForecast[i])^2)
          }
        }
      }
      
      efficiency = 1-(Error/length(vReal))
      return(efficiency)
    }
    
    realAppelsInterface <- 0
    
    appelsInHoursFinal <- finalDataFrame$theCalls
    
    if(nPredictionForecast != 0){
      
      realAppelsInterface <- appelsInHoursFinal[(length(appelsInIntervals11)+1):(length(appelsInHoursFinal))]
    }
    
    #These lines aim to calculate the efiency of the methods, only if there is prediction to perform.
    
    efficiencyInterfacenn <- 0
    efficiencyInterfaceArima <- 0
    efficiencyInterfaceKnn <- 0
    
    if(nPredictionForecast != 0){
      
      efficiencyInterfacenn <- efficiencyForecast1(realAppelsInterface , predicted_nn1)
      efficiencyInterfaceArima  <- efficiencyForecast1(realAppelsInterface , predicted_arima1)
      efficiencyInterfaceKnn  <- efficiencyForecast1(realAppelsInterface ,forecastKnn1 )
      efficiencyInterfacenn <-round(efficiencyInterfacenn,3)
      efficiencyInterfaceArima <-round(efficiencyInterfaceArima,3)
      efficiencyInterfaceKnn <-round(efficiencyInterfaceKnn,3)
      
      if(yesKnn == 0){
        efficiencyInterfaceKnn <- "There is not R squared calculation for the Knn method"
      }
     
      if(yesnn == 0){
        efficiencyInterfacenn <- "There is not R squared calculation for the nn method"
      }
         
    }
    message("Ten fase done")
    #This line allows us to round the calculated eficiency in a certain number of decimal nits. 
    
    output$textB <- renderText(b)
    output$textC <- renderText(c)
    #It exposes the text related with the total number of calls in the overall used method.
    output$textD <- renderText(theTextD)
    
    output$distPlotbarPlot <- renderPlot({
      
      if(length(interfaceFilteredData) !=0){
      barplot(finalDataFrame$theCalls, main= " CALL CENTER'S CALLS PROFILE  ",names.arg = finalDataFrame$theLabels,
                                               xlab=plotXlab, ylab = "# of calls " , col= "darkblue") }
     })
    
    dtfinal <- finalDataFrame
    
    #These lines aim to make the graph of the forecast analysis that is going to be exposed in the interface.
    
    output$distPlot <- renderPlot({
      
      if((length(interfaceFilteredData) !=0)){
      plot(appelsInHoursFinal,type = "l",main="CALL CENTER'S FORECAST",
           
           xlab="time period",
           ylab="number of calls",
           sub="t",
           ylim= c(0,max(appelsInHoursFinal)+10),lwd = 2.5) #Include showes us from the data the last n-values of the graph. 
      
      #It depicts a graph for the predicted values using the neural network approach.
      message("eleven fase done")  
      if((length(nPredictionForecast)!=0)&&(nPredictionForecast !=0)&&(length(interfaceFilteredData) !=0)){
        
        
        uno <- max(predicted_nn1)
        dos <- max(predicted_arima1)
        tres <- max(forecastKnn1)
        cuatro <- max(appelsInHoursFinal)
        maximunValue <- max(uno,dos,tres,cuatro)
        
        
        plot(appelsInHoursFinal,type = "l",main="CALL CENTER'S FORECAST TRAINING ANALYSIS",
             
             xlab="time period",
             ylab="number of calls",
             sub="t",
             ylim= c(0,maximunValue+10),lwd = 2.5,col='black') #Include showes us from the data the last n-values of the graph. 
        
        if(yesnn == 1){
        lines(c((length(df11$appelsInIntervals11)+1):(length(df11$appelsInIntervals11)+nPredictionForecast)),predicted_nn1,col='Blue',lwd = 2.5)
        }
          
        lines(c((length(df11$appelsInIntervals11)+1):(length(df11$appelsInIntervals11)+nPredictionForecast)),predicted_arima1,col='Red',lwd = 2.5) 
        
        if(yesKnn == 1){
        lines(c((length(df11$appelsInIntervals11)+1):(length(df11$appelsInIntervals11)+nPredictionForecast)),forecastKnn1,col='Green',lwd = 2.5)
        }
          
        #legend(1,0, legend=c("appelsInHoursFinal", "predicted_nn1","predicted_arima1","forecastKnn1"),
        #col=c("black","blue","red","Green"), lty=1:4, cex=0.8)
        
        #legend(2.8,0,c("group A", "group B"), pch = c(1,2), lty = c(1,2))
        
        # so turn off clipping:
        ###par(xpd=TRUE)
        ###legend(2.8,-1,c("group A", "group B"), pch = c(1,2), lty = c(1,2))
        legend("right", inset = c(-0.7,0),legend=c("Calls within the periods", "Neural network prediction","Arima model prediction","Knn model prediction"), xpd = TRUE,
               col=c("black","blue","red","Green"), lty=1:4, cex=0.8)
      }
      }
    })
    
    introForecast <- "The following graph exposes the forecast and regression metric score for the calls prediction"
    
    output$textIntroductionForecast <- renderText(introForecast)
    
    message("Twelve fase done")
    theTextE1 <- as.character("Neural Network's R squared is (blue): ")
    theTextE2 <- as.character(efficiencyInterfacenn)
    theTextE <- paste(theTextE1,theTextE2)
    
    theTextF1 <- as.character("Arima's R squared is (red): ")
    theTextF2 <- as.character( efficiencyInterfaceArima)
    theTextF <- paste(theTextF1,theTextF2)
    
    theTextG1 <- as.character("Knn's R squared is: (green) ")
    theTextG2 <- as.character(efficiencyInterfaceKnn)
    theTextG <- paste(theTextG1,theTextG2)
    
  
    #It exposes the text related with the effiency of the neural network method. 
    message("Thirteen done")
    if(nPredictionForecast != 0){
      output$textE <- renderText(theTextE)
    }
    #It exposes the text related with the effiency of the Arima method. 
    
    if(nPredictionForecast != 0){
      output$textF <- renderText(theTextF)
    }
    
    #It exposes the text related with the effiency of the Knn method. 
    if(nPredictionForecast != 0){
      output$textG <- renderText(theTextG)
    }
    
    
    #It restablishes the data base for restarting procedures  within the application.
    message("fourteen fase done")
    
    
    #------------------------------------------------------------------------------------------------
    #------------------------------------------ FORECAST RESULTS -------------------------------------
    #------------------------------------------------------------------------------------------------
    forecastSelected <-input$forecast
    
    if((nPredictionForecast!=0)&&(forecastSelected!= 4)){
    #It catches the parameters to be used for the prediction.
    finalDataFrame  <- dtfinal 
    
    #forecastSelectedMethod  <- "arima"
    
    color <- ""
    
    #It takes the name of the method that is going to be used to generate the prediction.
    if(forecastSelected==1){
     forecastSelectedMethod <- "arima"
     color <-"Red"
    }
    if(forecastSelected==2){
    forecastSelectedMethod <- "knn"
     color <-"Green"
    }
    if(forecastSelected==3){
    forecastSelectedMethod <- "nn"
     color <-"Blue"
    }
    
    #The following lines generate the prediction regarding on the selected method.
    message("15 fase done")
    
    
    colorfinalDataFrame <- c(rep("darkblue",length(finalDataFrame$theCalls)))
    
    
    predictionDataFrame <- calculateFutureForecast(finalDataFrame$theLabels,finalDataFrame$theCalls,forecastSelectedMethod, nPredictionForecast,unitsOfintervalsDivisions)
    
    forecastValues <- predictionDataFrame$forecastValues
    seriesForecastlabels <- predictionDataFrame$seriesForecastlabels
    
    
    
    #mycalls <- c(4,4,4,5,5,4,9,5,7,9,4,7,5,8,4,9,8,6)
    #mylabels <- c("2019-07-15 00:00:01","2019-07-16 00:00:01","2019-07-17 00:00:01","2019-07-18 00:00:01","2019-07-19 00:00:01","2019-07-20 00:00:01","2019-07-21 00:00:01","2019-07-22 00:00:01","2019-07-23 00:00:01","2019-07-24 00:00:01","2019-07-25 00:00:01","2019-07-26 00:00:01","2019-07-27 00:00:01","2019-07-28 00:00:01","2019-07-29 00:00:01","2019-07-30 00:00:01","2019-07-31 00:00:01","2019-08-01 00:00:01")
    
    #The following code aims to apply some corrections tp the obtained forecast.
    #1. Correction of negative values.
    forecastValues <- correctionOfNegativeValues(forecastValues)
    
    
    #It obtains the weekdays name for the forecastcorrection.
    
    #2. Correction of weekend days values.
    if(unitsOfintervalsDivisions == "days"){
      
    weekDayNamesForecast <- weekdays(as.POSIXct(seriesForecastlabels, format="%Y-%m-%d", abbreviate = F))
    
    forecastValues <- deletingSaturdayAndSundaysValues(forecastValues,weekDayNamesForecast)
    }
    
    
    predictionDataFrame <- data.frame(forecastValues,seriesForecastlabels)
    
    
    message(as.character(predictionDataFrame$forecastValues[1]))
    message(as.character(predictionDataFrame$seriesForecastlabels[1]))
    
    colorpredictionDataFrame <- c(rep(color,length(predictionDataFrame$forecastValues)))
  
    
    message("16 fase done")
    fdl <- length(finalDataFrame$theCalls)
    pdfl <- length(predictionDataFrame$forecastValues)
    
    allInfoCalls <- c(rep(0,fdl+pdfl))
    allInfoColors <- c(rep("",fdl+pdfl))
    allInfoLabels <- c(rep("",fdl+pdfl))
    
    #Add the information of the two data frames.
    message("17 fase done")
    
    for(i in 1:fdl){
      allInfoCalls[i] <- finalDataFrame$theCalls[i]
      allInfoColors[i] <- colorfinalDataFrame[i]
      allInfoLabels[i] <- as.character(finalDataFrame$theLabels[i])
    }
    message("17.1 fase done")
    for(i in (fdl+1):(fdl+pdfl)){
      allInfoCalls[i] <- predictionDataFrame$forecastValues[i-fdl]
      allInfoColors[i] <- colorpredictionDataFrame[i-fdl]
      allInfoLabels[i] <- as.character(predictionDataFrame$seriesForecastlabels[i-fdl])
    }
    message("17.2 fase done")
    
    introductionFutureForecast <- "The following graph exposes the forecast prediction"
    
    totalPredictedCalls <- sum(predictionDataFrame$forecastValues)
    
    
    message("18 fase done")
    output$textIntroductionFutureForecast <- renderText(introductionFutureForecast)
    
    #The following code aims to plot the final bar-plot that contains the information of the forecast. 
    output$distPlot1 <- renderPlot({
      
      
      barplot(allInfoCalls, main= " CALL CENTER'S CALLS FORECASTING  ",names.arg = allInfoLabels,
              xlab=plotXlab, ylab = "# of calls " , col= allInfoColors)
      
      
    })
    
    message("19 fase done")
    introductionFutureForecast1 <- paste("The total number of predicted calls is" ,totalPredictedCalls,sep = " : ")
    output$totalPredictedCalls <- renderText(introductionFutureForecast1)
    
    
    number_of_predicted_Calls <- predictionDataFrame$forecastValues
    
    number_of_predicted_Calls <- as.integer(number_of_predicted_Calls)
      
    periods_of_prediction <- predictionDataFrame$seriesForecastlabels
      
    Forecasting_data_table <- data.frame(periods_of_prediction,number_of_predicted_Calls) 
    
    output$textI <- renderText("The results of the prediction are shown in the following table :")
    
    output$predictedCalls <- renderTable({
      
      Forecasting_data_table
      
    })
    
    }
  })
  
  observeEvent(input$action2, {
    
    interfaceData <- dates_ok
    interfaceFilteredData <- interfaceData 
    
    output$textA <- renderText("")
    output$textB <- renderText("")
    output$textC <- renderText("")
    output$distPlotbarPlot <- renderPlot({})
    output$textIntroductionForecast <- renderText("")
    output$distPlot <- renderPlot({})
    output$textD <- renderText("")
    output$textE <- renderText("")
    output$textF <- renderText("")
    output$textG <- renderText("")
    output$textIntroductionFutureForecast <- renderText("")
    output$distPlot1 <- renderPlot({})
    output$totalPredictedCalls <- renderText("")
    output$textI <- renderText("")
    output$predictedCalls <- renderTable("")
    
  })
  
  
}

#Running the application.
shinyApp(ui = ui, server = server)

