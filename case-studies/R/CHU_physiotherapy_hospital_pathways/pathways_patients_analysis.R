#IMT Mines Albi - Internship in Business Intelligence and Data Science.
#Patient pathways analysis along a physiotherapy hospital through Process mining.
#This study aims to explore approaches in languages as R in regards to improve the customer service in healthcare.

#1.Install the required R libraries for the project(uncomment the scripts).
#install.packages('readxl')
#install.packages('xts')
#install.packages('lubridate')
#install.packages("edeaR")
#install.packages("eventdataR")
#install.packages("processmapR")
#install.packages("xesreadR")
#install.packages("processmonitR")
#install.packages("petrinetR")
#install.packages("bupaR")
#install.packages("DiagrammeR")
#install.packages("dplyr")
#install.packages("DiagrammeR")
#install.packages("V8")
#install.packages("DiagrammeRsvg")
#install.packages("magrittr")
#install.packages("rsvg")
#install.packages("webshot")
#webshot::install_phantomjs()
#install.packages("edeaR")
#install.packages("png")#Created By me.
#install.packages("DiagrammeRsvg")

#Check that libraries were installed correctly.
library(readxl)
library(xts)
library(lubridate)
library(edeaR)
library(eventdataR)#Created By me.
library(processmapR)
library(xesreadR)
library(processmonitR)
library(petrinetR)
library(bupaR)
library(DiagrammeR)
library(dplyr)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)
library(webshot)
library(edeaR)
library(DiagrammeRsvg)

#2.Import the file containing the logs of patients.
database <- read_excel("/home/juan-david/Documents/data_science/IMT_mines_DAlbi/TrialVer.xlsx")
info <- database
print(head(info,50))
print(str(info))


#3.Adjusting the data to bupaR's structure for the analysis of log files.
#Change the timestamp formmat for the patients times.
info$Timestampstart <- ymd_hms(info$Timestampstart)

#Generate the column related with the patient id for the logFle.
infoPatient <- info$ID

#Generate the column related with the activity for the logFle.
infoActivity <- info$Category
infoActivity_before <- info$Category
#print(head(infoActivity, 50))
#Creates the function that replace the a string for its correct form.
replacement1 <- function(character , activityToModify , characterToReplace){
  
  activity <- activityToModify
  
  index <- gregexpr(pattern = character, activityToModify)
       if(index != -1){
         substr(activity, index, index) <-  characterToReplace
          
           }
    return(activity)
}

#Replace the 's by _ of the data for add c#Created By me.leaned information to the logFile. 
for(i in 1:length(infoActivity)){
  
  infoActivity[i] = replacement1("'",infoActivity[i],"_")
}

print(head(infoActivity_before, 50))
print(head(infoActivity, 50))

#Generate the column related with the timestart for the logFile.
infoTimeStamp <- info$Timestampstart

infoTimeStamp<- ymd_hms(infoTimeStamp)

#column related with the status for the lo#Created By me.gFle.
InfoStatus <- info$Status

#column related with the resources for the logFle.
InfoResource <- info$Staff

#column related with the lifeCycle for the logFle.
InfoLifeCycle <- info$lifecycle

#column related with the activity instance for the logFle.
infoActivityInstance <- c(1:length(info$ID))

#Managing the data to obtain the logFile that includes all the extracted info columns.

times <- as.character(infoTimeStamp)

#Initialize all the empty list for each attribute column in the log file 
newTimes <- c("")
newActivities <- c("")
newStatus <- c("")
newResource <- c("")
newActivityInstances <- c(0)
newIds <- c(0)

#Filling each attribute column of the log file.

newTimes[1] <- times[1]
newActivityInstances[1] <- infoActivityInstance[1]
newIds[1] <- info$ID[1]
newActivities[1] <- infoActivity[1]
newStatus[1] <- InfoStatus[1]
newResource[1] <- InfoResource[1]

for(i in 2:length(infoTimeStamp)){  
  newTimes <- c(newTimes,times[i])
  newActivityInstances <- c(newActivityInstances,infoActivityInstance[i])
  newIds <- c(newIds,infoPatient[i])
  newActivities <- c(newActivities,infoActivity[i])
  newStatus <- c(newStatus,InfoStatus[i])
  newResource <- c(newResource,InfoResource[i])
  
  if(is.na(info$TimestampEnd[i])){
  }
  else{
    newTimes <- c(newTimes,as.character(info$TimestampEnd[i]))
    newActivityInstances <- c(newActivityInstances,infoActivityInstance[i])
    newIds <- c(newIds,infoPatient[i])
    newActivities <- c(newActivities,infoActivity[i])
    newStatus <- c(newStatus,InfoStatus[i])
    newStatus[length(newStatus)-1] <- "Commence"
    newResource <- c(newResource,InfoResource[i])
  }
  
}

#Print the log file columns to check the results.
print(head(newTimes,15))
print(head(newActivityInstances,15))
print(head(newIds,15))
print(head(newActivities,15))
print(head(newStatus,15))
print(head(newResource,15))

infoPatient <-newIds

infoTimeStamp <- newTimes
infoTimeStamp <- ymd_hms(infoTimeStamp)

infoActivity <- newActivities

infoActivityInstance <- newActivityInstances

InfoStatus <- newStatus

InfoResource <- newResource

InfoLifeCycle <-c(rep("Start",length(infoPatient)))

#4.Create the data frame for the eventLog file with all the extracted information.
dataForEventLogFile <- data.frame(case = infoPatient,
                    activity_id =infoActivity,
                    timestamp = infoTimeStamp,
                    activity_instance = infoActivityInstance,
                    lifecycle = InfoLifeCycle,
                    resource = InfoResource)



#Check the dataframe after all the modifications.
print(head(info,25))
print(head(dataForEventLogFile,25))
print(str(info))
print(str(dataForEventLogFile))

#Create the eventLogFile for the process mining analysis of the whole data.
eventLogOfAnalysis1 <-eventlog(dataForEventLogFile ,
                              case_id = "case",
                              activity_id = "activity_id",
                              timestamp = "timestamp",
                              activity_instance_id = "activity_instance",
                              lifecycle_id = "lifecycle",
                              resource_id = "resource", order ="auto" ,validate = TRUE)

#?eventlog
#Show a summary if the event log file object.
summary(eventLogOfAnalysis1)

#Check cases, activities, and resources related with the event log.
cases_unique = unique(infoPatient)
print(head(cases_unique))

activities_unique = unique(infoActivity)
print(head(activities_unique))

resources_unique = unique(InfoResource)
print(head(resources_unique))

#5.Create process maps regarding in pre-especified aspects.
#?process_map
#Process maps will vary in regards to fi#Created By me.lters over cases, activities, resources, and others.

#Create a general process map with the info of the log file.
process_map_general = process_map(eventLogOfAnalysis1)
class(process_map_general)

#Exporting the process map as a png image.
export_svg(process_map_general) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("process_map_general.png")

#Another way to export images but with low resolution.
#process_map_general %>%
#htmltools::html_print() %>%
#webshot::webshot(file = "process_map_general.jpg")

#Create a general process map in which the performance is the mean of the time.
process_map_general_mean = process_map(eventLogOfAnalysis1, type = performance(FUN = mean, units = "mins" ))

export_svg(process_map_general_mean) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("process_map_general_mean.png")

#6.Filtering the information to obtain sub-process maps.
#?filter_case
#Overt the cases.
idFilterEvLog <-filter_case(eventLogOfAnalysis1,c(8,9,10),reverse = FALSE)

#6.1.Create a process map for the selected cases.
#Filter over the cases 8, 9, and 10.
process_map_id_FilterEvLog = process_map(idFilterEvLog)

export_svg(process_map_id_FilterEvLog) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("process_map_id_FilterEvLog.png")

process_map_id_FilterEvLog_performance = process_map(idFilterEvLog, type = performance(FUN = mean, units = "mins" ))

export_svg(process_map_id_FilterEvLog_performance) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("process_map_id_FilterEvLog_performance.png")


#Filtering the log by activities.
#FIlter over the activities Entrée des Consultations, Salle d_attente ACCUEIL, File d_attente SORTIE URO, and Salle Exam parcours URO.
actFilterEvLog <-filter_activity(eventLogOfAnalysis1,c('Entrée des Consultations','Salle d_attente ACCUEIL','File d_attente SORTIE URO','Salle Exam parcours URO','Sortie des Consultations'))

#6.2.Create a process map for the selected activities.
actFilterEvLog_process_map = process_map(actFilterEvLog)

export_svg(actFilterEvLog_process_map) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("actFilterEvLog_process_map.png")

actFilterEvLog_process_map_performance = process_map(actFilterEvLog, type = performance(FUN = mean, units = "mins" ))

export_svg(actFilterEvLog_process_map_performance) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("actFilterEvLog_process_map_performance.png")

#6.3.Create a process map for the selected resources.
#Filter over the resources ACCUEIL.Agent_Acc_1(1).
resFilterEvLog <- filter_resource(eventLogOfAnalysis1,c('ACCUEIL.Agent_Acc_1(1)'))

resFilterEvLog_process_map = process_map(resFilterEvLog)

export_svg(resFilterEvLog_process_map) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("resFilterEvLog_process_map.png")

resFilterEvLog_process_map_performance =process_map(resFilterEvLog, type = performance(FUN = mean, units = "mins" ))

export_svg(resFilterEvLog_process_map_performance) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("resFilterEvLog_process_map_performance.png")

#Creating an activity dashboard associated with the logfile.
activity_dashboard = activity_dashboard(eventLogOfAnalysis1)
class(activity_dashboard)

#Creating a performance dashboard associated with the logfile.
performance_dashboard = performance_dashboard(eventLogOfAnalysis1)
class(performance_dashboard)

#Creating a resource_map associated with the logfile.
#resource_map = resource_map(eventLogOfAnalysis1,type = frequency("absolute"),render = T)
#class(resource_map)
#?resource_map

#Creating an idotted_chart associated with the logfile.
idotted_chart = idotted_chart(eventLogOfAnalysis1)
class(idotted_chart)
#?idotted_chart

#Creating a trace_explore associated with the logfile.
trace_explorer(eventLogOfAnalysis1 ,coverage = 100,.abbreviate=TRUE)
#?trace_explorer

#7.Creating a process map with specified locations for each activity regarding on some coordinates.
activityPositions <- data.frame( act  = c("Start","Entr�e des Consultations","Salle d_attente ACCUEIL", "File d_attente SORTIE URO", "Salle Exam parcours URO", "Sortie des Consultations", "End"),
                                 x =  c(1.0,5.0,9.0,13.0,7.0,11.0,17.0),
                                 y =  c(2.5,5.0,5.0,5.0,1.0,1.0,2.5))

process_map(actFilterEvLog, fixed_edge_width = FALSE, fixed_node_pos = activityPositions)

process_map(actFilterEvLog, type = performance(FUN = mean, units = "mins" ), fixed_edge_width = FALSE, fixed_node_pos = activityPositions)
