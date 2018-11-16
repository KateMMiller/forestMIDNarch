#' @title joinLocEvent
#'
#' @description This function combines location and event data. Must run importData first.
#'
#' @param park Combine data from all parks or one park at a time. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APCO"}{Appomattox Court House NHP only}
#' \item{"BOWA"}{Booker T. Washington NM only}
#' \item{"COLO"}{Colonial NHP only}
#' \item{"FRSP"}{Fredericksburg & Spotsylvania NMP only}
#' \item{"GETT"}{Gettysburg NMP only}
#' \item{"GEWA"}{George Washington Birthplace NM only}
#' \item{"HOFU"}{Hopewell Furnace NHS only}
#' \item{"PETE"}{Petersburg NBP only}
#' \item{"RICH"}{Richmond NB only}
#' \item{"SAHI"}{Sagamore Hill NHS only}
#' \item{"THST"}{Thomas Stone NHS only}
#' \item{"VAFO"}{Valley Forge NHP only}}
#' @param from Year to start analysis, ranging from 2007-2018
#' @param to Year to stop analysis, ranging from 2007-2018
#' @param QAQC Allows you to remove or include QAQC events.
#' \describe{
#' \item{FALSE}{Default. Only returns visits that are not QAQC visits}
#' \item{TRUE}{Returns all visits, including QAQC visits}}
#' @param rejected Allows you to remove (FALSE) or include (TRUE) rejected plots.
#' \describe{
#' \item{FALSE}{Default. Only returns plots that were not rejected.}
#' \item{TRUE}{returns all records}}
#' @param locType Allows you to only include plots that are part of the GRTS sample design or include all plots, such as deer exclosures
#' \describe{
#' \item{"VS"}{Only include plots that are part of the Vital Signs GRTS sample design}
#' \item{"all"}{Include all plots, such as deer exclosures and bonus plots}}
#' @return returns a dataframe with location and visit events
#'
#' @export
#'

#------------------------
# Joins tbl_Locations and tbl_Events tables and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park="all", from=2007,to=2018, QAQC=FALSE, rejected=FALSE, locType='VS',output='short'){
  loc2<-loc %>% mutate(Unit_Code=as.factor(str_sub(Unit_ID,1,4)))
  loc2$Plot_Number<-str_pad(loc2$Plot_Number,width=3,side="left",pad=0) #Pad plot number so retains 3-digits
  loc2$Plot_Name<-paste(loc2$Unit_Code, loc2$Plot_Number, sep="-")

  loc3<- if (locType=='VS') {filter(loc2,Loc_Type=="VS")
  } else if (locType=='all') {(loc2)
  } else if (locType!='VS'|locType!='all') {stop("locType must either be 'VS' or 'all'")}

  loc4<- if (rejected==FALSE) {filter(loc3, Rejected==F)
  } else if (rejected==TRUE) {(loc3)
  } else {stop("rejected must be TRUE or FALSE")}

  loc5<- if (park=='all') {(loc4)
  } else if (park %in% levels(loc4$Unit_Code)){filter(loc4,Unit_Code==park)
  } else {stop("park must be one of the factor levels of Unit_Code")}

  park.ev<-merge(loc5,event,by="Location_ID",all.x=T)
  park.ev2<- if (QAQC==FALSE) {filter(park.ev, Event_QAQC==0)
  } else if (QAQC==TRUE) {(park.ev)
  } else {stop("QAQC must be TRUE or FALSE")}

  park.ev3<- park.ev2 %>% mutate(Year=year(Start_Date))

  # add cycles for MIDN/NCBN parks
  cycle1<-(2007:2010)
  cycle2<-(2011:2014)
  cycle3<-(2015:2018)
  cycle4<-(2019:2022)
  coloc1<-(2011:2014)
  coloc2<-(2015:2018)
  coloc3<-(2019:2022)
  ncbnc1<-(2008:2011)
  ncbnc2<-(2012:2015)
  ncbnc3<-(2016:2019)
  ncbnc4<-(2020:2023)
  ncbn<-c("GEWA","SAHI","THST")
  midn<-c("APCO","BOWA","FRSP","GETT","HOFU","PETE","RICH","VAFO")

  park.ev3<-park.ev3 %>% mutate(cycle=NA)
  park.ev3$cycle[park.ev3$Unit_Code=="COLO" & park.ev3$Year %in% coloc1]<-"Cycle1"
  park.ev3$cycle[park.ev3$Unit_Code=="COLO" & park.ev3$Year %in% coloc2]<-"Cycle2"
  park.ev3$cycle[park.ev3$Unit_Code=="COLO" & park.ev3$Year %in% coloc3]<-"Cycle3"

  park.ev3$cycle[park.ev3$Unit_Code %in% ncbn & park.ev3$Year %in% ncbnc1]<-"Cycle1"
  park.ev3$cycle[park.ev3$Unit_Code %in% ncbn & park.ev3$Year %in% ncbnc2]<-"Cycle2"
  park.ev3$cycle[park.ev3$Unit_Code %in% ncbn & park.ev3$Year %in% ncbnc3]<-"Cycle3"

  park.ev3$cycle[park.ev3$Unit_Code %in% midn & park.ev3$Year %in% cycle1]<-"Cycle1"
  park.ev3$cycle[park.ev3$Unit_Code %in% midn & park.ev3$Year %in% cycle2]<-"Cycle2"
  park.ev3$cycle[park.ev3$Unit_Code %in% midn & park.ev3$Year %in% cycle3]<-"Cycle3"

  park.ev4<-park.ev3 %>% filter(Year>=from & Year <=to) %>% droplevels()

  park.plots<- if (output=='short') {park.ev4 %>% dplyr::select(Location_ID,Event_ID,Unit_Code,
    Plot_Name,Plot_Number,X_Coord,Y_Coord,Panel,Year,Event_QAQC,cycle)
  } else if (output=='verbose') {park.ev4 %>% dplyr::select(Location_ID:Y_Coord,Coord_Units:Physiographic_Class,
    Plot_Name,Unit_Code:Start_Date,Event_QAQC, Year,cycle)}

  return(data.frame(park.plots))
} # end of function

