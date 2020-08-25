#' @title joinLocEvent: merges Location and Event level data with options for filtering.
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by between
#' @importFrom magrittr %>%
#' @importFrom stringr str_pad str_sub
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
#' @param from Year to start analysis, ranging from 2007-2019
#' @param to Year to stop analysis, ranging from 2007-2019
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
#' @param eventType Allows you to only include complete sampling events, or to include all sampling events
#' \describe{
#' \item{"complete"}{Only include sampling events for a plot that are complete.}
#' \item{"all}{Include all plots with an Event_ID, including plots that are missing all data associated with that event (eg COLO-380.2019).}
#' }
#' @param panels Allows you to select individual panels from 1 to 4. Default is all 4 panels (1:4).
#' If more than one panel is selected, specify by c(1,3), for example.
#' @return returns a dataframe with location and visit events
#'
#' @examples
#' importCSV('./forest_csvs')
#' # Select most recent survey of data from APCO
#' APCO_data <- joinLocEvent(park = 'APCO', from = 2016, to = 2019)
#'
#' # Select data from cycle 3
#' cycle3 <- joinLocEvent(from = 2015, to = 2018) # all parks is default
#'
#' # Select data from plots that had a QA/QC event in GETT in 2018
#' GETT_data<-joinLocEvent(park = 'GETT', QAQC = T, from = 2018)
#' QAQC_plots<-GETT_data$Plot_Name[which(GETT_data$Event_QAQC==TRUE)]
#' GETT_QAQC<-GETT_data %>% filter(Plot_Name %in% QAQC_plots) %>% droplevels()
#'
#' @export
#'
#------------------------
# Joins tbl_Locations and tbl_Events tables and filters by park, year, and plot/visit type
#------------------------
joinLocEvent<-function(park = "all", from = 2007, to = 2019, QAQC = FALSE, rejected = FALSE,
                       panels = 1:4, locType = 'VS', eventType = c('complete', 'all'),
                       output = 'short', ...){

  eventType <- match.arg(eventType)

  loc2 <- loc %>% mutate(Unit_Code = as.factor(str_sub(Unit_ID, 1, 4)))
  loc2$Plot_Number <- str_pad(loc2$Plot_Number, width = 3, side = "left", pad = 0) #Pad plot number so retains 3-digits
  loc2$Plot_Name <- paste(loc2$Unit_Code, loc2$Plot_Number, sep="-")

  loc3 <- if (locType == 'VS') {filter(loc2,Loc_Type=="VS")
   } else if (locType == 'all') {(loc2)
   } else if (locType != 'VS' || locType != 'all') {stop("locType must either be 'VS' or 'all'")}

  loc4 <- if (rejected == FALSE) {filter(loc3, Rejected == FALSE)
   } else if (rejected == TRUE) {(loc3)
   } else {stop("rejected must be TRUE or FALSE")}

  loc5 <- if (park == 'all') {(loc4)
   } else if (park %in% levels(loc4$Unit_Code)){filter(loc4, Unit_Code %in% park)
   } else {stop("park must be one of the factor levels of Unit_Code")}

  park.ev <- merge(loc5, event, by = "Location_ID", all.x = TRUE)

  park.ev2 <- if (QAQC == FALSE) {filter(park.ev, Event_QAQC == FALSE)
   } else if (QAQC == TRUE) {(park.ev)
   } else {stop("QAQC must be TRUE or FALSE")}


  park.ev3 <- park.ev2 %>% filter(Panel %in% panels) %>% droplevels()

  park.ev4 <- if (eventType == 'complete') {
    filter(park.ev3, Event_ID != '92470D3A-EB34-4FC2-B058-325B37F185AF') %>% droplevels()
   } else {park.ev3}

  #park.ev4<- suppressWarnings(park.ev3 %>% mutate(Year=lubridate::year(Start_Date)))
  park.ev4$Year <- format(as.Date(park.ev4$Start_Date, format = "%Y-%m-%d"), "%Y")
    # add cycles for MIDN/NCBN parks
  cycle1 <- (2007:2010)
  cycle2 <- (2011:2014)
  cycle3 <- (2015:2018)
  cycle4 <- (2019:2022)
  coloc1 <- (2011:2014)
  coloc2 <- (2015:2018)
  coloc3 <- (2019:2022)
  coloc4 <- (2023:2026)
  ncbnc1 <- (2008:2011)
  ncbnc2 <- (2012:2015)
  ncbnc3 <- (2016:2019)
  ncbnc4 <- (2020:2023)
  ncbn <- c("GEWA", "SAHI", "THST")
  midn <- c("APCO", "BOWA", "FRSP", "GETT", "HOFU", "PETE", "RICH", "VAFO")
  asisc1 <- c(2019:2020)

  park.ev5 <- park.ev4 %>% mutate(cycle = NA)
  park.ev5$cycle[park.ev5$Unit_Code == "COLO" & park.ev5$Year %in% coloc1] <- "Cycle1"
  park.ev5$cycle[park.ev5$Unit_Code == "COLO" & park.ev5$Year %in% coloc2] <- "Cycle2"
  park.ev5$cycle[park.ev5$Unit_Code == "COLO" & park.ev5$Year %in% coloc3] <- "Cycle3"
  park.ev5$cycle[park.ev5$Unit_Code == "COLO" & park.ev5$Year %in% coloc4] <- "Cycle4"

  park.ev5$cycle[park.ev5$Unit_Code == "ASIS" & park.ev5$Year %in% asisc1] <- "Cycle1"

  park.ev5$cycle[park.ev5$Unit_Code %in% ncbn & park.ev5$Year %in% ncbnc1] <- "Cycle1"
  park.ev5$cycle[park.ev5$Unit_Code %in% ncbn & park.ev5$Year %in% ncbnc2] <- "Cycle2"
  park.ev5$cycle[park.ev5$Unit_Code %in% ncbn & park.ev5$Year %in% ncbnc3] <- "Cycle3"
  park.ev5$cycle[park.ev5$Unit_Code %in% ncbn & park.ev5$Year %in% ncbnc4] <- "Cycle4"

  park.ev5$cycle[park.ev5$Unit_Code %in% midn & park.ev5$Year %in% cycle1] <- "Cycle1"
  park.ev5$cycle[park.ev5$Unit_Code %in% midn & park.ev5$Year %in% cycle2] <- "Cycle2"
  park.ev5$cycle[park.ev5$Unit_Code %in% midn & park.ev5$Year %in% cycle3] <- "Cycle3"
  park.ev5$cycle[park.ev5$Unit_Code %in% midn & park.ev5$Year %in% cycle4] <- "Cycle4"

  park.ev6 <- park.ev5 %>% filter(Year >= from & Year <= to) %>% droplevels()

  park.plots <- if (output == 'short') {
    park.ev6 %>% dplyr::select(Location_ID, Event_ID, Unit_Code, Plot_Name, Plot_Number,
                               X_Coord, Y_Coord, Panel, Year, Event_QAQC, cycle)
    } else if (output=='verbose') {
    park.ev6 %>% dplyr::select(Location_ID:Y_Coord, Coord_Units:Physiographic_Class,
                                Plot_Name, Unit_Code:Start_Date, Event_QAQC, Year, cycle)}

  return(data.frame(park.plots))
} # end of function

