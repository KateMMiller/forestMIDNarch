#' @include joinLocEvent.R
#' @title joinTreeData: compiles tree data
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by
#' @importFrom magrittr %>%
#'
#' @description This function combines location and event-level Tree data. Note that BA_cm2 was corrected
#' for Loc_Type=='Deer' to be the same as Loc_Type=='VS' by multiplying BA_cm2 by 4 to get BA_cm2/400m2.
#' Must run importData first.
#'
#' @param status Filter by live, dead, or all. Acceptable options are:
#' \describe{
#' \item{"all"}{Includes all standing trees}
#' \item{"live"}{live trees only}
#' \item{"dead"}{dead trees only}
#' }
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @param dist_m Filter trees by a distance that is less than or equal to the specified distance in meters
#' of the tree to the center of the plot. If no distance is specified, then all trees will be selected. For
#' example, to select an area of trees that is 100 square meters in area, use a distance of 5.64m.
#'
#' @return returns a dataframe with plot-level and visit-level tree data
#'
#' @examples
#' importData()
#' # compile tree data for live trees only in most recent survey in all parks
#' live_trees <- joinTreeData(status = 'live', from = 2015, to = 2018)
#'
#' # compile FRSP trees within 7.3152m radius (FIA subplot size) in most recent survey
#' FRSP_100m <- joinTreeData(park = 'FRSP', from = 2015, to = 2018, dist_m = 7.3152)
#'
#' # compile dead trees in GETT in most recent survey
#' GETT_dead <- joinTreeData(park = 'GETT', from = 2015, to = 2018, status = 'dead')
#'
#' # compile exotic trees in VAFO in all years
#' VAFO_exotic <- joinTreeData(park = 'VAFO', from = 2015, to = 2018, speciesType = 'exotic')
#'
#' @export
#'

#------------------------
# Joins tbl_Trees and tbl_Tree_Data tables and filters by park, year, and plot/visit type
#------------------------
joinTreeData<-function(status = c('all','live','dead'), speciesType = c('all', 'native', 'exotic'), park = 'all',
                       from = 2007, to = 2018, QAQC = FALSE, locType = 'VS', panels = 1:4, dist_m = NA, output, ...){

  status <- match.arg(status)
  speciesType <- match.arg(speciesType)

  treeTSN <- merge(trees[,c("Tree_ID", "Location_ID", "TSN", "Tree_Number_MIDN", "Distance", "Azimuth")],
    plants[,c('TSN', 'Latin_Name', 'Common', 'Exotic')], by = "TSN", all.x = T)

  tree2 <- merge(treeTSN, treedata, by = "Tree_ID", all.x = T, all.y = T)
  tree2 <- tree2 %>% select(Tree_ID:HWA_Status, Event_ID, -Location_ID)
  tree2$BA_cm2 <- round(pi*((tree2$DBH/2)^2),4)# basal area (cm^2)

  alive <- c("1", "AB", "AF", "AL" ,"AM" ,"AS", "RB", "RF", "RL", "RS")
  dead <- c("2","DB" ,"DF" ,"DL", "DM","DS")

  tree3 <- if (status == 'live') {filter(tree2, Status_ID %in% alive)
  } else if (status == 'dead') {filter(tree2, Status_ID %in% dead)
  } else if (status == 'all') {(tree2)
  }

  tree4 <- if (speciesType == 'native'){filter(tree3, Exotic == FALSE)
  } else if (speciesType == 'exotic'){filter(tree3, Exotic == TRUE)
  } else if (speciesType == 'all'){(tree3)
  }

  tree5 <- if (!is.na(dist_m)){filter(tree4, Distance <= dist_m)
  } else {tree4}

  park.plots <- force(joinLocEvent(park = park, from = from,to = to, QAQC = QAQC,locType = locType,
                                   rejected = F, panels = panels, output = 'verbose'))

  park.plots <- park.plots %>% select(Location_ID, Event_ID, Unit_Code, Plot_Name, Plot_Number, X_Coord, Y_Coord,
                                      Panel, Year, Event_QAQC, cycle, Loc_Type)


  tree6 <- merge(park.plots, tree5, by='Event_ID', all.x=T)

  tree7 <- tree6 %>% mutate(BA_cm2= ifelse(Loc_Type!='Deer', BA_cm2, BA_cm2*4)) # Deer exclosure plots only 100m2

  return(data.frame(tree7))
} # end of function

