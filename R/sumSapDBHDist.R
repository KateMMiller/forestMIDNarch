#' @include joinLocEvent.R
#' @include joinRegenData.R
#' @title sumSapDBHDist: calculates DBH distribution of saplings
#'
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by case_when
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#'
#' @description This function calculates DBH distribution by 1cm size classes. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @param canopyForm Allows you to filter on canopy species only or include all species.
#' \describe{
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns canopy-forming species only}
#'
#' @param units Allows you to choose which metric to calculate: basal area or stem density
#' \describe{
#' \item{"density"}{Default. Returns stems/ha}
#' \item{"ba"}{Returns basal area in sq.m/ha}
#' }
#'
#'
#' @return returns a dataframe with one row for each plot and either density or BA
#'
#' @examples
#' importData()
#' sap_diam_dist <- sumSapDBHDist(park = 'RICH', speciesType = 'native', from = 2015, to = 2018, units = 'ba')
#'
#' @export
#'
#------------------------
# Calculates sapling diameter distribution
#------------------------
sumSapDBHDist<-function(speciesType = c('all', 'native','exotic'), canopyForm = c('canopy', 'all'),
                        units = c('density', 'ba'), park = 'all',
                        from = 2007, to = 2019, QAQC = FALSE, locType = 'VS',
                        panels = 1:4, output, ...){

  speciesType <- match.arg(speciesType)
  canopyForm <- match.arg(canopyForm)
  units <- match.arg(units)

  park.plots <- force(joinLocEvent(park = park, from = from,to = to, QAQC = QAQC,locType = locType,
                                   rejected = F, panels = panels, output = 'verbose'))

  park.plots <- park.plots %>% select(Location_ID, Event_ID, Unit_Code, Plot_Name, Plot_Number, X_Coord, Y_Coord,
                                      Panel, Year, Event_QAQC, cycle, Loc_Type)

  # Prepare the sapling data
  saps1 <- merge(micro, saps[,c("Microplot_Sapling_Data_ID", "Microplot_Characterization_Data_ID","Tree_ID",
                                "DBH","Status_ID")],by="Microplot_Characterization_Data_ID", all.y=T, all.x=T)

  saps2 <- merge(saps1, trees[,c("Location_ID","Tree_ID","TSN","Tree_Number_MIDN")],
                 by = c("Tree_ID"), all.x = T, all.y = F)

  alive <- c("1", "AB", "AF", "AL" ,"AM" ,"AS", "RB", "RF", "RL", "RS")

  saps2_live <- saps2 %>% filter(Status_ID %in% alive) %>%
                          filter(DBH > 0 & !(is.na(DBH))) %>% droplevels()

  saps3 <- merge(park.plots, saps2_live, by = c('Location_ID','Event_ID'), all.x = T)

  saps4 <- merge(saps3, plants[,c("TSN","Latin_Name", "Common", 'Canopy_Exclusion','Exotic')], by = "TSN", all.x = T)
  saps4$DBH[is.na(saps4$DBH)] <- 0

  saps5 <- if (speciesType == 'native'){filter(saps4, Exotic == FALSE)
    } else if (speciesType == 'exotic'){filter(saps4, Exotic == TRUE)
    } else if (speciesType == 'all'){(saps4)
    }

  saps6 <- if(canopyForm == 'canopy'){filter(saps5, Canopy_Exclusion == FALSE)
    } else if(canopyForm == 'all'){(saps5)
    }

  saps6 <- saps6 %>% mutate(sap = ifelse(DBH > 0 & DBH < 10, 1, 0)) %>%
                     filter(!is.na(Status_ID))

  saps7 <- saps6 %>% mutate(size_class= as.factor(case_when(between(DBH, 1, 1.9)~ 'd1_1.9',
                                                            between(DBH, 2, 2.9)~ 'd2_2.9',
                                                            between(DBH, 3, 3.9)~ 'd3_3.9',
                                                            between(DBH, 4, 4.9)~ 'd4_4.9',
                                                            between(DBH, 5, 5.9)~ 'd5_5.9',
                                                            between(DBH, 6, 6.9)~ 'd6_6.9',
                                                            between(DBH, 7, 7.9)~ 'd7_7.9',
                                                            between(DBH, 8, 8.9)~ 'd8_8.9',
                                                            between(DBH, 9, 9.9)~ 'd9_9.9',
                                                            TRUE ~ 'unknown')),
                            stem = 1,
                            unit_conv = ifelse(Loc_Type == 'Deer', 100, (pi*3^2)*3),
                            BA_cm2 = round(pi*((DBH/2)^2),4))

  # In case there's a size class not represented


  sap_dist <- saps7 %>% group_by(Event_ID, Plot_Name, size_class, unit_conv) %>%
    summarise(num_stems_ha = sum(stem)*10000/first(unit_conv),
              BA_m2ha = sum(BA_cm2)/first(unit_conv),
              .groups = 'keep') %>% ungroup() # BA already corrected for Deer Ex. in joinTreeData

    sap_dist_wide <- if (units=='density') {
    sap_dist %>% select(Event_ID, Plot_Name, size_class, num_stems_ha) %>%
      spread(size_class, num_stems_ha, fill = 0)
    } else if (units=='ba') {
    sap_dist %>% select(Event_ID, Plot_Name, size_class, BA_m2ha) %>%
      spread(size_class, BA_m2ha, fill = 0)
    }

  # next few lines find if a size class is missing, and adds it later
  sizes=c('d1_1.9', 'd2_2.9', 'd3_3.9', 'd4_4.9',
          'd5_5.9', 'd6_6.9', 'd7_7.9', 'd8_8.9',
          'd9_9.9', 'unknown')

  missing_sizes <- setdiff(sizes, names(sap_dist_wide))

  sap_dist_wide[missing_sizes] <- 0

  sap_dist_final <- merge(park.plots, sap_dist_wide, by=c('Event_ID', 'Plot_Name'), all.x=T) %>%
    select(Location_ID, Event_ID, Plot_Name, Unit_Code:cycle, Loc_Type,
           d1_1.9, d2_2.9, d3_3.9, d4_4.9, d5_5.9, d6_6.9,
           d7_7.9, d8_8.9, d9_9.9, unknown) %>% arrange(Plot_Name, cycle)

  return(sap_dist_final)

} # end of function

