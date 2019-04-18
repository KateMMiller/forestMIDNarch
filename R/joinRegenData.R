#' @include joinLocEvent.R
#' @title joinRegenData
#'
#' @importFrom dplyr select filter arrange mutate summarise group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub str_pad
#'
#' @description This function combines seedling and sapling data, and calculates stocking index. Must run importData first.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#' @param canopyForm Allows you to filter on canopy species only or include all species.
#' \describe{
#' \item{"all"}{Returns all species, including low canopy species.}
#' \item{"canopy"}{Default. Returns canopy-forming species only}
#'}
#' @param units Calculates seedling and sapling densities based on different units.
#' \describe{
#' \item{"sq.m"}{Default. Returns seedling and sapling densities per square meter.}
#' \item{"ha"}{Returns seedling and sapling densities per hectare}
#' \item{"acres"}{Returns densities per acre}
#'}
#' @param height Determines whether all height classes are included, or just >15cm seedlings.
#' \describe{
#' \item{"ht15"}{Default. Returns seedlings >15cm and all saplings.}
#' \item{"all"}{Returns all seedlings and saplings, including 5-15cm seedlings in VAFO.}
#'}
#'
#' @return returns a dataframe with seedling and sapling densities, and stocking index
#'
#' @export
#'
# consider making it so that individual species can be filtered?
#------------------------
# Joins quadrat and microplot tables and filters by park, year, and plot/visit type
#------------------------
joinRegenData<-function(speciesType=c('all','native','exotic'), canopyForm=c('canopy','all'),
  units=c('sq.m','ha','acres'), park='all',from=2007, to=2018, QAQC=FALSE, locType='VS',
  height=c('ht15','all'), output){

  speciesType<-match.arg(speciesType)
  canopyForm<-match.arg(canopyForm)
  units<-match.arg(units)
  height<-match.arg(height)

  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, rejected=F, output='short'))

# Prepare the seedling data
  quad1<-merge(park.plots,quadsamp[,1:14], all.x=T)
  quad1$NumQuads<-rowSums(quad1[,13:24])
  seed<-merge(quad1[,c(1:11,25)],sdlg[,c(1:4,6:12)], by="Event_ID",all.x=T)
  seed[,16:21][is.na(seed[,16:21])]<-0
  seed$Cover<-as.numeric(seed$Cover)

  seed2<- if(height=='ht15'){
    seed %>% group_by(Event_ID,TSN) %>%
      summarise(NumQuads=first(NumQuads),seed15.30m2=sum(Seedlings_15_30cm)/NumQuads,
      seed30.100m2=sum(Seedlings_30_100cm)/NumQuads,seed100.150m2=sum(Seedlings_100_150cm)/NumQuads,
      seed150pm2=sum(Seedlings_Above_150cm)/NumQuads, avg.cover=sum(Cover)/NumQuads)
  } else if (height=='all'){
    seed %>% group_by(Event_ID,TSN) %>%
      summarise(NumQuads=first(NumQuads),seed5.15m2=sum(Seedlings_5_15cm)/NumQuads,
      seed15.30m2=sum(Seedlings_15_30cm)/NumQuads, seed30.100m2=sum(Seedlings_30_100cm)/NumQuads,
      seed100.150m2=sum(Seedlings_100_150cm)/NumQuads,seed150pm2=sum(Seedlings_Above_150cm)/NumQuads,
      avg.cover=sum(Cover)/NumQuads)
  } # need to add a path for including 5-15 in summary below

  seed3<-seed2 %>% mutate(seed.stock=(1*seed15.30m2 + 2*seed30.100m2 + 20*seed100.150m2 + 50*seed150pm2),
    seed.dens.m2=seed15.30m2+seed30.100m2+seed100.150m2+seed150pm2)

  # Prepare the sapling data
  saps1<-merge(micro,saps[,c("Microplot_Sapling_Data_ID", "Microplot_Characterization_Data_ID","Tree_ID",
    "DBH","Status_ID")],by="Microplot_Characterization_Data_ID", all.y=T)
  saps2<-merge(saps1,trees[,c("Location_ID","Tree_ID","TSN","Tree_Number_MIDN")],
    by=c("Tree_ID"), all.x=T, all.y=F)
  saps3<-merge(park.plots,saps2,by='Event_ID',all.x=T)
  saps4<-merge(saps3,plants[,c("TSN","Latin_Name","Common", 'Canopy_Exclusion','Exotic')],by="TSN",all.x=T)
  saps4$DBH[is.na(saps4$DBH)]<-0
  saps4$sap<-ifelse(saps4$DBH>0 & saps4$DBH<10.0, 1,0)
  saps5<-saps4 %>% group_by(Event_ID,TSN, Status_ID) %>%
    summarise(Latin_Name=first(Latin_Name),Common=first(Common),Canopy_Exclusion=first(Canopy_Exclusion),
      Exotic=first(Exotic),sap.stems=sum(sap, na.rm=T),avg.sap.dbh=mean(DBH, na.rm=T))

  alive<-c("AB","AF","al","AL","AM","AS","RB","RF","RL","RS")
  wgt.sap.stock<-50/((pi*3^2)*3)
  wgt.sap.dens<-(pi*3^2)*3

  saps6<-saps5 %>% filter(Status_ID %in% alive & Canopy_Exclusion ==0) %>%
    group_by(Event_ID,TSN,Latin_Name,Common,Exotic) %>%
    summarise(sap.dens.m2=sum(sap.stems)/wgt.sap.dens,sap.stock=wgt.sap.stock*sum(sap.stems)) %>% droplevels()

  saps7<-merge(park.plots,saps6, by="Event_ID",all.x=T) # left join sapling back to plot visit data to show all plots
  saps7$sap.stock[is.na(saps7$sap.stock)]<-0
  saps7$sap.dens.m2[is.na(saps7$sap.dens.m2)]<-0

# Combine seedling and sapling data
  regen1<-merge(park.plots,seed3,by='Event_ID', all.x=T,all.y=F)
  regen2<-merge(regen1,saps7[,c("Event_ID","TSN","sap.dens.m2","sap.stock")],by=c("Event_ID","TSN"),all.x=T,all.y=F)
  regen3<-merge(regen2,plants[,c('TSN','Latin_Name','Common','Exotic','Canopy_Exclusion')], by='TSN',all.x=T)

  regen4<-if(canopyForm=='canopy'){filter(regen3, Canopy_Exclusion!=1)
  } else if(canopyForm=='all'){(regen3)
  }

  regen5<- if (speciesType=='native'){filter(regen4,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(regen4,Exotic==TRUE)
  } else if (speciesType=='all'){(regen4)
  }

  regen5[,14:22][is.na(regen5[,14:22])]<-0
  regen5<-regen5 %>% mutate(stock=seed.stock+sap.stock) %>% select(-seed.stock,-sap.stock)

  regen6<- if (units=='sq.m'){
    regen5 %>%
        mutate(seed15.30=seed15.30m2,
          seed30.100=seed30.100m2,
          seed100.150=seed100.150m2,
          seed150p=seed150pm2,
          seed.den=seed.dens.m2,
          sap.den=sap.dens.m2)
    } else if (units=='ha'){
    regen5 %>%
      mutate(seed15.30=seed15.30m2*10000,
        seed30.100=seed30.100m2*10000,
        seed100.150=seed100.150m2*10000,
        seed150p=seed150pm2*10000,
        seed.den=seed.dens.m2*10000,
        sap.den=sap.dens.m2*10000)
    } else if (units=='acres'){
    regen5 %>%
      mutate(seed15.30=seed15.30m2*4046.856,
        seed30.100=seed30.100m2*4046.856,
        seed100.150=seed100.150m2*4046.856,
        seed150p=seed150pm2*4046.856,
        seed.den=seed.dens.m2*4046.856,
        sap.den=sap.dens.m2*4046.856)
    }

  regen7<-regen6 %>% select(Event_ID,TSN,Latin_Name,Common,Exotic,Canopy_Exclusion,seed15.30,
    seed30.100,seed100.150, seed150p,seed.den,sap.den,stock,avg.cover) %>% droplevels()

  regen8<-merge(park.plots,regen7,by="Event_ID",all.x=T)
  names(regen8)
  regen8[,17:24][is.na(regen8[,17:24])]<-0

  return(data.frame(regen8))
} # end of function

# Need to account for SAGA 008 being NA and ACAD 029 as being NA because missing data
