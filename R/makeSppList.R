#' @include joinLocEvent.R
#' @include joinTreeData.R
#' @include joinQuadData.R
#' @include joinRegenData.R
#' @include joinMicroShrubData.R
#' @title makeSppList
#'
#' @description This function creates a plot-level species list from live trees, microplots, quadrats, and additional species lists.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return Returns a dataframe with species list for each plot.
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
makeSppList<-function(speciesType='all', park='all',from=2007, to=2018, QAQC=FALSE, locType='VS', output,...){

  park.plots<-force(joinLocEvent(park=park,from=from,to=to,QAQC=QAQC,locType=locType,rejected=F,output='short'))
  plants<-plants %>% mutate(Shrub=ifelse(Shrub+Vine>0,1,0))

  trees1<-force(joinTreeData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short', status='live'))
  trees2<-trees1 %>% group_by(Event_ID,TSN,Latin_Name,Common) %>% summarise(tree.stems=length(DBH>10),
    tree.BAcm2=sum(BA_cm2)) %>% ungroup()
  trees3<-trees2 %>% select(Event_ID,TSN,tree.stems,tree.BAcm2)

  regen1<-force(joinRegenData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short'))
  regen2<-regen1 %>% select(Event_ID,TSN,seed.den,sap.den,stock)

  quads1<-force(joinQuadData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short'))
  quads2<-quads1 %>% select(Event_ID,TSN,avg.cover,avg.freq)

  shrub1<-force(joinMicroShrubData(park=park, from=from,to=to,QAQC=QAQC,locType=locType,
    output='short'))
  shrub2<-shrub1 %>% select(Event_ID,TSN,present.old,cover)

  addspp2<-addspp %>% select(Event_ID,TSN) %>% mutate(addspp=1)

  comb1<-merge(trees3,regen2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb2<-merge(comb1,quads2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb3<-merge(comb2,shrub2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb4<-merge(comb3,addspp2,by=c("Event_ID","TSN"),all.x=T,all.y=T)
  comb5<-comb4 %>% filter(TSN!=-9999999951)

  comb6<-merge(comb5,plants[,c("TSN","Latin_Name","Common","Exotic","Tree","Shrub","Herbaceous","Graminoid")],
    by="TSN",all.x=T)

  comb7<-if (speciesType=='native'){filter(comb6,Exotic==FALSE)
  } else if (speciesType=='exotic'){filter(comb6,Exotic==TRUE)
  } else if (speciesType=='all'){(comb6)
  } else if (speciesType!='native'|speciesType!='exotic'|speciesType!='all'){
    stop("speciesType must be either 'native','exotic', or 'all'")}

  comb8<-merge(park.plots,comb7,by="Event_ID",all.x=T,all.y=F)

  colnames(comb8)<-c("Event_ID","Location_ID","Unit_Code","Plot_Name","Plot_Number","X_Coord","Y_Coord","Panel",
    "Year","Event_QAQC","cycle","TSN","tree.stems","tree.BAcm2","seed.den","sap.den","stocking.index","avg.quad.cover",
    "avg.quad.freq","shrub.present.old","shrub.cover","addspp.present","Latin_Name","Common","Exotic","Tree","Shrub",
    "Herbaceous","Graminoid")

  comb8[,c(13:19,22)][is.na(comb8[,c(13:19,22)])]<-0
  comb8<-comb8 %>% mutate(shrub.cover=ifelse(Year>2010 & is.na(shrub.cover),0,shrub.cover),
    shrub.present.old=ifelse(Year<=2010 & is.na(shrub.present.old),0,shrub.present.old))
  return(data.frame(comb8))
  } # end of function