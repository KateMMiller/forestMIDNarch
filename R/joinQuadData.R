#' @include joinLocEvent.R
#' @title joinQuadData
#'
#' @description This function combines quadrat species and tree seedling cover data with species names and allows you to filter on species types, park, years, and visit type. Note that the Shrub guild also includes woody vine species.
#'
#' @param speciesType Allows you to filter on native, exotic or include all species.
#' \describe{
#' \item{"all"}{Default. Returns all species.}
#' \item{"native"}{Returns native species only}
#' \item{"exotic"}{Returns exotic species only}
#' }
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @export
#'
#------------------------
# Joins quadrat tables and filters by park, year, and plot/visit type
#------------------------
joinQuadData<-function(speciesType='all', park='all',from=2007, to=2018, QAQC=FALSE, locType='VS', output){
  # Prepare the quadrat data
  quadsamp$numQuadrats<-apply(quadsamp[,c(3:14)], 1,sum)
  park.plots<-force(joinLocEvent(park=park, from=from,to=to,QAQC=QAQC,locType=locType, output='short'))

  quads1<-merge(park.plots, quadsamp[,c("Event_ID","numQuadrats")], by="Event_ID", all.x=T)

  plants<-plants %>% mutate(Shrub=ifelse(Shrub==1|Vine==1,1,0),
    Tree=ifelse(Latin_Name=="Rhamnus cathartica",0,Tree))

  quads2<-merge(quads1,
    quads[,c("Event_ID","TSN","qA2_Cover_Class_ID","qA5_Cover_Class_ID","qA8_Cover_Class_ID","qAA_Cover_Class_ID",
    "qB2_Cover_Class_ID","qB5_Cover_Class_ID","qB8_Cover_Class_ID","qBB_Cover_Class_ID",
    "qC2_Cover_Class_ID","qC5_Cover_Class_ID","qC8_Cover_Class_ID","qCC_Cover_Class_ID")],by='Event_ID',all.x=T)

 # Convert coverclasses to midpoints for all 8 quadrats
  quads2[,14:25][quads2[,14:25]==1]<-0.1
  quads2[,14:25][quads2[,14:25]==2]<-1.5
  quads2[,14:25][quads2[,14:25]==3]<-3.5
  quads2[,14:25][quads2[,14:25]==4]<-7.5
  quads2[,14:25][quads2[,14:25]==5]<-17.5
  quads2[,14:25][quads2[,14:25]==6]<-37.5
  quads2[,14:25][quads2[,14:25]==7]<-62.5
  quads2[,14:25][quads2[,14:25]==8]<-85
  quads2[,14:25][quads2[,14:25]==9]<-97.5

  old.names<-names(quads2[,14:25])
  new.names<-c('A2','A5','A8','AA','B2','B5','B8','BB','C2','C5','C8','CC')
  quads2<-quads2 %>% rename_at(vars(old.names),~new.names)

  quads2[,c(14:25)][is.na(quads2[,c(14:25)])]<-0
  quads3<-quads2 %>% mutate(avg.cover=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats) #%>% select(Event_ID:TSN,avg.cover)
  quads3[,c(14:25)][quads3[,c(14:25)]>0]<-1
  quads3<-quads3 %>% mutate(avg.freq=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats)

  # Now to add the tree seedling cover data
  seed<-merge(quads1,sdlg[,c(1:4,11)], by=c("Event_ID"),all.x=T)
  seed[,"Cover"][is.na(seed[,"Cover"])]<-0
  seed[,'Cover'][seed[,'Cover']==1]<-0.1
  seed[,'Cover'][seed[,'Cover']==2]<-1.5
  seed[,'Cover'][seed[,'Cover']==3]<-3.5
  seed[,'Cover'][seed[,'Cover']==4]<-7.5
  seed[,'Cover'][seed[,'Cover']==5]<-17.5
  seed[,'Cover'][seed[,'Cover']==6]<-37.5
  seed[,'Cover'][seed[,'Cover']==7]<-62.5
  seed[,'Cover'][seed[,'Cover']==8]<-85
  seed[,'Cover'][seed[,'Cover']==9]<-97.5

  seed2<-seed %>% group_by(Event_ID,Quadrat,TSN) %>% summarize(numQuadrats=first(numQuadrats),Cover=sum(Cover)) %>%
    left_join(park.plots,.,by="Event_ID")

  seed.wide<-seed2 %>% spread(Quadrat, Cover, fill=0)  %>% select(-26) %>%
    mutate(avg.cover=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats)
  seed.wide<-seed.wide[,c(1:11,13,12,14:26)]
  seed.wide[,14:25][seed.wide[,14:25]>0]<-1
  seed.wide<-seed.wide %>% mutate(avg.freq=(A2+A5+A8+AA+B2+B5+B8+BB+C2+C5+C8+CC)/numQuadrats)

  quad.comb<-rbind(quads3,seed.wide)
  quad.comb2<-merge(quad.comb,plants[,c('TSN',"Latin_Name","Common","Tree","Shrub","Vine","Graminoid","Herbaceous","Exotic")],
    by="TSN", all.x=T)

  quad.comb3<-if (speciesType=='native'){filter(quad.comb2,Exotic==FALSE)
    } else if (speciesType=='exotic'){filter(quad.comb2,Exotic==TRUE)
    } else if (speciesType=='all'){(quad.comb2)
    } else if (speciesType!='native'|speciesType!='exotic'|speciesType!='all'){
      stop("speciesType must be either 'native','exotic', or 'all'")}

  quad.comb4<-quad.comb3 %>% mutate(avg.cover=ifelse(TSN==-9999999951,0,avg.cover),avg.freq=ifelse(TSN==-9999999951,0,avg.freq))

  return(data.frame(quad.comb4))
} # end of function

