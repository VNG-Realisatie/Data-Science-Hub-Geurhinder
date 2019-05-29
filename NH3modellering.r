#-------------------------------------------------------------------------------
# Analyse Geurhinder
# data import, -preparation, -visualisation 
# auteur : Arie van Weeren (VAA)
# modificaties : Mark Gremmen, Data Science Hub / VNG
# lud 2019-05-29
#-------------------------------------------------------------------------------

# Libraries

#packages
packages <- c("deldir", "fields", "tidyverse", "ggmap", "raster", "dismo", "gstat", "rmarkdown", "knitr")
#installeer de packages die (nog) niet aanwezig zijn 
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE)

#review R statistics setup
#sessionInfo()


#-------------------------------------------------------------------------------
# Global settings

#gemeente/regionaam
gemeente <- "De Peel" #aanpassen
#onderwerp analyse
analyse <- "Geurhinder"

#root locatie van deze procedure (workdirectory)
root <- getwd()
root

#(import)locatie data-bestanden, Creeer de map eerst
data.loc <- paste0(root,'/DATA/')

#(export)locatie grafieken en andere visualisaties, Creeer de map eerst
plots.loc <- paste0(root,'/PLOTS/')

#dimensie grafieken
graph_height <- 8
aspect_ratio <- 2.5 # breedte = graph_height * aspect_ratio

#prefix titel grafieken 
subject.nme <- paste0(analyse,' ', gemeente, ' ')

brondata.loc <- paste0(data.loc,"Alle-Gebieden-jaar.csv") #aanpassen


dfNH3 <- read.table(brondata.loc,
                    header = TRUE, sep = ";", dec = ",",
                    stringsAsFactors=FALSE, quote="")
dfNH3 <- dfNH3[,-17]
dfNH3 <- dfNH3 %>% gather(X2005, X2006, X2007, X2008, X2009, 
                          X2010, X2011, X2012, X2013, 
                          X2014, X2015, X2016, X2017, 
                          key="jaar",value="conc") %>% 
  na.omit() %>%
  mutate(jaar=sub("X","",jaar)) 
dfNH3$jaar <- as.numeric(dfNH3$jaar)
head(dfNH3)

dfNH3 <- dfNH3 %>% filter(Gebied == "Groote Peel")
head(dfNH3)

dfNH3_mean <- dfNH3 %>% group_by(jaar) %>% summarise(conc=mean(conc))
dfNH3_mean


plot.title = paste0(subject.nme,' Evolutie gemiddelde concentratie NH3',' ')
ggplot(dfNH3_mean) + geom_line(aes(jaar,conc)) +
  labs(x = "jaar",y = "concentratie",
       title = plot.title) +
  scale_x_continuous(breaks=seq(2005,2017,2))
plot.nme = paste0(plot.title,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


plot.title = paste0(subject.nme,' Concentratie NH3 per locatie',' ')
ggplot(dfNH3) + geom_line(aes(jaar,conc,color=as.factor(Locatienummer))) +
  labs(x="jaar",y="concentratie"
       ,title=plot.title) +
  scale_color_brewer(name="Locatie Nummer",palette = "Set1") +
  scale_x_continuous(breaks = seq(2005,2017,2))
plot.nme = paste0(plot.title,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)



m <- Tps(coordinates(dsp),dsp$conc)
tps <- interpolate(r,m)
tps <- mask(tps,grootepeel)
plot(tps, col = 
       colorRampPalette(c("darkblue","purple3","red",
                          "orange","yellow","lightyellow"))(500))


knmi.loc <- paste0(data.loc,"knmi.csv") 
dfknmi = read_csv(knmi.loc)
dfknmi <- dfknmi %>% filter(jaar>2004) %>% filter(jaar<2018)
dfknmi <- dfknmi %>% filter(station=="Volkel")
dfknmi

dfNH3_mean <- merge(dfNH3_mean,dfknmi,by ="jaar")


plot.title = paste0(subject.nme,' Gemiddelde temperatuur per jaar',' ')
ggplot(dfNH3_mean) + geom_line(aes(jaar,temperatuur)) +
  labs(x="Jaar",y="Temperatuur [C]",
       title=plot.title) +
  scale_x_continuous(breaks=seq(2005,2017,2))
plot.nme = paste0(plot.title,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


plot.title = paste0(subject.nme,' Verband tussen temperatuur en NH3 concentratie',' ')
ggplot(dfNH3_mean) + geom_point(aes(temperatuur,conc)) +
  labs(x="Temperatuur [C]",y="Concentratie NH3",
       title=plot.title)
plot.nme = paste0(plot.title,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


cor.mat <- cor(dfNH3_mean[,c(2,4:8)])
print(round(cor.mat,2))

mdl <- lm(conc~temperatuur+zonneschijn+luchtdruk+neerslag+luchtvochtigheid,
          data=dfNH3_mean)
summary(mdl)

mdl <- update(mdl,.~.-luchtvochtigheid)
summary(mdl)

mdl <- update(mdl,.~.-luchtdruk)
summary(mdl)

mdl <- update(mdl,.~.-1)
summary(mdl)

mdl <- update(mdl,.~.-temperatuur)
summary(mdl)

mdl <- update(mdl,.~.-zonneschijn+1)
summary(mdl)

locations.loc <- paste0(data.loc,"locations.csv") 
dfpeel <- read_csv(locations.loc)
dfloc <- merge(dfNH3,dfpeel,by.x="Locatienummer",by.y="Nummer")
dfloc <- dfloc %>% select(Locatienummer,jaar,conc,Longitude,Latitude)
dfloc <- as.data.frame(dfloc)


peel.box <- c(left=5.78,right=5.84,top=51.36,bottom=51.32)
peel.map <- get_stamenmap(peel.box,zoom=12,maptype="terrain")


plot.title = paste0(subject.nme,' Concentratie NH3',' ')
ggmap(peel.map) +
  geom_point(aes(x=Longitude,y=Latitude,color=conc),data=dfloc) +
  scale_color_gradient2(guide="colorbar",name="Concentratie NH3",
                        low="darkblue",mid="red",
                        high="yellow",midpoint=9) +
  facet_wrap(~jaar,ncol=4) +
  theme(axis.ticks = element_blank(),axis.text = element_blank())
plot.nme = paste0(plot.title,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)



grootepeel <- shapefile("grootepeel.shp")
plot(grootepeel)

bbpeel <- bbox(grootepeel)
dsp <- SpatialPoints(dfpeel[,3:4],
                     proj4string = 
                       CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                     bbox = bbpeel)

d <- dfloc %>% group_by(Locatienummer) %>% 
  summarise(Longitude=first(Longitude),
            Latitude=first(Latitude),
            conc = mean(conc))
d <- as.data.frame(d)
dsp <- SpatialPointsDataFrame(dsp,d)


v <- voronoi(dsp,ext=extent(grootepeel))
plot(v)



png("PLOTS/plot1.png", width = 800, height = 400)
plot.title = paste0(subject.nme,' Plot1',' ')
plot1 <- spplot(v,"conc", col.regions = 
         colorRampPalette(c("darkblue", "purple3",
                            "red","orange","yellow"))(500))
spplot(plot1, "value")
dev.off()



vgpeel <- intersect(v,grootepeel)

png("PLOTS/plot2.png", width = 800, height = 400)
plot.title = paste0(subject.nme,' Plot2',' ')
plot2 <- spplot(vgpeel,"conc", col.regions =
         colorRampPalette(c("darkblue", "purple3","red",
                            "orange","yellow"))(500))
spplot(plot2, "value")
dev.off()



r <- raster(grootepeel,ncols=500,nrows=500)
vr <- rasterize(vgpeel,r,"conc")

png("PLOTS/plot3.png", width = 800, height = 400)
plot.title = paste0(subject.nme,' Plot3',' ')
plot3 <- plot(vr, col = 
       colorRampPalette(c("darkblue", "purple3","red",
                          "orange","yellow"))(500))
spplot(plot3, "value")
dev.off()



gs <- gstat(formula=conc~1,locations=dsp)
nn <- interpolate(r,gs)
nnmsk <- mask(nn,grootepeel)

png("PLOTS/plot4.png", width = 800, height = 400)
plot.title = paste0(subject.nme,' Plot4',' ')
plot4 <- plot(nnmsk, col = 
       colorRampPalette(c("darkblue","purple3",
                          "red","orange","yellow"))(500))

spplot(plot4, "value")
dev.off()








