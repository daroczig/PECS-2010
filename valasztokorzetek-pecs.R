# Pécs szavazóköreinek válaszátási eredményei (2010. október 3.)
# --------------------------------------------------------------
# Az alábbi R [http://www.r-project.org/] szkript:
#	> legyűjti a [választás.hu] oldalról Pécs választóköreinek címeit,
#	> egy egyszerű tisztítást végez,
#	> majd geokódolja azokat a Google API segítségével,
#	> WGS84 vetületből EOV-ba transzformál,
#	> legyűjti minden válaszókör névjegyzékében szereplő választópolgárok és a leadott szavazatok számát,
#	> koordináták szerint aggregál (azonos címen lévő szavazókörök eredményeinek szummázása),
#	> egy egyszerű, részleges térket készít a legyűjtött adatokról,
#	> végül ESRI shape formátumban menti a legyűjtött adatok körét.
# --------------------------------------------------------------
#	Készítette: Daróczi Gergely (2011)

library(XML)
library(R.utils)
library(tcltk)
options(width=100)

url <- 'http://valasztas.hu/dyn/ov10/outroot/onktjk1/02/tjk02217.htm'
x <- readHTMLTable(url, stringsAsFactors=FALSE)

# TEVK címek legyűjtése
valasztokorok <- x[[127]]
valasztokorok <- valasztokorok[-1, c('Sorsz', 'TEVK', 'Cím')]
valasztokorok$Cím

# címek rendezése
valasztokorok$Cím <- gsub(' ISKOLA| KERESK. KÖZPONT| ORVOSI RENDELŐ| NAPPALI ELLÁTÓ|
| ÓVODA| KÁVÉZÓ| POLG. HIVATAL| RENDELŐINTÉZET| JPTE ÁLL.ÉS JOGTUD.KAR| BÓBITA BÁBSZÍNHÁZ|
| KOLLÉGIUM|PTE MŰVÉSZETI KAR| FOGORV. RENDELŐ| HOTEL KIKELET| PTE TANÁRKÉPZŐ| MINERVA KÖNYVTÁR|
| SZÍNHÁZ| NÖVÉNY ÉS TALAJV.IG.| KULTÚRHÁZ| VÁSÁRTÉR MÁZSAHÁZ| PTE. EÜ. KAR| SZOCIÁLIS KÖZPONT|
| IDŐSEK NAPKÖZIJE| KÖZÉPISKOLA| KÖNYVTÁR| ANK(.)*|KÖZPONT| VOLT IDŐSEK KLUBJA|ÓVODA|
| SOLARFITT| INTEG.SZOC.INT.| PIZZ.| SZABADIDŐKÖZPONT| GANT KFT.| MŰVELŐDÉSI HÁZ| FALUHÁZ|
| volt KÖZSÉGHÁZA| ERFO KFT.| ÚJHEGYI ESZPRESSZÓ| ÜSZÖGI KÖZÖSSÉGI HÁZ| SZABADIDŐ|
| KÖZÖSSÉGI HÁZ', '', valasztokorok$Cím)
valasztokorok$Cím <- gsub('APÁCZAI CS.J.', 'APÁCZAI CSERE JÁNOS ', valasztokorok$Cím)

valasztokorok$Cím

# geokódolás
source('helpers.R')
valasztokorok$WGS.X <- valasztokorok$WGS.Y <- 0
valasztokorok$ADDR <- valasztokorok$PREC <- ''

valasztokorok

pb <- tkProgressBar(title = "A munka folyamatban...", min = 0, max = nrow(valasztokorok), width = 300)
for (i in 1:length(valasztokorok$Cím)) {
	geocode <- gGeoCode(valasztokorok$Cím[i], 'Pécs')
	valasztokorok$WGS.X[i] <- as.numeric(geocode[1])
	valasztokorok$WGS.Y[i] <- as.numeric(geocode[2])
	valasztokorok$ADDR[i] <- geocode[3]
	valasztokorok$PREC[i] <- geocode[4]
	Sys.sleep(0.5)
	setTkProgressBar(pb, i, label=paste( round(i/nrow(valasztokorok)*100, 0), "% kész!"))
}
close(pb)

# nem talált címeket belőni középre
valasztokorok$WGS.X[which(valasztokorok$PREC == "ZERO_RESULTS")] <- mean(valasztokorok$WGS.X, na.rm=TRUE)
valasztokorok$WGS.Y[which(valasztokorok$PREC == "ZERO_RESULTS")] <- mean(valasztokorok$WGS.Y, na.rm=TRUE)

valasztokorok

# WGS -> EOV transzformáció
library(rgdal)
EOV <- "+proj=somerc +lat_0=47.14439372222222 +lon_0=19.04857177777778 
		+alpha=90 +k=0.99993 +x_0=650000 +y_0=200000 +ellps=GRS67 +units=m 
		+no_defs +towgs84=+57.01,-69.97,-9.29"
WGS84 <- "+proj=longlat +datum=WGS84"
data.wgs <- valasztokorok
coordinates(data.wgs) <- c("WGS.X", "WGS.Y")
proj4string(data.wgs) <- CRS(WGS84)
data.eov <- spTransform(data.wgs, CRS(EOV))
newcoords <- as.data.frame(data.eov@coords)
names(newcoords) <- c("EOV.X", "EOV.Y")
valasztokorok <- cbind(valasztokorok, newcoords)

valasztokorok

# további adatok legyűjtése
valasztokorok$szavazok <- valasztokorok$nevjegyzek <- 0

webpage <- getURL(url)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE, encoding='UTF-8')
urls <- paste('http://valasztas.hu/dyn/ov10/outroot/onktjk1/02/', as.character(xpathSApply(pagetree, "//center/table/tr/td/a", xmlAttrs)), sep='')
pb <- tkProgressBar(title = "A munka folyamatban...", min = 0, max = length(urls), width = 300)
for (i in 1:length(urls)) {
	t <- readHTMLTable(urls[i], stringsAsFactors=FALSE)
	valasztokorok$nevjegyzek[i] <- t[[4]][2,1]	# A szavazásmegkezdésekor a 
												# névjegyzéken lévő választópolgárok száma
	valasztokorok$szavazok[i] <- t[[7]][2,1]	# Urnában lévő szavazólapok száma
	Sys.sleep(0.5)
	setTkProgressBar(pb, i, label=paste( round(i/length(urls)*100, 0), "% kész!"))
}
close(pb)

valasztokorok

valasztokorok$szavazok <- as.numeric(gsub(' ', '', valasztokorok$szavazok))
valasztokorok$nevjegyzek <- as.numeric(gsub(' ', '', valasztokorok$nevjegyzek))

# aggregálás
eov_aggr <- aggregate(cbind(szavazok, nevjegyzek) ~ EOV.X + EOV.Y, valasztokorok, sum)
eov_aggr$ID <- 1:nrow(eov_aggr)

# térkép
library(ReadImages)
library(RgoogleMaps)
MyMap <- GetMap(center=c(46.07132,18.233143), zoom=13, destfile = "Pécs.png", maptype = 'roadmap')

library(png)
img <- readPNG("Pécs.png")
r = as.raster(img[,,1:3])
r[img[,,4] == 0] = "white"
par(xpd=T, mar=par()$mar+c(0, 0, 0, 5))
plot(1:2, ylim=c(MyMap$BBOX$ll[1], MyMap$BBOX$ur[1]),
		xlim=c(MyMap$BBOX$ll[2],MyMap$BBOX$ur[2]), xlab='', ylab='')
rasterImage(r, MyMap$BBOX$ll[2], MyMap$BBOX$ll[1], MyMap$BBOX$ur[2], MyMap$BBOX$ur[1])
valasztokorok$szavazok_cut <- cut(valasztokorok$szavazok, 10)
points(valasztokorok$WGS.X, valasztokorok$WGS.Y, pch=23, cex=2, 
		bg=heat.colors(10)[valasztokorok$szavazok_cut])
legend(par("usr")[2], mean(par("usr")[3:4])+0.015, legend=levels(valasztokorok$szavazok_cut), fill=heat.colors(10), bty="n", 
		title='Szavazók száma')

# ESRI shape file export
library(shapefiles)
data.id <- eov_aggr[,c("ID", "EOV.X","EOV.Y")]
data.table <- eov_aggr
ddShapefile <- convert.to.shapefile(data.id, data.table, "ID", 1)
write.shapefile(ddShapefile, "eov", arcgis=T)

# reproducible research
save(valasztokorok, eov_aggr, file='valasztokorok.Rdata')
setwd('/home/omleny/Documents/melo/pubs/Geostat - KSH konferencia')
load('valasztokorok.Rdata')
