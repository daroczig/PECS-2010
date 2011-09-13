###########################################################
#					geokódoló segédfüggvények
###########################################################
library(RCurl)
library(RJSONIO)
getDocNodeVal=function(doc, path)
{
	sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}
#' Geokódoló fv.
#' 
#' Adott karaktervektort a Google API segítségével geokódol WGS84 vetületbe.
#' Az eredmények során szűr a \code{g} paraméterben megadott sztringre, így egyszerűen törölhetőek
#' az egyértelműen fals eredmények (l. más településen való találatok).
#' 
#' @param str Geokódolni kívánt cím (character string)
#' @param g	Szűrőkifejezés (regexpr is)
#' @return Vektor (lng, lat, addr, prec)
#' @export
#' @examples \dontrun{
#' 	gGeoCode('1024 Budapest, Keleti Károly u. 5-7')
#'  gGeoCode('Keleti Károly u.', 'Budapest')
#'  gGeoCode('Keleti Károly u.', 'Szajol')
#' }
gGeoCode=function(str, g=NULL)
{
	w <- 1 # it is my lucky day
	library(XML)
	u=paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&language=hu&region=h&&address=',str)
	doc = xmlTreeParse(u, useInternal=TRUE)
	if (getDocNodeVal(doc, "/GeocodeResponse/status") == "ZERO_RESULTS") {
		return(c(NA, NA, NA, "ZERO_RESULTS"))
	} else {
		if (!is.null(g)) w <- grep(g, getDocNodeVal(doc, "/GeocodeResponse/result/formatted_address"))
		if (length(w) == 0) {
			return(c(NA, NA, NA, "ZERO_RESULTS"))
		}
		lng=as.numeric(getDocNodeVal(doc, paste("/GeocodeResponse/result[",w,"]/geometry/location/lat", sep='')))
		lat=as.numeric(getDocNodeVal(doc, paste("/GeocodeResponse/result[",w,"]/geometry/location/lng", sep='')))
		addr <- getDocNodeVal(doc, paste("/GeocodeResponse/result[",w,"]/formatted_address", sep=''))
		prec <- getDocNodeVal(doc, paste("/GeocodeResponse/result[",w,"]/geometry/location_type", sep=''))
		c(lat, lng, addr, prec)
	}
}


