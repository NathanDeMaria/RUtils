library(XML)

get_config <- function(key, location='config.xml') {
	if (!file.exists(location)) {
		stop(sprintf("No config file found at %s", location))
	}
	doc <- xmlParse(location)
	xpath_query <-  sprintf("//config/item[@key='%s']", key)
	matches <- getNodeSet(doc, xpath_query)
	if (length(matches) < 1) {
		stop(sprintf("Key %s not found!", key))
	}
	if (length(matches) > 1) {
		warning(sprintf("Multiple keys matching %s, returned the first", key))
	}
	xmlGetAttr(matches[[1]], 'value')
}