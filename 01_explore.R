############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Archivo Narrow"

library(quantmod)

############################################################################################
### aggregate
############################################################################################

data.table <- read.csv("data_refined.csv")

#reformat the date
data.table$date <- as.POSIXct(gsub("^(.*)T.*$", "\\1", as.character(data.table$date)))

#reformat the reason - removing the last dot
data.table$description <- factor(gsub("\\.$", "", as.character(data.table$description)))

## collapse fines by bank, reason and description
data <- do.call(rbind, by(data.table, interaction(data.table$bank, data.table$reason, data.table$description, drop = T), function(d) {
	if(nrow(d) == 1) {
		d[1,c('date', 'bank', 'reason', 'valuemillions', 'description')]
	} else {
		cbind(d[nrow(d),c('date', 'bank', 'reason')], valuemillions = sum(d$valuemillions), description = d[nrow(d),'description'])
	}
}))
rownames(data) <- NULL

# reorder by date
data <- data[order(data$date),]


head(data[order(data$valuemillions, decreasing = T), ])







############################################################################################
### VIS
############################################################################################
require(rCharts)

data$numBank <- as.numeric(data$bank)
data$numDate <- as.Date(data$date)


### NVD3 version

# # n1 <- nPlot(numBank ~ numDate, data = data, type = "scatterChart", group = "reason")
# n1$chart(size = '#! function(d){return d.valuemillions} !#')
# n1$xAxis( tickFormat="#!function(d) {return d3.time.format('%Y')(new Date( d * 86400000 ));}!#" )
# n1$chart(tooltipContent = "#! function(key, x, y){
#   return 'Bank: ' + x + '  y: ' + y
# } !#")
# n1$chart(showControls = FALSE)
# n1$yAxis(tickFormat = "#! function(d) {return} !#")
# n1

data$formatDescription <- sapply(data$description, function(txt) {
	txt <- as.character(txt)
	if(nchar(as.character(txt)) > 50) {
		gsub('(.{1,50})(\\s|$)', '\\1<br>', txt)
	} else {
		txt
	}
})


## old highchart version
require(data.table)
dat <- data.table(
  x = as.numeric(data$date) * 1000,
  y = as.numeric(data$bank),
  z = data$valuemillions ,
  name = sprintf("<table cellpadding='1'><tr><th colspan = '2'>%1$s</th></tr><tr><td>%3$s</td></tr><tr><td>%2$s</td><td align='left'>Fine million USD: %4$s</td></tr><tr><td colspan = '2'><br>%5$s</td></tr></table>",
    as.character(data$bank),
	as.character(data$date),
    data$reason,
    data$valuemillions,
    data$formatDescription),
  category = data$reason,
  fine = data$valuemillions
)

# Split the list into categories
datSeries <- lapply(split(dat, dat$category), function(x) {
    res <- lapply(split(x, rownames(x)), as.list)
    names(res) <- NULL
    return(res)
})


a <- rCharts::Highcharts$new()
invisible(sapply(datSeries, function(x) a$series(data = x, type = c("bubble"), name = x[[1]]$category)))

a$plotOptions(
  bubble = list(
    cursor = "pointer",
    minSize = 7,
    maxSize = 30#,
    # point = list(
  #     events = list(
  #       click = "#! function() { window.open(this.options.url); } !#"))
  )
)
a$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")


a$yAxis(gridLineColor = "white", min = 0, labels = "enabled: false", title = "text: null")
a$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m}',
	style = list(fontSize = '11px', fontFamily = 'Verdana, sans-serif')))
a$credits(text = "swissinfo.ch - data source: Financial Times", href = "http://blogs.ft.com/ftdata/2014/03/28/bank-fines-data/")
a$title(text = "Bank fines by the US regulators since 2007")
a$subtitle(text = "Banks have paid more than $100bn in legal settlements with US regulators since the financial crisis in 2007<br>Mouse over the bubbles under to know more about each fine. Bubble areas are proportional to the fines, the different type of offenses are in different colors, different banks are on different horizontal level",
	 style = list(fontSize = '9px', fontFamily = 'Verdana, sans-serif'), useHTML = T)
a$legend(title = list(text = "Select bank offenses"))
a$params$width = 740
a$params$height = 500


a





## using the new highchart
# data$numDate <- as.numeric(data$date) * 1000
# data$name = sprintf("<table cellpadding='2' style='line-height:1.5'><tr><th>%1$s</th></tr><tr><td>%2$s</td>
# 	<td align='left'>Profitability: %3$s<br>Fine million USD: %4$s<br>%5$s</td></tr></table>",
#     as.character(data$bank),
# 	as.character(data$date),
#     data$reason,
#     data$valuemillions,
#     as.character(data$description))
#
#
#
#
# h1 <- hPlot(
#   numBank ~ numDate,
#   data = data,
#   type = "bubble",
#   size = "valuemillions",
#   group = "reason"
# )
# h1$plotOptions(
#   bubble = list(
#     cursor = "pointer",
#     minSize = 5,
#     maxSize = 30
# ))
#
# h1$yAxis(gridLineColor = "white", min = 0, labels = "enabled: false", title = "text: null")
# h1$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m}'  ))
# #h1$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")
# h1











banks <- levels(data.table$bank)
# http://stackoverflow.com/questions/3693189/programmatic-api-for-downloading-historical-financial-statements
statements <- sapply(banks, function(b) {
	cat("\n", b)
	getFinancials(b)
})

getFinancials(banks)


# split the data by country and return the ratio of job in finance intermediation for the latest year available

table <- do.call(rbind, by(data.table, data.table$COUNTRY, function(dd) {
	year <- max(dd$year)
	ddd <- dd[dd$year == year & dd$CLASSIFICATION == 'ISIC-Rev.3',]

	dddd <- cbind(ddd[1,c(1:2)], year = year, total = ddd[ddd$CODE.SUB.CLASSIFICATION == "00_",'value'], finance =
		ddd[ddd$CODE.SUB.CLASSIFICATION == "10_",'value'])

	dddd$ratio <- (dddd$finance / dddd$total) * 100
	dddd
}))
