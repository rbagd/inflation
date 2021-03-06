library(shiny)
library(xts)
library(lattice)
library(latticeExtra)
library(reshape)
library(plyr)

rm(list=ls())
source('functions.R')


# imported.data <- read.xls("cpi.xls", na.strings=c(".", "(*)"), blank.lines.skip=TRUE, fileEncoding="latin1")
# b <- imported.data[1:(nrow(imported.data)-2),]

# Previous two lines import Excel file as it is furnished by SPF Economie but importing is much slower, so
# performance-wise it's better to convert to CSV. There are some small changes for adjustment.

imported.data <- read.csv("cpi.csv", na.strings=c(".", "(*)","#N/A"), blank.lines.skip=TRUE,
                          fileEncoding='utf8', stringsAsFactors=FALSE)

top.levels <- which(imported.data$LVL != 3 & imported.data$LVL != 4)
data <- imported.data[top.levels,]

# Replace Dutch month abbreviations by English ones for better compatibility

dutch_abbr <- c("mrt.", "mei.", "okt."); eng_abbr <- c("mar.", "may.", "oct.")
for (i in 1:3) { colnames(data) <- gsub(dutch_abbr[i], eng_abbr[i], colnames(data)) }

time.data <- data[,10:ncol(data)]
time.data <- as.xts(t(time.data), order.by=as.Date(paste0("15.", colnames(time.data)), format='%d.%b.%y'))

data.supra <- time.data[,which(data$LVL == 0)]
data.top <- time.data[,which(data$LVL == 1)]
data.sub <- time.data[,which(data$LVL == 2)]

data.supra.unweighted <- lagged.dataset(data.supra)
data.top.unweighted <- lagged.dataset(data.top)
data.sub.unweighted <- lagged.dataset(data.sub)

top.weights <- data[which(data$LVL == 1), "Pond.2015"]

coicop <- data[which(data$LVL == 2), c("COICOP", "Pond.2015")]
coicop[,3:4] <- as.data.frame(t(simplify2array(strsplit(as.character(coicop$COICOP), ".", fixed=TRUE))))
names(coicop)[3:4] <- c("top", "sub")

parent <- with(coicop, rep(aggregate(Pond.2015, list(level=top), sum)$x,
                 aggregate(Pond.2015, list(level=top), length)$x))
rel.sub.weights <- coicop$Pond.2015/parent

data.supra.weighted <- as.xts(t(apply(data.supra.unweighted, 1, "*", data[which(data$LVL == 0),'Pond.2015']/1000)))
data.top.weighted <- as.xts(t(apply(data.top.unweighted, 1, "*", top.weights/1000)))
data.sub.weighted <- as.xts(t(apply(data.sub.unweighted, 1, "*", rel.sub.weights)))
data.supra.weighted <- melt.dataset(data.supra.weighted, imported.data)
data.supra.unweighted <- melt.dataset(data.supra.unweighted, imported.data)

data.top.unweighted <- melt.dataset(data.top.unweighted, imported.data)
data.top.weighted <- melt.dataset(data.top.weighted, imported.data)

data.sub.unweighted <- melt.dataset(data.sub.unweighted, imported.data)
data.sub.weighted <- melt.dataset(data.sub.weighted, imported.data)

poids.table <- data[which(data$LVL == 1), c('Dénomination', 'Pond.2015')];
rownames(poids.table) <- NULL; colnames(poids.table) <- c("Categorie", "Poids")

# This is small script to compute the date at which the pivot index was reached.

indice.lisse <- ts(rollmean(time.data[,2], 4, align="right"), freq=12, start=c(2006,4))
indice.pivot <- gen.indice.pivot(indice.lisse, init=86.2196, start=c(2006,4))
depassement.table <- indice.pivot < indice.lisse & indice.pivot > lag(indice.lisse, -1)
dates.depassement <- as.Date(depassement.table)[depassement.table] + 14

# This is the reactive part. No more changes on above during the session.

shinyServer(function(input, output) {

  output$subcategories <- renderUI({ selectInput("subcategories",
                                                 "ou bien une sous-catégorie",
                                                 data[which(data$LVL==1),'Dénomination']) })
  
  output$view <- renderTable({ poids.table })
  
  output$testPlot <- renderPlot({
    
    if (input$lags.choice == "Annuelle") {lags.choice <- 12}
    if (input$lags.choice == "Mensuelle") {lags.choice <- 1}
    #    if (input$lags.choice == "Indices") {lags.choice <- 0}
    
    window.start <- c(input$window.start.year,input$window.start.month)
    window.start.char <- paste0(paste(window.start[1], window.start[2], sep="-"), "-15")
    
    window.end <- c(input$window.end.year, input$window.end.month)
    window.end.char <- paste0(paste(window.end[1], window.end[2], sep="-"), "-15")
    
    if(as.Date(window.start.char) > as.Date(window.end.char)) { stop("La date du début ne peut pas être après la date de la fin.") }
    
#    dates <- unique(data.top.weighted$date[data.top.weighted$date >= window.start.char & data.top.weighted$date <= window.end.char])
 #   x_labels <- gsub("-14", "", dates)
  #  x_labels <- gsub("-15", "", dates)
    
 #   if (length(x_labels) > 13) { x_labels[-seq(1,length(x_labels), floor(length(x_labels)/12))] <- "" }
    
    if (input$data.type == "Catégories générales") { data.plot <- subset(data.supra.weighted, !(Produit %in% c("Indice santé", "Indice des prix à la consommation"))) }
    else
    {
      if (input$weighted == TRUE)
      {
        if (input$data.type == "Toutes les catégories") { data.plot <- data.top.weighted }
        else if (input$data.type == "Sous-catégorie")
        {
          tmp <- as.character(data[which(data$'Dénomination' == input$subcategories),'COICOP'])
          sub.categories <- as.character(data[which(substr(as.character(data$'COICOP'), 1, nchar(tmp)) == tmp ),
                                              'Dénomination'])[-1]
          data.plot <- data.sub.weighted[which(data.sub.weighted$Produit %in% sub.categories),]
        }
      }
      else
      {
        if (input$data.type == "Toutes les catégories") { data.plot <- data.top.unweighted }
        else if (input$data.type == "Sous-catégorie")
        {
        tmp <- as.character(data[which(data$'Dénomination' == input$subcategories),'COICOP'])
        sub.categories <- as.character(data[which(substr(as.character(data$'COICOP'), 1, nchar(tmp)) == tmp ),
                                            'Dénomination'])
        data.plot <- data.sub.unweighted[which(data.sub.unweighted$Produit %in% sub.categories),]
        }
      }
    }
    
    data.plot <- subset(data.plot, date >= window.start.char & date <= window.end.char & Lag == lags.choice)
    x_labels <- gsub("-14", "", unique(as.character(data.plot$date))); x_labels <- gsub("-15", "", x_labels)
    if (length(x_labels) > 13) { x_labels[-seq(1,length(x_labels), floor(length(x_labels)/12))] <- "" }

     coloring <- hsv(seq(0,0.9,length.out=length(unique(data.plot$Produit))),0.6,0.6)
     foo <- barchart(value ~ date, stack=TRUE, data=data.plot, groups=Produit, horiz=FALSE,
                      par.settings = list(superpose.polygon = list(col=coloring)),
                      auto.key=list(space="right", rectangles=TRUE, points=FALSE),
                      scales=list(abbreviate=FALSE, tick.number=10, x=list(labels=x_labels, rot=30)),
                      ylab="Value (in %)", xlab="Month",
                      panel = function(y, x, ...){
                        panel.grid(h = -10, v = -length(x_labels), col = "gray", lty=2, lwd=1)
                        panel.barchart(x, y, ...)
                      })
    
    # The rest of the code plots the optional lines if those are selected

    data.supra.plot <- subset(data.supra.weighted, date >= window.start.char & date <= window.end.char & Lag == lags.choice)
    if (input$sante == TRUE)
    {
      sub.plot.sante <- subset(data.supra.plot, Produit == "Indice santé")
      foo <- foo + layer(panel.lines(y=value, x=1:length(value), col="darkred", lwd=2), data=sub.plot.sante)     
    }
    if (input$ipc == TRUE)
    {
      sub.plot.ipc <- subset(data.supra.plot, Produit == "Indice des prix à la consommation")
      foo <- foo + layer(panel.lines(y=value, x=1:length(value), col="steelblue", lwd=2,
                                     auto.key=TRUE), data=sub.plot.ipc)
    }
#     if (input$pivot == TRUE)
#     {
#       plot.depassement <- which((data.plot$date+1) %in% dates.depassement)
#       print(plot.depassement)
#       data.pivot <- cbind(date=data.plot[plot.depassement, "date"], value=rep(1, length(plot.depassement)))
#       print(data.pivot)
#       foo <- foo + layer(panel.lines(y=value, x=1:length(value)), data=data.pivot)
#     }
    if (input$alimentaires == TRUE)
    {
      sub.plot.alim <- subset(data.supra.plot, Produit == "Alimentaires")
      foo <- foo + layer(panel.lines(y=value, x=1:length(value), col="steelblue", lwd=2), data=sub.plot.alim)
    }
    if (input$nonalimentaires == TRUE)
    {
      sub.plot.nonalim <- subset(data.supra.plot, Produit == "Non-alimentaires")
      foo <- foo + layer(panel.lines(y=value, x=1:length(value), col="steelblue", lwd=2), data=sub.plot.nonalim)
    }
    if (input$services == TRUE)
    {
      sub.plot.services <- subset(data.supra.plot, Produit == "Services")
      foo <- foo + layer(panel.lines(y=value, x=1:length(value), col="steelblue", lwd=2), data=sub.plot.services)
    }
    if (input$loyers == TRUE)
    {
      sub.plot.loyers <- subset(data.supra.plot, Produit == "Loyers")
      foo <- foo + layer(panel.lines(y=value, x=1:length(value), col="steelblue", lwd=2), data=sub.plot.loyers)
    }

    print(foo) })
})

