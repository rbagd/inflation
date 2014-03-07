library(shiny)
library(ggplot2); library(reshape2); library(scales); library(xts);
# library(gdata)
source('functions.R')

# imported.data <- read.xls("cpi.xls", na.strings=c(".", "(*)"), blank.lines.skip=TRUE, fileEncoding="latin1")
# b <- imported.data[1:(nrow(imported.data)-2),]

# Previous two lines import Excel file as it is furnished by SPF Economie but importing is much slower, so
# performance-wise it's better to convert to CSV. There are some small changes for adjustment.

imported.data <- read.csv("cpi.csv", na.strings=c(".", "(*)"), blank.lines.skip=TRUE,
                          fileEncoding='latin1')

top.levels <- which(imported.data$LVL != 3 & imported.data$LVL != 4)
data <- imported.data[top.levels,]

# Replace Dutch month abbreviations by English ones for better compatibility

dutch_abbr <- c("mrt.", "mei.", "okt."); eng_abbr <- c("mar.", "may.", "oct.")
for (i in 1:3) { colnames(data) <- gsub(dutch_abbr[i], eng_abbr[i], colnames(data)) }

time.data <- data[,9:ncol(data)]
time.data <- as.xts(t(time.data), order.by=as.Date(paste0("15.", colnames(time.data)), format='%d.%b.%y'))

data.supra <- time.data[,which(data$LVL == 0)]
data.top <- time.data[,which(data$LVL == 1)]
data.sub <- time.data[,which(data$LVL == 2)]

data.supra.unweighted <- lagged.dataset(data.supra)
data.top.unweighted <- lagged.dataset(data.top)
data.sub.unweighted <- lagged.dataset(data.sub)

top.weights <- data[which(data$LVL == 1), "Pond.2014"]

coicop <- data[which(data$LVL == 2), c("COICOP", "Pond.2014")]
coicop[,3:4] <- as.data.frame(t(simplify2array(strsplit(as.character(coicop$COICOP), ".", fixed=TRUE))))
names(coicop)[3:4] <- c("top", "sub")

parent <- with(coicop, rep(aggregate(Pond.2014, list(level=top), sum)$x,
                 aggregate(Pond.2014, list(level=top), length)$x))
rel.sub.weights <- coicop$Pond.2014/parent


data.supra.weighted <- data.supra.unweighted * data[which(data$LVL == 0),'Pond.2014']/1000
data.top.weighted <- data.top.unweighted * top.weights/1000
data.sub.weighted <- data.sub.unweighted * rel.sub.weights

data.supra.unweighted <- melt.dataset(data.supra.unweighted, imported.data)
data.supra.weighted <- melt.dataset(data.supra.weighted, imported.data)

data.top.unweighted <- melt.dataset(data.top.unweighted, imported.data)
data.top.weighted <- melt.dataset(data.top.weighted, imported.data)

data.sub.unweighted <- melt.dataset(data.sub.unweighted, imported.data)
data.sub.weighted <- melt.dataset(data.sub.weighted, imported.data)

poids.table <- data[which(data$LVL == 1), c('Dénomination', 'Pond.2014')];
rownames(poids.table) <- NULL; colnames(poids.table) <- c("Categorie", "Poids")
#poids.table$Categorie <- as.character(poids.table$Categorie)
#poids.table[(nrow(poids.table)-1):nrow(poids.table),1] <- c("Indice des prix", "Indice santé")

# This is small script to compute the date at which the pivot index was reached.

#indice.lisse <- rollmean(data[which(data$Dénomination == 'Indice santé'),], 4, align="right")
#indice.pivot <- gen.indice.pivot(indice.lisse, init=104.1399115502, start=c(2006,4))
#depassement.table <- indice.pivot < indice.lisse & indice.pivot > lag(indice.lisse, -1)
#dates.depassement <- as.Date(depassement.table)[depassement.table] + 14

# This is the reactive part. No more changes on above during the session.

shinyServer(function(input, output) {

  output$subcategories <- renderUI({ selectInput("subcategories",
                                                 "ou bien une sous-catégorie",
                                                 data[which(data$LVL==1),'Dénomination']) })
  
  output$view <- renderTable({ poids.table })
  
  output$testPlot <- renderPlot({
    
    if (input$weighted == TRUE)
    {
      if (input$category.choice == TRUE) { data.plot <- data.top.weighted }
      else
      {
        tmp <- as.character(data[which(data$'Dénomination' == input$subcategories),'COICOP'])
        sub.categories <- as.character(data[which(substr(as.character(data$'COICOP'), 1, nchar(tmp)) == tmp ),
                                            'Dénomination'])
        data.plot <- data.sub.weighted[which(data.sub.weighted$Produit %in% sub.categories),]
      }
    }
    else
    {
      if (input$category.choice == TRUE) { data.plot <- data.top.unweighted }
      else
      {
        tmp <- as.character(data[which(data$'Dénomination' == input$subcategories),'COICOP'])
        sub.categories <- as.character(data[which(substr(as.character(data$'COICOP'), 1, nchar(tmp)) == tmp ),
                                            'Dénomination'])
        data.plot <- data.sub.weighted[which(data.sub.weighted$Produit %in% sub.categories),]
      }
    }
      
    p <- ggplot(data = data.plot, aes(x=date, y=value)) + labs(x="", y = "Pourcentage", title="") + 
      theme(legend.position="right", axis.text.x = element_text(angle = 30, hjust = 1)) +
      scale_fill_discrete(name = "Gamme de produits") +
      scale_color_manual(name = "Indices", values=c("black","red")) +
      scale_x_date(labels = date_format("%Y-%m"), breaks="1 month")
    
    window.start <- c(input$window.start.year,input$window.start.month)
    window.start.char <- paste(window.start[1], window.start[2], sep="-")
    
    window.end <- c(input$window.end.year, input$window.end.month)
    window.end.char <- paste(window.end[1], window.end[2], sep="-")
    
    start.date.plot <- which(as.Date(paste0(window.start.char, "-15")) <= data.plot$date, arr.ind=TRUE)
    end.date.plot <- which(as.Date(paste0(window.end.char, "-15")) >= data.plot$date, arr.ind=TRUE)
    span <- intersect(start.date.plot, end.date.plot)
    
    data.p <- subset(data.plot[span,], data.plot[span,]$value >= 0) # & indices == "Sous-indice")
    data.n <- subset(data.plot[span,], data.plot[span,]$value < 0) # & indices == "Sous-indice")
    ipc <- subset(data.plot[span,], data.plot[span,]$Produit == "IPC-2004")
    sante <- subset(data.plot[span,], data.plot[span,]$Produit == "Indice santé-2004")
    
    if(length(sante$date[sante$Lag == 0]) > 13) { p <- p + scale_x_date(labels = date_format("%Y-%m"), breaks="3 months")}
    
    if (input$lags.choice == "Annuelle") {lags.choice <- 12}
    if (input$lags.choice == "Mensuelle") {lags.choice <- 1}
    if (input$lags.choice == "Indices") {lags.choice <- 0}
    
    if (dim(data.p[data.p$Lag == lags.choice,])[1] > 0)
    {
      if (input$lags.choice != "Indices")
      { p <- p + geom_bar(data = data.p[data.p$Lag == lags.choice,], aes(fill=Produit), stat="identity") }
      else
      { p <- p + geom_bar(data = data.p[data.p$Lag == lags.choice,], aes(fill=Produit), stat="identity", position="dodge") + labs(y = "Valeur de l'indice") }  
    }
    
    if (dim(data.n[data.n$Lag == lags.choice,])[1] > 0)
    {p <- p + geom_bar(data = data.n[data.n$Lag == lags.choice,], aes(fill=Produit), stat="identity") }
    
    if (input$sante == TRUE)
    {
      p <- p + geom_line(data = sante[sante$Lag == lags.choice,], aes(group=indices, color="Indice santé"), size=1.1)
    }
    if (input$ipc == TRUE)
    {
      p <- p + geom_line(data = ipc[ipc$Lag == lags.choice,], aes(group=indices, color="Indice des prix"), size=1.1)
    }
    if (input$pivot == TRUE)
    {
      vis.depassement <- dates.depassement[dates.depassement %in% data.plot[span,]$date]
      if (length(vis.depassement) > 0)
      { p <- p + geom_vline(xintercept=as.numeric(vis.depassement), linetype=2, alpha=0.4) }
    }
    
    print(p) })
})

