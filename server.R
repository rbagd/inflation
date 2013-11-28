library(shiny)
library(ggplot2); library(reshape2); library(scales); library(xts);
# library(gdata)
source('functions.R')

# imported.data <- read.xls("cpi.xls", na.strings=c(".", "(*)"), blank.lines.skip=TRUE, fileEncoding="latin1")
# b <- imported.data[1:(nrow(imported.data)-2),]

# Previous two lines import Excel file as it is furnished by SPF Economie but importing is much slower, so
# performance-wise it's better to convert to CSV. There are some small changes for adjustment.

imported.data <- read.csv("cpi.csv", na.strings=c(".", "(*)"), blank.lines.skip=TRUE)
b <- imported.data[1:(nrow(imported.data)-3),]

# The code separates numerical and descriptional data which is only recombined back at the end with the long
# format dataframe.

code <- as.character(b[,1])
full_code <- as.data.frame(t(simplify2array(strsplit(code, ".", fixed=TRUE))))
full_code <- cbind(full_code, b[,2:6])
colnames(full_code) <- c("top", "sub1", "sub2", "sub3", "sub4", "naam", "nom", "pond2004", "pond2008", "pond2010")
full_code[,8:10] <- apply(full_code[,8:10], 2, as.numeric)

data.top <- select.dataset(b, full_code, "top")$data
categories.top <- select.dataset(b, full_code, "top")$categories

data.sub <- select.dataset(b, full_code, "sub")$data
categories.sub <- select.dataset(b, full_code, "sub")$categories

data.top.unweighted <- lagged.dataset(data.top)
data.top.weighted <- weighted.dataset(data.top.unweighted, categories.top)
data.top.unweighted <- melt.dataset(data.top.unweighted, categories.top)
data.top.weighted <- melt.dataset(data.top.weighted, categories.top)

data.sub.unweighted <- lagged.dataset(data.sub)
data.sub.weighted <- weighted.dataset(data.sub.unweighted, categories.sub, selection="sub")
data.sub.unweighted <- melt.dataset(data.sub.unweighted, categories.sub)
data.sub.weighted <- melt.dataset(data.sub.weighted, categories.sub)

poids.table <- categories.top[,c(7,10)]; rownames(poids.table) <- NULL; colnames(poids.table) <- c("Categorie", "Poids")
poids.table$Catégorie <- as.character(poids.table$Categorie)
poids.table[(nrow(poids.table)-1):nrow(poids.table),1] <- c("Indice des prix", "Indice santé")

# This is small script to compute the date at which the pivot index was reached.

indice.lisse <- rollmean(data.top[,"806"], 4, align="right")
indice.pivot <- gen.indice.pivot(indice.lisse, init=104.1399115502, start=c(2006,4))
depassement.table <- indice.pivot < indice.lisse & indice.pivot > lag(indice.lisse, -1)
dates.depassement <- as.Date(depassement.table)[depassement.table] + 14

# This is the reactive part. No more changes on above during the session.

shinyServer(function(input, output) {

  output$subcategories <- renderUI({ selectInput("subcategories", "ou bien une sous-catégorie", categories.top[1:(nrow(categories.top)-2),7]) })
  
  output$view <- renderTable({ poids.table })
  
  output$testPlot <- renderPlot({
    
    if (input$weighted == TRUE)
    {
      if (input$category.choice == TRUE) { data.plot <- data.top.weighted }
      else
      {
        sub.level <- as.numeric(categories.top[categories.top$nom == input$subcategories,][1])
        sub.category <- categories.sub[as.numeric(categories.sub$top) == sub.level, ]
        data.plot <- data.sub.weighted[data.sub.weighted$Produit %in% c(as.character(sub.category[,7]), "IPC-2004", "Indice santé-2004"), ]
      }
    }
    else
    {
      if (input$category.choice == TRUE) { data.plot <- data.top.unweighted }
      else
      {
        sub.level <- as.numeric(categories.top[categories.top$nom == input$subcategories,][1])
        sub.category <- categories.sub[as.numeric(categories.sub$top) == sub.level, ]
        data.plot <- data.sub.unweighted[data.sub.unweighted$Produit %in% c(as.character(sub.category[,7]), "IPC-2004", "Indice santé-2004"), ]
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
    
    data.p <- subset(data.plot[span,], data.plot[span,]$value >= 0 & indices == "Sous-indice")
    data.n <- subset(data.plot[span,], data.plot[span,]$value < 0 & indices == "Sous-indice")
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

