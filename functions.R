select.dataset <- function(all.data, categories, selection = "top")
{
  if (selection == "top")
  {
    top.test.0 <- categories[,c(2:5)] != "0"; top.test.00 <- categories[,c(2:5)] != "00";
    category.index <- which(rowSums(top.test.0 + top.test.00) == 4)
  }
  if (selection == "sub")
  {
    sub.test.0 <- categories[,c(2:5)] != "0"; sub.test.00 <- categories[,c(2:5)] != "00";
    category.index <- which(rowSums(sub.test.0 + sub.test.00) == 5)
  }
  data <- all.data[category.index,7:ncol(all.data)]; colnames(data) <- gsub("X", "", colnames(data))
  dates <- as.Date(paste("15.", colnames(data), sep=""), format="%d.%m.%Y"); start.date <- c(); end.date <- c();
  start.date[1] <- as.numeric(format(dates[1], format="%Y"));
  start.date[2] <- as.numeric(format(dates[1], format="%m"));
  end.date[1] <- as.numeric(format(dates[length(dates)], format="%Y"));
  end.date[2] <- as.numeric(format(dates[length(dates)], format="%m"));
  indices <- all.data[c(805,806), 7:ncol(all.data)]; colnames(indices) <- colnames(data)
  data <- rbind(data, indices);
  
  data <- ts(t(data), start=start.date, end=end.date, frequency=12)
  categories <- categories[c(category.index,805,806),]
  
  return(list("data"=data, "categories"=categories))
} 

lagged.dataset <- function(x, lags=c(1,12))
{
  require(xts)
  x <- as.xts(x)
  initial <- x
  column.names <- paste0("Lag0.", colnames(x))
  for (i in 1:length(lags))
  {
    x <- cbind(x, diff(initial, lag=lags[i])/lag(initial,lags[i])*100)
    column.names <- c(column.names, paste0(paste0("Lag",paste0(eval(parse(text=lags[i]))),"."), colnames(initial)))
  }
  colnames(x) <- gsub("X", "", column.names)
  return(x)
}

weighted.dataset <- function(x, categories, selection="top")
{
  rows <- nrow(x); cols <- ncol(x); elements <- length(categories[,10])
  if (selection == "top")
  {
    weights <- matrix(rep(matrix(rep(t(categories[,10]), rows), byrow=TRUE, nrow=rows), cols/elements), ncol=cols, byrow=FALSE)/1000
  }
  else
  {
    top.category.weights <- as.vector(by(categories$pond2010, categories$top, sum))
    sub.count <- as.vector(table(categories$top))
    replicate.top <- c();
    for (i in 1:length(top.category.weights)) { replicate.top <- c(replicate.top,rep(top.category.weights[i], sub.count[i])) }  
    weights.nom <- matrix(rep(t(categories$pond2010), rows), nrow=rows, byrow=TRUE)
    weights.denom <- matrix(rep(t(replicate.top), rows), nrow=rows, byrow=TRUE)
    weights <- matrix(rep(weights.nom/weights.denom, ncol(x)/ncol(weights.nom)), nrow=rows, byrow=FALSE)
  }
  weighted.data <- x * weights
  return(weighted.data)
}

melt.dataset <- function(x, imported.data)
{
  
  require(reshape)
  data.melt <- melt(cbind(data.frame(x), date=(as.Date(time(x), format="%Y-%m-%d"))), id.vars="date")
  lag.cat <- t(simplify2array(strsplit(as.character(data.melt$variable), ".", fixed=TRUE)))
  lag.cat[,1] <- gsub("Lag", "", lag.cat[,1]); lag.cat <- as.data.frame(lag.cat)
  
  data.melt$Lag <- lag.cat[,1]
  
  lag.cat[,2] <- as.numeric(as.character(lag.cat[,2]))
  data.melt$Produit <- rep(imported.data[unique(lag.cat[,2]), 'Dénomination'],
                           aggregate(lag.cat[,2], list(lag.cat[,2]), length)$x)
  data.melt$variable <- NULL
  
  #data.melt$indices <- rep("Sous-indice", ncol(x))
  #data.melt$indices[data.melt$Produit %in% c("IPC-2004", "Indice santé-2004")] <- "Indice"
  
  return(data.melt)
}

gen.indice.pivot <- function(indice.lisse, init=104.1399115502, start=c(2006,4))
{
  indice.pivot <- rep(NA, length(indice.lisse)); indice.pivot[1] <- init;
  for (i in 1:(length(indice.lisse)-1))
  {
    ifelse(indice.pivot[i] > indice.lisse[i], indice.pivot[i+1] <- indice.pivot[i], 
           indice.pivot[i+2] <- indice.pivot[i]*1.02)
  }
  
  indice.pivot[which(is.na(indice.pivot), arr.ind=TRUE)] <- indice.pivot[which(is.na(indice.pivot), arr.ind=TRUE) - 1]
  return(ts(indice.pivot, freq=12, start=start))
}