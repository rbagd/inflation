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

melt.dataset <- function(x, imported.data)
{
  
  require(reshape)
  data.melt <- melt(cbind(data.frame(x), date=(as.Date(time(x), format="%Y-%m-%d"))), id.vars="date")
  lag.cat <- t(simplify2array(strsplit(as.character(data.melt$variable), ".", fixed=TRUE)))
  lag.cat[,1] <- gsub("Lag", "", lag.cat[,1]); lag.cat <- as.data.frame(lag.cat)
  
  data.melt$Lag <- lag.cat[,1]
  
  lag.cat[,2] <- as.numeric(as.character(lag.cat[,2]))
  n_lags <- length(unique(lag.cat[,1]))
  
  data.melt$Produit <- rep((rep(as.character(imported.data[unique(lag.cat[,2]), 'Dénomination']), 
                                (aggregate(lag.cat[,2], list(lag.cat[,2]), length)$x)/n_lags)), n_lags)
  
  #data.melt$Produit <- rep(as.character(imported.data[unique(lag.cat[,2]), 'Dénomination']), 
  #                         aggregate(lag.cat[,2], list(lag.cat[,2]), length)$x)
  data.melt$Produit <- as.character(data.melt$Produit)
  data.melt$variable <- NULL
  #data.melt$indices <- rep("Sous-indice", ncol(x))
  #data.melt$indices[data.melt$Produit %in% c("IPC-2004", "Indice santé-2004")] <- "Indice"
  
  return(data.melt)
}

gen.indice.pivot <- function(indice.lisse, init=86.2196, start=c(2006,4))
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
