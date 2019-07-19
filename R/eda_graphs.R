#' @title Insights on the variables through graphs for analysis.
#'
#' @description This packages find numeric and categorical variables. Then plots the distribution of values/ levels. Boxplots, Histograms, Pie Charts, Density plots are included.
#'
#' @param (dataframe, columns, filepath)
#'
#' @return
#'
#' @examples eda_graphs(cars,c(1:5),filepath="../folder")
#'
#' @export eda_graphs


eda_graphs <- function(data,variables=c(1:ncol(data)),filepath=getwd())
{
  setwd(filepath) #

  if (!is.data.frame(data))
    stop("The given object is not a dataframe")


  for (i in variables)
  {
    if(!is.numeric(data[, i]) | (length(unique(data[,i]))/nrow(data)*100 < 20))
    {
      counts <- table(data[i])
      slices <- c(counts)
      lbls <- c(counts)

      print(paste(names(data)[i],'is categorical & graphs exported'))
      png(paste(names(data)[i], ".png", sep=""))
      par(mfrow = c(1,2))

      barplot(counts, main = paste("Barplot of", names(data[i])), col = rainbow(length(slices)))
      pie(slices, labels = lbls, main = paste("Pie chart of", names(data[i])), col = rainbow(length(slices)))
      dev.off()
    }
    else if(is.numeric(data[,i])) #for numeric variables.
    {
      print(paste(names(data)[i],'is numerical & graphs exported'))
      png(paste(names(data)[i],".png",sep=""))
      par(mfrow=c(4,1))
      boxplot(data[,i], main = paste("Boxplot of",names(data)[i]),col="green",
              border = "Black", horizontal = T)
      hist(data[,i], main = paste("Histogram of",names(data)[i]),
           xlab=names(data)[i],ylab="No. of Houses", col="Skyblue2", border = "Black")

      #Density Functions
      plot(ecdf(data[,i]),main=paste("CDF of",names(data)[i]))
      plot(density(data[,i]),main=paste("PDF of",names(data)[i]))

      dev.off()
    }
  }
}
