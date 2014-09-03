leven <- function(x, y)
{
      firstRow <- c(0:length(y))
      for (i in 0:(length(x)-1))
      {
          nextRow <- c(i+1)
          for (j in 0:(length(y)-1))
              nextRow[j+2] = min(nextRow[j+1] + 1, firstRow[j+2] + 1, firstRow[j+1] + ifelse(x[i+1] == y[j+1], 0, 1))
          firstRow <- nextRow
      }
      return (firstRow[length(y)+1])
}

lsi <- function(x, y)
{
    return (1-leven(x, y)/max(length(x), length(y)))
}

loadVectors <- function(filename)
{    
    fc <- file(filename)
    mylist <- strsplit(readLines(fc), ", ")
    close(fc)
    return (mylist)
}

similarityMatrix <- function(vectors)
{
    m = matrix(NA, length(vectors), length(vectors))
    for (i in 1:length(vectors))
    {
        for (j in 1:length(vectors))
        {
            m[i,j] <- lsi(vectors[[i]], vectors[[j]]);
        }                 
    }
    return (m);
}
