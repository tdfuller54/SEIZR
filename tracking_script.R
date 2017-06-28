##Creates a function to get the total distance per well for every
##well in a row
distsum <- function(a)
{
  x <- a
  x.ds <- aggregate(x[, "distsum"],by = list(x$PTZ, x$wellnum, x$Group), sum)
names(x.ds) <- c("PTZ", "Well Number", "Group", "Total Distance")
x.ds <- x.ds[ order( -xtfrm(x.ds$PTZ), x.ds$`Well Number`), ]
}

# Function to get standard errors
se <- function(x) sqrt(var(x, na.rm = TRUE)/sum(!is.nan(x)))

##Function to get Average distance per group
distave <- function(a)
{
  x <- a
  x.dm<- aggregate(x[, "Total Distance"],by = list(x$PTZ, x$Group), mean, na.action = na.pass,
                   na.rm = TRUE)
  names(x.dm) <- c("PTZ", "Group", "Average Distance")
  x.dm <- x.dm[ order( -xtfrm(x.dm$PTZ)), ]
  x.se <- aggregate(x[, "Total Distance"],by = list(x$PTZ, x$Group), se)
  names(x.se) <- c("PTZ", "Group", "Standard Error")
  x.se <- x.se[ order( -xtfrm(x.se$PTZ)), ]
  x.dm$'Standard Error' <- x.se[, "Standard Error"]
  x.dm$`Average Distance` <- round(as.numeric(x.dm$`Average Distance`), digits=4)
  x.dm$`Standard Error` <- round(as.numeric(x.dm$`Standard Error`), digits=4)
  return(x.dm)
}

