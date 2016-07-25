##Creates a function to get the total distance per well for every
##well in a row
distsum <- function(a)
{
  x <- a
  x.ds <- aggregate(x[, "distsum"],by = list(x$PTZ, x$per, x$wellnum, x$Group),
                    sum)
names(x.ds) <- c("PTZ", "Period", "Well Number", "Group", "Total Distance")
x.ds <- x.ds[ order(x.ds$Period, -xtfrm(x.ds$PTZ), x.ds$`Well Number`), ]
}

# Function to get standard errors
se <- function(x) sqrt(var(x)/length(x))

##Function to get Average distance per group
distave <- function(a)
{
  x <- a
  x.dm<- aggregate(x[, "Total Distance"],by = list(x$PTZ, x$Period, x$Group),
                   mean)
  names(x.dm) <- c("PTZ", "Period", "Group", "Average Distance")
  x.dm <- x.dm[ order(x.dm$Period, -xtfrm(x.dm$PTZ)), ]
  x.se <- aggregate(x[, "Total Distance"],by = list(x$PTZ, x$Period, x$Group),
                    se)
  names(x.se) <- c("PTZ", "Period", "Group", "Standard Error")
  x.se <- x.se[ order(x.se$Period, -xtfrm(x.se$PTZ)), ]
  x.dm$'Standard Error' <- x.se[, "Standard Error"]
  x.dm$`Average Distance` <- round(as.numeric(x.dm$`Average Distance`),
                                        digits=4)
  x.dm$`Standard Error` <- round(as.numeric(x.dm$`Standard Error`),
                                      digits=4)
  return(x.dm)
}

