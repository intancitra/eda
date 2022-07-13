setwd('E:/AED')

data=read.table('E:/AED/test.csv',sep=',',header=TRUE)

head(data)

data2 = within(data,{
  UNDER_CONSTRUCTION = factor(UNDER_CONSTRUCTION, labels = c("Yes", "No"))
  RERA = factor(RERA, labels = c("Approved", "Not Approved"))
  READY_TO_MOVE = factor(READY_TO_MOVE, labels = c("Yes", "No"))
  RESALE = factor(RESALE, labels = c("Yes", "No"))
  BHK_OR_RK = factor(BHK_OR_RK)
  POSTED_BY = factor(POSTED_BY)
})

data3=data2[1:1500,]

head(data3)
summary(data3)

boxplot(data3$BHK_NO., horizontal=TRUE, main = "Boxplot of House Price Dataset", xlab="Number of Rooms")
hist(data3$BHK_NO., main = "Histogram of House Price Dataset", xlab="Number of Rooms")
install.packages('lvplot')
library(lvplot)
lv2 = LVboxplot(data3$BHK_NO., k=11, xlab=expression(bold("Number of Rooms")),
                col=c('#D8F3DC','#B7E4C7','#95D5B2','#74C69D','#52B788','#40916C','#2D6A4F',
                      '#255640','#1B4332','#081C15','#080D0B'),
                bg='#eae2b7', median.col='#d90429', main = "LV Boxplot of House Price Dataset",
                col.main='#081C15', col.axis='#52B788', cex.lab=1.2)
lv2

abline(v = lv2$letter.val[, 'lower'], col = rainbow(11), lty = 5)
abline(v = lv2$letter.val[, 'upper'], col = rainbow(11), lty = 5)

#Ambil dari Source yang dikasih masnya
lsum <- function (x, l = 5, all = TRUE) 
{
  # Limit max letter summaries to 9
  if (l > 9) {
    print("Limit level summary to 9")
    return()
  }
  # letter summary labels
  let <- c("M", "H", "E", "D", "C", "B", "A", "Z", "Y", "X")
  # Remove missing values
  x <- na.omit(x)
  # Sort values
  x <- sort(x)
  # Find depths from each end
  n <- length(x)
  Lrnk <- vector()
  Mrnk <- vector()
  Rrnk <- vector()
  Lrnk[1] <- n
  Mrnk[1] <- n
  Rrnk[1] <- n
  i = 1
  while( (i <= l) & (Lrnk[i] > 1) ){
    i=i + 1
    Lrnk[i] <- floor(Lrnk[i-1] + 1 ) /2
    Mrnk[i] <- floor(Lrnk[i])
    Rrnk[i] <- floor(Lrnk[i] + 0.5)
  }
  # Get final set of letters
  val <- factor(let[1:length(Lrnk[-1])],levels=let[1:length(Lrnk[-1])])
  # Find the summary values
  LO <- (x[Mrnk[-1]] + x[Rrnk[-1]])  / 2
  HI <- (  x[n-Mrnk[-1] + 1] + x[n-Rrnk[-1]+1] ) / 2
  MD <- (LO + HI) / 2
  SP <- HI - LO
  # Generate output
  if(all == TRUE) {
    out <- data.frame(letter=val, depth=Lrnk[-1], lower=LO, 
                      mid=MD, upper=HI, spread=SP)
  } else {
    out <- data.frame(letter=val, mid=MD)
  }
  return(out)
} 

lsum(data3$BHK_NO.,l=9)