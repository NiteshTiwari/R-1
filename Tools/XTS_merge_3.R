# Function to merge xts objects in R
# Michael Kilchenmann

XTS_merge_3 <- function(x,y,z){
  mat <- merge(sc.data, XAU, SPX)
  mat <- data.frame(value=coredata(mat),timestamp=index(mat))
  mat <- na.omit(mat)
  mat <- xts(mat[,-4],order.by=mat[,4])
}