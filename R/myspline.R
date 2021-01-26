myspline <- function( inputtime, inputdata, cop ) {

# This function splines a time series 
# 20.7.2000 JF

# Weight is included in the error delta_y (see Enting 1983 JGR for details)

# Data must be in a text file with 3 columns:   x   y   delta_y
# No header in the file!

# num_p: number of data points
# x_d: x-values (time) of data
# y_d: y-values of data
# e_d: error of data
# w_d: weight of data
# x_gr: grid of x-values where spline is calculated
# data_sp: y-values of the spline at x-values x_gr

# ==========================================================================
# Evaluate Length of Matrix
num_p <- length(inputtime)

if ( length(inputdata) != length(inputtime) ){
  print(paste("WARNING: length of inputdata and inputtime are not of equal length"))
  print(paste("length inputtime: ", length(inputtime)))
  print(paste("length inputdata: ", length(inputdata)))
}

# Create 3-column-matrix for adding errors by choice in its 3rd column
helpmatrix <- matrix( 0, nrow=num_p, ncol=3)
helpmatrix[,1] <- inputtime
helpmatrix[,2] <- inputdata
helpmatrix[,3] <- sd(inputdata[num_p-cop:num_p],na.rm=TRUE)

# Evaluate number of data points
#[num_p,dummy] <- size(inputmatrix)

# Put data from matrix in corresponding vectors
x_d <- helpmatrix[,1]
y_d <- helpmatrix[,2]
e_d <- helpmatrix[,3]

# Assign weights: Weight=1/(error^2)
#w_d=rdivide(1,e_d.^2)
w_d <- unlist( lapply( e_d, function(x) 1/x^2 ) )
#w_d=w_d/sum(w_d)*num_p
w_d <- w_d / sum( w_d ) * num_p

# Weight=1 for all datapoints
# w_d=ones(1,num_p)

# Mean data spacing
#dx=abs(x_d(1)-x_d(length(x_d)))/num_p
dx <- abs( x_d[1] - x_d[length(x_d)] ) / num_p

# Calculate lambda
lambda <- ( cop / (2*pi) )^4/dx

# Calculate smoothing factor p
#p <- 1 / (1+lambda)*100
p <- 0.8
#print(p)

# Spline at datapoints
#outputdata=csaps(x_d,y_d,p,x_d,w_d)
out <- smooth.spline( x_d, y_d, w=w_d, spar=p )

return(out$y)

# End of program
  
}
