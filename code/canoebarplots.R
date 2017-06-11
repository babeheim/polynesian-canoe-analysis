d = read.csv( file = "bardataNov2010.csv", header = T )
#d = read.csv( file = "bardataJan.csv", header = T )

stack = function(x){
model.classes = c( "N", "C", "E", "CE" )
res = sapply( model.classes, function(z) sum( x[ x[,"modelclass"]==z, "w" ] ) )
res
}

all.bar = stack( d[ d[,"trait"]=="All",] )
decor.bar = stack( d[ d[,"trait"]=="Decorative",] )
double.bar = stack(d[ d[,"trait"]=="Double canoe",])
outrigger.bar = stack(d[ d[,"trait"]=="Outrigger",])
hull.bar = stack(d[ d[,"trait"]=="Hull",])
paddle.bar = stack(d[ d[,"trait"]=="Paddle",])
sail.rig.bar = stack(d[ d[,"trait"]=="Sail and Rigging",])

bars = matrix( c(sail.rig.bar,paddle.bar,outrigger.bar,hull.bar,double.bar,decor.bar,all.bar), byrow=F, nrow=4)
par( family="Times", mar=c(0,9,0,0.1), las=1, adj=0.5)
names.traits = c("Sail & Rigging", "Paddle", "Outrigger", "Hull", "Double canoe", "Decorative", "All traits")
barplot( bars, space=0.5, width = 0.5, horiz = TRUE, axes=FALSE, xlim=c(0,1), axisnames=TRUE, border=TRUE, col = c("white","gray80","gray50","gray10"), xpd = F, names.arg = names.traits, cex.names=1.5)

numlabel = function( x ){ 
	x1 = cumsum(x)
	y = rep(NA, length(x1) )
	y[1] = x1[1]/2 
	for( i in 2:length(x) ) y[i] = x1[i-1] + x[i]/2
	y
		}
numsize=1		
format2 = function( x, digits=0 ) round(x*100, digits=digits)

text( x=numlabel(decor.bar), y=4.25 , labels=c("Null", "Inheritance", "Ecology", "Inheritance & Ecology" ), col=c("black", "black", "white", "white"), cex=1.2 )

text( x=all.bar[4]/2, y=5, labels=format2( all.bar[4] ), col="white", cex=numsize )
text( x=numlabel(decor.bar), y=4.1 , labels=format2( decor.bar ), col=c("black", "black", "white", "white"), cex=numsize )
text( x=numlabel(double.bar), y=3.5 , labels=format2( double.bar ), col=c("black", "black", "white", "white"), cex=numsize )
text( x=numlabel(hull.bar), y=2.75 , labels=format2( hull.bar ), col=c("black", "black", "white", "white"), cex=numsize )
text( x=numlabel(outrigger.bar), y=2 , labels=format2( outrigger.bar ), col=c("black", "black", "white", "white"), cex=numsize )
text( x=numlabel(paddle.bar), y=1.25 , labels=format2( paddle.bar ), col=c("black", "black", "white", "white"), cex=numsize )
text( x=numlabel(sail.rig.bar), y=0.5 , labels=format2( sail.rig.bar ), col=c("black", "black", "white", "white"), cex=numsize )


