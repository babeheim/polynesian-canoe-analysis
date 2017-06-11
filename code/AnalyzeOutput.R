library(arm)
model.summary = function(model){
	m = get(model)
	m$summary
	parameters = unique( names( m$sims.list ) )
	
	par( mfcol = c( length(parameters), 1 ) )
	sapply( parameters, function(z) plot( 1:length(m$sims.list[[z]]), m$sims.list[[z]], type = "l", main = z ) )
	 
	 z = "deviance"
	 plot( 1:length(m$sims.list[[z]]), m$sims.list[[z]], type = "l", main = z ) 

	}