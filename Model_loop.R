# Loop over all purposes
for (p in purposes){
  
  # We build a negative binomial model
  
  for(v in variables){
    
      # Check if any classifications of a variable have an insignificant p-value
      if( p-value > 0.05){
        
        # Then aggregate with another classification within the same variable
        
        # Try the regression model again
        
        # If the new classification is still not significant then aggregate with another variable by changing the data set
        
        # The above will most likely need to be another loop (not sure at this)
        
        # Finally: If you have only two classifications remaining and they are still insignificant, choose to drop the first classification
    }
  }
}