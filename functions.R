auto_garch = function(p_min=1,
                      p_max=1, 
                      q_min=1, 
                      q_max=1,
                      garch_type=c('sGARCH'),#sGARCH,iGARCH,gjrGARCH,apGARCH,fGARCH,csGARCH,mcsGARCH,realGARCH
                      #fiGARCH,tGARCH,avgGARCH,nGARCH,naGARCH,allGARCH
                      distribuation=c('norm') #norm, ged, std, snorm, sged, sstd,ghyp,nig,ghst,jsu
){
  i=1
  P <- c(0)
  Q <- c(0)
  
  Akaike <- c(0)
  Bayes <- c(0)
  Shibata <- c(0)
  HannanQuinn <- c(0)
  model <- c(0)
  Distribuation <- c(0)
  output <- data.frame(P,Q,Akaike,Bayes,Shibata,HannanQuinn, model,Distribuation)
  for (dist in distribuation){
    for (type in garch_type){
      for (pn in p_min:p_max){
        for (qn in q_min:q_max){
          spec1 <- ugarchspec(mean.model = list(armaOrder = c(0,0)),variance.model = list(model = type, garchOrder=c(pn,qn) ), distribution.model = dist)
          garchfit <- ugarchfit(data = insample$tot_ret, spec = spec1)
          
          
          
          output[i,] = c(pn,qn,infocriteria(garchfit)[1],infocriteria(garchfit)[2],infocriteria(garchfit)[3],infocriteria(garchfit)[4], type,dist)
          i=i+1
        }}}}
  output = output[order(output$Akaike),]
  return(output)
}

auto_garch_arma = function(p_min=1,
                           p_max=1, 
                           q_min=1, 
                           q_max=1,
                           garch_type=c('sGARCH'),#sGARCH,iGARCH,gjrGARCH,apGARCH,fGARCH,csGARCH,mcsGARCH,realGARCH
                           #fiGARCH,tGARCH,avgGARCH,nGARCH,naGARCH,allGARCH
                           distribuation=c('norm'),#norm, ged, std, snorm, sged, sstd,ghyp,nig,ghst,jsu
                           p_garch=1,
                           q_garch=1
){
  i=1
  P <- c(0)
  Q <- c(0)
  
  Akaike <- c(0)
  Bayes <- c(0)
  Shibata <- c(0)
  HannanQuinn <- c(0)
  model <- c(0)
  Distribuation <- c(0)
  output <- data.frame(P,Q,Akaike,Bayes,Shibata,HannanQuinn, model,Distribuation)
  for (dist in distribuation){
    for (type in garch_type){
      for (pn in p_min:p_max){
        for (qn in q_min:q_max){
          spec1 <- ugarchspec(mean.model = list(armaOrder = c(pn,qn)),variance.model = list(model = type, garchOrder=c(p_garch, q_garch)), distribution.model = dist)
          garchfit <- ugarchfit(data = insample$tot_ret, spec = spec1)
          
          
          
          output[i,] = c(pn,qn,infocriteria(garchfit)[1],infocriteria(garchfit)[2],infocriteria(garchfit)[3],infocriteria(garchfit)[4], type,dist)
          i=i+1
        }}}}
  output = output[order(output$Akaike),]
  return(output)
}