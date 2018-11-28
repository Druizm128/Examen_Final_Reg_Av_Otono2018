# Regresion Avanzada: Examen Final
# Modelo dinámico sin intercepto (lambda=100)

#- Specifying hyperparameters
niter<-20000
nchains<-2
nburning<-0.1*niter
semilla<-3567


#-Defining data-
data_dinam_suav_1000 <- list("n"=n,"m"=m,"y"=c(datos$WTI[1:(n-3)],rep(NA,m)), "x"=select(datos, -WTI))

#-Defining inits-
inits<-function(){list(beta=matrix(0,k,n),tau.y=1,yp=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","tau.y","tau.b","yp")

#-Running code in JAGS-
set.seed(semilla)
mod_dinam_suav_1000.sim<-jags(data_dinam_suav_1000,inits,parameters,model.file="suavizamiento2_lambda_1000.txt",
                         n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1)


#-Monitoring the chains-

#JAGS Output
out_dinam_suav_1000<-mod_dinam_suav_1000.sim$BUGSoutput$sims.list
# Simulations
out_dinam_suav_1000.sum<-mod_dinam_suav_1000.sim$BUGSoutput$summary
# Modes
modas_dinam_suav_1000_beta<-unlist(lapply(1:n,function(x){apply(out_dinam_suav_1000$beta[,,x],2,getmode)}))

# Summary

out_dinam_suav_1000.sum.t_beta<-cbind(out_dinam_suav_1000.sum[grep("beta",rownames(out_dinam_suav_1000.sum)),c(1,3,5,7)],modas_dinam_suav_1000_beta)
out_dinam_suav_1000.sum.t_beta<-cbind(out_dinam_suav_1000.sum.t_beta,apply(out_dinam_suav_1000$beta,2,prob))
out_dinam_suav_1000.sum.t_beta<-out_dinam_suav_1000.sum.t_beta[,c(1,3,5,2,4,6)]
colnames(out_dinam_suav_1000.sum.t_beta)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam_suav_1000.sum.t_beta)<-paste(rep(c('JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR'),n),rep(1:n,each=k),sep=' t=')

#-DIC-
out_dinam_suav_1000.dic<-mod_dinam_suav_1000.sim$BUGSoutput$DIC

#-Predictions-
out_dinam_suav_1000.yp<-out_dinam_suav_1000.sum[grep("yp",rownames(out_dinam_suav_1000.sum)),]

#-Betas-
out_dinam_suav_1000.beta<-out_dinam_suav_1000.sum[grep("beta",rownames(out_dinam_suav_1000.sum)),]