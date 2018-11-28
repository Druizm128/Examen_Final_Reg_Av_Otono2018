# Regresion Avanzada: Examen Final
# Modelo Dinamico sin Intercepto

#- Specifying hyperparameters
niter<-20000
nchains<-2
nburning<-0.1*niter
semilla<-3567

#-Defining data-
data_dinam <- list("n"=n,"m"=m,"y"=c(datos$WTI[1:(n-3)],rep(NA,m)), "x"=select(datos, -WTI))

#-Defining inits-
inits<-function(){list(beta=matrix(0,k,n),tau.y=1,tau.a=1,tau.b=rep(1,k),yp=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","tau.y","tau.a","tau.b","yp")


#-Running code in JAGS-
set.seed(semilla)
mod_dinam.sim<-jags(data_dinam,inits,parameters,model.file="mod2_dinamico1.txt",
                    n.iter=niter,n.chains=nchains,n.burnin=nburning,n.thin=1,
                    progress.bar='none')

#-Monitoring the chains-

#JAGS Output
out_dinam<-mod_dinam.sim$BUGSoutput$sims.list

# # Chain for coefficients
# z<-out_dinam$beta[,1,1] # simulaciones de beta 1 al tiempo 1
# par(mfrow=c(2,2))
# plot(z,type="l")
# plot(cumsum(z)/(1:length(z)),type="l")
# hist(z,freq=FALSE)
# acf(z)

#-Coefficient Summary-

# Simulations
out_dinam.sum<-mod_dinam.sim$BUGSoutput$summary

# Modes
modas_dinam_beta<-unlist(lapply(1:n,function(x){apply(out_dinam$beta[,,x],2,getmode)}))

# Summary
out_dinam.sum.t_beta<-cbind(out_dinam.sum[grep("beta",rownames(out_dinam.sum)),c(1,3,5,7)],modas_dinam_beta)
out_dinam.sum.t_beta<-cbind(out_dinam.sum.t_beta,apply(out_dinam$beta,2,prob))
out_dinam.sum.t_beta<-out_dinam.sum.t_beta[,c(1,3,5,2,4,6)]
colnames(out_dinam.sum.t_beta)<-c("Media","Mediana","Moda","2.5%","97.5%","Prob.")
rownames(out_dinam.sum.t_beta)<-paste(rep(c('JPM Dollar Ind.','VIX Ind','Prod. OPEP','Dem. OPEP','T-Bill 10YR','T-Bill 1YR'),n),rep(1:n,each=k),sep=' t=')

#-DIC-
out_dinam.dic<-mod_dinam.sim$BUGSoutput$DIC

#-Predictions-
out_dinam.yp<-out_dinam.sum[grep("yp",rownames(out_dinam.sum)),]

#-Betas-
out_dinam.beta<-out_dinam.sum[grep("beta",rownames(out_dinam.sum)),]
