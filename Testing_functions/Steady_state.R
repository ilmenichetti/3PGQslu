
library(r3PG)

u0_calc<-function(latitude){
  u0_calc=(0.0855+0.0157*(50.6-0.768*latitude))
}

pars<-as.data.frame(t(i_parsQlitter[,c(4)]))


Q_ss= function(I, e0, eta11, b, u0, q0, Tmax){
  
  a = 0.5*b*eta11*u*q0^b 
  z = (1-e0)/(b*eta11*e0) #zeta is not variable over time, it is edaphic

  C_ss=(1/a*1/(z-1)+Tmax/3)*I
  return(C_ss)
}


colnames(pars)<-t(i_parsQlitter[,c(1)])


#testing
Q_ss(b=pars$beta_fol,
     e0=pars$e0_fol,
     q0=0.87, #pine material, Menichetti et al., 2021
     u0=u0_calc(45),
     eta11=pars$eta_11_fol,
     Tmax=3,
     I=3)
      
