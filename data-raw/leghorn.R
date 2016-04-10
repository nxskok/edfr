x=c(156,162,168,182,186,190,190,196,202,210,214,220,226,230,230,236,236,242,246,270)
z1=pnorm(x,200,35)
z2=pnorm(x,mean(x),sd(x))

leghorn=data.frame(x,z1,z2)

# also, some beta data for which uniform will be rejected

beta_data=rbeta(50,1,1.2)
hist(beta_data)
devtools::use_data(beta_data,overwrite = T)
