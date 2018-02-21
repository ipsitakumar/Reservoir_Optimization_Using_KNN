#################################################################################################################
######## Water Management Optimization in R - Laureline
######## laureline.josset@gmail.com
#################################################################################################################
foldername <- "test/"
save(A_d,file=paste0(foldername,"A_d.Rdata"))
save(rhs_d,file=paste0(foldername,"rhs_d.Rdata"))
save(s_d,file=paste0(foldername,"s_d.Rdata"))
#-
if(with_Pconstraints){
  save(A_pmin,file=paste0(foldername,"A_pmin.Rdata"))
  save(rhs_pmin,file=paste0(foldername,"rhs_pmin.Rdata"))
  save(s_pmin,file=paste0(foldername,"s_pmin.Rdata"))
  save(A_pmax,file=paste0(foldername,"A_pmax.Rdata"))
  save(rhs_pmax,file=paste0(foldername,"rhs_pmax.Rdata"))
  save(s_pmax,file=paste0(foldername,"s_pmax.Rdata"))}
#-
if(nIMP>0){
  save(A_impmax,file=paste0(foldername,"A_impmax.Rdata"))
  save(rhs_impmax,file=paste0(foldername,"rhs_impmax.Rdata"))
  save(s_impmax,file=paste0(foldername,"s_impmax.Rdata"))}
#-
if(with_Smin_constraints){
  save(A_smin,file=paste0(foldername,"A_smin.Rdata"))
  save(rhs_smin,file=paste0(foldername,"rhs_smin.Rdata"))
  save(s_smin,file=paste0(foldername,"s_smin.Rdata"))}
if(with_Smax_constraints){
  save(A_smax,file=paste0(foldername,"A_smax.Rdata"))
  save(rhs_smax,file=paste0(foldername,"rhs_smax.Rdata"))
  save(s_smax,file=paste0(foldername,"s_smax.Rdata"))}
save(A_s,file=paste0(foldername,"A_s.Rdata"))
save(rhs_s,file=paste0(foldername,"rhs_s.Rdata"))
save(s_s,file=paste0(foldername,"s_s.Rdata"))