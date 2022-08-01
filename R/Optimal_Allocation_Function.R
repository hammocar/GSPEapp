options(error=NULL)

# Function to determine optimal allocation for GSPE using Neyman allocation & Gasaway formulas

# Write function so can just use output from gspe function to determine allocation based on multiple surveys results
#' Title
#'
#' @param data
#' @param results
#' @param sample.size
#' @param RP
#' @param CI.level
#'
#' @return
#' @export
#'
#' @examples
optim.alloc <- function(data,results,sample.size=100,RP=.20,CI.level=1.96){
  data$Strat<- factor(data$Strat)
  p.hi = seq(.3,.9,.01)
  p.lo = 1-p.hi
  pop.guess = c()
  for (i in 1:length(names(results))){
    pop.guess[i] = results[[paste(unique(names(results))[i])]]$estimate.total[1]
  }
  var.hi = c()
  for (i in 1:length(names(results))){ # grabbing the hi.vars
    var.hi[i] = (results[[paste(unique(names(results))[i])]]$estimate.standard.error[grep("H",levels(data$Strat))+1])^2}
  var.lo = c()
  for (i in 1:length(names(results))){ # grabbing the lo.vars
    var.lo[i] = (results[[paste(unique(names(results))[i])]]$estimate.standard.error[grep("L",levels(data$Strat))+1])^2}
  n.high.sampled = c()
  for (i in 1:length(names(results))){ # grabbing the n highs sampled
    n.high.sampled[i] = results[[paste(unique(names(results))[i])]]$sample.sizes[grep("H",levels(data$Strat)),2]}
  n.low.sampled = c()
  for (i in 1:length(names(results))){ # grabbing the n lows sampled
    n.low.sampled[i] = results[[paste(unique(names(results))[i])]]$sample.sizes[grep("L",levels(data$Strat)),2]}
  n.high.total = c()
  for (i in 1:length(names(results))){
    n.high.total[i] = results[[paste(unique(names(results))[i])]]$total.samples[grep("H",levels(data$Strat)),2]}
  n.low.total = c()
  for (i in 1:length(names(results))){
    n.low.total[i] = results[[paste(unique(names(results))[i])]]$total.samples[grep("L",levels(data$Strat)),2]}
  sh.hi2 = (var.hi*n.high.sampled)/n.high.total^2*(n.high.total/(n.high.total-n.high.sampled))
  sh.lo2 = (var.lo*n.low.sampled)/n.low.total^2*(n.low.total/(n.low.total-n.low.sampled))
  sh.hi = sqrt(sh.hi2)
  sh.lo = sqrt(sh.lo2)
  neyman.high = (sample.size*n.high.total*sh.hi)/(n.high.total*sh.hi+n.low.total*sh.lo)
  neyman.low = sample.size-neyman.high
  B = 2*(RP*pop.guess/CI.level)
  D = .25*B^2/(n.high.total+n.low.total)^2
  K12 = matrix(ncol=length(names(results)),nrow=length(p.hi))
  for(i in 1:length(names(results))){
  K12[,i] = (n.high.total[i]^2%o%sh.hi2[i]/p.hi+n.low.total[i]^2*sh.lo2[i]/p.lo) # These variable names refer to cells in excel allocation sheet
  }
  L12 = (n.high.total+n.low.total)^2*D
  M12 = (n.high.total*sh.hi2+n.low.total*sh.lo2)
  Samples.Required = matrix(ncol=length(names(results)),nrow=length(p.hi))
  for(i in 1:length(names(results))){
  Samples.Required[,i] = K12[,i]/(L12[i]+M12[i])}

  return(list(data.frame(high=neyman.high,low=neyman.low,sh.lo=sh.lo,sh.hi=sh.hi,
                         sh.hi2=sh.hi2,sh.lo2=sh.lo2,prop.hi=round(neyman.high/sample.size,2),
                         Survey = names(results)),
              cbind(Prop.hi=p.hi,Samples.Required)))
}




