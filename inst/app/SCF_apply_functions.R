

# Apply SCF by stratum. This function uses the delta method to correct BOTH mean and variance
#' Title
#'
#' @param hi.est
#' @param lo.est
#' @param hi.se
#' @param lo.se
#' @param n.trials.hi
#' @param n.trials.lo
#' @param n.seen.hi
#' @param n.seen.lo
#'
#' @return
#' @export
#'
#' @examples
scf.apply <- function(hi.est,lo.est,hi.se,lo.se,n.trials.hi,n.trials.lo,n.seen.hi,n.seen.lo){
  hi.scf = (n.trials.hi/n.seen.hi)-((n.seen.hi/n.trials.hi)*(1-(n.seen.hi/n.trials.hi))/(n.trials.hi-1))*(1/((n.seen.hi/n.trials.hi)^3))
  lo.scf = (n.trials.lo/n.seen.lo)-((n.seen.lo/n.trials.lo)*(1-(n.seen.lo/n.trials.lo))/(n.trials.lo-1))*(1/((n.seen.lo/n.trials.lo)^3))
  hi.scf.se = sqrt((((n.seen.hi/n.trials.hi)*(1-(n.seen.hi/n.trials.hi))/(n.trials.hi-1)))/((n.seen.hi/n.trials.hi)^4))
  lo.scf.se = sqrt((((n.seen.lo/n.trials.lo)*(1-(n.seen.lo/n.trials.lo))/(n.trials.lo-1)))/((n.seen.lo/n.trials.lo)^4))
  tot.hi = hi.est*hi.scf
  tot.lo = lo.est*lo.scf
  tot.moose = sum(tot.hi,tot.lo)
  tot.hi.se = sqrt(hi.est^2*hi.scf.se^2+hi.scf^2*hi.se^2-hi.se^2*hi.scf.se^2)
  tot.lo.se = sqrt(lo.est^2*lo.scf.se^2+lo.scf^2*lo.se^2-lo.se^2*lo.scf.se^2)
  tot.moose.se = sqrt(tot.hi.se^2+tot.lo.se^2)
  rp.90.hi = tot.hi.se/tot.hi*1.645
  rp.90.lo = tot.lo.se/tot.lo*1.645
  rp.tot = tot.moose.se/tot.moose*1.645
  total = data.frame(tot.moose,tot.moose.se,tot.hi,tot.lo,tot.lo.se,tot.hi.se,rp.90.hi,rp.90.lo,rp.tot,
                     hi.scf,lo.scf,hi.scf.se,lo.scf.se)
  return(total)
}

# Function that does NOT change the SCF via delta-method (same as dividing by detection)
#' Title
#'
#' @param hi.est
#' @param lo.est
#' @param hi.se
#' @param lo.se
#' @param n.trials.hi
#' @param n.trials.lo
#' @param n.seen.hi
#' @param n.seen.lo
#'
#' @return
#' @export
#'
#' @examples
scf.apply1 <- function(hi.est,lo.est,hi.se,lo.se,n.trials.hi,n.trials.lo,n.seen.hi,n.seen.lo){
  hi.scf = (n.trials.hi/n.seen.hi)
  lo.scf = (n.trials.lo/n.seen.lo)
  hi.scf.se = sqrt((((n.seen.hi/n.trials.hi)*(1-(n.seen.hi/n.trials.hi))/(n.trials.hi-1)))/((n.seen.hi/n.trials.hi)^4))
  lo.scf.se = sqrt((((n.seen.lo/n.trials.lo)*(1-(n.seen.lo/n.trials.lo))/(n.trials.lo-1)))/((n.seen.lo/n.trials.lo)^4))
  tot.hi = hi.est*hi.scf
  tot.lo = lo.est*lo.scf
  tot.moose = sum(tot.hi,tot.lo)
  tot.hi.se = sqrt(hi.est^2*hi.scf.se^2+hi.scf^2*hi.se^2-hi.se^2*hi.scf.se^2)
  tot.lo.se = sqrt(lo.est^2*lo.scf.se^2+lo.scf^2*lo.se^2-lo.se^2*lo.scf.se^2)
  tot.moose.se = sqrt(tot.hi.se^2+tot.lo.se^2)
  rp.90.hi = tot.hi.se/tot.hi*1.645
  rp.90.lo = tot.lo.se/tot.lo*1.645
  rp.tot = tot.moose.se/tot.moose*1.645
  total = data.frame(tot.moose,tot.moose.se,tot.hi,tot.lo,tot.lo.se,tot.hi.se,rp.90.hi,rp.90.lo,rp.tot,
                     hi.scf,lo.scf,hi.scf.se,lo.scf.se)
  return(total)
}

# Single Stratum SCF apply function using Brian's conservative delta method approach to shrink SCF towards 1 rather than the normal
# inflation that delta method for mean of ratio typically does... (This is mainly for emma to be consistent with Brian's old stuff...)
#' Title
#'
#' @param est
#' @param se
#' @param n.trials
#' @param n.seen
#'
#' @return
#' @export
#'
#' @examples
scf.apply2 <- function(est,se,n.trials,n.seen){
  scf = (n.trials/n.seen)-((n.seen/n.trials)*(1-(n.seen/n.trials))/(n.trials-1))*(1/((n.seen/n.trials)^3))
  scf.se = sqrt((((n.seen/n.trials)*(1-(n.seen/n.trials))/(n.trials-1)))/((n.seen/n.trials)^4))
  tot = est*scf
  tot.se = sqrt(est^2*scf.se^2+scf^2*se^2-se^2*scf.se^2)
  rp.tot = tot.se/tot*1.645
  total = data.frame(tot,tot.se,rp.tot,scf,scf.se)
  return(total)
}

# Single strat SCF apply function that moves away from one...
#' Title
#'
#' @param est
#' @param se
#' @param n.trials
#' @param n.seen
#'
#' @return
#' @export
#'
#' @examples
scf.apply3 <- function(est,se,n.trials,n.seen){
  scf = (n.trials/n.seen) # +(((n.seen/n.trials)*(1-(n.seen/n.trials)))/n.trials)/((n.seen/n.trials)^3) # Additional term here is delta method correction
  scf.se = sqrt((((n.seen/n.trials)*(1-(n.seen/n.trials))/(n.trials-1)))/((n.seen/n.trials)^4))
  tot = est*scf
  tot.se = sqrt(est^2*scf.se^2+scf^2*se^2-se^2*scf.se^2)
  rp.90.tot = tot.se/tot*1.645
  total = data.frame(tot,tot.se,rp.90.tot,scf,scf.se)
  return(total)
}


