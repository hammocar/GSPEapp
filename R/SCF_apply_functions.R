

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
  hi.scf.se = sqrt((((n.seen.hi/n.trials.hi)*(1-(n.seen.hi/n.trials.hi))/(n.trials.hi)))/((n.seen.hi/n.trials.hi)^4))
  lo.scf.se = sqrt((((n.seen.lo/n.trials.lo)*(1-(n.seen.lo/n.trials.lo))/(n.trials.lo)))/((n.seen.lo/n.trials.lo)^4))
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
scf.apply1 <- function(hi.est,lo.est,hi.se,lo.se,n.trials.hi,n.trials.lo,n.seen.hi,n.seen.lo, corr = 0){
  hi.scf = (n.trials.hi/n.seen.hi)
  lo.scf = (n.trials.lo/n.seen.lo)
  hi.scf.se = sqrt((((n.seen.hi/n.trials.hi)*(1-(n.seen.hi/n.trials.hi))/(n.trials.hi)))/((n.seen.hi/n.trials.hi)^4))
  lo.scf.se = sqrt((((n.seen.lo/n.trials.lo)*(1-(n.seen.lo/n.trials.lo))/(n.trials.lo)))/((n.seen.lo/n.trials.lo)^4))
  tot.hi = hi.est*hi.scf
  tot.lo = lo.est*lo.scf
  tot.moose = sum(tot.hi,tot.lo)
  tot.hi.se = sqrt(hi.est^2*hi.scf.se^2+hi.scf^2*hi.se^2-hi.se^2*hi.scf.se^2)
  tot.lo.se = sqrt(lo.est^2*lo.scf.se^2+lo.scf^2*lo.se^2-lo.se^2*lo.scf.se^2)
  tot.moose.se = sqrt(tot.hi.se^2+tot.lo.se^2+2*corr*tot.lo.se*tot.hi.se)
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
  scf = (n.trials/n.seen)-((n.seen/n.trials)*(1-(n.seen/n.trials))/(n.trials))*(1/((n.seen/n.trials)^3))
  scf.se = sqrt((((n.seen/n.trials)*(1-(n.seen/n.trials))/(n.trials)))/((n.seen/n.trials)^4))
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
  scf.se = sqrt((((n.seen/n.trials)*(1-(n.seen/n.trials))/(n.trials)))/((n.seen/n.trials)^4))
  tot = est*scf
  tot.se = sqrt(est^2*scf.se^2+scf^2*se^2-se^2*scf.se^2)
  rp.90.tot = tot.se/tot*1.645
  total = data.frame(tot,tot.se,rp.90.tot,scf,scf.se)
  return(total)
}

# Function that does NOT change the SCF via delta-method (same as dividing by detection) also corrects using proportion grouped
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
scf.apply_grouped <- function(hi.est,lo.est,hi.se,lo.se,n.trials.grouped,n.trials.ungrouped, n.seen.grouped, n.seen.ungrouped,
                              n.grouped.high, n.grouped.low, n.denom.high, n.denom.low){
  grouped.scf = (n.trials.grouped/n.seen.grouped)
  ungrouped.scf = (n.trials.ungrouped/n.seen.ungrouped)
  grouped.scf.se = sqrt((((n.seen.grouped/n.trials.grouped)*(1-(n.seen.grouped/n.trials.grouped))/(n.trials.grouped)))/((n.seen.grouped/n.trials.grouped)^4))
  ungrouped.scf.se = sqrt((((n.seen.ungrouped/n.trials.ungrouped)*(1-(n.seen.ungrouped/n.trials.ungrouped))/(n.trials.ungrouped)))/((n.seen.ungrouped/n.trials.ungrouped)^4))

  p.grouped.high = n.grouped.high/n.denom.high
  p.grouped.low = n.grouped.low/n.denom.low
  p.grouped.high.se = p.grouped.high*(1-p.grouped.high)/n.denom.high
  p.grouped.low.se = p.grouped.low*(1-p.grouped.low)/n.denom.low

  grouped.scf_times_p.grouped.high_var = ((grouped.scf^2)*(p.grouped.high.se^2)/n.denom.high)+
    ((p.grouped.high^2)*(grouped.scf.se^2)/n.trials.grouped)-
    ((p.grouped.high.se^2)*(grouped.scf.se^2)/(n.denom.high*n.trials.grouped))

  ungrouped.scf_times_p.grouped.high_var = ((ungrouped.scf^2)*(p.grouped.high.se^2)/n.denom.high)+
    ((1-p.grouped.high^2)*(ungrouped.scf.se^2)/n.trials.ungrouped)-
    ((p.grouped.high.se^2)*(ungrouped.scf.se^2)/(n.denom.high*n.trials.ungrouped))

  grouped.scf_times_p.grouped.low_var = ((grouped.scf^2)*(p.grouped.low.se^2)/n.denom.low)+
    ((p.grouped.low^2)*(grouped.scf.se^2)/n.trials.grouped)-
    ((p.grouped.low.se^2)*(grouped.scf.se^2)/(n.denom.low*n.trials.grouped))

  ungrouped.scf_times_p.grouped.low_var = ((ungrouped.scf^2)*(p.grouped.low.se^2)/n.denom.low)+
    ((1-p.grouped.low^2)*(ungrouped.scf.se^2)/n.trials.ungrouped)-
    ((p.grouped.low.se^2)*(ungrouped.scf.se^2)/(n.denom.low*n.trials.ungrouped))


  tot.hi.grouped = hi.est*p.grouped.high*grouped.scf
  tot.hi.ungrouped =  hi.est*(1-p.grouped.high)*ungrouped.scf
  tot.lo.grouped = lo.est*p.grouped.low*grouped.scf
  tot.lo.ungrouped = lo.est*(1-p.grouped.low)*ungrouped.scf


  tot.hi = tot.hi.grouped + tot.hi.ungrouped
  tot.lo = tot.lo.grouped + tot.lo.ungrouped

  tot.moose = sum(tot.hi.grouped,tot.hi.ungrouped, tot.lo.grouped, tot.lo.ungrouped)

  tot.hi.grouped.var = (hi.est^2*grouped.scf_times_p.grouped.high_var) + (grouped.scf^2*hi.se^2) - (hi.se^2*grouped.scf.se^2)
  tot.hi.ungrouped.var = (hi.est^2*ungrouped.scf_times_p.grouped.high_var) + (ungrouped.scf^2*hi.se^2) - (hi.se^2*ungrouped.scf.se^2)
  tot.lo.grouped.var = (lo.est^2*grouped.scf_times_p.grouped.low_var) + (grouped.scf^2*lo.se^2) - (lo.se^2*grouped.scf.se^2)
  tot.lo.ungrouped.var = (lo.est^2*ungrouped.scf_times_p.grouped.low_var) + (ungrouped.scf^2*lo.se^2) - (lo.se^2*ungrouped.scf.se^2)

  tot.var = sum(tot.hi.grouped.var, tot.hi.ungrouped.var, tot.lo.grouped.var, tot.lo.ungrouped.var)
  tot.moose.se = sqrt(tot.var)
  tot.lo.se = sqrt(sum(tot.lo.grouped.var, tot.lo.ungrouped.var))
  tot.hi.se = sqrt(sum(tot.hi.grouped.var, tot.hi.ungrouped.var))

  rp.90.hi = sqrt(sum(tot.hi.grouped.var, tot.hi.ungrouped.var))/tot.hi*1.645
  rp.90.lo = sqrt(sum(tot.lo.grouped.var, tot.lo.ungrouped.var))/tot.lo*1.645

  rp.tot = sqrt(tot.var)/tot.moose*1.645
  total = data.frame(tot.moose,tot.moose.se,tot.hi,tot.lo,tot.lo.se,tot.hi.se,rp.90.hi,rp.90.lo,rp.tot,
                     grouped.scf,ungrouped.scf)
  return(total)
}
