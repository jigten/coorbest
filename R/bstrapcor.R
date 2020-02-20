#' Bootsrap Pearson Correlation Coefficient
#'
#' This function computes the Pearson correlation coefficient of a group
#' versus an arbritary given index. It divides the group based on the quantile
#' intervals. Values less than Q1 are termed "small" and values largers than Q2
#' are termened "big".
#'
#' @param index The vector containing the values for the index
#' @param population Optional parameter of vector containing the values for the population. If this parameter is specified,
#' the return values will be given for three subdivisions of the data based on the quantiles of this parameter
#' @param ... Vectors containing the values for control and test groups. The first vector is assumed to be the control group
#' @return A list containing two list of bootstap resample correlation values and C.I. for each pair
#' @export
bstrapcor <- function(index, groups, population, Brep = 1000) {
  n = length(index)
  res = list()
  vals = list()
  CIs = list()
  cnt = 1
  
  if(missing(population)) {
    for(i in groups) {
      df <- data.frame(i, index)
      x = df$i
      y = df$index
      
      x_y <- data.frame(x, y)

      bootcorr <- boot::boot(data=x_y,statistic=pearson,R=Brep)
      
      ci <- boot::boot.ci(bootcorr,conf=.95)
      
      if (cnt == 1) {
        vals[["Control"]] <- bootcorr$t
        CIs[["Control C.I"]] <- c(ci$percent[4], ci$percent[5])
        cnt = cnt + 1
      } else {
        vals[[sprintf("Test %s", cnt - 1)]] <- bootcorr$t
        CIs[[sprintf("Test %s C.I", cnt - 1)]] <- c(ci$percent[4], ci$percent[5])
        cnt = cnt + 1
      }
    }
    res[["Values"]] <- vals
    res[["CIs"]] <- CIs
    return(res)
  } 
  else {
    q <- quantile(population, c(0.25,0.75))
    for(i in groups) {
      df <- data.frame(population, i, index)
      x_small = df$i[df$population <= q[1]]
      x_medium = df$i[df$population > q[1] & df$population < q[2]]
      x_big = df$i[df$population >= q[2]]
      y_small = df$index[df$population <= q[1]]
      y_medium = df$index[df$population > q[1] & df$population < q[2]]
      y_big = df$index[df$population >= q[2]]
      xs_y <- data.frame(x_small, y_small)
      xm_y <- data.frame(x_medium, y_medium)
      xb_y <- data.frame(x_big, y_big)
      bootcorr_b <- boot::boot(data=xb_y,statistic=pearson,R=Brep)
      bootcorr_m <- boot::boot(data=xm_y, statistic=pearson,R=Brep)
      bootcorr_s <- boot::boot(data=xs_y,statistic=pearson,R=Brep)
      ci_s <- boot::boot.ci(bootcorr_s,conf=.95)
      ci_m <- boot::boot.ci(bootcorr_m,conf =.95)
      ci_b <- boot::boot.ci(bootcorr_b,conf=.95)
      
      if (cnt == 1) {
        vals[["Control_Big"]] <- bootcorr_b$t
        CIs[["Control_Big_C.I"]] <- c(ci_b$percent[4], ci_b$percent[5])
        vals[["Control_Medium"]] <- bootcorr_m$t
        CIs[["Control_Medium_C.I"]] <- c(ci_m$percent[4], ci_m$percent[5])
        vals[["Control_Small"]] <- bootcorr_s$t
        CIs[["Control_Small_C.I"]] <- c(ci_s$percent[4], ci_s$percent[5])
        cnt = cnt + 1
      } else {
        vals[[sprintf("Test_Big_%s", cnt - 1)]] <- bootcorr_b$t
        CIs[[sprintf("Test_Big_%s_C.I", cnt - 1)]] <- c(ci_b$percent[4], ci_b$percent[5])
        vals[[sprintf("Test_Medium_%s", cnt - 1)]] <- bootcorr_m$t
        CIs[[sprintf("Test_Medium_%s_C.I", cnt - 1)]] <- c(ci_m$percent[4], ci_m$percent[5])
        vals[[sprintf("Test_Small_%s", cnt - 1)]] <- bootcorr_s$t
        CIs[[sprintf("Test_Small_%s_C.I", cnt - 1)]] <- c(ci_s$percent[4], ci_s$percent[5])
        cnt = cnt + 1
      }
    }
    res[["Values"]] <- vals
    res[["CIs"]] <- CIs
    return(res)
  }
}

#' Pearson Correlation Coefficient
#'
#' Helper function to compute the Pearson correlation coefficient
pearson <- function(d,i=c(1:n)){
  d2 <- d[i,]
  return(cor(d2[,1],d2[,2]))
}
