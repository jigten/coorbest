#' Differences between correlation groups with Bootstrap Confidence Intervals
#' @param d The list returned after running function bstrapcor
#' @return A list containing the results of calculating bootstrapped mean difference between control group and all test groups
#' @export

meandifcor <- function(d) {
  res = list()
  cnt = 1

  if (startsWith(names(d$Values)[2], "Control")) {
    grps <- split(d$Values, ceiling(seq_along(d$Values)/3))
    for (grp in grps[-1]) {
      cor_vals_b = c(grp[[1]], d$Values$Control_Big)
      cor_vals_m = c(grp[[2]], d$Values$Control_Medium)
      cor_vals_s = c(grp[[3]], d$Values$Control_Small)

      group = c(rep("test", 1000), rep("control", 1000))

      corr_data_b = data.frame(group, cor_vals_b)
      corr_data_m = data.frame(group, cor_vals_m)
      corr_data_s = data.frame(group, cor_vals_s)

      res[[sprintf("Test_%s_Big", cnt)]] <- dabestr::dabest(corr_data_b, group, cor_vals_b, idx=c("control", "test"), paired = FALSE)
      res[[sprintf("Test_%s_Medium", cnt)]] <- dabestr::dabest(corr_data_m, group, cor_vals_m, idx=c("control", "test"), paired = FALSE)
      res[[sprintf("Test_%s_Small", cnt)]] <- dabestr::dabest(corr_data_s, group, cor_vals_s, idx=c("control", "test"), paired = FALSE)

      cnt = cnt + 1
    }
    return(res)

  } else {
    for(t in d$Values[-1]) {
      cor_vals = c(t, d$Values$Control)
      group = c(rep("test", 1000), rep("control", 1000))

      corr_data = data.frame(group, cor_vals)

      res[[sprintf("Test %s", cnt)]] <- dabestr::dabest(corr_data, group, cor_vals, idx=c("control", "test"), paired = FALSE)

      cnt = cnt + 1
    }
    return(res)
  }
}
