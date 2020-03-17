#' Differences between correlation groups with Bootstrap Confidence Intervals
#' @param d The list returned after running function bstrapcor
#' @return A list containing the results of calculating bootstrapped mean difference between control group and all test groups
#' @export

meandifcor <- function(d) {
  res = list()
  cnt = 1

  if (startsWith(names(d$Values)[2], "Control")) {

    group = c(rep("control", 1000))

    cor_vals_b = c(d$Values$Control_Big)
    cor_vals_m = c(d$Values$Control_Medium)
    cor_vals_s = c(d$Values$Control_Small)

    grps <- split(d$Values, ceiling(seq_along(d$Values)/3))
    for (grp in grps[-1]) {

      group = c(group, rep(sprintf("test_%s", cnt), 1000))

      cor_vals_b = c(cor_vals_b, grp[[1]])
      cor_vals_m = c(cor_vals_m, grp[[2]])
      cor_vals_s = c(cor_vals_s, grp[[3]])

      cnt = cnt + 1
    }

    corr_data_b = data.frame(group, cor_vals_b)
    corr_data_m = data.frame(group, cor_vals_m)
    corr_data_s = data.frame(group, cor_vals_s)

    grp_arr = c("control")
    test_id = 1

    while (count >= 1) {
      count = count - 1
      grp_arr = c(grp_arr, sprintf("test_%s", test_id))
      test_id = test_id + 1
    }

    res[["Test_Big"]] <- dabest(corr_data_b, group, cor_vals_b, idx=grp_arr, paired = FALSE)
    res[["Test_Medium"]] <- dabest(corr_data_m, group, cor_vals_m, idx=grp_arr, paired = FALSE)
    res[["Test_Small"]] <- dabest(corr_data_s, group, cor_vals_s, idx=grp_arr, paired = FALSE)

    return(res)

  } else {
    group = c(rep("control", 1000))
    cor_vals = c(d$Values$Control)

    for(t in d$Values[-1]) {
      cor_vals = c(cor_vals, t)
      group = c(group, rep(sprintf("test_%s", cnt), 1000))

      cnt = cnt + 1
    }

    grp_arr = c("control")
    test_id = 1

    while (count >= 1) {
      count = count - 1
      grp_arr = c(grp_arr, sprintf("test_%s", test_id))
      test_id = test_id + 1
    }
    corr_data = data.frame(group, cor_vals)
    res[["Test"]] <- dabest(corr_data, group, cor_vals, idx=grp_arr, paired = FALSE)

    return(res)
  }
}
