#' Performs Mann-Whitney U-Test on correlation values of test groups versus control group
#'
#' @param d The list returned after running function bstrapcor
#' @return A list containing the results of running Mann-Whitney U test between control group and all test groups
#' @export

manntest <- function(d) {
  res = list()
  grps <- split(d$Values, ceiling(seq_along(d$Values)/3))
  cnt = 1
  if (startsWith(names(d$Values)[2], "Control")) {
    for(grp in grps[-1]) {
      res[[sprintf("Test_%s_Big", cnt)]] <- wilcox.test(d$Values$Control_Big, grp[[1]], alternative = c("less"))
      res[[sprintf("Test_%s_Medium", cnt)]] <- wilcox.test(d$Values$Control_Medium, grp[[2]], alternative = c("less"))
      res[[sprintf("Test_%s_Small", cnt)]] <- wilcox.test(d$Values$Control_Small, grp[[3]], alternative = c("less"))
      cnt = cnt + 1
    }
    return(res)
  } else {
    for(t in d$Values[-1]) {
      res[[sprintf("Test %s", cnt)]] <- wilcox.test(d$Values$Control, t, alternative = c("less"))
      cnt = cnt + 1
    }
    return(res)
  }
}
