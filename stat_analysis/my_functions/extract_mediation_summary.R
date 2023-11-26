# extract mediation summary

extract_mediation_summary <- function (x) { 
  # Debug: print raw p-values before populating them into smat
  print(paste("Raw p-value for ACME (control): ", x$d0.p))
  print(paste("Raw p-value for ACME (treated): ", x$d1.p))
  print(paste("Raw p-value for ADE (control): ", x$z0.p))
  print(paste("Raw p-value for ADE (treated): ", x$z1.p))
  
  clp <- 100 * x$conf.level
  isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) || 
                   (inherits(x$model.y, "glm") && x$model.y$family$family == 
                      "gaussian" && x$model.y$family$link == "identity") || 
                   (inherits(x$model.y, "survreg") && x$model.y$dist == 
                      "gaussian"))
  
  printone <- !x$INT && isLinear.y
  
  if (printone) {
    smat <- matrix(
      c(x$d1, x$z0, x$tau.coef, x$n0,
        x$d1.ci[1], x$z0.ci[1], x$tau.ci[1], x$n0.ci[1],
        x$d1.ci[2], x$z0.ci[2], x$tau.ci[2], x$n0.ci[2],
        x$d1.p, x$z0.p, x$tau.p, x$n0.p),
      ncol = 4
    )
    rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
  } else {
    smat <- matrix(
      c(x$d0, x$d1, x$z0, x$z1, x$tau.coef, x$n0, x$n1, x$d.avg, x$z.avg, x$n.avg,
        x$d0.ci[1], x$d1.ci[1], x$z0.ci[1], x$z1.ci[1], x$tau.ci[1], x$n0.ci[1], x$n1.ci[1], x$d.avg.ci[1], x$z.avg.ci[1], x$n.avg.ci[1],
        x$d0.ci[2], x$d1.ci[2], x$z0.ci[2], x$z1.ci[2], x$tau.ci[2], x$n0.ci[2], x$n1.ci[2], x$d.avg.ci[2], x$z.avg.ci[2], x$n.avg.ci[2],
        x$d0.p, x$d1.p, x$z0.p, x$z1.p, x$tau.p, x$n0.p, x$n1.p, x$d.avg.p, x$z.avg.p, x$n.avg.p),
      ncol = 4
    )
    rownames(smat) <- c("ACME (control)", "ACME (treated)", 
                        "ADE (control)", "ADE (treated)", "Total Effect", 
                        "Prop. Mediated (control)", "Prop. Mediated (treated)", 
                        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
  }
  
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep = ""), 
                      paste(clp, "% CI Upper", sep = ""), "p-value")
  
  # Convert matrix to data frame with explicit type control
  smat_df <- as.data.frame(smat, stringsAsFactors = FALSE)
  smat_df$Estimate <- as.numeric(as.character(smat_df$Estimate))
  smat_df$`p-value` <- as.numeric(as.character(smat_df$`p-value`)) # Ensuring high precision for p-values
  
  mediation_status <- NULL
  acme_pval <- smat["ACME (treated)", "p-value"]
  ade_pval <- smat["ADE (treated)", "p-value"]
  
  if (acme_pval < 0.05 && ade_pval < 0.05) {
    mediation_status <- "Partial Mediation"
  } else if (acme_pval < 0.05 && ade_pval >= 0.05) {
    mediation_status <- "Full Mediation"
  } else {
    mediation_status <- "No Mediation"
  }
  
  return(list(stats=smat_df, mediation=mediation_status))
}
