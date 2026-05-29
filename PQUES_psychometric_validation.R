# =============================================================================
# Cross-cultural Adaptation and Psychometric Validation of the
# Perceived Quality of University Experiences Scale (PQUES)
# among Business Students in Chile
# -----------------------------------------------------------------------------
# Supplementary material — R analysis script
#
# Authors : C. Frex; R. Monge; R. Fuentes; H. Fernández
#           
# Contact : rmonge@udla.cl
# Date    : January 12, 2026
#
# Purpose : Split-sample exploratory and confirmatory factor analysis (EFA/CFA),
#           reliability, convergent and discriminant validity, exploratory graph
#           analysis (EGA), and measurement invariance for the PQUES.
#
# Notes   : - All scale items are treated as ordinal; polychoric correlations
#             and the robust WLSMV estimator are used throughout.
#           - A fixed random seed (set.seed) makes the EFA/CFA split reproducible.
#           - Section numbers follow the analytic workflow of the manuscript.
#           - Tested with R >= 4.3. Package versions are reported by
#             sessionInfo() at the end of the script.
# =============================================================================


# =============================================================================
# 1. PACKAGES
# =============================================================================

# Data manipulation and import
library(dplyr)
library(tidyverse)
library(readr)
library(purrr)
library(tibble)

# Tables and reporting
library(gtsummary)
library(huxtable)
library(labelled)
library(matrixStats)

# Graphics
library(ggplot2)
library(grid)
library(gridExtra)
library(corrplot)
library(RColorBrewer)
library(likert)
library(scales)

# Psychometrics and structural equation modeling
library(psych)
library(lavaan)
library(semTools)
library(semPlot)

# Factor-analysis utilities
library(parameters)
library(performance)
library(nFactors)
library(FactoMineR)
library(factoextra)

# Normality and multivariate tests
library(QuantPsyc)
library(nortest)
library(MVN)

# Network psychometrics
library(EGAnet)

# Model visualization
library(see)


# =============================================================================
# 2. DATA LOADING AND PREPROCESSING
# =============================================================================

# Load the raw dataset
file_path1 <- "data/data.csv"
df <- read_csv(file_path1)

# Build composite (sum) scores per dimension and the Net Promoter Score (NPS)
# categories for the university and the academic program.
df <- df %>%
  mutate(
    # Composite scores (item columns 14:47)
    total                   = rowSums(dplyr::select(., 14:47)),
    calidad_docente         = rowSums(dplyr::select(., 14:21)),
    calidad_clases          = rowSums(dplyr::select(., 22:28)),
    director_carrera        = rowSums(dplyr::select(., 29:33)),
    personal_administrativo = rowSums(dplyr::select(., 34:41)),
    infraestructura         = rowSums(dplyr::select(., 42:47)),
    
    # NPS categorization — University
    PDR_universidad = case_when(
      NPS_universidad >= 0 & NPS_universidad <= 6  ~ "Detractores",
      NPS_universidad >= 7 & NPS_universidad <= 8  ~ "Pasivos",
      NPS_universidad >= 9 & NPS_universidad <= 10 ~ "Promotores",
      TRUE ~ NA_character_
    ),
    
    # NPS categorization — Academic program
    PDR_carrera = case_when(
      NPS_carrera >= 0 & NPS_carrera <= 6  ~ "Detractores",
      NPS_carrera >= 7 & NPS_carrera <= 8  ~ "Pasivos",
      NPS_carrera >= 9 & NPS_carrera <= 10 ~ "Promotores",
      TRUE ~ NA_character_
    )
  )

# Optional outlier screening with a Hampel filter on the total score
# (kept commented out; uncomment to apply).
# cota_inferior <- median(df$total) - 3 * mad(df$total, constant = 1)
# cota_superior <- median(df$total) + 3 * mad(df$total, constant = 1)
# outlier_ind   <- which(df$total < cota_inferior | df$total > cota_superior)
# df_so         <- df[!(row.names(df) %in% outlier_ind), ]


# =============================================================================
# 3. VARIABLE LABELING
# =============================================================================

# Attach human-readable variable labels (used by the table-reporting functions).
df_so <- df |>
  labelled::set_variable_labels(
    sex            = "Sex",
    edad           = "Age",
    universidad    = "University",
    carrera        = "Academic program",
    anio_ingreso   = "Year of enrollment",
    modalidad      = "Study modality",
    trabaja        = "Employment status",
    donde_trabaja  = "Work schedule",
    financiamiento = "Tuition financing",
    aceptacion     = "Degree of understanding and acceptance of this instrument",
    comprension    = "Degree of comprehension of the questions",
    satisfaccion   = "Degree of satisfaction with this instrument"
  )


# =============================================================================
# 4. DESCRIPTIVE ANALYSIS
# =============================================================================

# -- 4.1 Sociodemographic characteristics -------------------------------------
df_so_demo <- df_so |>
  dplyr::select("sex", "edad", "universidad", "carrera", "anio_ingreso",
                "modalidad", "trabaja", "donde_trabaja", "financiamiento")

theme_gtsummary_eda(set_theme = TRUE)
tbl_summary(df_so_demo) %>%
  modify_header(label = "Variable") %>%
  modify_caption("Table 1. Participant Characteristics") %>%
  as_hux_table()

# -- 4.2 Satisfaction with the instrument -------------------------------------
df_so_sat <- df_so %>%
  dplyr::select("aceptacion", "comprension", "satisfaccion")

theme_gtsummary_eda(set_theme = TRUE)
tbl_summary(df_so_sat) %>%
  modify_header(label = "Variable") %>%
  modify_caption("Table 2. Frequency Distribution of Satisfaction with Instrument") %>%
  as_hux_table()


# =============================================================================
# 5. SAMPLE SPLIT FOR EFA AND CFA
# =============================================================================

set.seed(321)                      # fixed seed for a reproducible split
df_so    <- as.data.frame(df_so)
df_items <- dplyr::select(df_so, 14:47)
names(df_items)

# Random 50/50 split: one half for EFA, the other for CFA.
N            <- nrow(df_items)
indices      <- seq(1, N)
indices_AFE  <- sample(indices, floor(0.5 * N))
indices_AFC  <- indices[!(indices %in% indices_AFE)]
df_EFA       <- df_items[indices_AFE, ]
df_AFC       <- df_items[indices_AFC, ]

# Descriptive statistics for the EFA subsample
knitr::kable(head(df_EFA), booktabs = TRUE, format = "markdown")
knitr::kable(describe(df_EFA, type = 3, fast = FALSE), booktabs = TRUE,
             format = "markdown")
knitr::kable(response.frequencies(df_EFA), booktabs = TRUE,
             format = "markdown")


# =============================================================================
# 6. DATA-QUALITY CHECKS AND SPLIT COMPARABILITY
# =============================================================================

items_ecpeu <- names(df_items)

# -- 6.1 Missing data, incomplete cases and duplicated rows -------------------
missing_item        <- sapply(df_so[, items_ecpeu], function(x) mean(is.na(x)) * 100)
missing_total_cases <- mean(!complete.cases(df_so[, items_ecpeu])) * 100
n_duplicated_rows   <- sum(duplicated(df_so[, items_ecpeu]))

missing_summary <- tibble(
  Item        = names(missing_item),
  Missing_pct = round(missing_item, 2)
)

cat("Incomplete cases (%):", round(missing_total_cases, 2), "\n")
cat("Duplicated rows:     ", n_duplicated_rows, "\n")
print(missing_summary)

# -- 6.2 Comparability of the EFA and CFA subsamples --------------------------
# Label each case according to its subsample.
df_so$split_sample <- ifelse(seq_len(nrow(df_so)) %in% indices_AFE, "EFA", "CFA")

# Inspect the available column names.
cat("Columns available in df_so:\n")
print(names(df_so))

# Build the comparison frame (kept independent of haven_labelled types).
df_compare <- data.frame(
  split_sample   = factor(df_so$split_sample),
  gender_self    = factor(df_so$sex),
  edad           = as.numeric(df_so$edad),
  universidad    = factor(df_so$universidad),
  modalidad      = factor(df_so$modalidad),
  trabaja        = factor(df_so$trabaja),
  financiamiento = factor(df_so$financiamiento)
)

# Comparability table (EFA vs CFA) with appropriate per-variable tests.
tabla_equivalencia <- df_compare %>%
  tbl_summary(
    by        = split_sample,
    missing   = "no",
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_p(
    test = list(
      edad           ~ "wilcox.test",
      gender_self    ~ "fisher.test",   # Fisher's exact test (sparse cells)
      universidad    ~ "chisq.test",
      modalidad      ~ "chisq.test",
      trabaja        ~ "chisq.test",
      financiamiento ~ "chisq.test"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("Table 3. Comparability of the EFA and CFA Subsamples") %>%
  modify_header(label = "Variable")

tabla_equivalencia


# =============================================================================
# 7. NORMALITY ASSESSMENT
# =============================================================================

# -- 7.1 Multivariate normality (Mardia's test) -------------------------------
mardia_result <- QuantPsyc::mult.norm(df_EFA)
print(mardia_result$mult.test)

# -- 7.2 Univariate normality (Anderson-Darling test) -------------------------
univar_ad <- apply(df_EFA, 2, ad.test)
resultado_uni <- data.frame(
  Variable     = names(univar_ad),
  AD_statistic = sapply(univar_ad, function(x) round(x$statistic, 4)),
  p_value      = sapply(univar_ad, function(x) round(x$p.value, 4))
)
knitr::kable(resultado_uni, booktabs = TRUE, format = "markdown",
             caption = "Univariate Normality Tests (Anderson-Darling)")


# =============================================================================
# 8. FACTORABILITY ASSESSMENT
# =============================================================================

# Adequacy of the data for factor analysis (KMO, Bartlett's test, etc.).
performance::check_factorstructure(df_EFA)

# Polychoric correlation matrix.
salida        <- polychoric(df_EFA, smooth = TRUE)
salida_matriz <- salida$rho

# Determinant of the polychoric matrix (a near-zero value flags singularity).
det_salida <- det(salida_matriz)
det_salida

# Ordinal alpha computed on the polychoric matrix.
alpha_ordinal <- psych::alpha(salida_matriz, check.keys = TRUE)

# Item analysis: corrected item-total correlations and alpha-if-item-deleted.
item_analysis <- data.frame(
  Item                 = rownames(alpha_ordinal$item.stats),
  Corrected_Item_Total = alpha_ordinal$item.stats$r.drop,
  Alpha_if_Deleted     = alpha_ordinal$alpha.drop$raw_alpha,
  stringsAsFactors     = FALSE
)

# Screening flags based on common cut-off values.
item_analysis <- item_analysis %>%
  dplyr::mutate(
    Flag_Low_ItemTotal   = Corrected_Item_Total < 0.20,
    Flag_High_ItemTotal  = Corrected_Item_Total > 0.90,
    Flag_Alpha_Increases = Alpha_if_Deleted > alpha_ordinal$total$raw_alpha
  )

# Item diagnostics, sorted by discrimination (corrected item-total correlation).
print(item_analysis %>% dplyr::arrange(Corrected_Item_Total))

# Confidence interval for ordinal alpha.
intervalo_alpha_ordinal <- psych::alpha.ci(
  alpha_ordinal[["total"]][["raw_alpha"]],
  nrow(df_EFA), ncol(df_EFA),
  p.val = .05, digits = 4
)
for (i in 1:length(intervalo_alpha_ordinal)) print(intervalo_alpha_ordinal[i])

# Correlation-matrix visualization (upper: tiles; lower: numeric values).
corrplot(salida_matriz, type = "upper", tl.pos = "tp")
corrplot(salida_matriz, add = TRUE, type = "lower", method = "number",
         col = "black", diag = FALSE, tl.pos = "n", cl.pos = "n",
         number.cex = 0.9)


# =============================================================================
# 9. FACTOR-RETENTION ANALYSIS
# =============================================================================

# Determine the number of factors using multiple convergent criteria.
resultado_nfactors <- n_factors(
  df_EFA,
  cor       = salida_matriz,
  rotation  = "oblimin",
  algorithm = "mle",
  n         = nrow(df_EFA)
)

plot(resultado_nfactors)
as.data.frame(resultado_nfactors)
summary(resultado_nfactors)


# =============================================================================
# 10. EXPLORATORY FACTOR ANALYSIS (EFA)
# =============================================================================

# Coerce items to integer for the polychoric correlation engine.
df_EFA <- as.data.frame(lapply(df_EFA, as.integer))

# Five-factor EFA: weighted least squares, oblique (oblimin) rotation,
# polychoric correlations. Loadings below |.40| are suppressed for readability.
NPSfactor <- fa(
  df_EFA,
  nfactors = 5,
  fm       = "wls",        # weighted least squares
  rotate   = "oblimin",    # oblique rotation
  cor      = "poly"        # polychoric correlations
)
print(NPSfactor, digits = 2, cut = .40, sort = FALSE)


# =============================================================================
# 11. CONFIRMATORY FACTOR ANALYSIS (CFA)
# =============================================================================

# -- 11.1 Model specification (five correlated factors) -----------------------
cincofactores <- '
  IS =~ IS.1 + IS.2 + IS.3 + IS.4 + IS.5 + IS.6
  CC =~ CC.1 + CC.2 + CC.3 + CC.4 + CC.5 + CC.6 + CC.7
  PA =~ PA.1 + PA.2 + PA.3 + PA.4 + PA.5 + PA.6 + PA.7 + PA.8
  CA =~ CA.1 + CA.2 + CA.3 + CA.4 + CA.5 + CA.6 + CA.7 + CA.8
  DC =~ DC.1 + DC.2 + DC.3 + DC.4 + DC.5
'

# -- 11.2 Model estimation ----------------------------------------------------
CFA_cinco <- cfa(
  model            = cincofactores,
  data             = df_AFC,
  ordered          = names(df_AFC),   # treat all items as ordinal
  estimator        = "WLSMV",         # robust weighted least squares
  parameterization = "theta"          # recommended for ordinal indicators
)

# -- 11.3 Model fit (robust, scaled indices) ----------------------------------
fm <- fitMeasures(
  CFA_cinco,
  c("chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "srmr")
)

cat(sprintf(
  "Chi2robust(%.0f) = %.2f, p = %.3f; CFIrob = %.3f; TLIrob = %.3f; RMSEArob = %.3f (90%% CI %.3f-%.3f); SRMR = %.3f\n",
  fm["df.scaled"], fm["chisq.scaled"], fm["pvalue.scaled"],
  fm["cfi.scaled"], fm["tli.scaled"],
  fm["rmsea.scaled"], fm["rmsea.ci.lower.scaled"], fm["rmsea.ci.upper.scaled"],
  fm["srmr"]
))


# =============================================================================
# 12. RELIABILITY AND CONVERGENT VALIDITY (CR, AVE)
# =============================================================================

# Composite Reliability (CR) and Average Variance Extracted (AVE) per factor.
cr_results  <- semTools::compRelSEM(CFA_cinco)
ave_results <- semTools::AVE(CFA_cinco)

tabla_cr_ave <- data.frame(
  Factor = names(ave_results),
  CR     = as.numeric(cr_results[names(ave_results)]),
  AVE    = as.numeric(ave_results)
)
print(tabla_cr_ave)


# =============================================================================
# 13. RELIABILITY BY SUBSCALE (ORDINAL ALPHA, OMEGA)
# =============================================================================

subscales <- list(
  IS = paste0("IS.", 1:6),
  CC = paste0("CC.", 1:7),
  PA = paste0("PA.", 1:8),
  CA = paste0("CA.", 1:8),
  DC = paste0("DC.", 1:5)
)

tabla_reliability <- imap_dfr(subscales, function(items, fac) {
  dat <- df_AFC[, items, drop = FALSE] %>%
    mutate(across(everything(), as.integer))
  
  R  <- psych::polychoric(dat, smooth = TRUE)$rho
  a  <- psych::alpha(R)
  om <- psych::omega(R, n.obs = nrow(dat), plot = FALSE)
  
  tibble(
    Factor        = fac,
    Alpha_ordinal = round(a$total$raw_alpha, 3),
    Omega_total   = round(om$omega.tot, 3)
  )
})

tabla_reliability


# =============================================================================
# 14. ITEM-LEVEL R-SQUARED AND STANDARDIZED LOADINGS (CFA)
# =============================================================================

# Item-level R-squared (communalities) and error variance from the fitted CFA.
r2_items <- inspect(CFA_cinco, "r2") %>%
  enframe(name = "Item", value = "R2") %>%
  mutate(Error = 1 - R2)

r2_items

# Standardized factor loadings.
cargas_std <- standardizedSolution(CFA_cinco) %>%
  filter(op == "=~") %>%
  transmute(Factor = lhs, Item = rhs, Loading = est.std)

# Combined table: standardized loadings with R-squared and error variance.
tabla_cargas_error <- cargas_std %>%
  left_join(r2_items, by = c("Item" = "Item")) %>%
  arrange(Factor, Item)

tabla_cargas_error


# =============================================================================
# 15. DISCRIMINANT VALIDITY (FORNELL-LARCKER, HTMT)
# =============================================================================

# -- 15.1 Fornell-Larcker criterion -------------------------------------------
# Latent-variable correlations from the CFA.
R_lv <- lavInspect(CFA_cinco, "cor.lv")

# Enforce a consistent factor ordering.
factores <- names(ave_results)
R_lv     <- R_lv[factores, factores]

# Classic Fornell-Larcker matrix: diagonal = sqrt(AVE), off-diagonal = r.
FL_classic <- R_lv
diag(FL_classic) <- sqrt(as.numeric(ave_results[factores]))
print(round(FL_classic, 3))

# Alternative presentation: diagonal = AVE, off-diagonal = r^2.
FL_r2 <- R_lv^2
diag(FL_r2) <- as.numeric(ave_results[factores])
# print(round(FL_r2, 3))

# -- 15.2 Heterotrait-Monotrait ratio (HTMT) ----------------------------------
# Factor-to-item structure.
grupos <- list(
  IS = c("IS.1", "IS.2", "IS.3", "IS.4", "IS.5", "IS.6"),
  CC = c("CC.1", "CC.2", "CC.3", "CC.4", "CC.5", "CC.6", "CC.7"),
  PA = c("PA.1", "PA.2", "PA.3", "PA.4", "PA.5", "PA.6", "PA.7", "PA.8"),
  CA = c("CA.1", "CA.2", "CA.3", "CA.4", "CA.5", "CA.6", "CA.7", "CA.8"),
  DC = c("DC.1", "DC.2", "DC.3", "DC.4", "DC.5")
)

# Confirm that every item is present in the dataset.
stopifnot(all(unlist(grupos) %in% names(df_AFC)))

# Polychoric correlations among the items used for HTMT.
items_AFC <- df_AFC[, unlist(grupos), drop = FALSE]
R_poly    <- psych::polychoric(items_AFC)$rho

# HTMT computation.
htmt_manual <- function(cor_matrix, grupos) {
  n           <- length(grupos)
  htmt_values <- matrix(NA_real_, n, n)
  rownames(htmt_values) <- colnames(htmt_values) <- names(grupos)
  
  # Mean of the off-diagonal (absolute) elements of a sub-matrix.
  mean_offdiag <- function(M) {
    M <- abs(M)
    diag(M) <- NA
    mean(M, na.rm = TRUE)
  }
  
  # HTMT for every pair of factors.
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      items_i <- grupos[[i]]
      items_j <- grupos[[j]]
      
      cor_cross    <- abs(cor_matrix[items_i, items_j])
      cor_within_i <- cor_matrix[items_i, items_i, drop = FALSE]
      cor_within_j <- cor_matrix[items_j, items_j, drop = FALSE]
      
      htmt_ij <- mean(cor_cross) / sqrt(
        mean_offdiag(cor_within_i) * mean_offdiag(cor_within_j)
      )
      
      htmt_values[i, j] <- htmt_ij
      htmt_values[j, i] <- htmt_ij
    }
  }
  diag(htmt_values) <- 1
  return(htmt_values)
}

HTMT <- htmt_manual(R_poly, grupos)
print(round(HTMT, 3))


# =============================================================================
# 16. MODIFICATION INDICES (OPTIONAL)
# =============================================================================

# Largest residual correlations as candidate model modifications.
mi <- modindices(CFA_cinco)
mi %>%
  filter(op == "~~") %>%
  arrange(desc(mi)) %>%
  head(20)


# =============================================================================
# 17. EXPLORATORY GRAPH ANALYSIS (EGA)
# =============================================================================

# -- 17.1 EGA with GLASSO network estimation ----------------------------------
ega_glasso <- EGA(df_EFA, model = "glasso", plot.EGA = FALSE)

# -- 17.2 Bootstrap EGA for dimensional stability -----------------------------
boot_glasso <- bootEGA(
  df_EFA,
  model  = "glasso",
  iter   = 250,
  seed   = 3,
  ncores = 4
)

summary(boot_glasso)


# =============================================================================
# 18. MEASUREMENT INVARIANCE
# =============================================================================

items_ecpeu <- names(df_items)

# -----------------------------------------------------------------------------
# 18.1 Helper function: invariance sequence
# -----------------------------------------------------------------------------
# Fits the configural, metric (loadings), threshold (loadings + thresholds) and
# scalar/means (loadings + thresholds + means) models for ordinal indicators,
# keeps only the converged models, and returns the LRT comparison and fit
# indices.
run_invariance <- function(model, data, group, ordered_vars) {
  
  # Safe CFA wrapper for a given set of cross-group equality constraints.
  safe_cfa <- function(group.equal_arg) {
    tryCatch(
      cfa(
        model            = as.character(measEq.syntax(
          configural.model = model,
          data             = data,
          ordered          = ordered_vars,
          parameterization = "theta",
          estimator        = "WLSMV",
          group            = group,
          group.equal      = group.equal_arg)),
        data             = data,
        group            = group,
        ordered          = ordered_vars,
        estimator        = "WLSMV",
        parameterization = "theta",
        control          = list(iter.max = 500, rel.tol = 1e-6),
        se               = "none"
      ),
      error = function(e) {
        message("CFA failed (group.equal = '",
                paste(group.equal_arg, collapse = ", "), "'): ", e$message)
        NULL
      }
    )
  }
  
  # Fit the four nested models.
  fit_config     <- safe_cfa("")
  fit_loadings   <- safe_cfa("loadings")
  fit_thresholds <- safe_cfa(c("loadings", "thresholds"))
  fit_means      <- safe_cfa(c("loadings", "thresholds", "means"))
  
  # Keep only converged models.
  fits_all <- list(
    configural = fit_config,
    loadings   = fit_loadings,
    thresholds = fit_thresholds,
    means      = fit_means
  )
  
  converged <- Filter(function(f) {
    !is.null(f) && isTRUE(lavInspect(f, "converged"))
  }, fits_all)
  
  message("Converged models: ", paste(names(converged), collapse = ", "))
  
  # Likelihood-ratio test (scaled difference test).
  lrt <- if (length(converged) >= 2) {
    tryCatch(
      do.call(lavTestLRT, c(
        converged,
        list(method = "satorra.2000", model.names = names(converged))
      )),
      error = function(e) {
        message("LRT failed: ", e$message)
        NULL
      }
    )
  } else {
    message("At least two converged models are required for the LRT.")
    NULL
  }
  
  # Fit indices for each converged model.
  fit_indices <- do.call(rbind, lapply(names(converged), function(nm) {
    fm <- fitmeasures(converged[[nm]], c("cfi.scaled", "rmsea.scaled", "srmr"))
    data.frame(
      model        = nm,
      cfi.scaled   = fm["cfi.scaled"],
      rmsea.scaled = fm["rmsea.scaled"],
      srmr         = fm["srmr"],
      row.names    = NULL
    )
  }))
  
  c(converged, list(lrt = lrt, fit_indices = fit_indices))
}

# -----------------------------------------------------------------------------
# 18.2 Helper function: pre-invariance diagnostics
# -----------------------------------------------------------------------------
# Reports group sizes and flags items with empty response categories in any
# group (these would prevent the ordinal model from being estimated).
diagnose_invariance <- function(data, group, ordered_vars) {
  
  cat("\n=== Group sizes ===\n")
  print(table(data[[group]]))
  
  cat("\n=== Empty cells by item and group ===\n")
  problemas <- lapply(ordered_vars, function(it) {
    tbl   <- table(data[[it]], data[[group]], useNA = "ifany")
    zeros <- any(tbl == 0)
    if (zeros) cat("  [!] ", it, "has empty cells\n")
    list(item = it, table = tbl, has_zeros = zeros)
  })
  
  items_con_problema <- sapply(problemas, `[[`, "has_zeros")
  if (!any(items_con_problema)) {
    cat("  [OK] No item has empty cells.\n")
  }
  
  invisible(problemas)
}

# -----------------------------------------------------------------------------
# 18.3 Invariance by sex
# -----------------------------------------------------------------------------
# Exclude the small "Prefiero no decirlo" group.
df_inv_gender <- df_so %>%
  filter(sex %in% c("Femenino", "Masculino")) %>%
  mutate(sex = factor(sex))

inv_gender <- run_invariance(
  model        = cincofactores,
  data         = df_inv_gender,
  group        = "sex",
  ordered_vars = items_ecpeu
)

cat("\n=== LRT: invariance by sex ===\n")
print(inv_gender$lrt)

cat("\n=== Fit indices: invariance by sex ===\n")
print(inv_gender$fit_indices)

# -----------------------------------------------------------------------------
# 18.4 Invariance by study modality (Online vs Other)
# -----------------------------------------------------------------------------
# Selective collapsing of sparse low categories that are empty in one group:
#   CA.1: "Online" lacks category 1 -> collapse 1 into 2
#   CA.8: "Online" lacks 1 and "Other" lacks 2 -> collapse 1->2 and then 2->3
df_inv_mod <- df_so %>%
  mutate(
    modality2 = factor(ifelse(modalidad == "Online o A distancia", "Online", "Other")),
    
    # Collapse sparse category in CA.1: 1 -> 2
    CA.1 = ifelse(CA.1 == 1, 2L, as.integer(CA.1)),
    
    # Collapse sparse categories in CA.8: 1 -> 2, then 2 -> 3
    CA.8 = ifelse(CA.8 == 1, 2L, as.integer(CA.8)),
    CA.8 = ifelse(CA.8 == 2, 3L, CA.8)
  )

# Verify CA.1 and CA.8 after the selective collapse.
cat("\n--- Check CA.1 after collapse ---\n")
print(table(df_inv_mod$modality2, df_inv_mod$CA.1))

cat("\n--- Check CA.8 after collapse ---\n")
print(table(df_inv_mod$modality2, df_inv_mod$CA.8))

# Confirm that no item still has empty categories.
cat("\n--- Items with empty categories after the selective collapse ---\n")
hay_vacias <- FALSE
for (item in items_ecpeu) {
  tab <- table(df_inv_mod$modality2, df_inv_mod[[item]])
  if (any(tab == 0)) {
    hay_vacias <- TRUE
    cat("\nStill empty categories in item:", item, "\n")
    print(tab)
  }
}
if (!hay_vacias) cat("No empty categories. Ready to run invariance.\n")

inv_mod <- run_invariance(
  model        = cincofactores,
  data         = df_inv_mod,
  group        = "modality2",
  ordered_vars = items_ecpeu
)

cat("\n=== LRT: invariance by modality ===\n")
print(inv_mod$lrt)

cat("\n=== Fit indices: invariance by modality ===\n")
print(inv_mod$fit_indices)

# -----------------------------------------------------------------------------
# 18.5 Invariance by university
# -----------------------------------------------------------------------------
# Group distribution.
cat("\n--- Distribution by university ---\n")
print(table(df_so$universidad))

# Items with empty categories by university (before any correction).
cat("\n--- Items with empty categories by university (uncorrected) ---\n")
hay_vacias_univ <- FALSE
for (item in items_ecpeu) {
  tab <- table(df_so$universidad, df_so[[item]])
  if (any(tab == 0)) {
    hay_vacias_univ <- TRUE
    cat("\nEmpty categories in item:", item, "\n")
    print(tab)
  }
}
if (!hay_vacias_univ) cat("No empty categories. Ready to run invariance.\n")

# Selective collapsing of empty categories by university:
#   CA.2: University C lacks category 1 -> collapse 1 into 2
#   CA.8: A and B lack category 2, C lacks category 1 ->
#         step 1: collapse 1 -> 2; step 2: collapse 2 -> 3 (removes the gap)
#   CC.4: University A lacks category 1 -> collapse 1 into 2
df_inv_univ <- df_so %>%
  mutate(
    universidad = factor(universidad),
    
    CA.2 = ifelse(CA.2 == 1, 2L, as.integer(CA.2)),
    
    CA.8 = ifelse(CA.8 == 1, 2L, as.integer(CA.8)),
    CA.8 = ifelse(CA.8 == 2, 3L, CA.8),
    
    CC.4 = ifelse(CC.4 == 1, 2L, as.integer(CC.4))
  )

# Verify the three recoded items.
cat("\n--- Check CA.2 after collapse ---\n")
print(table(df_inv_univ$universidad, df_inv_univ$CA.2))

cat("\n--- Check CA.8 after collapse ---\n")
print(table(df_inv_univ$universidad, df_inv_univ$CA.8))

cat("\n--- Check CC.4 after collapse ---\n")
print(table(df_inv_univ$universidad, df_inv_univ$CC.4))

# Global check.
cat("\n--- Items with empty categories after the collapse ---\n")
hay_vacias_univ2 <- FALSE
for (item in items_ecpeu) {
  tab <- table(df_inv_univ$universidad, df_inv_univ[[item]])
  if (any(tab == 0)) {
    hay_vacias_univ2 <- TRUE
    cat("\nStill empty categories in item:", item, "\n")
    print(tab)
  }
}
if (!hay_vacias_univ2) cat("No empty categories. Ready to run invariance.\n")

# Model without PA.8 (dropped for the by-university comparison).
cincofactores_sinPA8 <- '
  IS =~ IS.1 + IS.2 + IS.3 + IS.4 + IS.5 + IS.6
  CC =~ CC.1 + CC.2 + CC.3 + CC.4 + CC.5 + CC.6 + CC.7
  PA =~ PA.1 + PA.2 + PA.3 + PA.4 + PA.5 + PA.6 + PA.7
  CA =~ CA.1 + CA.2 + CA.3 + CA.4 + CA.5 + CA.6 + CA.7 + CA.8
  DC =~ DC.1 + DC.2 + DC.3 + DC.4 + DC.5
'

items_sin_PA8 <- items_ecpeu[items_ecpeu != "PA.8"]

inv_univ <- run_invariance(
  model        = cincofactores_sinPA8,
  data         = df_inv_univ,
  group        = "universidad",
  ordered_vars = items_sin_PA8
)

cat("\n=== LRT: invariance by university ===\n")
print(inv_univ$lrt)

cat("\n=== Fit indices: invariance by university ===\n")
print(inv_univ$fit_indices)

# -----------------------------------------------------------------------------
# 18.6 Comparative summary
# -----------------------------------------------------------------------------
cat("\n=== SUMMARY: INVARIANCE BY SEX ===\n")
cat("\nLRT:\n");          print(inv_gender$lrt)
cat("\nFit indices:\n");  print(inv_gender$fit_indices)

cat("\n=== SUMMARY: INVARIANCE BY MODALITY ===\n")
cat("\nLRT:\n");          print(inv_mod$lrt)
cat("\nFit indices:\n");  print(inv_mod$fit_indices)

cat("\n=== SUMMARY: INVARIANCE BY UNIVERSITY ===\n")
cat("\nLRT:\n");          print(inv_univ$lrt)
cat("\nFit indices:\n");  print(inv_univ$fit_indices)

# -----------------------------------------------------------------------------
# 18.7 Partial invariance by university (stepwise approach)
# -----------------------------------------------------------------------------
# Strategy:
#   1. Configural model (no cross-group constraints).
#   2. Metric model (all loadings constrained equal).
#   3. Identify, item by item, which thresholds are invariant.
#   4. Partial threshold model: constrain only the invariant thresholds.
#   5. Means model on top of the partial threshold model.
#
# Inputs already prepared above: df_inv_univ (CA.2, CA.8, CC.4 collapsed),
# items_sin_PA8 (excludes PA.8) and cincofactores_sinPA8.

# -- Step 1: Configural model -------------------------------------------------
syntax_config <- as.character(
  measEq.syntax(
    configural.model = cincofactores_sinPA8,
    data             = df_inv_univ,
    ordered          = items_sin_PA8,
    parameterization = "theta",
    estimator        = "WLSMV",
    group            = "universidad",
    group.equal      = ""
  )
)

fit_config_univ <- cfa(
  model            = syntax_config,
  data             = df_inv_univ,
  group            = "universidad",
  ordered          = items_sin_PA8,
  estimator        = "WLSMV",
  parameterization = "theta",
  control          = list(iter.max = 1000, rel.tol = 1e-6),
  se               = "none"
)

cat("\n--- Configural converged:", lavInspect(fit_config_univ, "converged"), "---\n")

# -- Step 2: Metric model (invariant loadings) --------------------------------
syntax_load <- as.character(
  measEq.syntax(
    configural.model = cincofactores_sinPA8,
    data             = df_inv_univ,
    ordered          = items_sin_PA8,
    parameterization = "theta",
    estimator        = "WLSMV",
    group            = "universidad",
    group.equal      = "loadings"
  )
)

fit_load_univ <- cfa(
  model            = syntax_load,
  data             = df_inv_univ,
  group            = "universidad",
  ordered          = items_sin_PA8,
  estimator        = "WLSMV",
  parameterization = "theta",
  control          = list(iter.max = 1000, rel.tol = 1e-6),
  se               = "none"
)

cat("\n--- Loadings converged:", lavInspect(fit_load_univ, "converged"), "---\n")

# -- Step 3: Score test for threshold equality --------------------------------
# Starting from the metric model, test whether constraining each threshold to be
# equal across groups produces misfit (i.e., which thresholds are non-invariant).
pt_load <- parTable(fit_load_univ)

# Restricted threshold parameters in the metric model.
umbrales_restringidos <- pt_load[pt_load$op == "|" & pt_load$group > 1 & pt_load$free == 0, ]
cat("\n--- Restricted thresholds in the metric model:", nrow(umbrales_restringidos), "---\n")

# Row indices of the restricted thresholds (passed to 'release').
indices_umbrales <- umbrales_restringidos$id

cat("\n--- Running lavTestScore on the restricted thresholds... ---\n")
score_umbrales <- tryCatch(
  lavTestScore(
    fit_load_univ,
    univariate = TRUE,
    cumulative = FALSE,
    release    = indices_umbrales
  ),
  error = function(e) {
    message("lavTestScore failed: ", e$message)
    NULL
  }
)

if (!is.null(score_umbrales)) {
  
  mi_umbrales <- score_umbrales$uni
  mi_sorted   <- mi_umbrales[order(-mi_umbrales$X2), ]
  
  cat("\n=== Score test - thresholds (top 30) ===\n")
  print(head(mi_sorted, 30))
  
  # Thresholds with p < .05 are candidates for being non-invariant.
  no_inv_params <- mi_sorted[mi_sorted$p.value < .05, ]
  cat("\n--- Non-invariant threshold parameters (p < .05):", nrow(no_inv_params), "---\n")
  print(no_inv_params)
  
  # Extract item names from the 'lhs' column (e.g., "IS.1|t1", "CA.2|t2").
  items_no_inv_thresh <- unique(
    gsub("\\|t[0-9]+$", "", no_inv_params$lhs)
  )
  cat("\n--- Items with non-invariant thresholds:\n")
  print(items_no_inv_thresh)
  
} else {
  
  # Fallback: if lavTestScore also fails, identify problematic items by
  # comparing the estimated thresholds across groups in the configural model.
  cat("\nFallback: comparing configural-model thresholds across groups...\n")
  
  thresh_config <- lavInspect(fit_config_univ, "thresholds")
  
  # Maximum between-group threshold difference per item.
  diffs <- sapply(items_sin_PA8, function(it) {
    vals <- sapply(thresh_config, function(g) {
      if (it %in% rownames(g)) g[it, ] else rep(NA, ncol(g))
    })
    if (is.list(vals)) return(NA)
    max(apply(vals, 1, function(x) diff(range(x, na.rm = TRUE))), na.rm = TRUE)
  })
  
  diffs_sorted <- sort(diffs, decreasing = TRUE)
  cat("\n--- Maximum between-group threshold difference (per item) ---\n")
  print(round(diffs_sorted, 3))
  
  # Free items whose difference exceeds 0.30 (conservative empirical cut-off).
  items_no_inv_thresh <- names(diffs_sorted[diffs_sorted > 0.3])
  cat("\n--- Items flagged to be freed (diff > 0.30):\n")
  print(items_no_inv_thresh)
}

# -- Step 4: Partial threshold model ------------------------------------------
build_partial_thresholds <- function(items, data, group) {
  partials <- c()
  for (it in items) {
    n_cats   <- length(unique(na.omit(data[[it]])))
    n_thresh <- n_cats - 1
    partials <- c(partials, paste0(it, "|t", seq_len(n_thresh)))
  }
  partials
}

group_partial_thresh <- build_partial_thresholds(
  items = items_no_inv_thresh,
  data  = df_inv_univ,
  group = "universidad"
)

cat("\n--- Parameters freed in the partial model:\n")
print(group_partial_thresh)

syntax_thresh_partial <- as.character(
  measEq.syntax(
    configural.model = cincofactores_sinPA8,
    data             = df_inv_univ,
    ordered          = items_sin_PA8,
    parameterization = "theta",
    estimator        = "WLSMV",
    group            = "universidad",
    group.equal      = c("loadings", "thresholds"),
    group.partial    = group_partial_thresh
  )
)

fit_thresh_partial <- tryCatch(
  cfa(
    model            = syntax_thresh_partial,
    data             = df_inv_univ,
    group            = "universidad",
    ordered          = items_sin_PA8,
    estimator        = "WLSMV",
    parameterization = "theta",
    control          = list(iter.max = 1000, rel.tol = 1e-6),
    se               = "none"
  ),
  error = function(e) { message("Partial thresholds failed: ", e$message); NULL }
)

cat("\n--- Partial thresholds converged:",
    !is.null(fit_thresh_partial) && isTRUE(lavInspect(fit_thresh_partial, "converged")),
    "---\n")

# -- Step 5: Partial means model ----------------------------------------------
syntax_means_partial <- as.character(
  measEq.syntax(
    configural.model = cincofactores_sinPA8,
    data             = df_inv_univ,
    ordered          = items_sin_PA8,
    parameterization = "theta",
    estimator        = "WLSMV",
    group            = "universidad",
    group.equal      = c("loadings", "thresholds", "means"),
    group.partial    = group_partial_thresh
  )
)

fit_means_partial <- tryCatch(
  cfa(
    model            = syntax_means_partial,
    data             = df_inv_univ,
    group            = "universidad",
    ordered          = items_sin_PA8,
    estimator        = "WLSMV",
    parameterization = "theta",
    control          = list(iter.max = 1000, rel.tol = 1e-6),
    se               = "none"
  ),
  error = function(e) { message("Partial means failed: ", e$message); NULL }
)

cat("\n--- Partial means converged:",
    !is.null(fit_means_partial) && isTRUE(lavInspect(fit_means_partial, "converged")),
    "---\n")

# -- Step 6: Fit indices and LRT ----------------------------------------------
fits_univ_partial <- list(
  configural         = fit_config_univ,
  loadings           = fit_load_univ,
  thresholds_parcial = fit_thresh_partial,
  means_parcial      = fit_means_partial
)

converged_partial <- Filter(function(f) {
  !is.null(f) && isTRUE(lavInspect(f, "converged"))
}, fits_univ_partial)

cat("\n--- Converged models:", paste(names(converged_partial), collapse = ", "), "\n")

# Fit indices.
fit_indices_partial <- do.call(rbind, lapply(names(converged_partial), function(nm) {
  fm <- fitmeasures(converged_partial[[nm]], c("cfi.scaled", "rmsea.scaled", "srmr"))
  data.frame(
    model        = nm,
    cfi.scaled   = fm["cfi.scaled"],
    rmsea.scaled = fm["rmsea.scaled"],
    srmr         = fm["srmr"],
    row.names    = NULL
  )
}))

cat("\n=== FIT INDICES - partial invariance by university ===\n")
print(fit_indices_partial)

# Sequential LRT.
lrt_partial <- tryCatch(
  do.call(lavTestLRT, c(
    converged_partial,
    list(method = "satorra.2000", model.names = names(converged_partial))
  )),
  error = function(e) { message("LRT failed: ", e$message); NULL }
)

cat("\n=== LRT - partial invariance by university ===\n")
print(lrt_partial)

# -- Step 7: Delta-CFI and Delta-RMSEA (Cheung & Rensvold, 2002) --------------
cat("\n=== Delta-CFI and Delta-RMSEA between models ===\n")

if (nrow(fit_indices_partial) >= 2) {
  delta <- do.call(rbind, lapply(2:nrow(fit_indices_partial), function(i) {
    data.frame(
      comparacion = paste(fit_indices_partial$model[i - 1], "->",
                          fit_indices_partial$model[i]),
      delta_cfi   = round(fit_indices_partial$cfi.scaled[i] -
                            fit_indices_partial$cfi.scaled[i - 1], 4),
      delta_rmsea = round(fit_indices_partial$rmsea.scaled[i] -
                            fit_indices_partial$rmsea.scaled[i - 1], 4)
    )
  }))
  delta$cfi_ok   <- abs(delta$delta_cfi)   <= .010
  delta$rmsea_ok <- abs(delta$delta_rmsea) <= .015
  print(delta)
}


# =============================================================================
# 19. SESSION INFORMATION (REPRODUCIBILITY)
# =============================================================================

sessionInfo()

# =============================================================================
# END OF SCRIPT
# =============================================================================