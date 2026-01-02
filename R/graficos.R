#' Narrow Boxplot for Paired Data
#'
#' Creates a narrow boxplot suitable for publications.
#'
#' @param var1 First variable
#' @param var2 Second variable
#' @param alpha Significance level
#' @param p_value p-value to display
#' @param lang Language for labels ("en" or "pt")
#' @export
plot_boxplot_estreito <- function(var1, var2, alpha = 0.05, p_value = NULL, lang = "en") {

  textos <- if (lang == "pt") {
    list(
      titulo = "Comparacao de Medias",
      eixo_y = "Valores",
      grupo1 = "Variavel 1",
      grupo2 = "Variavel 2",
      valor_p = "Valor-p = ",
      intervalo = sprintf("IC %.0f%%", (1 - alpha) * 100)
    )
  } else {
    list(
      titulo = "Means Comparison",
      eixo_y = "Values",
      grupo1 = "Variable 1",
      grupo2 = "Variable 2",
      valor_p = "p-value = ",
      intervalo = sprintf("%.0f%% CI", (1 - alpha) * 100)
    )
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mar = c(4, 5, 4, 2), mgp = c(3, 0.7, 0))

  dados <- list(var1, var2)
  names(dados) <- c(textos$grupo1, textos$grupo2)

  graphics::boxplot(
    dados,
    ylab = textos$eixo_y,
    main = textos$titulo,
    col = c("#4E79A7", "#F28E2B"),
    border = "black",
    boxwex = 0.25,
    staplewex = 0.4,
    whisklty = 1,
    outcol = c("#4E79A7", "#F28E2B"),
    outpch = 16,
    outcex = 0.7,
    medcol = "white",
    medlwd = 1.8,
    lwd = 1.3,
    cex.lab = 1.1,
    cex.main = 1.2,
    frame.plot = FALSE
  )

  # Preserve RNG state, do not change user's seed permanently
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  }

  set.seed(123)

  graphics::points(jitter(rep(1, length(var1)), 0.1), var1,
                   pch = 16, col = grDevices::adjustcolor("#4E79A7", 0.5), cex = 0.8
  )
  graphics::points(jitter(rep(2, length(var2)), 0.1), var2,
                   pch = 16, col = grDevices::adjustcolor("#F28E2B", 0.5), cex = 0.8
  )

  graphics::points(1:2, c(mean(var1), mean(var2)),
                   pch = 18, col = "red", cex = 1.8, lwd = 2
  )

  if (!is.null(p_value)) {
    p_text <- if (p_value < 0.001) {
      paste0(textos$valor_p, "< 0.001")
    } else {
      paste0(textos$valor_p, format.pval(p_value, digits = 3))
    }

    usr <- graphics::par("usr")
    graphics::text(1.5, usr[4] - 0.05 * (usr[4] - usr[3]), p_text,
                   col = "black", cex = 1, font = 2
    )
  }
}

#' Paired Profile Plot
#'
#' Creates a profile plot connecting paired observations.
#'
#' @param var1 First variable
#' @param var2 Second variable
#' @param lang Language for labels ("en" or "pt")
#' @export
grafico_perfil_pareado <- function(var1, var2, lang = "en") {

  textos <- if (lang == "pt") {
    list(
      titulo = "Perfil das Observacoes Pareadas",
      eixo_x = "Variaveis",
      eixo_y = "Valores",
      grupo1 = "Variavel 1",
      grupo2 = "Variavel 2",
      leg_obs = "Observacoes",
      leg_mean = "Media"
    )
  } else {
    list(
      titulo = "Paired Observations Profile",
      eixo_x = "Variables",
      eixo_y = "Values",
      grupo1 = "Variable 1",
      grupo2 = "Variable 2",
      leg_obs = "Observations",
      leg_mean = "Mean"
    )
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mar = c(5, 5, 4, 2))

  n <- length(var1)
  cores <- grDevices::hcl.colors(n, "Set 3")

  graphics::plot(1, type = "n",
                 xlim = c(0.5, 2.5),
                 ylim = range(c(var1, var2)),
                 xlab = textos$eixo_x,
                 ylab = textos$eixo_y,
                 main = textos$titulo,
                 xaxt = "n"
  )

  graphics::axis(1, at = 1:2, labels = c(textos$grupo1, textos$grupo2))

  for (i in seq_len(n)) {
    graphics::lines(c(1, 2), c(var1[i], var2[i]),
                    col = cores[i], lwd = 1, lty = 2
    )
  }

  graphics::points(rep(1, n), var1, pch = 19, col = cores, cex = 1.2)
  graphics::points(rep(2, n), var2, pch = 19, col = cores, cex = 1.2)

  graphics::points(1, mean(var1), pch = 17, col = "red", cex = 2, lwd = 2)
  graphics::points(2, mean(var2), pch = 17, col = "red", cex = 2, lwd = 2)

  graphics::legend("topright",
                   legend = c(textos$leg_obs, textos$leg_mean),
                   pch = c(19, 17),
                   col = c("gray", "red"),
                   bty = "n"
  )
}
