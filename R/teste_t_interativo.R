#' Interactive Paired t-Test
#'
#' Guides the user through a paired t-test with bilingual support
#' and optional graphical output.
#'
#' @param lang Interface language ("auto", "en", "pt")
#' @param plot Logical, whether to display plot
#' @param plot_type Type of plot: "boxplot" or "profile"
#' @return Invisibly returns test results
#' @export
#'
#' @examples
#' \dontrun{
#' teste_t_interativo()
#' }
teste_t_interativo <- function(lang = "auto", plot = TRUE, plot_type = "boxplot") {

  # Manage language option
  if (lang != "auto") {
    old_lang <- getOption("LearnStats.lang")
    set_language(lang)
    on.exit(options(LearnStats.lang = old_lang), add = TRUE)
  }

  current_lang <- get_language()

  msgs <- list(
    pt = list(
      titulo = "TESTE t PAREADO - MODO INTERATIVO",
      etapa1 = "ETAPA 1: Entrada de dados da primeira variavel",
      instrucao1 = "Digite os valores separados por virgula (use ponto decimal).\nExemplo: 4.05, 3.69, 4.10, 4.70",
      prompt_var1 = "Primeira variavel: ",
      ok_var1 = "[OK] Primeira variavel registrada: ",
      observacoes = " observacoes",
      valores = "Valores: ",
      erro_vazio = "Entrada vazia. Digite valores separados por virgula.",
      erro_invalido = "Entrada invalida. Use apenas numeros separados por virgula, com ponto decimal.",
      etapa2 = "ETAPA 2: Entrada de dados da segunda variavel",
      instrucao2 = "Digite os valores separados por virgula (use ponto decimal).\nIMPORTANTE: Deve ter o mesmo numero de observacoes",
      prompt_var2 = "Segunda variavel: ",
      ok_var2 = "[OK] Segunda variavel registrada: ",
      erro_tamanho = "As variaveis devem ter o mesmo numero de observacoes.",
      etapa3 = "ETAPA 3: Definicao do nivel de significancia",
      instrucao3 = "Para 95% de confianca, digite: 0.05\nPara 99% de confianca, digite: 0.01\nPara 90% de confianca, digite: 0.10",
      prompt_alpha = "Nivel de significancia (alpha): ",
      erro_alpha = "Alpha deve estar entre 0 e 1 (exemplo: 0.05).",
      ok_confianca = "[OK] Nivel de confianca: ",
      ok_alpha = "[OK] Nivel de significancia (alpha): ",
      percentual = "%",
      executando = "EXECUTANDO ANALISE...",
      resultado_titulo = "RESULTADO DO TESTE t PAREADO",
      estatisticas = "ESTATISTICAS DESCRITIVAS:",
      media_var1 = "Media Variavel 1: ",
      media_var2 = "Media Variavel 2: ",
      diferenca_media = "Diferenca media: ",
      resultado_teste = "RESULTADO DO TESTE:",
      estatistica_t = "Estatistica t = ",
      graus_liberdade = "Graus de liberdade = ",
      valor_p = "Valor-p = ",
      intervalo_confianca = "INTERVALO DE CONFIANCA:",
      interpretacao = "INTERPRETACAO:",
      rejeita_h0 = "[REJEITA H0] Ha diferenca estatistica significativa",
      aceita_h0 = "[ACEITA H0] Nao ha diferenca estatistica significativa",
      diferenca_texto = "--> As medias das duas variaveis sao estatisticamente diferentes",
      nao_diferenca_texto = "--> Nao ha evidencias de diferenca entre as medias"
    ),
    en = list(
      titulo = "PAIRED t-TEST - INTERACTIVE MODE",
      etapa1 = "STEP 1: First variable data entry",
      instrucao1 = "Enter values separated by comma (use decimal point).\nExample: 4.05, 3.69, 4.10, 4.70",
      prompt_var1 = "First variable: ",
      ok_var1 = "[OK] First variable recorded: ",
      observacoes = " observations",
      valores = "Values: ",
      erro_vazio = "Empty input. Please enter values separated by comma.",
      erro_invalido = "Invalid input. Use only numbers separated by comma, with decimal point.",
      etapa2 = "STEP 2: Second variable data entry",
      instrucao2 = "Enter values separated by comma (use decimal point).\nIMPORTANT: Must have the same number of observations",
      prompt_var2 = "Second variable: ",
      ok_var2 = "[OK] Second variable recorded: ",
      erro_tamanho = "Variables must have the same number of observations.",
      etapa3 = "STEP 3: Significance level definition",
      instrucao3 = "For 95% confidence, enter: 0.05\nFor 99% confidence, enter: 0.01\nFor 90% confidence, enter: 0.10",
      prompt_alpha = "Significance level (alpha): ",
      erro_alpha = "Alpha must be between 0 and 1 (example: 0.05).",
      ok_confianca = "[OK] Confidence level: ",
      ok_alpha = "[OK] Significance level (alpha): ",
      percentual = "%",
      executando = "EXECUTING ANALYSIS...",
      resultado_titulo = "PAIRED t-TEST RESULT",
      estatisticas = "DESCRIPTIVE STATISTICS:",
      media_var1 = "Mean Variable 1: ",
      media_var2 = "Mean Variable 2: ",
      diferenca_media = "Mean difference: ",
      resultado_teste = "TEST RESULT:",
      estatistica_t = "t statistic = ",
      graus_liberdade = "Degrees of freedom = ",
      valor_p = "p-value = ",
      intervalo_confianca = "CONFIDENCE INTERVAL:",
      interpretacao = "INTERPRETATION:",
      rejeita_h0 = "[REJECT H0] There is statistically significant difference",
      aceita_h0 = "[ACCEPT H0] No statistically significant difference",
      diferenca_texto = "--> The means of the two variables are statistically different",
      nao_diferenca_texto = "--> No evidence of difference between means"
    )
  )

  txt <- msgs[[current_lang]]
  if (is.null(txt)) txt <- msgs[["en"]]

  .parse_numeric <- function(input) {
    input <- trimws(input)
    if (nchar(input) == 0) return(numeric(0))
    parts <- trimws(unlist(strsplit(input, ",")))
    out <- suppressWarnings(as.numeric(parts))
    out
  }

  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("          ", txt$titulo, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Step 1
  cat(txt$etapa1, "\n")
  cat(txt$instrucao1, "\n\n")

  var1 <- numeric(0)
  repeat {
    entrada1 <- readline(prompt = txt$prompt_var1)
    var1 <- .parse_numeric(entrada1)
    if (length(var1) == 0) {
      cat("\n", txt$erro_vazio, "\n\n", sep = "")
      next
    }
    if (any(is.na(var1))) {
      cat("\n", txt$erro_invalido, "\n\n", sep = "")
      next
    }
    break
  }

  cat("\n", txt$ok_var1, length(var1), txt$observacoes, "\n", sep = "")
  cat("  ", txt$valores, paste(round(var1, 4), collapse = ", "), "\n\n", sep = "")

  # Step 2
  cat(txt$etapa2, "\n")
  cat(txt$instrucao2, " (", length(var1), ")\n\n", sep = "")

  var2 <- numeric(0)
  repeat {
    entrada2 <- readline(prompt = txt$prompt_var2)
    var2 <- .parse_numeric(entrada2)
    if (length(var2) == 0) {
      cat("\n", txt$erro_vazio, "\n\n", sep = "")
      next
    }
    if (any(is.na(var2))) {
      cat("\n", txt$erro_invalido, "\n\n", sep = "")
      next
    }
    if (length(var2) != length(var1)) {
      cat("\n", txt$erro_tamanho, "\n\n", sep = "")
      next
    }
    break
  }

  cat("\n", txt$ok_var2, length(var2), txt$observacoes, "\n", sep = "")
  cat("  ", txt$valores, paste(round(var2, 4), collapse = ", "), "\n\n", sep = "")

  # Step 3
  cat(txt$etapa3, "\n")
  cat(txt$instrucao3, "\n\n")

  alpha <- NA_real_
  repeat {
    entrada_alpha <- readline(prompt = txt$prompt_alpha)
    a <- suppressWarnings(as.numeric(trimws(entrada_alpha)))
    if (is.na(a) || a <= 0 || a >= 1) {
      cat("\n", txt$erro_alpha, "\n\n", sep = "")
      next
    }
    alpha <- a
    break
  }

  conf <- (1 - alpha) * 100
  cat("\n", txt$ok_confianca, sprintf("%.1f", conf), txt$percentual, "\n", sep = "")
  cat(txt$ok_alpha, alpha, "\n\n", sep = "")

  # Run test
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("          ", txt$executando, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  resultado <- stats::t.test(var1, var2, paired = TRUE, conf.level = 1 - alpha)

  # Output
  cat("========== ", txt$resultado_titulo, " ==========\n\n", sep = "")
  cat(txt$estatisticas, "\n")
  cat("  ", txt$media_var1, round(mean(var1), 4), "\n", sep = "")
  cat("  ", txt$media_var2, round(mean(var2), 4), "\n", sep = "")
  cat("  ", txt$diferenca_media, round(mean(var1 - var2), 4), "\n\n", sep = "")

  cat(txt$resultado_teste, "\n")
  cat("  ", txt$estatistica_t, round(as.numeric(resultado$statistic), 4), "\n", sep = "")
  cat("  ", txt$graus_liberdade, as.numeric(resultado$parameter), "\n", sep = "")
  cat("  ", txt$valor_p, format.pval(resultado$p.value, digits = 4), "\n\n", sep = "")

  cat(txt$intervalo_confianca, " (", sprintf("%.1f", conf), "%):\n", sep = "")
  cat("  [", round(resultado$conf.int[1], 4), " ; ", round(resultado$conf.int[2], 4), "]\n\n", sep = "")

  cat(txt$interpretacao, "\n")
  if (resultado$p.value < alpha) {
    cat("  ", txt$rejeita_h0, "\n  ", txt$diferenca_texto, "\n\n", sep = "")
  } else {
    cat("  ", txt$aceita_h0, "\n  ", txt$nao_diferenca_texto, "\n\n", sep = "")
  }

  # Optional plot
  if (isTRUE(plot)) {
    if (plot_type == "boxplot") {
      plot_boxplot_estreito(
        var1, var2,
        alpha = alpha,
        p_value = resultado$p.value,
        lang = current_lang
      )
    } else if (plot_type == "profile") {
      grafico_perfil_pareado(var1, var2, lang = current_lang)
    }
  }

  invisible(resultado)
}
