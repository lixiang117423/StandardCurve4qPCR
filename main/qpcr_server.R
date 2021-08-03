# 提交成功显示
observeEvent(input$submit_qpcr, {
  if (input$submit_qpcr > 0) {
    sendSweetAlert(
      session = session,
      title = "提交成功!",
      text = "数据上传成功，参数设置正确",
      type = "success"
    )
  }
})
# 导入数据
# 运行正常20210111
user_data_qpcr <- reactive({
  table_in_test <- data.table::fread(input$data_input_qpcr$datapath,
    header = TRUE,
    encoding = "UTF-8"
  )
})

# 展示统计分析结果
output$taboutput <- renderDataTable(df_table_qpcr(),
  options = list(
    pageLength = 15
  )
)

df_table_qpcr <- eventReactive(input$submit_qpcr, {
  if (input$submit_qpcr > 0) {
    df <- user_data_qpcr()

    df <- df %>%
      dplyr::select(Position, Cq) %>%
      dplyr::mutate(
        P = stringr::str_sub(Position, 1, 1),
        N = stringr::str_sub(Position, 2, nchar(Position))
      ) %>%
      dplyr::select(N, P, Cq) %>%
      reshape2::dcast(P ~ N) %>%
      dplyr::select(as.character(1:12)) %>%
      dplyr::mutate(group = paste0("C", 1:8)) %>%
      dplyr::mutate(relativa.conc = c(
        1 * (1 / input$dilution)^0,
        1 * (1 / input$dilution)^1,
        1 * (1 / input$dilution)^2,
        1 * (1 / input$dilution)^3,
        1 * (1 / input$dilution)^4,
        1 * (1 / input$dilution)^5,
        1 * (1 / input$dilution)^6,
        1 * (1 / input$dilution)^7
      )) %>%
      dplyr::mutate(temp.conc = log(relativa.conc, base = 2)) %>%
      dplyr::mutate(Relative.Conc = temp.conc + max(abs(temp.conc))) %>%
      dplyr::select(1:12, 16)


    min1 <- min(min(df[2:7, 1]), min(df[2:7, 2]), min(df[2:7, 3]), min(df[2:7, 4]))
    max1 <- max(max(df[2:7, 1]), max(df[2:7, 2]), max(df[2:7, 3]), max(df[2:7, 4]))

    min2 <- min(min(df[2:7, 5]), min(df[2:7, 6]), min(df[2:7, 7]), min(df[2:7, 8]))
    max2 <- max(max(df[2:7, 5]), max(df[2:7, 6]), max(df[2:7, 7]), max(df[2:7, 8]))

    min3 <- min(min(df[2:7, 9]), min(df[2:7, 10]), min(df[2:7, 11]), min(df[2:7, 12]))
    max3 <- max(max(df[2:7, 9]), max(df[2:7, 10]), max(df[2:7, 11]), max(df[2:7, 12]))

    df.max.min <- data.frame(
      Gene.name = c(
        input$gene_name_1,
        input$gene_name_2,
        input$gene_name_3
      ),
      Min = c(min1, min2, min3),
      Max = c(max1, max2, max3)
    )



    df.gene.1 <- df[2:7, c(13, 1:4)]
    mean.value.1 <- c()
    for (i in 1:6) {
      mean.value.temp <- mean(as.numeric(df.gene.1[i, 2:5]))
      mean.value.1 <- c(mean.value.1, mean.value.temp)
    }
    df.gene.1$Mean <- mean.value.1
    df.gene.1$SD <- apply(df.gene.1[, 2:5], 1, sd)


    df.gene.2 <- df[2:7, c(13, 5:8)]
    mean.value.2 <- c()
    for (i in 1:6) {
      mean.value.temp <- mean(as.numeric(df.gene.2[i, 2:5]))
      mean.value.2 <- c(mean.value.2, mean.value.temp)
    }
    df.gene.2$Mean <- mean.value.2
    df.gene.2$SD <- apply(df.gene.2[, 2:5], 1, sd)



    df.gene.3 <- df[2:7, c(13, 9:12)]
    mean.value.3 <- c()
    for (i in 1:6) {
      mean.value.temp <- mean(as.numeric(df.gene.3[i, 2:5]))
      mean.value.3 <- c(mean.value.3, mean.value.temp)
    }
    df.gene.3$Mean <- mean.value.3
    df.gene.3$SD <- apply(df.gene.3[, 2:5], 1, sd)


    df.gene.1 <- df.gene.1 %>%
      reshape2::melt(id.vars = c(1, 6:7)) %>%
      dplyr::select(-4) %>%
      dplyr::rename(Cq = value)

    df.gene.2 <- df.gene.2 %>%
      reshape2::melt(id.vars = c(1, 6:7)) %>%
      dplyr::select(-4) %>%
      dplyr::rename(Cq = value)

    df.gene.3 <- df.gene.3 %>%
      reshape2::melt(id.vars = c(1, 6:7)) %>%
      dplyr::select(-4) %>%
      dplyr::rename(Cq = value)


    # 构建模型
    # gene1
    fit <- lm(Relative.Conc ~ Mean, data = df.gene.1)
    intercept <- fit[["coefficients"]][["(Intercept)"]] %>%
      round(2)
    slope <- fit[["coefficients"]][["Mean"]] %>%
      round(2)

    formula <- paste0("RC = ", intercept, slope, " x Cq")

    r.2 <- broom::glance(fit)[1, 1] %>%
      round(4) %>%
      as.numeric()

    p.value <- broom::glance(fit)[1, 5] %>%
      round(5) %>%
      as.numeric()

    df.temp.1 <- data.frame(
      Gene.name = input$gene_name_1,
      Date = as.character(Sys.Date()),
      # Description = "",
      Formula = formula,
      Slope = slope,
      Intercept = intercept,
      R2 = r.2,
      P.Value = p.value,
      Other = ""
    )

    # gene2
    fit <- lm(Relative.Conc ~ Mean, data = df.gene.2)
    intercept <- fit[["coefficients"]][["(Intercept)"]] %>%
      round(2)
    slope <- fit[["coefficients"]][["Mean"]] %>%
      round(2)

    formula <- paste0("RC = ", intercept, slope, " x Cq")

    r.2 <- broom::glance(fit)[1, 1] %>%
      round(4) %>%
      as.numeric()

    p.value <- broom::glance(fit)[1, 5] %>%
      round(5) %>%
      as.numeric()

    df.temp.2 <- data.frame(
      Gene.name = input$gene_name_2,
      Date = as.character(Sys.Date()),
      # Description = "",
      Formula = formula,
      Slope = slope,
      Intercept = intercept,
      R2 = r.2,
      P.Value = p.value,
      Other = ""
    )

    # gene3
    fit <- lm(Relative.Conc ~ Mean, data = df.gene.3)
    intercept <- fit[["coefficients"]][["(Intercept)"]] %>%
      round(2)
    slope <- fit[["coefficients"]][["Mean"]] %>%
      round(2)

    formula <- paste0("RC = ", intercept, slope, " x Cq")

    r.2 <- broom::glance(fit)[1, 1] %>%
      round(4) %>%
      as.numeric()

    p.value <- broom::glance(fit)[1, 5] %>%
      round(5) %>%
      as.numeric()

    df.temp.3 <- data.frame(
      Gene.name = input$gene_name_3,
      Date = as.character(Sys.Date()),
      # Description = "",
      Formula = formula,
      Slope = slope,
      Intercept = intercept,
      R2 = r.2,
      P.Value = p.value,
      Other = ""
    )
    # 合并数据
    res <- rbind(df.temp.1, df.temp.2, df.temp.3) %>%
      as.data.frame() %>%
      merge(df.max.min, by = "Gene.name")
    res <- res[!duplicated(res$Gene.name), ] %>%
      dplyr::select(Gene.name, Date, Formula, Slope, Intercept, R2, P.Value, Max, Min, Other)

    res.1 <- res %>%
      dplyr::filter(Gene.name == input$gene_name_1)
    res.2 <- res %>%
      dplyr::filter(Gene.name == input$gene_name_2)
    res.3 <- res %>%
      dplyr::filter(Gene.name == input$gene_name_3)

    res <- rbind(res.1, res.2, res.3)

    # 保存分析结果
    if (input$qpcr_stat_res_filetype == ".xlsx") {
      xlsx::write.xlsx(res, file = "./results/rest_tab.xlsx", row.names = FALSE)
    } else if (input$qpcr_stat_res_filetype == ".csv") {
      write.csv(res, file = "./results/rest_tab.csv", row.names = FALSE, quote = FALSE)
    } else {
      write.table(res, file = "./results/rest_tab.txt", row.names = FALSE, quote = FALSE)
    }
  }
  res # 返回要展示的结果
})

################################################################################
output$plot4qPCR <- renderPlot(plot_table_qpcr(),
  options = list(
    pageLength = 15
  )
)

plot_table_qpcr <- eventReactive(input$submit_qpcr, {
  if (input$submit_qpcr > 0) {
    df <- user_data_qpcr()

    df <- df %>%
      dplyr::select(Position, Cq) %>%
      dplyr::mutate(
        P = stringr::str_sub(Position, 1, 1),
        N = stringr::str_sub(Position, 2, nchar(Position))
      ) %>%
      dplyr::select(N, P, Cq) %>%
      reshape2::dcast(P ~ N) %>%
      dplyr::select(as.character(1:12)) %>%
      dplyr::mutate(group = paste0("C", 1:8)) %>%
      dplyr::mutate(relativa.conc = c(
        1 * (1 / input$dilution)^0,
        1 * (1 / input$dilution)^1,
        1 * (1 / input$dilution)^2,
        1 * (1 / input$dilution)^3,
        1 * (1 / input$dilution)^4,
        1 * (1 / input$dilution)^5,
        1 * (1 / input$dilution)^6,
        1 * (1 / input$dilution)^7
      )) %>%
      dplyr::mutate(temp.conc = log(relativa.conc, base = 2)) %>%
      dplyr::mutate(Relative.Conc = temp.conc + max(abs(temp.conc))) %>%
      dplyr::select(1:12, 16)


    df.gene.1 <- df[2:7, c(13, 1:4)]
    mean.value.1 <- c()
    for (i in 1:6) {
      mean.value.temp <- mean(as.numeric(df.gene.1[i, 2:5]))
      mean.value.1 <- c(mean.value.1, mean.value.temp)
    }
    df.gene.1$Mean <- mean.value.1
    df.gene.1$SD <- apply(df.gene.1[, 2:5], 1, sd)


    df.gene.2 <- df[2:7, c(13, 5:8)]
    mean.value.2 <- c()
    for (i in 1:6) {
      mean.value.temp <- mean(as.numeric(df.gene.2[i, 2:5]))
      mean.value.2 <- c(mean.value.2, mean.value.temp)
    }
    df.gene.2$Mean <- mean.value.2
    df.gene.2$SD <- apply(df.gene.2[, 2:5], 1, sd)



    df.gene.3 <- df[2:7, c(13, 9:12)]
    mean.value.3 <- c()
    for (i in 1:6) {
      mean.value.temp <- mean(as.numeric(df.gene.3[i, 2:5]))
      mean.value.3 <- c(mean.value.3, mean.value.temp)
    }
    df.gene.3$Mean <- mean.value.3
    df.gene.3$SD <- apply(df.gene.3[, 2:5], 1, sd)


    df.gene.1 <- df.gene.1 %>%
      reshape2::melt(id.vars = c(1, 6:7)) %>%
      dplyr::select(-4) %>%
      dplyr::rename(Cq = value)

    df.gene.2 <- df.gene.2 %>%
      reshape2::melt(id.vars = c(1, 6:7)) %>%
      dplyr::select(-4) %>%
      dplyr::rename(Cq = value)

    df.gene.3 <- df.gene.3 %>%
      reshape2::melt(id.vars = c(1, 6:7)) %>%
      dplyr::select(-4) %>%
      dplyr::rename(Cq = value)


    # 绘图
    # gene1
    p.1 <- ggplot(df.gene.1, aes(Mean, Relative.Conc)) +
      # geom_errorbar(aes(ymin = Mean - SD,ymax = Mean + SD),width = 0.3) +
      geom_smooth(
        formula = y ~ x,
        method = "lm",
        se = TRUE, colour = "black", span = 0.8
      ) +
      geom_point() +
      stat_poly_eq(aes(label = paste(..eq.label..,
        # ..adj.rr.label..,
        ..rr.label..,
        ..p.value.label..,
        sep = "~~~~"
      )),
      formula = y ~ x,
      parse = T,
      rr.digits = 4,
      coef.digits = 3,
      label.x = c(0.05),
      label.y = c(0.03)
      ) +
      # geom_vline(xintercept = max(df.gene.1$Relative.Conc) + 0.5, color = "white") +
      labs(title = input$gene_name_1, y = "Relative.Conc (log2)", x = "Cq") +
      scale_y_continuous(breaks = round(seq(
        min(df.gene.1$Relative.Conc),
        max(df.gene.1$Relative.Conc), 1
      ), 2)) +
      scale_x_continuous(breaks = round(seq(
        min(df.gene.1$Mean),
        max(df.gene.1$Mean) + 1, 1
      ), 1)) +
      theme_prism(base_size = 10)
    p.1


    # gene2
    p.2 <- ggplot(df.gene.2, aes(Mean, Relative.Conc)) +
      # geom_errorbar(aes(ymin = Mean - SD,ymax = Mean + SD),width = 0.3) +
      geom_smooth(
        formula = y ~ x,
        method = "lm",
        se = TRUE, colour = "black", span = 0.8
      ) +
      geom_point() +
      stat_poly_eq(aes(label = paste(..eq.label..,
        # ..adj.rr.label..,
        ..rr.label..,
        ..p.value.label..,
        sep = "~~~~"
      )),
      formula = y ~ x,
      parse = T,
      rr.digits = 4,
      coef.digits = 3,
      label.x = c(0.05),
      label.y = c(0.03)
      ) +
      # geom_vline(xintercept = max(df.gene.1$Relative.Conc) + 0.5, color = "white") +
      labs(title = input$gene_name_2, y = "Relative.Conc (log2)", x = "Cq") +
      scale_y_continuous(breaks = round(seq(
        min(df.gene.2$Relative.Conc),
        max(df.gene.3$Relative.Conc), 1
      ), 2)) +
      scale_x_continuous(breaks = round(seq(
        min(df.gene.2$Mean),
        max(df.gene.2$Mean) + 1, 1
      ), 1)) +
      theme_prism(base_size = 10)
    p.2

    # gene3
    p.3 <- ggplot(df.gene.3, aes(Mean, Relative.Conc)) +
      # geom_errorbar(aes(ymin = Mean - SD,ymax = Mean + SD),width = 0.3) +
      geom_smooth(
        formula = y ~ x,
        method = "lm",
        se = TRUE, colour = "black", span = 0.8
      ) +
      geom_point() +
      stat_poly_eq(aes(label = paste(..eq.label..,
        # ..adj.rr.label..,
        ..rr.label..,
        ..p.value.label..,
        sep = "~~~~"
      )),
      formula = y ~ x,
      parse = T,
      rr.digits = 4,
      coef.digits = 3,
      label.x = c(0.05),
      label.y = c(0.03)
      ) +
      # geom_vline(xintercept = max(df.gene.1$Relative.Conc) + 0.5, color = "white") +
      labs(title = input$gene_name_3, y = "Relative.Conc (log2)", x = "Cq") +
      scale_y_continuous(breaks = round(seq(
        min(df.gene.3$Relative.Conc),
        max(df.gene.3$Relative.Conc), 1
      ), 2)) +
      scale_x_continuous(breaks = round(seq(
        min(df.gene.3$Mean),
        max(df.gene.3$Mean) + 1, 1
      ), 1)) +
      theme_prism(base_size = 10)
    p.3


    # 合并图片
    p1 <- p.1 + p.2 + p.3 + plot_layout(ncol = 3) # for 展示
    p2 <- p.1 + p.2 + p.3 + plot_layout(ncol = 1) # for 保存

    # 保存分析结果
    if (input$qpcr_figure_filetype == ".pdf") {
      export::graph2pdf(p1, file = "./results/res.pdf", width = 15, height = 5)
    } else if (input$qpcr_figure_filetype == ".png") {
      export::graph2png(p1, file = "./results/res.png", width = 15, height = 5, dpi = 500)
    } else if (input$qpcr_figure_filetype == ".jpg") {
      export::graph2jpg(p1, file = "./results/res.jpg", width = 15, height = 5, dpi = 500)
    } else {
      export::graph2tif(p1, file = "./results/res.tiff", width = 15, height = 5, dpi = 500)
    }
    # 保存分析结果结束
  }
  p2 # 返回要展示的结果
})


# 下载分析结果
# 运行正常20210111
output$table_download <- downloadHandler(
  filename <- function() {
    paste(stringr::str_sub(
      input$data_input_qpcr$name,
      1,
      (nchar(input$data_input_qpcr$name) - 4)
    ),
    "_分析结果", input$qpcr_stat_res_filetype,
    sep = ""
    )
  },
  content <- function(file) {
    if (input$qpcr_stat_res_filetype == ".csv") {
      file.copy("./results/rest_tab.csv", file)
    } else if (input$qpcr_stat_res_filetype == ".txt") {
      file.copy("./results/rest_tab.txt", file)
    } else {
      # return(NULL)
      file.copy("./results/rest_tab.xlsx", file)
    }
  }
)



output$figure_download <- downloadHandler(
  filename <- function() {
    paste(stringr::str_sub(
      input$data_input_qpcr$name,
      1,
      (nchar(input$data_input_qpcr$name) - 4)
    ),
    "_绘图结果", input$qpcr_figure_filetype,
    sep = ""
    )
  },
  content <- function(file) {
    if (input$qpcr_figure_filetype == ".pdf") {
      file.copy("./results/res.pdf", file)
    } else if (input$qpcr_figure_filetype == ".png") {
      file.copy("./results/res.png", file)
    } else if (input$qpcr_figure_filetype == ".jpg") {
      file.copy("./results/res.jpg", file)
    } else {
      file.copy("./results/res.tiff", file)
    }
  }
)