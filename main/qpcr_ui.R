# 定义t-test选项卡开始----------------------------------------------------------
fluidRow(
  # 使用box后就不能再用column
  # 第一个box
  column(
    width = 2,
    box(
      width = NULL,
      height = 920,
      title = "数据上传与主要设置",
      status = "danger",
      solidHeader = TRUE,
      collapsible = TRUE,
      background = "navy",
      # "随便打的文本", # 直接插入文本
      # br(), # 换行符
      # 上传数据
      fileInput("data_input_qpcr",
        label = h4("上传数据"),
        accept = ".csv",
        buttonLabel = "浏览..."
      ),
      # 下载示例数据
      # downloadLink('download_demo_data_qpcr',label = h4('下载示例数据')),
      # br(),
      # br(),
      # 输入基因名称
      textInput("gene_name_1",
        label = h4("请输入基因1名称")
      ),
      textInput("gene_name_2",
        label = h4("请输入基因2名称")
      ),
      textInput("gene_name_3",
        label = h4("请输入基因3名称")
      ),

      # 稀释倍数
      sliderInput("dilution",
        label = h4("稀释倍数"),
        min = 1, max = 10,
        step = 1, value = 4
      ),
      # 文件下载格式
      selectInput("qpcr_stat_res_filetype",
        label = h4("选择数据下载格式"),
        choices = list(
          "Excel文件" = ".xlsx",
          "txt文件" = ".txt",
          "csv文件" = ".csv"
        ),
        selected = ".xlsx"
      ),
      # 文件下载格式
      selectInput("qpcr_figure_filetype",
        label = h4("选择图片下载格式"),
        choices = list(
          "PDF文件" = ".pdf",
          "png文件" = ".png",
          "jpg(jpeg)文件" = ".jpg",
          "tiff文件" = ".tiff"
        ),
        selected = ".png"
      ),

      # 点击提交
      actionButton("submit_qpcr",
        label = h4("点击提交"),
        width = 230,
        icon = NULL
      ),
      br(),
      br(),
      # 下载结果

      fluidRow(
        column(
          width = 2,
          downloadButton("table_download",
            label = h4("下载结果"),
            width = 230
          )
        ),
        column(
          width = 2,
          offset = 4, # 偏移量
          downloadButton("figure_download",
            label = h4("下载图片"),
            width = 230
          )
        )
      )
    )
  ), # 第一个box完结

  # 第二列
  column(
    width = 10,
    # 第二列上面的box展示统计分析结果
    box(
      width = NULL,
      height = 500,
      title = "标准曲线",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      background = NULL,
      # "随便打的文本", # 直接插入文本
      br(),
      plotOutput("plot4qPCR")
    ),
    box(
      width = NULL,
      height = 400,
      title = "计算结果",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      background = NULL,
      # "随便打的文本", # 直接插入文本
      br(),
      dataTableOutput("taboutput")
    )
  )
)