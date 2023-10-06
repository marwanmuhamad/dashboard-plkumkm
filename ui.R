library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bs4Dash)
# library(reactable)
library(plotly)
library(DT)
library(gt)
library(gtExtras)

title <- tags$a(href = "www.google.com",
                tags$img(source = "www/PL-KUMKM.png", height = 50, width = 80), 
                "PL-KUMKM 2023 KKT")
ui <- dashboardPage(
  title = "pl-kumkmkkt2023",
  scrollToTop = TRUE,
  # options = list(sidebarExpandOnHover = TRUE),
  footer = dashboardFooter(left = tagList("designed by Marwan with ", icon("heart")), right = format(Sys.Date(), "%Y")),
    dashboardHeader(title = img(src = "PL-KUMKM.png"), skin = "light"), #span(tagList(img("PL-KUMKM.png"),
    dashboardSidebar(skin = "light", status = "warning",
      sidebarMenu(
        menuItem("Beranda", icon = icon("home"), tabName = "beranda"),
        menuItem("Tabel Alokasi", icon = icon("table"), tabName = "tabel"),
        menuItem("Grafik Progres", icon = icon("bar-chart"), tabName = "grafik")
      )
    ),
    dashboardBody(
      tabItems(
      tabItem(tabName = "beranda",
              h3("Selamat Datang di Dashboard PL-KUMKM 2023 KKT"),
              hr(),
              fluidRow(
                bs4ValueBoxOutput("prog_kab"),
                bs4ValueBoxOutput("max_kec"),
                bs4ValueBoxOutput("min_kec")
              ),
              fluidRow(
                box(width = 8, title = "Progres Pencacahan Kabupaten",
                    plotlyOutput("plot_line_home")), #line chart
                box(width = 4, title = "Persentase Kecamatan",
                    plotlyOutput("plot_pie_home")#,
                    # tags$p("*Persentase masing - masing kecamatan dihitung berdasarkan total dari capaian harian")
                    ) #pie chart
              )
              
              ),
      tabItem(tabName = "tabel",
              tabsetPanel(
                tabPanel(title = "Alokasi Petugas",
                         # hr(),
                         br(),
                         fluidRow(
                           box(width = 12, title = "Alokasi Petugas PL-KUMKM", 
                               dataTableOutput("alokasi_petugas"))
                         )
                         ),
                tabPanel(title = "Identitas Petugas",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Identitas Petugas PL-KUMKM",
                               dataTableOutput("identitas_petugas_umkm"))
                         )
                         ),
                tabPanel(title = "Progres Lapangan",
                         br(),
                         fluidRow(
                           box(title = "Progres Pencacahan dan Kendala Lapangan", width = 12,
                               dataTableOutput("tabel_progres"))
                         )),
                tabPanel(title = "Target Pencacahan Harian",
                         br(),
                         box(
                           width = 12,
                           title = "Target Harian",
                           dataTableOutput("tabel_target")
                         )
                         ),
                tabPanel(title = "Target dan Realisasi Kecamatan",
                         br(),
                         box(
                           width = 12,
                           title = "Target KK, Realisasi Listing dan Pencacahan Unit Usaha Menurut Kecamatan",
                           gt_output("tabel_target_realisasi")#,
                           # shiny::actionButton("download", label = 'Download')
                         )
                         )
              )
              ),
      tabItem(tabName = "grafik",
              tabsetPanel(
                tabPanel(title = "Progres Kecamatan",
                         br(),
                         box(width = 12,
                             title = "Progres Pencacahan Kecamatan",
                             plotlyOutput("plot_kecamatan")
                             )
                             ),
                tabPanel(title = "Progres Desa",
                         # br(),
                         selectInput("pilih_kecamatan", label = "Tampilkan Kecamatan",
                                     choices = c("All", "Tanimbar Selatan", "Wertamrian", "Wermaktian",
                                                 "Selaru", "Tanimbar Utara", "Fordata", "Wuarlabobar",
                                                 "Kormomolin", "Nirunmas", "Molu Maru")),
                         box(width = 12,
                             title = "Progres Pencacahan Desa",
                             plotlyOutput("plot_desa")
                             )
                         ),
                tabPanel(title = "Progres Petugas",
                         fluidRow(
                           column(width = 4, selectInput("pilih_tampilan_petugas", label = "Tampilkan Berdasarkan", choices = c("Kecamatan", "PML"))),
                           column(width = 4, selectInput("pilih_kriteria", label = "Pilih Kategori", choices = c("Kecamatan", "PML")))
                         ),
                         box(width = 12,
                             title = "Progres Pencacahan PCL",
                             plotlyOutput("plot_petugas")
                         )
                         ),
                tabPanel(title = "History Pencacahan",
                        fluidRow(column(width = 4, selectInput("pilih_tampilan_petugas2", label = "Tampilkan Berdasarkan", choices = c("PPL", "PML"))),
                                 column(width = 4, selectInput("pilih_kriteria2", label = "Pilih Kategori", choices = c("Kecamatan", "PML"), multiple = FALSE))
                                 ),
                         box(width = 12,
                             title = "History Pencacahan Petugas",
                             plotlyOutput("plot_history")
                             
                             )
                         ),
                tabPanel(title = "SLS Selesai",
                         br(),
                         box(width = 12,
                             title = "Jumlah SLS Selesai Cacah Menurut Kecamatan",
                             plotlyOutput("plot_sls_selesai")
                         )
              ),
              tabPanel(title = "Target dan Realisasi Kecamatan",
                       br(),
                       box(width = 12,
                           title = "Target dan Realisasi",
                           plotlyOutput("plot_target_realisasi")
                           )
                       )
              ))
    ))
)

