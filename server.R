server <- function(input, output, session) {
    
  # Tabel Alokasi Petugas
    tabel_alokasi <- reactive(df_alokasi) |> bindCache(df_alokasi)
    tabel_identitas <- reactive(df_identitas) |> bindCache(df_identitas)
    tabel_progres <- reactive(df_progres) |> bindCache(df_progres)
    tabel_target <- reactive(target_pencacahan) |> bindCache(target_pencacahan)
    # kecamatan_tertinggi <- reactive()
    tabel_progres_kecamatan <- reactive(tabel_progres() |> 
      select(Kecamatan, Progres, `Progres Listing`) |> 
      group_by(Kecamatan) |> 
      summarise(Cacah = sum(Progres, na.rm = TRUE), 
                Listing = sum(`Progres Listing`, na.rm = TRUE), .groups = "drop") |> 
      pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
      mutate(Kecamatan = str_remove(Kecamatan, pattern = "\\d+") |> str_trim()) |> 
      mutate(Kecamatan = reorder(Kecamatan, desc(Nilai))))
    tabel_target_realisasi <- reactive(df_gabung)
    
    tabel_progres_desa <- reactive(
      tabel_progres() |> 
        select(Desa, Progres, `Progres Listing`) |> 
        group_by(Desa) |> 
        summarise(Cacah = sum(Progres, na.rm = TRUE), 
                  Listing = sum(`Progres Listing`, na.rm = TRUE),
                  .groups = "drop") |> 
        pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
        mutate(Desa = str_remove(Desa, pattern = "\\d+") |> str_trim()) |> 
        mutate(Desa = reorder(Desa, desc(Nilai)))
    )
    tabel_gabung_wide <- reactive(df_gabung_wide)
    tabel_target_sls <- reactive(sls_kecamatan)

    # tabel
    output$alokasi_petugas <- renderDataTable({
      tabel_alokasi()
    }, options = list(scrollX = TRUE))
    
    output$identitas_petugas_umkm <- renderDataTable({
      tabel_identitas()
    }, options = list(scrollX = TRUE))
    
    output$tabel_progres <- renderDataTable({
      tabel_progres()
      # names(tabel_progres()) <- c("Kecamatan", "Desa", "PML", "PPL", "SLS", "Tanggal", "Progres Cacah", "Progres Listing", "Selesai", "Kendala")
    }, options = list(
                                              # paging = TRUE,
                                              searching = TRUE,
                                              # fixedColumns = TRUE,
                                              autoWidth = TRUE,
                                              # ordering = TRUE,
                                              scrollX = TRUE,
                                              # dom = 'tB', 
                                              buttons = c('copy', 'csv', 'excel', 'pdf'),
                                              extensions = 'Buttons'
                                              )) 
    
    output$tabel_target <- renderDataTable({tabel_target()},
                                           options = list(scrollX = TRUE))
    
    output$tabel_target_realisasi <- render_gt({
      tabel_gabung_wide() |> 
        gt() |> 
        gt::sub_missing() |> 
        gt::tab_header(title = "Jumlah KK Target, Listing Bangunan, dan Pencacahan Unit Usaha PL-KUMKM 2023",
                       subtitle = paste('Berdasarkan Kondisi Pencacahan Tanggal', format(Sys.Date(), "%d/%m/%Y"))) |> 
        gt::cols_align(columns = c("kecamatan"), align = "left") |> 
        fmt_number(columns = c("Target KK", "Jumlah Cacah", "Jumlah Listing"), sep_mark = ".",
                   decimals = 0) |> 
        fmt_number(columns = c("Persentase Listing (%)"), decimals = 2, dec_mark = ",") |> 
        gtExtras::gt_theme_nytimes()
    }) |> bindCache(tabel_gabung_wide())
    
    # grafik
    output$plot_line_home <- renderPlotly(
      {plot_line}
    ) |> bindCache(plot_line)
    
    output$plot_pie_home <- renderPlotly(
      {plot_pie}
    ) |> bindCache(plot_pie)
    
    # Value Box
    output$prog_kab <- renderbs4ValueBox(
      {bs4ValueBox(value = progres_kabupaten, subtitle = "Kabupaten Kepulauan Tanimbar", width = 4, color = "teal", icon = icon("bar-chart"))}) |> 
      bindCache(progres_kabupaten)
    
    output$max_kec <- renderbs4ValueBox(
      {bs4ValueBox(value = kecamatan_tertinggi$Total, subtitle = paste("Progres Tertinggi Kecamatan", kecamatan_tertinggi$Kecamatan), 
                   width = 4, color = "warning", icon = icon("line-chart"))}
    ) |> bindCache(kecamatan_tertinggi)
    
    output$min_kec <- renderbs4ValueBox(
      {bs4ValueBox(value = kecamatan_terendah$Total, subtitle = paste("Progres Terendah Kecamatan", kecamatan_terendah$Kecamatan), 
                   width = 4, color = "danger", icon = icon("pie-chart"))}
    ) |> bindCache(kecamatan_terendah)
    
    #Graphics
    output$plot_kecamatan <- renderPlotly({
      tabel_progres_kecamatan() |> 
        plot_ly(x = ~Kecamatan, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
        add_bars() |> 
        layout(showlegend = TRUE,
               title = "Progres Pencacahan Usaha dan Listing Bangunan dan KK Menurut Kecamatan")
    }) |> bindCache(tabel_progres_kecamatan())
    
    output$plot_desa <- renderPlotly({
      if(input$pilih_kecamatan == "All") {
      tabel_progres_desa() |> 
        plot_ly(x = ~Desa, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
        add_bars() |> 
        layout(showlegend = TRUE,
               title = "Progres Pencacahan Unit Usaha dan Listing Bangunan dan KK Menurut Desa di KKT")
      } else {
        tabel_progres() |> 
          select(Kecamatan, Desa, Progres, `Progres Listing`) |> 
          mutate(Kecamatan = str_remove(Kecamatan, pattern = "\\d+") |> str_trim()) |> 
          filter(Kecamatan == input$pilih_kecamatan) |> 
          group_by(Desa) |> 
          summarise(Cacah = sum(Progres, na.rm = TRUE), 
                    Listing = sum(`Progres Listing`, na.rm = TRUE),
                    .groups = "drop") |> 
          pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
          mutate(Desa = str_remove(Desa, pattern = "\\d+") |> str_trim()) |> 
          mutate(Desa = reorder(Desa, desc(Nilai))) |> 
          plot_ly(x = ~Desa, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
          add_bars() |> 
          layout(showlegend = TRUE,
                 title = paste0("Progres Pencacahan Unit Usaha dan Listing Bangunan dan KK Menurut Desa di Kecamatan ", input$pilih_kecamatan))
      }
    }) |> bindCache(input$pilih_kecamatan)
    
    output$text_kecamatan <- renderText(input$pilih_kecamatan)
    observeEvent(input$pilih_tampilan_petugas,
                 {updateSelectInput(inputId = "pilih_kriteria", 
                                   choices = tabel_progres() |> select(all_of(input$pilih_tampilan_petugas)) |> distinct() |> pull()
                 )}
                 )
    output$plot_petugas <- renderPlotly({
      if(input$pilih_tampilan_petugas == "Kecamatan") {
        tabel_progres() |> 
          select(Kecamatan, PPL, Progres, `Progres Listing`) |> 
          filter(Kecamatan == input$pilih_kriteria) |> 
          group_by(PPL) |> 
          summarise(Cacah = sum(Progres, na.rm = TRUE), 
                    Listing = sum(`Progres Listing`, na.rm = TRUE),.groups = "drop") |> 
          pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
          mutate(PPL = reorder(PPL, desc(Nilai))) |> 
          plot_ly(x = ~PPL, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
          add_bars() |> 
          layout(showlegend = TRUE,
                 title = paste("Progres Pencacahan Lapangan PCL di Kecamatan", input$pilih_kriteria))
      } else {
        tabel_progres() |> 
          select(PML, PPL, Progres, `Progres Listing`) |> 
          filter(PML == input$pilih_kriteria) |> 
          group_by(PPL) |> 
          summarise(Cacah = sum(Progres, na.rm = TRUE), 
                    Listing = sum(`Progres Listing`, na.rm = TRUE),.groups = "drop") |> 
          pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
          mutate(PPL = reorder(PPL, desc(Nilai))) |> 
          plot_ly(x = ~PPL, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
          add_bars() |> 
          layout(showlegend = TRUE,
                 title = paste("Progres Pencacahan Lapangan PCL tim PML", input$pilih_kriteria))
      }
    }) |> bindCache(input$pilih_kriteria)
    
    observeEvent(input$pilih_tampilan_petugas2,
                 {updateSelectInput(inputId = "pilih_kriteria2", 
                                    choices = tabel_progres() |> select(all_of(input$pilih_tampilan_petugas2)) |> distinct() |> pull()
                 )}
    )
    
    output$plot_history <- renderPlotly({
      if (input$pilih_tampilan_petugas2 == "PML") {
        tabel_progres() |> 
          filter(PML == input$pilih_kriteria2) |>
          group_by(Tanggal) |> 
          summarise(Cacah = sum(Progres, na.rm = TRUE), 
                    Listing = sum(`Progres Listing`, na.rm = TRUE),.groups = "drop") |> 
          pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
          plot_ly(x = ~Tanggal, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
          add_lines() |> 
          layout(title = "History Progres Pencacahan Unit Usaha dan Listing Petugas")
      } else {
        tabel_progres() |> 
          # group_by(PML, Tanggal) |> 
          # summarise(Progres = sum(Progres, na.rm = TRUE), .groups = "drop") |> 
          filter(PPL == input$pilih_kriteria2) |> 
          group_by(Tanggal) |> 
          summarise(Cacah = sum(Progres, na.rm = TRUE), 
                    Listing = sum(`Progres Listing`, na.rm = TRUE),.groups = "drop") |> 
          pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
          plot_ly(x = ~Tanggal, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
          add_lines() |> 
          layout(title = "History Progres Pencacahan dan Listing Petugas")
      }
    }) |> bindCache(input$pilih_kriteria2)
    
    output$plot_sls_selesai <- renderPlotly({
      req(tabel_progres())
      tabel_progres() |> 
        mutate(Selesai2 = ifelse(is.na(Selesai), 0, 1),
               Kecamatan = str_remove(Kecamatan, "\\d+") |> str_trim()) |> 
        group_by(Kecamatan) |>
        summarise(sls_selesai = sum(Selesai2, na.rm = TRUE), .groups = "drop") |> 
        left_join(tabel_target_sls(), by = join_by("Kecamatan")) |> 
        pivot_longer(cols = -1, names_to = "Jenis", values_to = "Nilai") |> 
        mutate(Kecamatan = reorder(Kecamatan, desc(Nilai))) |> 
        plot_ly(x = ~Kecamatan, y = ~Nilai, color = ~Jenis, colors = c("#e98924", "#5A5A5A")) |> 
        add_bars() |> 
        layout(showlegend = TRUE)
    }) |> bindCache(tabel_progres())
    
    output$plot_target_realisasi <- renderPlotly({
      tabel_target_realisasi() |> 
        plot_ly(x =~kecamatan, y = ~Jumlah, color = ~Kategori ) |> 
        add_bars() |> 
        layout(title = "Jumlah KK Target, Listing Bangunan dan Pencacahan Unit Usaha PL-KUMKM 2023 KKT Menurut Kecamatan")
    }) |> bindCache(tabel_target_realisasi())

}