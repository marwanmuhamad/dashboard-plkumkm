library(tidyverse)
library(janitor)
library(plotly)

link <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSi4sLGhR1QfJlC73_wK5H8aLl788z6L7lcWy0c9CPGOj3y3ZWg9P3fOb36rMyTSjJwkV4c7G4FFkAh/pub?output=csv"

link2 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRTu8GHJtUoqgAq1NDq5-mIE-tVQdcuLJ7PijURXmbZYdWdvhOmZ3ucdJWrdi4jdGZ_GjvzfaTptYOl/pub?output=csv"
link3 <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTyyHspn4GF1qAVBTrk8vmwd0L-psp1YSWytiyIJ-X2joiNqoPQJcDxP0VWThKJFUHCF4uR7hUcJ0wL/pub?output=csv"

df_alokasi <- read_csv(link)
df_identitas <- read_csv(link2)
df_progres <- read_csv(link3) |> 
  clean_names() |> 
  mutate(pml = paste0(pml_4, pml_7, pml_10, pml_13, pml_16, pml_19, pml_22, pml_25,pml_28, pml_31),
         ppl = paste0(ppl_5, ppl_8, ppl_11, ppl_14, ppl_17, ppl_20, ppl_23, ppl_26, ppl_29, ppl_32),
         desa = paste0(desa_3, desa_6, desa_9, desa_12, desa_15, desa_18, desa_21, desa_24, desa_27, desa_30)) |> 
  select(kecamatan, desa, pml, ppl, sls, tanggal_cacah, jumlah_cacah, jumlah_listing, selesai_cacah_sls_ini, kendala) |> #timestamp, 
  mutate(desa = str_remove_all(desa, patter = "NA"),
         pml = str_remove_all(pml, pattern = "NA"),
         ppl = str_remove_all(ppl, pattern = "NA"))

names(df_progres) <- c("Kecamatan", "Desa", "PML", "PPL", "SLS", "Tanggal", "Progres", "Progres Listing", "Selesai", "Kendala")
# df_progres |> select(contains("ppl"))

plot_line <- df_progres |> 
  group_by(Tanggal) |> 
  summarise(`Cacah` = sum(Progres), 
            `Listing` = sum(`Progres Listing`), .groups = "drop") |>
  pivot_longer(cols = -1, names_to = "Progres", values_to = "Nilai") |> 
  plot_ly(x = ~Tanggal, y = ~Nilai, color = ~Progres) |> 
  add_lines() |> 
  layout(legend = list(orientation = "h", valign = "bottom", xanchor = "center"), showlegend = FALSE)

plot_pie <- df_progres |> 
  group_by(Kecamatan) |> 
  summarise(Total = sum(Progres), .groups = "drop") |> 
  mutate(Kecamatan = str_remove(Kecamatan, pattern = "\\d+") |> str_trim()) |> 
  plot_ly() |> 
  add_pie(values = ~Total, labels = ~Kecamatan, hole = 0.7) |> 
  layout(showlegend = FALSE)

progres_kabupaten <- sum(df_progres$Progres)
kecamatan_tertinggi <- df_progres |> 
  group_by(Kecamatan) |> 
  summarise(Total = sum(Progres), .groups = "drop") |> 
  filter(Total == max(Total)) |> 
  mutate(Kecamatan = str_remove(Kecamatan, pattern = "\\d+") |> str_trim())

kecamatan_terendah <- df_progres |> 
  group_by(Kecamatan) |> 
  summarise(Total = sum(Progres), .groups = "drop") |> 
  filter(Total == min(Total)) |> 
  mutate(Kecamatan = str_remove(Kecamatan, pattern = "\\d+") |> str_trim())

kecamatan_terendah <- kecamatan_terendah[1, ]

target_pencacahan <- df_alokasi |> 
  group_by(PPL) |> 
  summarise(jumlah_kk = sum(JKK), .groups = "drop") |> 
  mutate(target_harian = ceiling(jumlah_kk/23)) |> 
  mutate(PPL = str_to_title(PPL))
names(target_pencacahan) <- c("PPL", "Jumlah KK", "Target Listing Harian")

df_target_realisasi <- df_progres |> 
  select(Kecamatan, Progres, `Progres Listing`) |> 
  mutate(Kecamatan = str_remove(Kecamatan, "\\d+") |> str_trim()) |> 
  group_by(Kecamatan) |>  
  summarise(`Jumlah Cacah` = sum(`Progres`, na.rm = TRUE),
            `Jumlah Listing` = sum(`Progres Listing`, na.rm = TRUE)) |> 
  mutate(Kecamatan = case_when(
    Kecamatan == "Wermaktian" ~ "Wer Maktian",
    Kecamatan == "Wertamrian" ~ "Wer Tamrian",
    Kecamatan == "Wuarlabobar" ~ "Wuar Labobar",
    TRUE ~ Kecamatan
  ))

df_target <- df_alokasi |> 
  select(nmkec, JKK) |> 
  mutate(nmkec = str_to_title(nmkec)) |> 
  rename(kecamatan = nmkec) |> 
  group_by(kecamatan) |> 
  summarise(`Target KK` = sum(JKK, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(`Target KK`)) |> 
  mutate(kecamatan = reorder(kecamatan, desc(`Target KK`)))

df_gabung <- df_target |> left_join(df_target_realisasi, by = join_by(kecamatan == Kecamatan)) |> 
  pivot_longer(cols = -1, names_to = "Kategori", values_to = "Jumlah") |> 
  mutate(kecamatan = factor(kecamatan, levels = df_target$kecamatan))

df_gabung_wide <- df_gabung |> 
  pivot_wider(id_cols = kecamatan, names_from = "Kategori", values_from = "Jumlah") |> 
  mutate(`Persentase Listing (%)` = round((`Jumlah Listing`/`Target KK`)*100,2))

df_total <- tibble(
  kecamatan = c("Total"),
  `Target KK` = sum(df_gabung_wide$`Target KK`),
  `Jumlah Cacah` = sum(df_gabung_wide$`Jumlah Cacah`),
  `Jumlah Listing` = sum(df_gabung_wide$`Jumlah Listing`),
  `Persentase Listing (%)` = round((`Jumlah Listing`/`Target KK`)*100, 2)
  )

df_gabung_wide <- rbind(df_gabung_wide, df_total)

sls_kecamatan <- df_alokasi |> 
  group_by(nmkec) |> 
  summarise(target_sls = n(), .groups = "drop") |> 
  rename(Kecamatan = nmkec) |> 
  mutate(Kecamatan = str_to_title(Kecamatan)) |> 
  mutate(Kecamatan = case_when(
    Kecamatan == "Wer Maktian" ~ "Wermaktian",
    Kecamatan == "Wer Tamrian" ~ "Wertamrian",
    Kecamatan == "Wuar Labobar" ~ "Wuarlabobar",
    TRUE ~ Kecamatan
  ))
