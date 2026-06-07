library(dplyr)
library(ggplot2)

#memahami dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")
head(data)
str(data)
dim(data)

#statistika deskriptif
data_num <- data[sapply(data, is.numeric)]
summary(data_num)

statistik <- data.frame(
  Mean    = sapply(data_num, mean, na.rm = TRUE),
  Min     = sapply(data_num, min, na.rm = TRUE),
  Q1      = sapply(data_num, quantile, probs = 0.25, na.rm =TRUE),
  Median  = sapply(data_num, median, na.rm = TRUE),
  Q3      = sapply(data_num, quantile, probs = 0.75, na.rm =TRUE),
  Max     = sapply(data_num, max, na.rm = TRUE),
  SD      = sapply(data_num, sd, na.rm = TRUE),
  Varians = sapply(data_num, var, na.rm = TRUE)
)
statistik <- round(statistik, 0)
table <- format(statistik, scientific = FALSE, trim = TRUE, nsmall = 0)
table

#analisis missing value
total_missing <- sum(is.na(data_num))
missing_value <- data.frame(
  Missing = colSums(is.na(data_num))
)
total_missing
missing_value

for(i in names(data_num)){
  data[[i]][is.na(data[[i]])] <- median(data[[i]], na.rm = TRUE)
}

total_missing
missing_value

#analisis outliner
boxplot(data$pdrb_perkapita,
        main  = "PDRB per Kapita",
        ylab  = "Nominal (Rp)",
        col = "forestgreen")

boxplot(data$kemiskinan,
        main  = "Kemiskinan",
        ylab  = "Persentase (%)",
        col = "firebrick")

boxplot(data$pengangguran,
        main  = "Pengangguran",
        ylab  = "Persentase (%)",
        col = "tomato")

boxplot(data$ipm,
        main  = "Indeks Pembangunan Manusia",
        ylab  = "Nilai",
        col = "mediumpurple")

boxplot(data$harapan_hidup,
        main  = "Angka Harapan Hidup",
        ylab  = "Tahun",
        col = "lightcoral")

boxplot(data$rata_lama_sekolah,
        main  = "Rata-rata Jumlah Tahun Pendidikan",
        ylab  = "Tahun",
        col = "steelblue")

boxplot(data$akses_internet,
        main  = "Akses Internet",
        ylab  = "Persentase (%)",
        col = "darkorange")

boxplot(data$jalan_baik,
        main  = "Akses Jalan Baik",
        ylab  = "Persentase (%)",
        col = "slategrey")

boxplot(data$air_bersih,
        main  = "Akses Air Bersih",
        ylab  = "Persentase (%)",
        col = "dodgerblue")

sapply(data_num, hitung_outlier)

hitung_outlier <- function(x){
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  IQR_value <- IQR(x, na.rm = TRUE)
  
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  sum(x < lower | x > upper, na.rm = TRUE)
}

iqr_pdrb <- IQR(data$pdrb_perkapita)
batas_bawah <- quantile(data$pdrb_perkapita, 0.25) - (1.5 * iqr_pdrb)
batas_atas <- quantile(data$pdrb_perkapita, 0.75) + (1.5 * iqr_pdrb)
data <- data %>%
  filter(pdrb_perkapita >= batas_bawah & pdrb_perkapita <= batas_atas)
