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

data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

IQR_PDRB <- IQR(data$pdrb_perkapita, na.rm = TRUE)
pdrb_bawah <- quantile(data$pdrb_perkapita, 0.25, na.rm = TRUE)-(1.5*IQR_PDRB)
pdrb_atas  <- quantile(data$pdrb_perkapita, 0.75, na.rm = TRUE)+(1.5*IQR_PDRB)
outlier_pdrb <- data$pdrb_perkapita[which(data$pdrb_perkapita < pdrb_bawah | data$pdrb_perkapita > pdrb_atas)]
length(outlier_pdrb)
outlier_pdrb

outliner_wilayah_pdrb <- data[which(data$pdrb_perkapita < pdrb_bawah | data$pdrb_perkapita > pdrb_atas), ]
outliner_wilayah_pdrb[, c("wilayah",
                          "provinsi",
                          "tahun",
                          "pdrb_perkapita")]

IQR_Kemiskinan <- IQR(data$kemiskinan, na.rm = TRUE)
miskin_bawah <- quantile(data$kemiskinan, 0.25, na.rm = TRUE)-(1.5*IQR_Kemiskinan)
miskin_atas <- quantile(data$kemiskinan, 0.75, na.rm = TRUE)+(1.5*IQR_Kemiskinan)
outliner_kemiskinan <- data$kemiskinan[which(data$kemiskinan < miskin_bawah | data$kemiskinan > miskin_atas)]
length(outliner_kemiskinan)
outliner_kemiskinan

outliner_wilayah_kemiskinan <- data[which(data$kemiskinan < miskin_bawah | data$kemiskinan > miskin_atas), ]
outliner_wilayah_kemiskinan[, c("wilayah",
                                "provinsi",
                                "tahun",
                                "kemiskinan")]

IQR_pengangguran <- IQR(data$pengangguran, na.rm = TRUE)
nganggur_bawah <- quantile(data$pengangguran, 0.25, na.rm = TRUE)-(1.5*IQR_pengangguran)
nganggur_atas <- quantile(data$pengangguran, 0.75, na.rm = TRUE)+(1.5*IQR_pengangguran)
outliner_pengangguran <- data$pengangguran[which(data$pengangguran < nganggur_bawah | data$pengangguran > nganggur_atas)]
length(outliner_pengangguran)
outliner_pengangguran

outliner_wilayah_pengangguran <- data[which(data$pengangguran < nganggur_bawah | data$pengangguran > nganggur_atas), ]
outliner_wilayah_pengangguran[, c("wilayah",
                                  "provinsi",
                                  "tahun",
                                  "pengangguran")]

#visualisasi data
#Histogram
ggplot(data, aes(x = pdrb_perkapita)) +
  geom_histogram(bins = 30,
                 fill = "forestgreen",
                 color = "black") +
  labs(title = "Distribusi PDRB Per Kapita",
       x = "PDRB Per Kapita (Rp)",
       y = "Frekuensi"
  ) +
  theme_minimal()

ggplot(data, aes(x = kemiskinan)) +
  geom_histogram(bins = 24,
                 fill = "tomato",
                 color = "black") +
  labs(title = "Distribusi Tingkat Kemiskinan",
       x = "Persentase Kemiskinan (%)",
       y = "Frekuensi"
  ) +
  theme_minimal()

ggplot(data,
       aes(x = kemiskinan,
           y = pengangguran)) +
  geom_point(color = "blue") +
  labs(
    title = "Hubungan Kemiskinan dan Pengangguran",
    x = "Kemiskinan (%)",
    y = "Pengangguran (%)"
  ) +
  theme_minimal()

rata_rata <- data.frame(
  Variabel = c("Kemiskinan",
               "Pengangguran",
               "IPM",
               "Harapan Hidup",
               "Internet",
               "Jalan Baik",
               "Air Bersih"),
  Mean = c(
    mean(data$kemiskinan, na.rm = TRUE),
    mean(data$pengangguran, na.rm = TRUE),
    mean(data$ipm, na.rm = TRUE),
    mean(data$harapan_hidup, na.rm = TRUE),
    mean(data$akses_internet, na.rm = TRUE),
    mean(data$jalan_baik, na.rm = TRUE),
    mean(data$air_bersih, na.rm = TRUE)
  )
)

ggplot(rata_rata,
       aes(x = Variabel, y = Mean)) +
  geom_bar(stat = "identity",
           fill = "steelblue") +
  labs(title = "Rata-rata Indikator Pembangunan",
       x = "Variabel",
       y = "Nilai Rata-rata"
  ) +
  theme_minimal()

status <- as.data.frame(table(data$catatan_data))
status$Persentase <- round((status$Freq / sum(status$Freq)) * 100, 1)
status$Label <- paste0(status$Persentase, "%")

ggplot(status,
       aes(x = "",
           y = Freq,
           fill = Var1)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "white") +
  coord_polar("y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 5) +
  labs(
    title = "Proporsi Status Data",
    fill = "Keterangan"
  ) +
  theme_void()

#analisis probabilitas dan distribusi data

shapiro.test(data$pdrb_perkapita)
shapiro.test(data$kemiskinan)
shapiro.test(data$pengangguran)

data_num <- data[sapply(data, is.numeric)]

hasil_normalitas <- data.frame(
  Variabel = names(data_num),
  P_Value = sapply(data_num, function(x)
    shapiro.test(na.omit(x))$p.value)
)

hasil_normalitas

# Q-Q Plot Kemiskinan

qqnorm(data$kemiskinan, main = "Q-Q Plot Uji Normalitas Kemiskinan")
qqline(data$kemiskinan, col = "red")


# PROBABILITAS AKSES INTERNET > 50%

mean_internet <- mean(data$akses_internet, na.rm = TRUE)
sd_internet <- sd(data$akses_internet,na.rm = TRUE)
jumlah_diatas50 <- sum(data$akses_internet > 50,na.rm = TRUE)
total_wilayah <- sum(!is.na(data$akses_internet))
prob_internet <- jumlah_diatas50 / total_wilayah
persentase <- prob_internet * 100

cat("Rata-rata Akses Internet :", round(mean_internet, 2), "\n")
cat("Jumlah Wilayah di Atas 50% :", jumlah_diatas50, "\n")
cat("Total Wilayah :", total_wilayah, "\n")
cat("Probabilitas :", round(prob_internet,4), "\n")
cat("Persentase :", round(persentase,2), "%")

ggplot(data, aes(x = akses_internet)) +
  geom_histogram(
    bins = 20,
    fill = "skyblue",
    color = "black"
  ) +
  geom_vline(
    xintercept = 50,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Distribusi Akses Internet",
    x = "Akses Internet (%)",
    y = "Frekuensi"
  ) +
  theme_minimal()


# PROBABILITAS AKSES AIR BERSIH DI ATAS RATA-RATA

mean_air_bersih <- mean(data$air_bersih,na.rm = TRUE)
jumlah_diatas_rata <- sum(data$air_bersih > mean_air_bersih,na.rm = TRUE)
total_wilayah <- sum(!is.na(data$air_bersih))
prob_air_bersih <- jumlah_diatas_rata / total_wilayah
persen_prob <- prob_air_bersih * 100

cat("Rata-rata Akses Air Bersih :", round(mean_air_bersih,2), "\n")
cat("Jumlah Wilayah di Atas Rata-rata :", jumlah_diatas_rata, "\n")
cat("Total Wilayah :", total_wilayah, "\n")
cat("Probabilitas :", round(prob_air_bersih,4), "\n")
cat("Persentase :", round(persen_prob,2), "%")

ggplot(data, aes(x = air_bersih)) +
  geom_histogram(
    bins = 20,
    fill = "skyblue",
    color = "black"
  ) +
  geom_vline(
    xintercept = mean_air_bersih,
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Distribusi Akses Air Bersih",
    x = "Akses Air Bersih (%)",
    y = "Frekuensi"
  ) +
  theme_minimal()

#analisis korelasi

ggplot(data,
       aes(x = pdrb_perkapita,
           y = ipm)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red") +
  labs(
    title = "Hubungan PDRB Per Kapita dan IPM",
    x = "PDRB Per Kapita",
    y = "IPM"
  ) +
  theme_minimal()
