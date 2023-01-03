#-------------------------------------TUGAS DATA PREPROCESSING MENGGUNAKAN DATA KAGGLE------------------------------------
# Sumber: https://www.kaggle.com/datasets/mirichoi0218/insurance

# Tentang Dataset
# age     : age of primary beneficiary
# sex     : insurance contractor gender, female, male
# bmi     : Body mass index, providing an understanding of body, weights that are relatively high or low relative to height,
#           objective index of body weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5 to 24.9
# children: Number of children covered by health insurance / Number of dependents
# smoker  : Smoking
# region  : the beneficiary's residential area in the US, northeast, southeast, southwest, northwest.
# charges : Individual medical costs billed by health insurance

#=======================================================PPT 1=================================================

# Import Dataset
df_raw = read.csv(file='D:/Kuliah/Semester 1/Data Mining dan Business Intelligence/Pertemuan 4/Tugas/insurance.csv')

# Print 5 dataset teratas
head(df_raw,5)

# Print 5 dataset terakhir
tail(df_raw,5)

# Print dimensi dataset
dim(df_raw)

# Struktur dataset
str(df_raw)

# Analisis deskriptif dataset
summary(df_raw)

#---------------------------------------------DATA PREPROCESSING-----------------------------------------------------------
# Drop kolom non parametrik
df_dc = df_raw[c(-2,-5,-6)]
head(df_dc)

# Menghitung rata-rata per kolom
df_mc = colMeans(df_dc)
df_mc

# Menghitung rata-rata per baris
df_mr = rowMeans(df_dc)
head(df_mr, 5) # Rata-rata 5 baris teratas

# Membuat tabel berisikan variabel parametrik
age = c(df_raw$age)
bmi = c(df_raw$bmi)
children = c(df_raw$children)
charges = c(df_raw$charges)

df_raw_cbind = cbind(age,bmi,children,charges)
df_raw_cbind = as.data.frame(df_raw_cbind)
head(df_raw_cbind)

# Mengurutkan berdasarkan nilai baris
df_sr = apply(df_raw_cbind,2,sort)
head(df_sr)

# Mengurutkan berdasarkan nilai kolom 
head(df_raw_cbind)
df_sc = apply(head(df_raw_cbind),1,sort)
head(df_sc)


# Melihat subset data
df_raw_cbind[1:6,1:2]

head(df_raw_cbind[df_raw_cbind$children>1, 1:3])

summary(df_raw_cbind)
head(df_raw_cbind[df_raw_cbind$children==5, 1:3])


head(subset(df_raw_cbind, df_raw_cbind$age >51))

#SCALLING
#Deklarasi untuk fungsi normalisasi z
normalisasi=function(x){
  xm=colMeans(x) # Mencari means per kolom 
  jb=dim(x)[1] # untuk kolom 1 dimensi ada berapa
  xc=x-rep(xm,each=jb) #rep = repetition (mengulang, ), xm = rata-rata
  sdx=apply(x,2,sd) # mencari standar deviasi
  xstd=xc/rep(sdx,each=jb) # x nya dibagi standar deviasi
  output=data.frame(xstd)
  #output=data.frame(xstd,x)
  return(output)
}

# Normalisasi
df_cbind_norm = normalisasi(df_raw_cbind)  
head(df_cbind_norm)

#==================================================PPT 2======================================================
# Membuat dataframe
Namakonsumen <- c("Abi", "Cidi") 
Alamat <- c("Jalan Babarsari 43", "Jalan Babarsari 44") 
Notelp <- c(08111111111, NA)
Totalpembelian <- c(20000,30000)
dt <- data.frame(Namakonsumen, Alamat, Notelp, Totalpembelian)

# Cek Null 
is.na(dt)
sum(is.na(dt)) # banyak missing data
mean(is.na(dt)) # rata-rata missing data 
                   
# Membuat dataframe
Namakonsumen <- c("Abi", "Cidi") 
Alamat <- c("Jalan Babarsari 43", "Jalan Babarsari 44") 
Notelp <- c(08111111111, 999)
Totalpembelian <- c(20000,30000)
MissingData <- data.frame(Namakonsumen, Alamat, Notelp, Totalpembelian)

# Mengganti 999 dengan NA
MissingData$Notelp[MissingData$Notelp == 999]<- NA
MissingData

# Menghapus baris yang memiliki NA
na.omit(MissingData)

#---------------------------------------Latihan 1 dengan data kaggle---------------------------------------
df_raw_na_mean = df_raw_cbind
head(df_raw_na_mean)

# Membuat missing value berdasarkan kolom children
df_raw_na_mean$children_na = df_raw_na_mean$children
df_raw_na_mean$children_na[df_raw_na_mean$children_na==0]<- NA
df_na_mean = df_raw_na_mean

# Menghitung jumlah missing value
sum(is.na(df_na_mean))
sum(is.na(df_na_mean$children_na))

# Menghitung rata rata kolom dengan missing value NA dan tidak
colMeans(df_na_mean[c("children", "children_na")])

#---------------------------------------Latihan 2 dengan data kaggle---------------------------------------
df_raw_enc = df_raw

# Encoding kolom smoker
df_raw_enc$smoker = factor(df_raw_enc$smoker, levels = c("yes","no"), labels = c(1,0))
df_enc = df_raw_enc
head(df_enc)

# Tipe data kolom smoker setelah encoding
typeof(df_enc$smoker)

#--------------------------------Latihan 3 membuat dataframe training dan test--------------------------------
df_raw_train_test = df_cbind_norm # Melanjutkan dari data hasil normalisasi

# Set index dan proporsi data training dan test
index = 1:nrow(df_raw_train_test)
testindex = sample(index, 0.2*length(index)) # Proporsi test dataframe sebesar 20%, dan train dataset 80%

# Membuat data train dan test berdasarkan index
testset = df_raw_train_test[testindex,]
trainset = df_raw_train_test[-testindex,]

# Perbandingan jumlah baris dataframe 
count(df_raw_train_test)
count(testset)
count(trainset)