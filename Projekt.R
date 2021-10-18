
# EELT√ñ√ñ
library(ggplot2)
library(dplyr) 
# Loen andmed sisse ja asendan k√µik t√ºhjad lahtrid NA-ga
df <- read.csv("https://raw.githubusercontent.com/markovkristo/RProject/master/Filmid.csv", header = T, na.strings = c("","NA"))

# Eemaldan ebavajalikud veerud. Mby peaks eemaldama kohe ka k√µik NA-d
df <- subset(df, select = -c(X))
#df <- na.omit(df)




# H1: Disney+ edastab k√µige v√§hem t√§iskasvanutele m√µeldud sisuga filme.

# Eraldan vajalikud andmed, et kontrollida seda h√ºpoteesi. J√§tan alles ainult read, kus filmi vanusepiirang on 18+
valik1 <- df %>% select(Age, Netflix, Hulu, Prime.Video) %>% filter(Age == "18+")

# Loen kokku, mitu t√§isealistele m√µeldud filmi igal firmal on.
netflix1 <- valik1 %>% filter(Netflix == 1) %>% count(Netflix)
hulu1 <- valik1 %>% filter(Hulu == 1) %>% count(Hulu)
amazon1 <- valik1 %>% filter(Prime.Video == 1) %>% count(Prime.Video)

# Moodustan uue data frame saadud andmetest, et oleks lihtsam joonist teha.
firmad <- c("Netflix", "Hulu", "Prime Video")
sagedus <- c(netflix1$n, hulu1$n, amazon1$n)
taiskasvanuteleMoeldud <- data.frame(firmad, sagedus)

# Joonistan tulemused v√§lja
h1 <- ggplot(taiskasvanuteleMoeldud, aes(x = firmad, y = sagedus, fill = firmad)) + geom_col() 



# H2: Netflixi filmivaliku keskmine rating on suurem kui muudel vaatamisplatvormidel.
# Eraldan vajalikud andmed, et kontrollida h¸poteesi. J‰tan alles ainult "rating" read.
valik2 <- df %>% select(IMDb, Rotten.Tomatoes, Netflix, Hulu, Prime.Video, Disney.)

# Eemaldan "rating"-utelt vırdlusm‰‰rad. (x/10 .. x/100)
valik2$IMDb = substr(valik2$IMDb,1,3)
valik2$Rotten.Tomatoes = substr(valik2$Rotten.Tomatoes,1,2)

# Muudan "rating" v‰‰rtused int-t¸¸biks
valik2$IMDb = as.double(valik2$IMDb)
valik2$IMDb = valik2$IMDb*10
valik2$Rotten.Tomatoes = as.double(valik2$Rotten.Tomatoes)

# Eraldan filmid platvormi j‰rgi
netflix2 <- valik2 %>% filter(Netflix == 1)
hulu2 <- valik2 %>% filter(Hulu == 1)
amazon2 <- valik2 %>% filter(Prime.Video == 1)
disney2 <- valik2 %>% filter(Disney. == 1)

# Leian keskmise ratingu
IMDb_mean_netflix = mean(netflix2$IMDb,trim=0.5, na.rm=TRUE)
Rotten.Tomates_mean_netflix = mean(netflix2$Rotten.Tomatoes, trim=0.5, na.rm=TRUE)
Total_rating_netflix = (IMDb_mean_netflix + Rotten.Tomates_mean_netflix ) / 2

IMDb_mean_hulu = mean(hulu2$IMDb,trim=0.5, na.rm=TRUE)
Rotten.Tomates_mean_hulu = mean(hulu2$Rotten.Tomatoes, trim=0.5, na.rm=TRUE)
Total_rating_hulu = (IMDb_mean_hulu + Rotten.Tomates_mean_hulu ) / 2

IMDb_mean_amazon = mean(amazon2$IMDb,trim=0.5, na.rm=TRUE)
Rotten.Tomates_mean_amazon = mean(amazon2$Rotten.Tomatoes, trim=0.5, na.rm=TRUE)
Total_rating_amazon = (IMDb_mean_amazon + Rotten.Tomates_mean_amazon ) / 2

IMDb_mean_disney = mean(disney2$IMDb,trim=0.5, na.rm=TRUE)
Rotten.Tomates_mean_disney = mean(2$Rotten.Tomatoes, trim=0.5, na.rm=TRUE)
Total_rating_disney = (IMDb_mean_disney + Rotten.Tomates_mean_disney ) / 2

firmad2 <- c("Netflix IMDb", "Netlfix Rotten Tomatoes", "Hulu IMDb", "Hulu Rotten Tomatoes", "Prime Video IMDb", "Prime Video Rotten Tomatoes", "Disney+ IMDb", "Disney+ Rotten Tomatoes")
firmad2_total <- c("Netflix", "Hulu", "Prime Video", "Disney+")
ratingud2 <- c(IMDb_mean_netflix,Rotten.Tomates_mean_netflix,IMDb_mean_hulu,Rotten.Tomates_mean_hulu,IMDb_mean_amazon, Rotten.Tomates_mean_amazon, IMDb_mean_disney,Rotten.Tomates_mean_disney)
total_ratings <- c(Total_rating_netflix, Total_rating_hulu, Total_rating_amazon, Total_rating_disney)

rating <- data.frame(firmad2, ratingud2)
total_ratings_df <- data.frame(firmad2_total, total_ratings )
h2 <- ggplot(rating, aes(x= firmad2, y=ratingud2, fill=firmad2)) + geom_col() + scale_fill_manual("Platvormid", values = c("Netflix IMDb"="red","Netflix Rotten Tomatoes" = "red", "Hulu IMDb" = "green", "Hulu Rotten Tomatoes" = "green", "Prime Video IMDb" = "orange", "Prime Video Rotten Tomatoes" = "orange", "Disney+ IMDb" = "skyblue", "Disney+ Rotten Tomatoes" = "skyblue"))
h2_total <- ggplot(total_ratings_df, aes(x = firmad2_total, y= total_ratings, fill=firmad2_total)) + geom_col()

# H3: Netflixi filmivalik on k√µige suurem.
valik3 <- df %>% select(Age, Netflix, Hulu, Prime.Video) %>% filter(Age != "NA")
netflix3 <- valik3 %>% filter(Netflix == 1) %>% count(Netflix)
hulu3 <- valik3 %>% filter(Hulu == 1) %>% count(Hulu)
amazon3 <- valik3 %>% filter(Prime.Video == 1) %>% count(Prime.Video)


firmad3 <- c("Netflix", "Hulu", "Prime Video")
sagedus3 <- c(netflix3$n, hulu3$n, amazon3$n)
filmiValik <- data.frame(firmad, sagedus3)

h3 <- ggplot(filmiValik, aes(x = firmad3, y = sagedus3, fill = firmad)) + geom_col() 

h1
h2
h2_total
h3
