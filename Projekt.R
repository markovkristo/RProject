library(ggplot2)
library(dplyr) 
library(stringr)
library(RColorBrewer)



# Loen andmed sisse ja asendan kõik tühjad lahtrid NA-ga
df <- read.csv("https://raw.githubusercontent.com/markovkristo/RProject/master/Filmid.csv", header = T, na.strings = c("","NA"))

# Esmane andmekirjeldus
summary(df)
ncol(df)
names(df)
nrow(df)

df <- subset(df, select = -c(X,Type)) # Eemaldan ebavajalikud tulbad

# Viin IMDb ja Rotten Tomatoes-e reitingud samale tasemele.
# Eemaldan "rating"-utelt vordlusmaarad. (x/10 .. x/100)
df$IMDb = substr(df$IMDb,1,3)
df$Rotten.Tomatoes = substr(df$Rotten.Tomatoes,1,2)

df$IMDb = as.double(df$IMDb) # Tüübiteisendus
df$IMDb = df$IMDb*10
df$Rotten.Tomatoes = as.double(df$Rotten.Tomatoes)


# Kirjeldav statistika. 
naVaba <- na.omit(df)
kirjeldused <- naVaba %>% mutate(keskmine_reiting = (IMDb + Rotten.Tomatoes) / 2)


kirjeldused <- kirjeldused %>% summarise(suurim_keskmine_reiting = max(keskmine_reiting),
                                     koigi_keskmine_reiting = mean(keskmine_reiting),
                                     vaikseim_keskmine_reiting = min(keskmine_reiting),
                                     pikim_film = max(Runtime),
                                     keskmine_film = mean(Runtime),
                                     luhim_film = min(Runtime))


# H1: Disney+ edastab kõige vähem täiskasvanutele mõeldud sisuga filme.

# Eraldan vajalikud andmed, et kontrollida seda hüpoteesi. Jätan alles ainult read, kus filmi vanusepiirang on 18+
valik1 <- df %>% select(Age, Netflix, Hulu, Prime.Video, Disney.) %>% filter(Age == "18+")

# Loen kokku, mitu täisealistele mõeldud filmi igal firmal on.
taisealisteleFilmid <- valik1 %>% summarise(Netflix_kokku = sum(Netflix),
                                            Disney._kokku = sum(Disney.),
                                            Hulu_kokku = sum(Hulu),
                                            Amazon_kokku = sum(Prime.Video))

# Moodustan uue data frame saadud andmetest, et oleks lihtsam joonist teha.
firmad <- c("Netflix", "Disney","Hulu", "Prime Video")
sagedused1 <- c(taisealisteleFilmid$Netflix_kokku, taisealisteleFilmid$Disney._kokku, taisealisteleFilmid$Hulu_kokku, taisealisteleFilmid$Amazon_kokku)
filmiValik <- data.frame(firmad, sagedused1)


# Joonistan tulemused välja
h1 <- ggplot(filmiValik, aes(x = firmad, y = sagedused1,fill = firmad)) + geom_col() + 
  geom_text(aes(label=sagedused1), vjust=-0.3, size=3.5) +
  labs(x = "Vaatamisplatvormid", y = "Filmide arvud", title = "18+ filmide arv erinevatel vaatamisplatvormidel. ") +
  scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")

h1


#h1 <- ggplot(taiskasvanuteleMoeldud, aes(x = firmad, y = sagedus, fill = firmad)) + geom_col()+ scale_fill_manual("Platvormid", values = c("Netflix"="red", "Hulu" = "green ", "Prime Video " = "orange", "Disney+ " = "skyblue"))
#h1_pie <- ggplot(taiskasvanuteleMoeldud, aes(x=firmad, y=sagedus, fill=firmad)) + geom_bar(stat="identity", width=1) + coord_polar("y",start=0)



# H2: Netflixi filmivaliku keskmine rating on suurem kui muudel vaatamisplatvormidel.
# Eraldan vajalikud andmed, et kontrollida hupoteesi.
valik2 <- df %>% select(IMDb, Rotten.Tomatoes, Netflix, Hulu, Prime.Video, Disney.)


# Eraldan filmid platvormi j?rgi
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
Rotten.Tomates_mean_disney = mean(disney2$Rotten.Tomatoes, trim=0.5, na.rm=TRUE)
Total_rating_disney = (IMDb_mean_disney + Rotten.Tomates_mean_disney ) / 2

firmad2 <- c("Netflix IMDb", "Netlfix Rotten Tomatoes", "Hulu IMDb", "Hulu Rotten Tomatoes", "Prime Video IMDb", "Prime Video Rotten Tomatoes", "Disney+ IMDb", "Disney+ Rotten Tomatoes")
firmad2_total <- c("Netflix", "Hulu", "Prime Video", "Disney+")
ratingud2 <- c(IMDb_mean_netflix,Rotten.Tomates_mean_netflix,IMDb_mean_hulu,Rotten.Tomates_mean_hulu,IMDb_mean_amazon, Rotten.Tomates_mean_amazon, IMDb_mean_disney,Rotten.Tomates_mean_disney)
total_ratings <- c(Total_rating_netflix, Total_rating_hulu, Total_rating_amazon, Total_rating_disney)

rating <- data.frame(firmad2, ratingud2)
total_ratings_df <- data.frame(firmad2_total, total_ratings )
h2 <- ggplot(rating, aes(x= firmad2, y=ratingud2, fill=firmad2)) + geom_col() + 
  labs(x = "Vaatamisplatvormid", y = "Reiting", title = "Vaatamisplatvormide IMDb ja Rotten tomatoes keskmised reitingud") +
  geom_text(aes(label=ratingud2),position=position_dodge(width = 0.9),vjust=-0.25) + ylim(0,100) + scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")


h2_total <- ggplot(total_ratings_df, aes(x = firmad2_total, y= total_ratings, fill=firmad2_total)) + geom_col() +
  labs(x = "Vaatamisplatvormid", y = "Reiting", title = "Vaatamisplatvormide IMDb ja Rotten tomatoes keskmised reitingud") +
  geom_text(aes(label=total_ratings),position=position_dodge(width = 0.9),vjust=-0.25) + ylim(0,100)+ scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")

h2
h2_total


# H3: Netflixi filmivalik on koige suurem.
valik3 <- df %>% select(Netflix, Hulu, Prime.Video, Disney.)

# Loen kokku, mitu filmi igal firmal on.
filmideSaadavus <- valik3 %>% summarise(Netflix_kokku = sum(Netflix),
                                            Disney._kokku = sum(Disney.),
                                            Hulu_kokku = sum(Hulu),
                                            Amazon_kokku = sum(Prime.Video))

sagedused3 <- c(filmideSaadavus$Netflix_kokku, filmideSaadavus$Disney._kokku, filmideSaadavus$Hulu_kokku, filmideSaadavus$Amazon_kokku)
filmideSagedus <- data.frame(firmad, sagedused3)

# Joonistan tulemused välja
h3 <- ggplot(filmideSagedus, aes(x = firmad, y = sagedused3, fill = firmad)) + geom_col() + 
  geom_text(aes(label=sagedused3), vjust=-0.3, size=3.5) +
  labs(x = "Vaatamisplatvormid", y = "Filmide arvud", title = "Filmide arv erinevatel vaatamisplatvormidel. ") + scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")
h3


sagedusedProtsentides <- round(sagedused3/sum(sagedused3)*100)
ggplot(filmideSagedus, aes(x="", y=sagedusedProtsentides, fill=firmad)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_minimal() +
  labs(y = "Filmide protsent kõikidest filmidest", title = "Filmide arv erinevatel vaatamisplatvormidel.")+ scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")



# Mis on kõige populaarsemad žanrid?
Genres <- unlist(str_split(df$Genres, ",")) # Eraldan kõik žanrid koma koha pealt ning moodustan listi.
zanriteSagedus <- data.frame(Genres)
zanriteSagedus <- zanriteSagedus %>% count(Genres, sort = T) %>% arrange(desc(n))

ggplot(data = zanriteSagedus, aes(Genres, n,fill=Genres)) + geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust=-0.3, size=3.5)  + labs(x = "Genres", y = "Count")


# Mis aastal tehtud filme on kõige rohkem nendel kanalitel.
aastateEsinemine <- df %>% count(Year,sort = T)  # Loen aasta arvud kokku ning sorteerin need ära.
#ggplot(data = aastateEsinemine, aes(Year, n)) + geom_bar() # Tulbana 
ggplot(data = aastateEsinemine, aes(Year, n)) + geom_line() + 
  labs(x = "Year", y = "Count", title = "Välja lastud filmide arv") +
  scale_x_continuous(limits = c(1915, 2025),   
                     breaks = seq(1915, 2025, by = 10), 
                     labels = paste0(seq(1915, 2025,by=10))) +
  scale_y_continuous(limits = c(0, 1100),   
                     breaks = seq(0, 1100, by = 150), 
                     labels = paste0(seq(0, 1100,by=150)))



# Uurin, mis filmi näidatakse mitmelt kanalilt
filmideKorduvus <- df %>% select (Title, IMDb, Rotten.Tomatoes, Netflix, Hulu, Prime.Video, Disney.) %>% mutate(korduvus = Netflix + Hulu + Disney. + Prime.Video)  %>% count(korduvus, sort = T)




h1
h1_pie
h2
h2_total
h3
