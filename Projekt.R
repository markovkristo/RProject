library(ggplot2)
library(dplyr) 
library(stringr)
library(RColorBrewer)


###############################################
### Eeltöö ja andmete uurimine

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


kirjeldused <- kirjeldused %>% summarise(suurim_keskmine_reiting = max(keskmine_reiting), # Mis on kõige suurem keskmine reiting filmile.
                                     koigi_keskmine_reiting = mean(keskmine_reiting), # Mis on keskmine filmide reiting
                                     vaikseim_keskmine_reiting = min(keskmine_reiting), # Mis on kõige madalam keskmine reitingfilmile.
                                     pikim_film = max(Runtime), # Mis on kõige pikem film.
                                     keskmine_film = mean(Runtime), # Keskmine filmi kestvus. 
                                     luhim_film = min(Runtime)) # Mis on kõige lühem film.

###############################################
### H1: Disney+ edastab kõige vähem täiskasvanutele mõeldud sisuga filme.

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
h1 <- ggplot(filmiValik, aes(x = reorder(firmad, -sagedused1), y = sagedused1,fill = firmad)) + geom_col() + 
  geom_text(aes(label=sagedused1), vjust=-0.3, size=3.5) +
  labs(x = "Vaatamisplatvormid", 
       y = "Filmide arv", 
       title = "18+ filmide arv erinevatel vaatamisplatvormidel",
       fill = "Vaatamisplatvormid") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(limits = c(0, 1200),   
                     breaks = seq(0, 1200, by = 200), 
                     labels = paste0(seq(0, 1200,by = 200)))+
  scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu") 

h1


###############################################


###############################################
### H2: Netflixi filmivaliku keskmine rating on suurem kui muudel vaatamisplatvormidel.
# Eraldan vajalikud andmed, et kontrollida hupoteesi.
valik2 <- df %>% select(IMDb, Rotten.Tomatoes, Netflix, Hulu, Prime.Video, Disney.)



# Funktsioon selleks, et leida erinevad vaatamisplatvormide uldiseid reitingud ja et kood oleks puhtam. 
reitinguteLeidmine <- function(data){
  imdbKeskmine <- mean(data$IMDb,trim=0.5, na.rm=TRUE)
  rottenTomatoKeskmine <- mean(data$Rotten.Tomatoes, trim=0.5, na.rm=TRUE)
  totalKeskmine <- (imdbKeskmine + rottenTomatoKeskmine ) / 2
  return (c(imdbKeskmine, rottenTomatoKeskmine, totalKeskmine))
}

# Eraldan filmid platvormi jargi js arvutan igale platvormile vastavad reitingud.
netflixiReitingud <- reitinguteLeidmine(valik2 %>% filter(Netflix == 1))
huluReitingud <- reitinguteLeidmine(valik2 %>% filter(Hulu == 1))
amazoniReitingud <- reitinguteLeidmine(valik2 %>% filter(Prime.Video == 1))
disneyReitingud <- reitinguteLeidmine(valik2 %>% filter(Disney. == 1))


firmad2 <- c("Netflix IMDb", "Netlfix Rotten Tomatoes", "Hulu IMDb", "Hulu Rotten Tomatoes", 
             "Prime Video IMDb", "Prime Video Rotten Tomatoes", "Disney+ IMDb", "Disney+ Rotten Tomatoes")

ratingud2 <- c(netflixiReitingud[1],netflixiReitingud[2],huluReitingud[1],huluReitingud[2],
               amazoniReitingud[1], amazoniReitingud[2], disneyReitingud[1], disneyReitingud[2])

total_ratings <- c(netflixiReitingud[3], disneyReitingud[3], huluReitingud[3], amazoniReitingud[3])

rating <- data.frame(firmad2, ratingud2)
total_ratings_df <- data.frame(firmad, total_ratings )

#  Joonis koigi filmide Rotten tomatoes-i ja IMDb reitingute keskmine (Rotten tomatoes ja IMDb eraldi) vaatamisplatvormi kohta.
h2 <- ggplot(rating, aes(x= firmad2, y=ratingud2, fill=firmad2)) + geom_col() +
  labs(x = "Vaatamisplatvormid", 
       y = "Reiting", 
       title = "Vaatamisplatvormide IMDb ja Rotten tomatoes keskmised reitingud",
       fill = "Vaatamisplatvormid") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_text(aes(label=ratingud2),position=position_dodge(width = 0.9),vjust=-0.25) + 
  ylim(0,100) + 
  scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")

h2



#  Joonis koigi filmide Rotten tomatoes-i ja IMDb reitingute keskmine ((Rotten tomato + IMDb) / 2) vaatamisplatvormi kohta.
h2_total <- ggplot(total_ratings_df, aes(x = firmad, y= total_ratings, fill=firmad)) + geom_col() +
  labs(x = "Vaatamisplatvormid", 
       y = "Keskmine reiting", 
       title = "Vaatamisplatvormide IMDb ja Rotten Tomatoes keskmised reitingud",
       fill = "Vaatamisplatvormid") +
  theme(plot.title = element_text(hjust=0.5)) + 
  geom_text(aes(label=total_ratings),position=position_dodge(width = 0.9),vjust=-0.25) + 
  ylim(0,100) + 
  scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")


h2_total
###############################################



###############################################
### H3: Netflixi filmivalik on koige suurem.
valik3 <- df %>% select(Netflix, Hulu, Prime.Video, Disney.)
valik3 <- na.omit(valik3)
pikkFormaat <- melt(valik3, measure.vars = 1:4) %>% filter(value == 1) # Andmete teisendamine laiast formaadist pikka formaati.

# Loen kokku, mitu filmi igal vaatamisplatvormil on.
netflix3 <- pikkFormaat %>% filter(variable == "Netflix") %>% count(variable)
hulu3 <- pikkFormaat %>% filter(variable == "Hulu") %>% count(variable)
amazon3 <- pikkFormaat %>% filter(variable == "Prime.Video") %>% count(variable)
disney3 <- pikkFormaat %>% filter(variable == "Disney.") %>% count(variable)


firmadeSagedused <- c(netflix3[1,2],  disney3[1,2], hulu3[1,2], amazon3[1,2])

# Moodustan uue dataframe, et oleks kergem joonist teha.
filmideSagedus <- data.frame(firmad, firmadeSagedused)


# Teen iga Vaatamisplatvormi filmidearvu kohta tulpdiagrammi.
h3 <- ggplot(filmideSagedus, aes(x = reorder(firmad, -firmadeSagedused), y = firmadeSagedused, fill = firmad)) + geom_col() + 
  geom_text(aes(label=firmadeSagedused), vjust=-0.3, size=3.5) +
  labs(x = "Vaatamisplatvormid", 
       y = "Filmide arv", 
       title = "Filmide arv erinevatel vaatamisplatvormidel. ", 
       fill = "Vaatamisplatvormid") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu") +
  scale_y_continuous(limits = c(0, 4200),   
                     breaks = seq(0, 4200, by = 500), 
                     labels = paste0(seq(0, 4200,by=500)))


h3

h3Pie <- ggplot(filmideSagedus, aes(x="", y=firmadeSagedused, fill=firmad)) +
  geom_bar(stat="identity", width=1, color="white") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(label = paste(round(firmadeSagedused / sum(firmadeSagedused) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Filmide arv erinevatel vaatamisplatvormidel.",
       fill = "Vaatamisplatvormid") + 
  scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "BuPu")

h3Pie


###############################################



###############################################
### Mis on koige populaarsemad žanrid?
Genres <- unlist(str_split(df$Genres, ",")) # Eraldan kõik žanrid koma koha pealt ning moodustan listi.
zanriteSagedus <- data.frame(Genres)
zanriteSagedus <- zanriteSagedus %>% count(Genres, sort = T) %>% filter(n > 1500) %>% arrange(desc(n))

ggplot(data = zanriteSagedus, aes(x = reorder(Genres, -n), n,fill=Genres)) + geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust=-0.3, size=3.5)  + 
  labs(x = "Žanrid", 
       y = "Filmide arv", 
       title = "Top5 levinud filmižanrid vaatamisplatvormidel kokku",
       fill = "Žanrid") + 
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_brewer(palette = "BuPu")
###############################################



###############################################
### Mis aastal tehtud filme on koige rohkem nendel kanalitel.
filmideArvAastates <- df %>% count(Year,sort = T)  # Loen aasta arvud kokku ning sorteerin need ara.
filmideArvAastates <- na.omit(filmideArvAastates)
# Teen joondiagrammi, et naha uldiselt, kuidas on filmide valjalaskmine ajas muutunud. 
ggplot(data = filmideArvAastates, aes(Year, n)) + geom_line() + 
  labs(x = "Aasta", 
       y = "Filmide arv", 
       title = "Välja lastud filmide arv") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(limits = c(1915, 2025),   
                     breaks = seq(1915, 2025, by = 10), 
                     labels = paste0(seq(1915, 2025,by=10))) +
  scale_y_continuous(limits = c(0, 1100),   
                     breaks = seq(0, 1100, by = 150), 
                     labels = paste0(seq(0, 1100,by=150)))
###############################################



###############################################
# Uurin, mis filme naidatakse mitmelt vaatamisplatvormilt. 
filmideKorduvus <- df %>% select (Title, IMDb, Rotten.Tomatoes, Netflix, Hulu, Prime.Video, Disney.) %>% mutate(korduvus = Netflix + Hulu + Disney. + Prime.Video)  %>% count(korduvus, sort = T)


###############################################




