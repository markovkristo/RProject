
# EELTÖÖ
library(ggplot2)
library(dplyr) 
# Loen andmed sisse ja asendan kõik tühjad lahtrid NA-ga
df <- read.csv("https://raw.githubusercontent.com/markovkristo/RProject/master/Filmid.csv", header = T, na.strings = c("","NA"))

# Eemaldan ebavajalikud veerud. Mby peaks eemaldama kohe ka kõik NA-d
df <- subset(df, select = -c(X))
#df <- na.omit(df)




# H1: Disney+ edastab kõige vähem täiskasvanutele mõeldud sisuga filme.

# Eraldan vajalikud andmed, et kontrollida seda hüpoteesi. Jätan alles ainult read, kus filmi vanusepiirang on 18+
valik1 <- df %>% select(Age, Netflix, Hulu, Prime.Video) %>% filter(Age == "18+")

# Loen kokku, mitu täisealistele mõeldud filmi igal firmal on.
netflix1 <- valik1 %>% filter(Netflix == 1) %>% count(Netflix)
hulu1 <- valik1 %>% filter(Hulu == 1) %>% count(Hulu)
amazon1 <- valik1 %>% filter(Prime.Video == 1) %>% count(Prime.Video)

# Moodustan uue data frame saadud andmetest, et oleks lihtsam joonist teha.
firmad <- c("Netflix", "Hulu", "Prime Video")
sagedus <- c(netflix1$n, hulu1$n, amazon1$n)
taiskasvanuteleMoeldud <- data.frame(firmad, sagedus)

# Joonistan tulemused välja
h1 <- ggplot(taiskasvanuteleMoeldud, aes(x = firmad, y = sagedus, fill = firmad)) + geom_col() 



# H2: Netflixi filmivaliku keskmine rating on suurem kui muudel vaatamisplatvormidel.



# H3: Netflixi filmivalik on kõige suurem.
valik3 <- df %>% select(Age, Netflix, Hulu, Prime.Video) %>% filter(Age != "NA")
netflix3 <- valik3 %>% filter(Netflix == 1) %>% count(Netflix)
hulu3 <- valik3 %>% filter(Hulu == 1) %>% count(Hulu)
amazon3 <- valik3 %>% filter(Prime.Video == 1) %>% count(Prime.Video)


firmad3 <- c("Netflix", "Hulu", "Prime Video")
sagedus3 <- c(netflix3$n, hulu3$n, amazon3$n)
filmiValik <- data.frame(firmad, sagedus3)

h3 <- ggplot(filmiValik, aes(x = firmad3, y = sagedus3, fill = firmad)) + geom_col() 

