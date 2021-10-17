
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
valik <- df %>% select(Age, Netflix, Hulu, Prime.Video) %>% filter(Age == "18+")

# Loen kokku, mitu täisealistele mõeldud filmi igal firmal on.
netflix <- valik %>% filter(Netflix == 1) %>% count(Netflix)
hulu <- valik %>% filter(Hulu == 1) %>% count(Hulu)
amazon <- valik %>% filter(Prime.Video == 1) %>% count(Prime.Video)

# Moodustan uue data frame saadud andmetest, et oleks lihtsam joonist teha.
firmad <- c("Netflix", "Hulu", "Prime Video")
sagedus <- c(netflix$n, hulu$n, amazon$n)
taiskasvanuteleMoeldud <- data.frame(firmad, sagedus)

# Joonistan tulemused välja
ggplot(taiskasvanuteleMoeldud, aes(x = firmad, y = sagedus, fill = firmad)) + geom_col() 
