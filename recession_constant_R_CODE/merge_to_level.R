pressure_transducer_package()
fgor <- read.zoo.csv("baro_comp_fgor.csv")
ftbn <- read.zoo.csv("baro_comp_ftbn.csv")
ftbr <- read.zoo.csv("baro_comp_ftbr.csv")
manf <- read.zoo.csv("baro_comp_manf.csv")
srs_ <- read.zoo.csv("baro_comp_srs_.csv")
tnc_ <- read.zoo.csv("baro_comp_tnc_.csv")
sgam <- read.zoo.csv("baro_comp_sgam.csv")

all <- merge(ftbr, sgam, manf, srs_, fgor, ftbn, tnc_)

level <- all[,grep("level", colnames(all))]

