#' Computing a dilution factor, a sample volume (Vorlagevolumen), concentration after dilution


# Dilution factor
dilfact <- function (vol.1, vol.2) {
    vol.1/vol.1 %>%
        print()
}

# sample volume
samp.vol <- function (conc.is, vol.is = 1, conc.aim) {
    (conc.is * vol.is) / conc.aim %>%
        print()
}

# concentration after dilution
conc.per.dil <- function (vol.is , vol.post, conc.is) {
    (conc.is * vol.is) / vol.post %>%
        print()
}
