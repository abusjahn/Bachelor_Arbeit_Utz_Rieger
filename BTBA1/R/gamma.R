pacman::p_load(tidyvers,
               MASS,
               lognorm)

Cd_19 <- tibble(
  "ConcRest" = c(.448, 1.218, 1.984, 3.56, 6.21, 9.125, 12.52, 15.12),
  "qCd/Alge" = c(.2743, .503, .733, .854, .828, .765, .635, .61)
) # die Messdaten
ggplot(mapping = aes(ConcRest, `qCd/Alge`), data=Cd_19)+ # ein einfachrer Plot um zu die Verteilung zu verstehen
  geom_point()
fit_params <- fitdistr(Cd_19$ConcRest
                       , "gamma") # mein versuch das ganze auf die gamma-Verteilung zu fitten
print(fit_params)
gammadis <- tibble(x = seq(0,15,.1),y=dgamma(x,shape = fit_params$estimate['shape'],rate = fit_params$estimate['rate']))
ggplot(data = Cd_19, aes(x = ConcRest,
                         y = `qCd/Alge`)) + # lot des Fits
  geom_point(size = 3)+
  geom_line(
    data = gammadis,
    aes(x=x, y=y), color="red", size = 1)+
  geom_smooth()
# Leider ist das gar nicht so wie's aussehen soll...
fit2 <- lm(Cd_19$`qCd/Alge`~poly(Cd_19$ConcRest,2,raw=F))
#third degree
fit3 <- lm(Cd_19$`qCd/Alge`~poly(Cd_19$ConcRest,3,raw=F))
#fourth degree
fit4 <- lm(Cd_19$`qCd/Alge`~poly(Cd_19$ConcRest,4,raw=F))
Cd_19 <-  mutate(Cd_19,
                 fit2=predict(fit2),
                 fit3=predict(fit3),
                 fit4=predict(fit4))
Cd_19 %>% pivot_longer(cols = c(`qCd/Alge`,starts_with('fit'))) %>%
  ggplot(aes(ConcRest,value,color=name))+
  geom_point()+
  geom_line()
