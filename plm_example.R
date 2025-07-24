jtrain.panel <- plm.data(jtrain, c("fcode", "year"))

#first-difference
jtrain.fd <- plm(lscrap ~ hrsemp+sales+employ, data = jtrain.panel, model="fd")
summary(jtrain.fd)

#fixed effect
jtrain.fe2 <- plm(lscrap ~ hrsemp+sales+employ, data = jtrain.panel, model="within")
summary(jtrain.fe2)
