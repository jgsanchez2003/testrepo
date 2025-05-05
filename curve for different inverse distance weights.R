curve(1/(1+exp(x)), from=0, to=20)
curve(1/(x^2), from=0, to=20, add=T, col="red")

# how many weighted shelters if 20 km away and there are 50 shelters?
# quite a small number
50*(1/20^2)
