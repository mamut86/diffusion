# Gompertz model example --------------------------------------------------
\dontrun{
data("chicken")
Gompertz(x = chicken$weight)

data("carstock")
Gompertz(carstock$raw)
}