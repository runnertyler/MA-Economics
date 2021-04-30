read_excel("C:\\Users\\tyler\\Documents\\Econometrics\\SP_Returns.xlsx")
bal <- 10000
for (i in 55:94){
  ret <- SP_Returns[i, 2]
  bal <- bal + bal * (ret*.01)
}
print(bal)