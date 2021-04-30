read_excel("C:\\Users\\tyler\\Documents\\Econometrics\\Homework 3\\SP_Returns.xlsx")

invest_amt <- 0:100000
ret <- prod((SP_Returns$Return[55:94] * .01)+1)
ret_amt <- invest_amt * ret 
    
plot(invest_amt, ret_amt)



