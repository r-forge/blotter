require(instrument)
currency("USD")
currency("GBP")
currency("EUR")
currency("YEN")
exchange_rate("USDGBP","USD","GBP")
exchange_rate("EURGBP","EUR","GBP")
exchange_rate("EURYEN","EUR","YEN")
exchange_rate("USDYEN","USD","YEN")
stock("IBM","USD",1)
option(".IBM","USD",100,underlying_id="IBM")
# Jeff to pull put and call option series for 2009