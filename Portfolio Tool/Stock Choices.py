Portfolio = []

#Looping to take inputs
while True:
    stock = input("Enter a Stock Ticker in All Caps Please (or press Enter to stop): ")
    if stock == "":
        break
    Portfolio.append(stock)

# Write the strings to a text file
with open("portfolio.txt", "w") as file:
    for stock in Portfolio:
        file.write(stock + "\n")

print("Stocks saved to portfolio.txt")
