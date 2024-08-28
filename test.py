num = 1
string = ""

while num <= 100:
    if num % 3 == 0 and num % 5 == 0:
        string = string + "FizzBuzz"
    elif num % 3 == 0:
        string = string + "Fizz"
    elif num % 5 == 0:
        string = string + "Buzz"
    else:
        num
    num = num + 1

string
