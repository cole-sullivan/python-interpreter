def a():
    b()

def b():
    c()

def c():
    c("too", "many")

a()
