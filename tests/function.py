def testcase1():
    print("Success! [1]")

def testcase2():
    def nested():
        return "Success! [2]"

    print(nested())

def testcase3(var):
    if var:
        print("Success! [3]")
    else:
        print("Failure... [3]")

def testcase4():
    a = "Success! 1/3 [4]"
    b = "Success! 2/3 [4]"
    c = "Success! 3/3 [4]"
    print(a)
    print(b)
    print(c)

testcase1()
testcase2()
testcase3(True)
testcase4()
