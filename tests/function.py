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

testcase1()
testcase2()
testcase3(True)
