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
    a = "[ A ]"
    b = "[ B ]"
    c = "[ C ]"
    print(a, b, c)

testcase1()
testcase2()
testcase3(True)
testcase4()
