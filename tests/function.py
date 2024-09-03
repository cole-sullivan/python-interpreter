def testcase1():
    return "Success! [1]"

def testcase2():
    def nested():
        return "Success! [2]"

    return nested()

def testcase3(var):
    if var:
        return "Success! [3]"
    else:
        return "Failure... [3]"

testcase1()
testcase2(True)
testcase3()
