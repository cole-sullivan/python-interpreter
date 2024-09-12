x = "Success! [2]"
def testcase_outer():
    x = "Success! [1]"
    def testcase_inner():
        print(x)
    testcase_inner()
testcase_outer()
print(x)
