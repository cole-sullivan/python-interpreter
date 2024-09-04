x = "global"
def testcase_outer():
    x = "outer"
    def testcase_inner():
        print(x)
    testcase_inner()
testcase_outer()
print(x)
