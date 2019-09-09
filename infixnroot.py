# definition of an Infix operator class
# this recipe also works in jython
# calling sequence for the infix is either:
#  x |op| y
# or:
# x <<op>> y

class Infix:
    def __init__(self, function):
        self.function = function
    def __ror__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __or__(self, other):
        return self.function(other)
    def __pow__(self, other):  # For hi prec. reverse order bec. right-assoc.
        return Infix(lambda x, self=self, other=other: self.function(x, other))
    def __rpow__(self, other):
        return self.function(other)
    def __rlshift__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __rshift__(self, other):
        return self.function(other)
    def __call__(self, value1, value2):
        return self.function(value1, value2)

from sympy import root
nroot = ROOT = Infix(lambda n,x: root(x,n))

## http://code.activestate.com/recipes/384122-infix-operators/
