# zad 1
def traceFor1(a, i, n):
    result = 0
    while i != n:
        x = a[i][i]
        result += x
        i = i + 1
    #end while i
    return result
#end def

def traceFor2(a, iis):
    result = 0
    for i in iis:
        x = a[i][i]
        result += x
    #end for i
    return result
#end def

def traceFor(a):
    n = len(a)
    #result = traceFor1(a, 0, n)
    iis = range(0, n)
    result = traceFor2(a, iis)
    return result
#end def


# zad 2
## prvi nacin
def for1(a, ijs):
    result = 0
    for (i,j) in ijs:
        x = a[i][j]
        if i==j:
            result += i*j*x
        elif x % 3 == 0:
            result += 1
        else:
            result += 0
        #end if
    #end for (i,j)
    return result
#end def

def forSum1(xs):
    n = len(xs)
    ijs = [(i,j) for j in range(n) for i in range(n)]
    return for1(xs, ijs)
#end def


## drugi nacin
def f(x,i,j):
    if i==j:
        return i*j*x
    elif x % 3 == 0:
        return 1
    else:
        return 0
    #end if
#end def

def f_row(xs, i):
    result = 0
    for (j, x) in enumerate(xs):
        result += f(x,i,j)
    #end for (j,x)
    return result
#end def

def forSum(xs):
    result = 0
    n = len(xs)
    for (i, r) in enumerate(xs):
        result += f_row(r, i)
    #end for (i,r)
    return result
#end def
