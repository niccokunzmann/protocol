
#protocol start


class ProtocolError(Exception):
    pass

class DecodeError(ProtocolError):
    pass

class EOFError(DecodeError):
    pass

class UnknownFunctionError(DecodeError):
    pass

class StackEmptyException(ProtocolError):
    pass

class StackFullException(ProtocolError):
    pass

SEPARATOR = ' \t\n\r\f\v'

class FunctionRegistration(object):
    # the function registration interface
    @classmethod
    def registerInClass(cls, func):
        return cls._register(cls, func)

    def registerInObject(self, func):
        return self._register(self, func)

    @staticmethod
    def _register(obj, func):
        try:
            obj.__functions.insert(0, func)
        except AttributeError:
            obj.__functions = [func]
        return func

    def getFunction(self, name):
        for f in self.__functions:
            if f.__name__ == name:
                return f
        raise UnknownFunctionError(name)

    def __init__(self):
        self.__functions = self.__functions[:]


class Decoder(FunctionRegistration):

    def __init__(self, file):
        self.__file = file
        self.__stack = []

    # the function use interfac
        self.state = {}

    def push(self, val):
        self.__stack.append(val)
        return val

    def pop(self):
        return self.__stack.pop()

    def read(self, length):
        return self.__file.read(length)

    def stop(self):
        self.__parsing = False

    def skip(self, s = SEPARATOR):
        c = self.read(1)
        while c in s and c:
            c = self.read(1)
        return c

    def readNext(self, until = SEPARATOR):
        s = self.skip()
        c = self.read(1)
        while c not in until:
            s+= c
            c = self.read(1)
        return s

    # thedecoder
    def parseFunction(self):
        s = self.readNext()
        if len(s) == 0:
            raise EOFError('nothing to read')
        return s

    def parse(self):
        self.__parsing = True
        while self.__parsing:
            funcName = self.parseFunction()
            #self.debug('functionname {0}'.format(funcName))
            func = self.getFunction(funcName)
            #self.debug('function {0}'.format(func))
            func(self)
        return self.pop()

    def debug(self, s):
        print s


#steuerung
@Decoder.registerInClass
def stop(dec):
    dec.stop()

@Decoder.registerInClass
def switch(dec):
    a = dec.pop()
    b = dec.pop()
    dec.push(a)
    dec.push(b)

@Decoder.registerInClass
def dup(dec):
    dec.push(dec.push(dec.pop()))

#daten parsen
_int = int
@Decoder.registerInClass
def int(dec):
    #dec.debug('calling int: {0}'.format(str(dec)))
    r = dec.readNext()
    #dec.debug('read: {0}'.format(r))
    dec.push(_int(r))

_float = float
@Decoder.registerInClass
def float(dec):
    #dec.debug('calling int: {0}'.format(str(dec)))
    r = dec.readNext()
    #dec.debug('read: {0}'.format(r))
    dec.push(_float(r))

#list
@Decoder.registerInClass
def lst(dec):
    int(dec)
    c = dec.pop()
    dec.push([])
    for i in range(c):
        ins(dec)



@Decoder.registerInClass
def ins(dec):
    l = dec.pop()
    val = dec.pop()
    if hasattr(l, 'insert'):
        l.insert(0, val)
    else:
        l = val + l
    dec.push(l)
    
@Decoder.registerInClass
def pop(dec):
    l = dec.pop()
    dec.push(l.pop(0))
    dec.push(l)

# function
@Decoder.registerInClass
def define(dec):
    name = dec.readNext()
    #dec.debug('def {0}'.format(name))
    l = []
    new = dec.readNext()
    while new != name:
        l.append(new)
        new = dec.readNext()
    def func(dec):
        for name in l:
            func = dec.getFunction(name)
            func(dec)
    func.__name__ = name
    dec.registerInObject(func)
    #dec.debug('def registered {0} as {1}'.format(name, l))
define.__name__ = 'def'

@Decoder.registerInClass
def save(dec):
    name = dec.readNext()
    val = dec.pop()
    #dec.debug('save {0} under {1}'.format(val, name))
    def func(dec):
        dec.push(val)
    func.__name__ = name
    dec.registerInObject(func)

# string
def readUntil(read, s):
    len_s = len(s)
    r = read(len_s)
    s0 = s[0]
    i0 = 0
    while 1:
        # todo: optimize
        i = r.find(s0, i0)
        if i == -1:
            i0 = len(r)
        elif i == i0:
            if r[i0:] == s:
                return r[:i0]
            i0 = r.find(s0, i0 + 1)
        else:
            pass
        c = read(i0 + len_s - len(r))
        if c == '':
            raise EOFError('empty chunk read from file')
        r+= c
            
def _str(dec):
    boundary = dec.readNext()
    s = readUntil(dec.read, boundary)
    dec.push(s)
_str.__name__ = 'str'
Decoder.registerInClass(_str)

@Decoder.registerInClass
def decode(dec):
    encoding = dec.readNext()# todo: str, und decode trennen
    dec.push(dec.pop().decode(encoding))
    


import unittest
from StringIO import StringIO

class DecoderTest(unittest.TestCase):
    DecoderClass = Decoder

    def setUp(self):
        pass

    def newdec(self, s):
        self.dec = self.DecoderClass(StringIO(s))
        return self.dec

    def parseEq(self, obj, s):
        self.newdec(s)
        res = self.dec.parse()
        try:
            self.assertEqual(obj, res)
        except RuntimeError:
            self.assertEqual(str(obj), str(res))
            self.assertEqual(repr(obj), repr(res))
        return res
        

    def test_int1(self):
        self.parseEq(123, 'int 123 stop')

    def test_int2(self):
        self.parseEq(11111111111111111111, 'int 11111111111111111111 stop')

    def test_float(self):
        self.parseEq(1.3344, 'float 1.3344 stop')

    def test_str1(self):
        self.parseEq('hello world', 'str " hello world" stop')

    def test_str2(self):
        self.parseEq(u'hello world', 'str " hello world" decode ascii stop')

    def test_str3(self):
        self.parseEq(u'hello world', 'str " aGVsbG8gd29ybGQ=\n" decode base64 stop')

    def test_lst1(self):
        self.parseEq([1,2,3], 'int 1 int 2 int 3 lst 3 stop')

    def test_ls2(self):
        self.parseEq([1,2,3], 'int 2 int 3  lst 2 int 1 switch ins stop')

    def test_lst3(self):
        self.parseEq([1,2,3], ' lst 0 int 3 switch ins int 2 switch ins int 1 switch ins stop')

    def test_lst4(self):
        self.parseEq([1,2,3], ' lst 0 int 1 switch int 2 switch int 3 switch ins ins ins stop')

    def test_lstapp1(self):
        self.parseEq([1,2,3], 'def app switch ins app lst 0 int 3 app int 2 app int 1 app stop')

    def test_lstsave1(self):
        l = [1,2,3]
        l.append(l)
        self.parseEq(l, '''  def app switch ins app
                            lst 0 save id1 id1
                            id1 app
                            int 3 app int 2 app int 1 app
                            stop''')
if __name__ == '__main__':
    unittest.main(exit = False)
    


