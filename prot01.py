
#protocol start


class ProtocolError(Exception):
    pass

class DecodeError(ProtocolError):
    pass

class EOFError(DecodeError, EOFError):
    pass

class UnknownFunctionError(DecodeError):
    pass

class UnsupportedOperationError(DecodeError):
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

    def load(self):
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


# --------- stack
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

# function definition
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

# stack push new
@Decoder.registerInClass
def push(dec):
    dec.push(dec.readNext())

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
##            i0 = r.find(s0, i0 + 1)
            i0 += 1
            continue
        else:
            i0 = i
##        else:
##            pass
##        print i, i0
        c = read(i0 + len_s - len(r))
##        print 'c:', repr(c), i0 + len_s - len(r), repr(r[i0:])
        if c == '':
            print repr(r)
            print repr(c)
            print i0, repr(s), (len(r)), i
            raise EOFError('empty chunk read from file')
        r+= c

@Decoder.registerInClass
def bound(dec):
    boundary = dec.readNext()
    s = readUntil(dec.read, boundary)
    dec.push(s)


# ------------ sprache python ------------

# string
@Decoder.registerInClass
def decode(dec):
    encoding = dec.readNext()# todo: str, und decode trennen
    dec.push(dec.pop().decode(encoding))
    
# int
@Decoder.registerInClass
def _int(dec):
    #dec.debug('calling int: {0}'.format(str(dec)))
    r = dec.pop()
    #dec.debug('read: {0}'.format(r))
    dec.push(int(r))
_int.__name__ = 'int'

# float

@Decoder.registerInClass
def _float(dec):
    #dec.debug('calling int: {0}'.format(str(dec)))
    r = dec.pop()
    #dec.debug('read: {0}'.format(r))
    dec.push(float(r))
_float.__name__ = 'float'

# list
@Decoder.registerInClass
def _list(dec):
    dec.push([])
_list.__name__ = 'list'

@Decoder.registerInClass
def insert(dec):
    val = dec.pop()
    l = dec.pop()
##    dec.debug('insert {0} into {1}'.format(val, l))
    if hasattr(l, 'insert'):
        l.insert(0, val)
##    elif isinstance(val, basestring):
    else:
        l = val + l
##    else:
##        raise UnsupportedOperationError('cannot apply insert to')
    dec.push(l)
    
@Decoder.registerInClass
def head(dec):
    l = dec.pop()
    dec.push(l)
    dec.push(l.pop(0))

#----------------- steam object
class Input..
class Out

@Decoder.registerInClass
def stream(dec):
    return 

# --------------------------------- Test -------------------------------


import unittest
from StringIO import StringIO

class DecoderTest(unittest.TestCase):
    DecoderClass = Decoder

    def setUp(self):
        pass

    def newdec(self, s):
        self.dec = self.DecoderClass(StringIO(s))
        return self.dec

    def loadEq(self, obj, s):
        self.newdec(s)
        res = self.dec.load()
        try:
            self.assertEqual(obj, res)
        except RuntimeError:
            self.assertEqual(str(obj), str(res))
            self.assertEqual(repr(obj), repr(res))
        return res
        

    def test_int1(self):
        self.loadEq(123, 'push 123 int stop')

    def test_int3(self):
        self.loadEq(123, 'def :int push int :int :int 123 stop')

    def test_int2(self):
        self.loadEq(11111111111111111111, 'push 11111111111111111111 int stop')

    def test_float1(self):
        self.loadEq(1.3344, 'push 1.3344 float stop')

    def test_float2(self):
        self.loadEq(1.3344, 'def :float push float :float :float 1.3344 stop')

    def test_str1(self):
        self.loadEq('hello world', 'bound " hello world" stop')

    def test_str2(self):
        self.loadEq(u'hello world', 'bound " hello world" decode ascii stop')

    def test_str3(self):
        self.loadEq(u'hello world', 'bound " aGVsbG8gd29ybGQ=\n" decode base64 stop')

    def test_lst1(self):
        self.loadEq([1,2,3], 'list push 1 int switch push 2 int switch push 3 int switch switch insert switch insert switch insert stop')

    def test_ls2(self):
        self.loadEq([1,2,3],   'list def :insint push int insert :insint '\
                                ':insint 3 :insint 2 :insint 1 stop')

##    def test_lst3(self):
##        self.loadEq([1,2,3], ' lst 0 int 3 switch ins int 2 switch ins int 1 switch ins stop')
##
##    def test_lst4(self):
##        self.loadEq([1,2,3], ' lst 0 int 1 switch int 2 switch int 3 switch ins ins ins stop')
##
##    def test_lstapp1(self):
##        self.loadEq([1,2,3], 'def app switch ins app lst 0 int 3 app int 2 app int 1 app stop')

    def test_lstsave1(self):
        l = [1,2,3]
        l.append(l)
        self.loadEq(l, ''' def :intins push int insert :intins
                            list save id1 id1
                            id1 insert
                            :intins 3 :intins 2 :intins 1
                            stop''')

import sys
import random

BOUND = 'qwertyuiopasdfghjklzxcvbnm1234567890QWERTYUI'\
        'OPASDFGHJKLZXCVBNM<>,.{}[]\\|\'"!@#$%^&*()'

class Encoder(object):
    def __init__(self, file):
        self.__file = file
        self.__setup = False
        self.memo = {}

    def write(self, s):
        self.__file.write(s)

    def setup(self):
        for name in dir(self):
            if name.startswith('setup_'):
                try:
                    getattr(self, name)()
                except TypeError:
                    ty, er, tb = sys.exc_info()
                    if tb.tb_next:
                        raise ty, er, tb

    def setup_int(self):
        self.write('def :i push int :i\n')

    def setup_list(self):
        self.write('def :ins insert :ins\n')
        self.write('def :app switch insert :app\n')

    def save(self, obj):
        if not self.__setup:
            self.setup()
            self.__setup = True
        self._save(obj)
        self.write('stop ')

    def _save(self, obj):
        func = self.getFunction(type(obj).__name__)
        func(obj)

    def getFunction(self, name):
        return getattr(self, 'enc_' + name)

    def enc_int(self, obj):
        self.write(':i %i ' % obj)
    enc_long = enc_int

    def enc_str(self, obj):
        c = random.choice(BOUND)
        i = obj.find(c, 0)
        while i != -1:
            c+= random.choice(BOUND)
            i = obj.find(c, i)
        self.write('bound ' + c + ' ')
        self.write(obj)
        self.write(c)

    def chooseEncoding(self, uni_s):
        return 'UTF-8'

    def enc_unicode(self, obj):
        enc = self.chooseEncoding(obj)
        self.enc_str(obj.encode(enc))
        self.write('decode ' + enc + ' ')

    def memoize(self, obj):
        # todo: memoize object
        if id(obj) in self.memo:
            return False
        self.memo[id(obj)] = obj
        return True

    def enc_list(self, obj):
        if self.memoize(obj):
            _id = 'l%i' % id(obj)
            self.write('list save {0} '.format(_id))
            for el in obj:
                self._save(el)
            self.write('{0} '.format(_id))
            for el in obj:
                self.write(':app ')
        else:
            self.write('l{0} '.format(id(obj)))
            
        
    
class EncoderTest(unittest.TestCase):
    def isEq(self, obj):
        f = StringIO()
        enc = Encoder(f)
        enc.save(obj)
        dec = Decoder(f)
        f.seek(0)
##        print '-------\n', f.read()
        f.seek(0)
        res = dec.load()
        try:
            self.assertEqual(obj, res)
        except RuntimeError:
            self.assertEqual(str(obj), str(res))
            self.assertEqual(repr(obj), repr(res))
        return res

    def test_int(self):
        self.isEq(1)
        self.isEq(100000)
        self.isEq(100000000000)
        self.isEq(-12331411)
    
    def test_str(self):
        self.isEq('')
        self.isEq('asd')
        self.isEq(''.join([chr(x) for x in range(256)]))
        self.isEq('Hello World')

    def test_unicode(self):
        self.isEq(u'')
        self.isEq(u'asd')
        self.isEq(u''.join([unichr(x) for x in range(1234)]))
        self.isEq(u'Hello World')

    def test_list(self):
        isEq = self.isEq
        isEq([])
        isEq([2,3,4,5])
        isEq(['12', '23', '34'])
        l = [123, 12,3]
        l.append(l)
        isEq(l)

if __name__ == '__main__':
    unittest.main(exit = False)
    

'''
sagen: welche funktionen angeboten
funktionen weg -> neuer stream.. doof
datei stream rein zum test, ob schnittstelle enspricht


'''
