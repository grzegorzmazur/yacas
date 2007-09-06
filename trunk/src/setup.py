import os, sys
from distutils.sysconfig import get_python_inc, get_python_lib
from distutils.core import setup, Extension
from distutils.command.build_ext import build_ext

yacas_source = [ 'deffile.cpp', 'infixparser.cpp', 'lispatom.cpp', 
                 'lispcleanupstack.cpp', 'lispenvironment.cpp', 'lispeval.cpp', 
                 'lispio.cpp', 'lispobject.cpp', 'lispparser.cpp',  
                 'lispuserfunc.cpp', 'mathcommands.cpp', 'mathenvironment.cpp', 
                 'mathuserfunc.cpp', 'standard.cpp',  'stdfileio.cpp', 'arggetter.cpp', 
                 'stringio.cpp', 'tokenizer.cpp', 'yacasapi.cpp', 'genericobject.cpp', 
                 'arrayclass.cpp', 'lispevalhash.cpp', 'patterns.cpp', 'patternclass.cpp', 
                 'substitute.cpp', 'mathcommands2.cpp', 'mathcommands3.cpp', 'errors.cpp',
                 'patcher.cpp', 'lispplugin.cpp', 'genericstructs.cpp',
                 'unipoly.cpp', 'xmltokenizer.cpp', 'archiver.cpp',
                 'cyacas.cpp', 'platmath.cpp', 'grower.cpp', 'stdstubs.cpp', 'obmalloc.cpp',
                 'lisphash.cpp', 'lispstring.cpp', 'mathutil.cpp', 'yacasbase.cpp',
                 'anumber.cpp', 'yacasnumbers.cpp', 'win32dll.cpp' ]

platdir = 'plat/win32'

setup(name="yacas", version="1.0",
      ext_modules = [Extension('yacasc',
                               ['yacasc.i'] + yacas_source,
                               include_dirs=[platdir, '.'],
                               define_macros=[('SCRIPT_DIR','\\"C:/yacas/scripts/\\"'),
                                              ('VERSION', '\\"Win32\\"')])]
      )
