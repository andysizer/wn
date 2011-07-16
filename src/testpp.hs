-- file: testpp.hs

import System.Environment
import System.Exit
import ApplicativeParsec
import PreProcessWml

main = do
    args <- getArgs
    doIt args

doIt [i] = pp i o
    where o = "pp.txt"
doIt [i,o] = pp i o
doIt _ = do
    usage
    exitFailure

usage = do
    p <- getProgName
    print ("Usage: " ++ p ++ " input [outfile]")
    
    

tn = 
   [
     "xxx" -- pass
   , "#" -- fail
   , "# gsgsg"
   , "zzz# sdsd"
   , "#define foo x y # sdsd\nv1={x}\nv2={y}\n#enddef\n{foo 1 2}" -- pass
   , "#define d1\n#enddef\n#define d2\n#enddef\n#ifdef d1\nxxx\n#ifdef d2\nyyy\n#else\nbbb\n#endif\n#else\naaa\n#endif\n"
   , "#define x a b c\n#ifndef d\nx1 = {a}\n#else\nx2={b}\n#endif\n#enddef\n"
   , "#define foo x y\n  bar {x}+1 {y} {x}\n#enddef\n#define bar x y z\n  x={x}\n  y={y}\n  z={z}\n#enddef\n{foo 1 3}\n"
   , "#textdomain zzz"
   , "#textdomain zzz\n#define foo x y\n  bar {x}+1 {y} {x}\n#enddef\n#define bar x y z\n  x={x}\n  y={y}\n  z={z}\n#enddef\n{foo 1 3}\n"
   , "#define RAMP_BRIDGE S0 S1 S2 S3 S4 S5\n        map=\"\n,  {S0}\n{S5},   {S1}\n,  1\n{S4},   {S2}\n,  {S3}\"\n#enddef"
   ]

tn1 = "#define RAMP_BRIDGE S0 S1 S2 S3 S4 S5\n        map=\"\n,  {S0}\n{S5},   {S1}\n,  1\n{S4},   {S2}\n,  {S3}\"\n#enddef"
tn2 = "#define foo x y # sdsd\nv1={x}\nv2={y}\n#enddef\n{foo 1 2}"

{--
test = runParser preProcessWml (initState "") "" 

p = preProcess

f = "~C:\\Users\\andy\\Projects\\wesnoth-1.8.6\\data\\themes\\"
fc = "~C:\\Users\\andy\\Projects\\wesnoth-1.8.6\\data\\core\\"
f1 = "~C:\\Users\\andy\\Projects\\wesnoth-1.8.6\\data\\themes/macros.cfg"
--}
