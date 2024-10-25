*help python
*python search
set python_exec "C:\Users\chadi\anaconda3\python.exe"

version 18.5
// (or version 18.5 for StataNow)
local a = 2
local b = 3
python:
from sfi import Scalar
def calcsum(num1, num2):
    res = num1 + num2
    Scalar.setValue("result", res)
calcsum(`a', `b')
end
display result

local a = 2
local b = 3
python script pyex.py
display result