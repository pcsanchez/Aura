import pickle
from decimal import *
import sys

# Calculates array offset from matrices dimensions
def GetOffset(dimensions, indexes):
  dimensionsProduct = 1
  indexes = indexes[::-1]
  for i in dimensions: 
    dimensionsProduct *= i
  dimensionsProduct /= dimensions[0]
  result = 0
  for i in range(len(indexes)):
    result += (indexes[len(indexes) - i - 1] * dimensionsProduct)
    dimensionsProduct /= dimensions[i]
  return int(result)

# Checks for the type of its argument
def CheckType(element):
  if(isinstance(element,str)):
    return 'str'
  elif(isinstance(element,list)):
    return "list"
  elif(isinstance(element,dict)):
    return 'dict'
  elif(isinstance(element,int)):
    return 'int'
  elif(isinstance(element,float)):
    return 'float'
  return 'None'

# Helper functions for getting numerical values from expressions
def GetValue(element):
  t = CheckType(element)
  if(t == 'str'):
    return program_variables[element].get('value')
  if(t == 'list'):
    return GetValueArray(element)
  if(t == 'int'):
    return int(element)
  if(t == 'float'):
    return Decimal(element)
    
def GetValueArray(element):
  elem = element.copy()
  indexes = elem[1].copy()
  for i in range(len(indexes)):
    indexes[i] = GetValue(indexes[i])
  dimensions = program_variables[elem[0]].get('dimensions').copy()
  offset = GetOffset(dimensions, indexes)
  real_value = int(program_variables[elem[0]].get('value')[offset])
  return real_value

def SetValue(val, target):
  global program_variables
  t = CheckType(target)
  if(t == 'list'):
    SetArrayValue(val, target)
    return
  ty = program_variables[target].get('type')
  if(ty == 'int'):
    program_variables[target]['value'] = int(GetValue(val))
  else:
    program_variables[target]['value'] = Decimal(GetValue(val))
  return

def SetArrayValue(val, target):
  global program_variables
  indexes = target[1].copy()
  for i in range(len(indexes)):
    indexes[i] = int(GetValue(indexes[i]))
  info = program_variables[target[0]]
  dimensions = info.get('dimensions').copy()
  offset = GetOffset(dimensions, indexes)
  if('int' in info.get('type')):
    info['value'][offset] = int(GetValue(val))
  else:
    info['value'][offset] = Decimal(GetValue(val))
  return
  
def ApplyOperation(op_one,op_two,operator):
  if(operator == 'plus'):
    return GetValue(op_one) + GetValue(op_two)
  elif(operator == 'minus'):
    return GetValue(op_one) - GetValue(op_two)
  elif(operator == 'multiply'):
    return GetValue(op_one) * GetValue(op_two)
  elif(operator == 'divide'):
    if(GetValue(op_two) == 0):
      print("Can't divide by 0")
      print("Program Terminated")
      sys.exit()
    return GetValue(op_one) / GetValue(op_two)
  elif(operator == 'pow'):
    return GetValue(op_one) ** GetValue(op_two)
  elif(operator == 'mod'):
    return GetValue(op_one) % GetValue(op_two)
  elif(operator == 'equals'):
    return GetValue(op_one) == GetValue(op_two)
  elif(operator == 'nequals'):
    return GetValue(op_one) != GetValue(op_two)
  elif(operator == 'gthan'):
    return GetValue(op_one) > GetValue(op_two)
  elif(operator == 'lthan'):
    return GetValue(op_one) < GetValue(op_two)
  elif(operator == 'geqthan'):
    return GetValue(op_one) >= GetValue(op_two)
  elif(operator == 'leqthan'):
    return GetValue(op_one) <= GetValue(op_two)
  elif(operator == 'and'):
    return GetValue(op_one) and GetValue(op_two)
  elif(operator == 'or'):
    return GetValue(op_one) or GetValue(op_two)
  
  

def ExecuteInstruction():
    global quads, program_counter
    inst = quads[program_counter]
    if(inst[0] == 'GOTO_UN'):
        UnconditionalGoto(inst)
    elif(inst[0] == 'GOTO_FALSE'):
        FalseGoto(inst)
    elif(inst[0] == 'GOTO_TRUE'):
        TrueGoto(inst)
    elif(inst[0] == 'FUNC_CALL'):
        FuncCall(inst)
    elif(inst[0] == 'PRINT'):
        Print(inst)
    elif(inst[0] == 'BCHECK'):
        BoundCheck(inst)
    elif(inst[0] == 'PRINTLN'):
        PrintLn(inst)
    elif(inst[0] == 'INPUT'):
        Input(inst)
    elif(inst[0] == 'ASSIGN'):
        Assign(inst)
    elif(inst == 'RETURN'):
        Return(inst)
    elif(inst[0] == 'EQUALS'):
        Equals(inst)
    elif(inst[0] == 'ADD'):
        Add(inst)
    elif(inst[0] == 'SUB'):
        Substract(inst)
    elif(inst[0] == 'MULT'):
        Multiply(inst)
    elif(inst[0] == 'DIV'):
        Divide(inst)
    elif(inst[0] == 'POW'):
        Pow(inst)
    elif(inst[0] == 'MOD'):
        Mod(inst)
    elif(inst[0] == 'NOTEQUALS'):
        NotEquals(inst)
    elif(inst[0] == 'GTHAN'):
        GreaterThan(inst)
    elif(inst[0] == 'LTHAN'):
        LessThan(inst)
    elif(inst[0] == 'GEQTHAN'):
        GreaterEqualThan(inst)
    elif(inst[0] == 'LEQTHAN'):
        LessEqualThan(inst)
    elif(inst[0] == 'AND'):
        And(inst)
    elif(inst[0] == 'OR'):
        Or(inst)

def UnconditionalGoto(inst):
  global program_counter
  program_counter = inst[1]
  return

def FalseGoto(inst):
  global program_counter
  if(not GetValue(inst[1])):
    program_counter = inst[2]
  else:
    program_counter += 1
  return

def TrueGoto(inst):
  global program_counter
  if(GetValue(inst[1])):
    program_counter = inst[2]
  else:
    program_counter+=1
  return

def FuncCall(inst):
  global program_counter, program_stack
  program_stack.append(program_counter+1)
  program_counter = inst[1]
  return

def Return(inst):
  global program_stack, program_counter
  program_counter = program_stack.pop()
  return

def Assign(inst):
  global program_counter
  SetValue(inst[1],inst[2])
  program_counter += 1
  return

def BoundCheck(inst):
    global program_variables, program_counter, quads
    bounds = inst[2]
    target = inst[1]
    if(len(bounds)!=len(target)):
        print("Matrix size error")
        print("Program terminated")
        program_counter == len(quads)
    else:
        for (indx, value) in enumerate(target):
            if(value in program_variables.keys()):
                value = program_variables[value].get('value')
            if(value >= bounds[indx]):
                print("Array out of bounds error")
                print("Program terminated")
                program_counter = len(quads)
    program_counter+=1
    return

def Input(inst):
  global program_counter, program_variables
  read_variable = float(input())
  program_variables[inst[1]] = {'value': read_variable, 'type' : 'float'}
  program_counter += 1
  return

def Add(inst):
  global program_counter, program_variables
  answer = ApplyOperation(inst[1],inst[2],'plus')
  program_variables[inst[3]] = {'value': answer, 'type' : 'float'}
  program_counter += 1
  return

def Substract(inst):
  global program_counter, program_variables
  answer = ApplyOperation(inst[1],inst[2],'minus')
  program_variables[inst[3]] = {'value': answer, 'type' : 'float'}
  program_counter += 1
  return

def Multiply(inst):
  global program_counter, program_variables
  answer = ApplyOperation(inst[1],inst[2],'multiply')
  program_variables[inst[3]] = {'value': answer, 'type' : 'float'}
  program_counter += 1
  return

def Divide(inst):
  global program_counter, program_variables
  answer = ApplyOperation(inst[1],inst[2],'divide')
  program_variables[inst[3]] = {'value': answer, 'type' : 'float'}
  program_counter += 1
  return

def Pow(inst):
  global program_counter, program_variables
  answer = ApplyOperation(inst[1],inst[2],'pow')
  program_variables[inst[3]] = {'value': answer, 'type' : 'float'}
  program_counter += 1
  return

def Mod(inst):
  global program_counter, program_variables
  answer = ApplyOperation(inst[1],inst[2],'mod')
  program_variables[inst[3]] = {'value': answer, 'type' : 'float'}
  program_counter += 1
  return

def Equals(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'equals')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def NotEquals(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'nequals')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def GreaterThan(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'gthan')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def LessThan(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'lthan')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def GreaterEqualThan(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'geqthan')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def LessEqualThan(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'leqthan')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def And(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'and')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def Or(inst):
  global program_counter, program_variables
  comparison = ApplyOperation(inst[1], inst[2], 'or')
  program_variables[inst[3]] = {'value': int(comparison), 'type': 'int'}
  program_counter += 1
  return

def Print(inst):
  global program_counter, program_variables
  val = inst[1]
  t = CheckType(val)
  if(t == 'str' and val not in program_variables.keys()):
    print(val.replace('"',""), end='')
  else:
    print(GetValue(val), end='')
  program_counter += 1
  return

def PrintLn(inst):
  global program_counter, program_variables
  val = inst[1]
  t = CheckType(val)
  if(t == 'str' and val not in program_variables.keys()):
    print(val.replace('"',""))
  else:
    print(GetValue(val))
  program_counter += 1
  return

def DefineVariables():
  global variables, program_variables
  for var in variables.items():
      if(isinstance(var[1], tuple) and 'Array' in var[1][0]):
          length = 1
          for n in var[1][1]:
              length = length * n
          program_variables[var[0]] = {"type":var[1][0],"value":[0]*length,"dimensions":var[1][1]}
      else:
          program_variables[var[0]] = {"type":var[1],"value":0}


if __name__ == '__main__':
    global program_counter, data, quads, variables, program_variables, program_stack
    getcontext().prec=2
    program_counter = 0
    data = pickle.load(open("out.p","rb"))
    quads = data['Quadruples']
    variables = data['SymbolTable']
    program_variables = {}
    program_stack = []
    DefineVariables()
    # print(quads)

    while(program_counter < len(quads)):
        ExecuteInstruction()

    # print(program_variables)
