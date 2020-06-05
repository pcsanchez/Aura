import ply.lex as lex
import ply.yacc as yacc
import sys
import pickle

quadruples = []
goto_stack = []
value_stack = []
error_list = []

symbol_table = {}

temporals = 0;

reserved = {
    'if' : 'IF',
    'end' : 'END',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'for' : 'FOR',
    'next' : 'NEXT',
    'to' : 'TO',
    'program' : 'PROGRAM',
    'start' : 'START',
    'finish' : 'FINISH',
    'dim' : 'DIM',
    'as' : 'AS',
    'int' : 'INT',
    'float' : 'FLOAT',
    'subroutine' : 'SUBROUTINE',
    'return' : 'RETURN',
    'gosub' : 'GOSUB',
    'print' : 'PRINT',
    'println' : 'PRINTLN',
    'then' : 'THEN',
    'input' : 'INPUT'
}

tokens = [
    'NUMBER',
    'ID',
    'ADDSUB',
    'MULTDIV',
    'EQUALS',
    'LPAREN',
    'RPAREN',
    'NOT',
    'AND',
    'OR',
    'RELATIONAL',
    'SEMICOLON',
    'FNUMBER',
    'LBRACKET',
    'RBRACKET',
    'STRING'
 ] + list(reserved.values())

t_STRING = r'\".*\"'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_EQUALS = r'\='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_NOT = r'\!'
t_AND = r'\&\&'
t_OR = r'\|\|'
t_RELATIONAL = r'\<\=|\>\=|\>|\<|\!\=|\=\='
t_SEMICOLON = r'\;'

t_ignore = ' \t'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_FNUMBER(t):
    r'\-?\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUMBER(t):
    r'\-?\d+'
    t.value = int(t.value)
    return t

def t_ADDSUB(t):
    r'\+|\-'
    return t

def t_MULTDIV(t):
    r'\*|\/|\^|\%'
    return t

def t_error(t):
    print("Illegal Character '%s'" % t.value[0])
    t.lexer.skip(1)

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

lexer = lex.lex()

def p_S(p):
    '''
    S : MAIN_ACTION PROGRAM ID START DECL R MAIN_ACTION_C B FINISH
    '''
    p[0] = "PROGRAM COMPILED SUCCESFULLY."

def p_MAIN_ACTION_C(p):
    '''
    MAIN_ACTION_C :
    '''
    global quadruples
    quadruples[0] = ('GOTO_UN', len(quadruples))

def p_MAIN_ACTION(p):
    '''
    MAIN_ACTION :
    '''
    global quadruples
    quadruples.append(('GOTO_UN'))

def p_R(p):
    '''
    R : SUBROUTINE_RULE R
    |
    '''

def p_SUBROURINTE_RULE(p):
    '''
    SUBROUTINE_RULE : SUBROUTINE ID SUBACTION B RETURN
    '''
    if(len(p) > 2):
        global quadruples, symbol_table
        quadruples.append(('RETURN'))
        symbol_table[p[2]] = symbol_table['temp']
        del symbol_table['temp']

def p_SUBACTION(p):
    '''
    SUBACTION :
    '''
    global symbol_table, quadruples
    symbol_table['temp'] = ('ROUTINE', len(quadruples))


def p_DECL(p):
    '''
    DECL : DIM ID AS TYPE SEMICOLON DECL
    | DIM ID ARRAY AS TYPE SEMICOLON DECL
    |
    '''
    global error_list
    if(len(p) > 2):
        if len(p) > 7:
            if p[2] in symbol_table.keys():
                error_list.append("Can't declare same variable more than once. Error in line " + str(p.lineno(2)))
            symbol_table[p[2]] = (p[5] + str('Array'),p[3])
        else:
            if p[2] in symbol_table.keys():
                error_list.append("Can't declare same variable more than once. Error in line " + str(p.lineno(2)))
            symbol_table[p[2]] = p[4]

def p_ARRAY(p):
    '''
    ARRAY : ARRAY_RULE ARRAY
    | ARRAY_RULE
    '''
    if(len(p) > 2):
        p[0] = [p[1]]+p[2]
    else:
        p[0] = [p[1]]

def p_ARRAY_RULE(p):
    '''
    ARRAY_RULE : LBRACKET EXPRESSION RBRACKET
    '''
    p[0] = p[2]

def p_TYPE(p):
    '''
    TYPE : INT
    | FLOAT
    '''
    p[0] = p[1]

def p_B(p):
    '''
    B : STATEMENT B
    | STATEMENT
    |
    '''

def p_STATEMENT(p):
    '''
    STATEMENT : IF_RULE
    | IFELSE_RULE
    | WHILE_RULE
    | GOSUB_RULE
    | FOR_RULE
    | PRINT_RULE
    | PRINTLN_RULE
    | INPUT_RULE
    | ASSIGN
    '''

def p_INPUT_RULE(p):
    '''
    INPUT_RULE : INPUT LPAREN EXPRESSION RPAREN SEMICOLON
    '''
    global quadruples, symbol_table, error_list
    if(isinstance(p[3],list)):
        if(p[3][0] not in symbol_table.keys()):
            error_list.append('Undeclared identifier in line ' + str(p.lineno(3)))
        else:
            temp = get_next_temporal()
            quadruples.append(('INPUT', temp))
            quadruples.append(('ASSIGN', temp, p[3]))
    else:
        if(p[3] not in symbol_table.keys()):
            error_list.append('Undeclared identifier in line ' + str(p.lineno(3)))
        else:
            temp = get_next_temporal()
            quadruples.append(('INPUT', temp))
            quadruples.append(('ASSIGN', temp, p[3]))

def p_PRINT_RULE(p):
    '''
    PRINT_RULE : PRINT LPAREN STRING RPAREN SEMICOLON
    | PRINT LPAREN EXPRESSION RPAREN SEMICOLON
    '''
    global quadruples
    quadruples.append(('PRINT', p[3]))

def p_PRINTLN_RULE(p):
    '''
    PRINTLN_RULE : PRINTLN LPAREN STRING RPAREN SEMICOLON
    | PRINTLN LPAREN EXPRESSION RPAREN SEMICOLON
    '''
    global quadruples
    quadruples.append(('PRINTLN', p[3]))

def p_GOSUB_RULE(p):
    '''
    GOSUB_RULE : GOSUB ID SEMICOLON
    '''
    global symbol_table, quadruples, error_list
    if( p[2] not in symbol_table.keys()):
        error_list.append("Undeclared Identifier in line " + str(p.lineno(2)))
    elif(not isinstance(symbol_table[p[2]], tuple)):
        error_list.append("Wrong Type of Identifier in line " + str(p.lineno(2)))
    else:
        quadruples.append(('FUNC_CALL', symbol_table[p[2]][1]))

def p_FOR_RULE(p):
    '''
    FOR_RULE : FOR ASSIGN TO EXPRESSION FOR_ACTION B NEXT ID
    '''
    global quadruples, goto_stack
    jump = goto_stack[-1]
    goto_stack.pop()
    temp = get_next_temporal()
    quadruples.append(('ADD', p[2], 1, temp))
    quadruples.append(('ASSIGN', temp, p[2]))
    quadruples.append(('GOTO_UN', jump))
    quadruples[jump+1] = ('GOTO_TRUE', quadruples[jump][3], len(quadruples))

def p_FOR_ACTION(p):
    '''
    FOR_ACTION :
    '''
    global quadruples, value_stack, goto_stack
    temp = get_next_temporal()
    quadruples.append(('EQUALS', quadruples[-1][2], value_stack[-1], temp))
    quadruples.append(('GOTO_TRUE', temp))
    goto_stack.append(len(quadruples)-2)
    


def p_WHILE_RULE(p):
    '''
    WHILE_RULE : WHILE LPAREN WHILE_ACTION EXPRESSION WHILE_ACTION_JUMP RPAREN B END
    '''
    global goto_stack, quadruples
    jump = goto_stack[-1]
    goto_stack.pop()
    jump2 = goto_stack[-1]
    goto_stack.pop()
    quadruples.append(('GOTO_UN', jump2))
    quadruples[jump] = ('GOTO_FALSE', quadruples[jump][1], len(quadruples))

def p_WHILE_ACTION(p):
    '''
    WHILE_ACTION : 
    '''
    global goto_stack, quadruples
    goto_stack.append(len(quadruples))

def p_WHILE_ACTION_JUMP(p):
    '''
    WHILE_ACTION_JUMP :
    '''
    global quadruples, value_stack, goto_stack
    quadruples.append(('GOTO_FALSE', value_stack[-1]))
    goto_stack.append(len(quadruples)-1)
    

def p_IF_RULE(p):
    '''
    IF_RULE : IF LPAREN EXPRESSION IF_ACTION RPAREN THEN B END
    '''
    global goto_stack, quadruples
    jump = goto_stack[-1]
    goto_stack.pop()
    quadruples[jump] = ('GOTO_FALSE', quadruples[jump][1], len(quadruples))

def p_IFELSE_RULE(p):
    '''
    IFELSE_RULE : IF LPAREN EXPRESSION IF_ACTION RPAREN THEN B ELSE_ACTION ELSE_RULE END 
    '''
    global goto_stack, quadruples
    jump = goto_stack[-1]
    goto_stack.pop()
    quadruples[jump] = ('GOTO_UN', len(quadruples))

def p_ELSE_RULE(p):
    '''
    ELSE_RULE : ELSE B
    '''


def p_IF_ACTION(p):
    '''
    IF_ACTION :
    '''
    global quadruples, goto_stack
    quadruples.append(('GOTO_FALSE',value_stack[-1]))
    goto_stack.append(len(quadruples)-1)

def p_ELSE_ACTION(p):
    '''
    ELSE_ACTION :
    '''
    global quadruples
    quadruples.append(('GOTO_UN'))
    jump = goto_stack[-1]
    goto_stack.pop()
    goto_stack.append(len(quadruples)-1)
    quadruples[jump] = ('GOTO_FALSE', quadruples[jump][1], len(quadruples))


def p_ASSIGN(p):
    '''
    ASSIGN : ID EQUALS EXPRESSION SEMICOLON
    | ID ARRAY BOUND_CHECK EQUALS EXPRESSION SEMICOLON
    '''
    global operand_stack, symbol_table, error_list, goto_stack
    if(not p[1] in symbol_table):
        print("Undeclared Identifier in line: " + str(p.lineno(1)))
        sys.exit()
    elif(len(p) < 6):
        operand1 = p[3]
        result = p[1]
        quadruples.append(('ASSIGN',operand1, result))
        p[0] = p[1]
    else:
        jump = goto_stack.pop()
        if(isinstance(symbol_table[p[1]], tuple)):
            if('Array' in symbol_table[p[1]][0]):
                quadruples[jump] = ('BCHECK', p[2], symbol_table[p[1]][1])
                quadruples.append(('ASSIGN',p[5], [p[1], p[2]]))
            else:
                error_list.append("Wrong Type of Identifier in line " + str(p.lineno(1)))
        else:
            error_list.append("Wrong Type of Identifier in line " + str(p.lineno(1)))

def p_BOUND_CHECK(p):
    '''
    BOUND_CHECK :
    '''
    global quadruples
    quadruples.append(('BCHECK',))
    goto_stack.append(len(quadruples)-1)

def p_EXPRESSION(p):
    '''
    EXPRESSION : OR_LEVEL
    '''
    p[0] = p[1]
    value_stack.append(p[0])

def p_OR_LEVEL(p):
    '''
    OR_LEVEL : OR_LEVEL OR AND_LEVEL
    | AND_LEVEL
    '''
    if(len(p) > 2):
        temp = get_next_temporal()
        quadruples.append(('OR', p[1], p[3], temp))
        p[0] = temp
    else:
        p[0] = p[1]

def p_AND_LEVEL(p):
    '''
    AND_LEVEL : AND_LEVEL AND RELATIONAL_LEVEL
    | RELATIONAL_LEVEL
    '''
    if(len(p) > 2):
        temp = get_next_temporal()
        quadruples.append(('AND', p[1], p[3], temp))
        p[0] = temp
    else:
        p[0] = p[1]

def p_RELATIONAL_LEVEL(p):
    '''
    RELATIONAL_LEVEL : RELATIONAL_LEVEL RELATIONAL ADDSUB_LEVEL
    | ADDSUB_LEVEL
    '''
    if(len(p) > 2):
        temp = get_next_temporal()
        if(p[2] == '<='):
            quadruples.append(('LEQTHAN',p[1],p[3],temp))
        elif(p[2] == '>='):
            quadruples.append(('GEQTHAN',p[1],p[3],temp))
        elif(p[2] == '=='):
            quadruples.append(('EQUALS',p[1],p[3],temp))
        elif(p[2] == '!='):
            quadruples.append(('NOTEQUALS',p[1],p[3],temp))
        elif(p[2] == '>'):
            quadruples.append(('GTHAN', p[1], p[3], temp))
        elif(p[2] == '<'):
            quadruples.append(('LTHAN', p[1], p[3], temp))
        p[0] = temp
    else:
        p[0] = p[1]

def p_ADDSUB_LEVEL(p):
    '''
    ADDSUB_LEVEL : ADDSUB_LEVEL ADDSUB MULTDIV_LEVEL
    | MULTDIV_LEVEL
    '''
    if(len(p) > 2):
        temp = get_next_temporal()
        if(p[2] == "+"):
            quadruples.append(('ADD',p[1], p[3], temp))
        else:
            quadruples.append(('SUB',p[1], p[3], temp))
        p[0] = temp
    else:
        p[0] = p[1]

def p_MULTDIV_LEVEL(p):
    '''
    MULTDIV_LEVEL : MULTDIV_LEVEL MULTDIV NOT_LEVEL
    | NOT_LEVEL
    '''
    if(len(p) > 2):
        temp = get_next_temporal()
        if(p[2] == '*'):
            quadruples.append(('MULT',p[1], p[3], temp))
        elif(p[2] == '%'):
            quadruples.append(('MOD',p[1], p[3], temp))
        elif(p[2] == '/'):
            quadruples.append(('DIV',p[1], p[3], temp))
        else:
            quadruples.append(('POW',p[1], p[3], temp))
        p[0] = temp
    else:
        p[0] = p[1]

    
def p_NOT_LEVEL(p):
    '''
    NOT_LEVEL : NOT NOT_LEVEL
    | F
    '''
    if(len(p) > 2):
        temp = get_next_temporal()
        quadruples.append(('NOT', p[2], temp))
        p[0] = temp
    else:
        p[0] = p[1]


def p_F(p):
    '''
    F : ID
    | ID ARRAY BOUND_CHECK
    | FNUMBER
    | NUMBER
    | LPAREN EXPRESSION RPAREN
    
    '''
    global symbol_table, error_list
    if(len(p)==4 and not p[1] == '('):
        jump = goto_stack.pop()
        if(p[1] not in symbol_table):
            print("Undeclared Identifier in line " + str(p.lineno(2)))
            sys.exit()
        if(isinstance(symbol_table[p[1]], tuple)):
            if('Array' in symbol_table[p[1]][0]):
                quadruples[jump] = ('BCHECK', p[2], symbol_table[p[1]][1])
                p[0] = [p[1], p[2]]
            else:
                error_list.append("Wrong Type of Identifier in line " + str(p.lineno(1)))
        else:
            error_list.append("Wrong Type of Identifier in line " + str(p.lineno(1)))
    elif(len(p) > 3):
        p[0] = p[2]
    else:
        if(isinstance(p[1], str)):
            if(p[1] not in symbol_table):
                error_list.append("Undeclared identifier in line " + str(p.lineno(1)))
        p[0] = p[1]

def get_next_temporal():
    global temporals
    temporals = temporals + 1
    return "t" + str(temporals)

def p_error(p):
   print("\tSyntax error in line " + str(p.lineno))
   for error in error_list:
            print(error)
   sys.exit()

parser = yacc.yacc()

if __name__ == '__main__':
    try:
        arch_name = input()
        arch = open(arch_name,'r')
        info = arch.read()
        arch.close()
        if(yacc.parse(info, tracking = True) == 'PROGRAM COMPILED SUCCESFULLY.'):
            print("Correct syntax.")
        else:
            print("Syntax error.")

        if(len(error_list) > 0):
            for error in error_list:
                print(error)
        else:
            pickle.dump({"Quadruples": quadruples, "SymbolTable" : symbol_table}, open("out.p", "wb"))
    except EOFError:
        print(EOFError)
