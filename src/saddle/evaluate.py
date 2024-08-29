#    Copyright (C) 2013, 2014 Konstantin S. Solnushkin
#
#    This file is part of "SADDLE", a scripting language used to design
#    high-performance computer clusters.
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Parser based on PLY

import ply.lex as lex
from ply.lex import TOKEN
import ply.yacc as yacc
import math

################  START OF LEXER DEFINITIONS ################

# The task of the lexer is to split the input string into a list of smaller
# strings called "tokens", using regular expressions.

# List of token names
tokens = [
    'FPNUMBER',     # Floating-point number
    'INTNUMBER',    # Integer number
    'STRING',       # String
    'PLUS',         # Addition
    'MINUS',        # Subtraction and unary minus
    'TIMES',        # Multiplication
    'DIVIDE',       # Division
    'LPAREN',       # Left parenthesis
    'RPAREN',       # Right parenthesis
    'ID',           # Identifier (a sequence of characters, say, "x" or "unit_price")
    'ASSIGN',       # Assignment operator
    'NE1',          # Not equal, first type
    'NE2',          # Not equal, second type
    'GREATER',      # Various types of comparison operators
    'LESS',         # 
    'GE',           #
    'LE',           #
    'EQ',           # Equality operator
    'PE',           # += operator
    'ME',           # -= operator
    'TE',           # *= operator
    'DE',           # /= operator
    'NOT',          # Used to negate statements
    'SEP',          # Separator between statements
]

# Reserved words (mathematical functions that can be used in expressions
# and the substring search operator)
reserved = {
   'ceil'  : 'CEIL',
   'floor' : 'FLOOR',
   'round' : 'ROUND',
   'in'    : 'IN',
}

# Add reserved words to the list of tokens
tokens += list(reserved.values())

# Ignored characters (spaces, tabs, new lines)
t_ignore = " \t\n"

# Regular expression rules for detecting simple tokens
# (they don't require any actions)
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_EQ      = r'=='  # A Python-style equality operator
t_ASSIGN  = r'='   # A Python-style assignment operator
t_NE1     = r'!='  # Two types of inequality operators, Python-style (this one)...
t_NE2     = r'<>'  # ...and this one (Pascal-style)
t_GREATER = r'>'
t_LESS    = r'<'
t_GE      = r'>='
t_LE      = r'<='
t_PE      = r'\+='
t_ME      = r'-='
t_TE      = r'\*='
t_DE      = r'/='
t_NOT     = r'!'
t_SEP     = r';'

# Some regular expressions do require actions; these are listed below as functions:

# Floating-point numbers
# First, define grammar for floating-point numbers,
# formed from similar expressions from Python docs:
#   floatnumber   ::=  pointfloat | exponentfloat
#   pointfloat    ::=  [intpart] fraction | intpart "."
#   exponentfloat ::=  (intpart | pointfloat) exponent
#   intpart       ::=  digit+
#   fraction      ::=  "." digit+
#   exponent      ::=  ("e" | "E") ["+" | "-"] digit+

digit            = r'[0-9]'
intpart          = digit + r'+'
exponent         = r'[eE][+-]?' + digit + r'+'
fraction         = r'\.' + digit + r'+'
pointfloat       = r'((' + intpart + r')?' + fraction + r')|(' + intpart + r'\.)'
exponentfloat    = r'((' + intpart + r')|(' + pointfloat + r'))' + exponent
floatnumber      = r'(' + exponentfloat + r')|(' + pointfloat + r')'

@TOKEN(floatnumber)
def t_FPNUMBER(t):
    # An (optional) string of digits, then a decimal point ("."), then some more digits.
    # Matches tokens like "1.3", "0.8" and ".6"
    # This rule goes _before_ the corresponding rule for integer numbers,
    # because we want the string "1.3" to match a single floating-point number,
    # and not two integer numbers "1" and "3" and a decimal point between them.
    # Convert the string representation into a float value:
    t.value = float(t.value)
    return t

# Integer numbers
def t_INTNUMBER(t):
    r'\d+'
    # Simply matches a string of digits. This function goes _after_
    # the corresponding function for floating-point numbers.
    t.value = int(t.value)    
    return t

# Strings
def t_STRING(t):
    r'\'[a-zA-Z_0-9 -]*\'|\"[a-zA-Z_0-9 -]*\"'
    # The regular expression above says: it is either a...
    # <single-quote> <zero or more alphanumeric characters, spaces or dashes> <single-quote>,
    # or it is...
    # <double-quote> <zero or more alphanumeric characters, spaces or dashes> <double-quote>.
    # That is, "a" and 'a' are both OK, but "a' or 'a" are illegal
    # (due to different quote types used).
    #
    # Now, strip the quotes -- the first and the last character:
    stripped_string = str(t.value[1:len(t.value)-1])
    t.value = stripped_string
    return t

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    # Matches a token that contains letters, digits and underscores, but the first symbol is not a digit
    # Matches "x", "unit_price", "MyValue2", etc.
    #
    # Check if the token is present in the dictionary of reserved words ("ceil", "round", etc.),
    # and if so, returns the corresponding string to be used by the parser ("CEIL", "ROUND", etc.)
    # If the token is not a reserved word, just return the string 'ID':
    t.type = reserved.get(t.value,'ID')    # Check for reserved words, otherwise return the string 'ID'
    return t

def t_COMMENT(t):
    r'\#.*'
    pass
    # Everything after a "#" character is a comment. Discard the whole token.

################    END OF LEXER DEFINITIONS #################

################  START OF PARSER DEFINITIONS ################

# The task of the parser is to look at the input sequence of tokens
# it receives, recognise expressions and reduce them.
# For example, "5+3" matches the right-hand side
# of the rule 'expression : expression PLUS expression'
# in the function "p_expression_plus" below, and is reduced
# to "8" by the same function.

# Precedence is used to avoid conflicts when the parser doesn't
# know what to do -- reduce the expression it already found or
# read more tokens.
precedence = (
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE'),
    ('right','UMINUS'),
    )

# The functions below match expressions and reduce them.

# Statements can be simple expressions or comparisons, or they can be assignments.

def p_statement_list_1(p):
    '''statement_list : statement'''
    # Return the statement
    p[0] = p[1]

def p_statement_list_2(p):
    '''statement_list : statement_list SEP statement'''
    # Return the statement
    p[0] = p[3]

def p_not_statement(p):
    '''statement : NOT statement'''
    p[0] = not p[2]

def p_statement(p):
    '''statement : expression
                 | comparison'''
    # If the statement is not an assignment, just return the right-hand side
    p[0] = p[1]

def p_statement_comparison(p):
    '''comparison : expression EQ expression
                  | expression GREATER expression
                  | expression LESS expression
                  | expression GE expression
                  | expression LE expression
                  | expression NE1 expression
                  | expression NE2 expression
                  | expression IN expression'''
    # Matches comparisions and returns their logical value (True or False).
    # For example, if "x" was assigned the value of "5",
    # comparison "x=7-2" would return "True", and "x=3" would return "False".
    #
    # Comparisons with a "None" value always return "False", which can be misleading
    # (except "None" == "None" which returns "True" and is even more misleading).
    # Therefore, for safety, we check whether we were passed "None" in one of the
    # expressions, and if so, return "None":
    if (p[1] == None) or (p[3] == None):
        p[0] = None
        return
    # Otherwise, return "True" or "False", depending on the comparison sign
    if p[2] == t_EQ: p[0] = (p[1] == p[3])
    elif p[2] == t_GREATER: p[0] = (p[1] > p[3])
    elif p[2] == t_LESS: p[0] = (p[1] < p[3])
    elif p[2] == t_GE: p[0] = (p[1] >= p[3])
    elif p[2] == t_LE: p[0] = (p[1] <= p[3])
    elif (p[2] == t_NE1) or (p[2] == t_NE2): p[0] = (p[1] != p[3])
    elif p[2] == 'in': p[0] = str(p[1]).lower() in str(p[3]).lower()
    # Should never reach here
    else: print('{}: Unrecognised comparison operator: {}'.format(__name__, p[2]))

def p_statement_assign(p):
    '''statement : ID ASSIGN expression
                 | ID ASSIGN comparison'''
    # Matches assignment statements, such as "x := 5+3", returns the evaluated result.
    # With more complex assignments such as "t := x=4" returns a boolean value.
    global added_names
    # Evaluates the expression and returns its value.
    p[0] = p[3]
    # If the result is not "None", save the value in two dictionaries below.
    # The latter dictionary is used to return results of evaluation
    # of large expressions.
    if p[0] != None:
        passed_names[p[1]] = p[0]
        added_names[p[1]] = p[0]

def p_statement_assign_arith(p):
    '''statement : ID PE expression
                 | ID ME expression
                 | ID TE expression
                 | ID DE expression'''
    # Operation such as "x += 3" is a special type of assignment:
    # it is equivalent to "x := x + 3"
    global added_names
    if p[2] == '+=': passed_names[p[1]] += p[3]
    elif p[2] == '-=': passed_names[p[1]] -= p[3]
    elif p[2] == '*=': passed_names[p[1]] *= p[3]
    elif p[2] == '/=': passed_names[p[1]] /= p[3]
    # Should never reach here
    else: print('{}: Unrecognised assignment operator: {}'.format(__name__, p[2]))
    # Return this:
    p[0] = passed_names[p[1]]
    # Save in the second dictionary
    added_names[p[1]] = p[0]

def p_expression_id(p):
    '''expression : ID'''
    # Matches identifiers which are not reserved words, such as "x", "unit_price" and "MyValue".
    # Looks up and returns their value.
    # Find the value of the identifier in the following dictionary:
    id_value = passed_names.get(p[1])
    if id_value != None:
        # Return what we found; can be a number or a logical value
        p[0] = id_value
    else:
        # If not found, it is a syntax error, and we cannot proceed:
        print('Identifier {} not defined, evaluation cancelled'.format(p[1]))
        # Return "None" for safety
        p[0] = None

def p_expression_uminus(p):
    '''expression : MINUS expression %prec UMINUS'''
    # Unary minus; it has the highest precedence
    p[0] = -p[2] 
 
def p_expression_arith(p):
    '''expression : expression TIMES expression
                  | expression DIVIDE expression
                  | expression PLUS expression
                  | expression MINUS expression'''
    # Encountering token "TIMES" indicates multiplication,
    # and this is just what we perform. Same with other functions.
    # Comparison with "t_TIMES" and "t_PLUS" doesn't work because
    # of the way they are defined, hence we simply use strings here.
    if p[2] == '*': p[0] = p[1] * p[3]
    elif p[2] == t_DIVIDE: p[0] = p[1] / p[3]
    elif p[2] == '+': p[0] = p[1] + p[3]
    elif p[2] == t_MINUS: p[0] = p[1] - p[3]
    # Should never reach here
    else: print('{}: Unrecognised arithmetic operator: {}'.format(__name__, p[2]))

def p_expression_simple(p):
    '''expression : INTNUMBER
                  | FPNUMBER
                  | STRING'''
    # Expression matched an integer or a floating-point number or a string;
    # simply return that.
    p[0] = p[1]

#def p_expression_substring(p):
#    '''expression : expression IN expression'''
    # Perform search in a string
#    p[0] = str(p[1]).lower() in str(p[3]).lower()

def p_expression_group(p):
    '''expression : LPAREN expression RPAREN'''
    # Expressions can be written inside the parenthesis;
    # return what is inside
    p[0] = p[2]

def p_parenthesised_expr(p):
    '''parenthesised_expression : LPAREN expression RPAREN'''
    # Expressions inside the parenthesis. Similar to the rule above, but
    # the left-hand side is different. Used in grammar rules for functions
    # (see below), such as FUNC(expr), where FUNC is "ceil", "round", etc.
    # For example, we would write "ceil(0.6)" rather than "ceil 0.6"
    p[0] = p[2]

def p_expr_ceil(p):
    '''expression : CEIL parenthesised_expression'''
    # "Ceiling" function: according to the Python manual,
    # ceil(x) returns "the smallest integer greater than or equal to x":
    # >>> from math import ceil
    # >>> ceil(2.3)
    # 3
    # >>> ceil(-2.3)
    # -2
    #
    p[0] = math.ceil(p[2])

def p_expr_floor(p):
    '''expression : FLOOR parenthesised_expression'''
    # According to the Python manual, floor(x) returns "the floor of x,
    # the largest integer less than or equal to x":
    # >>> from math import floor
    # >>> floor(2.8)
    # 2
    # >>> floor(-2.8)
    # -3
    #
    p[0] = math.floor(p[2])

def p_expr_round(p):
    '''expression : ROUND parenthesised_expression'''
    # Beware that rounding is done according to Python rules,
    # hence round(1.5) equals 2, but round(2.5) also equals 2, not 3, as some may expect:
    # >>> round(1.5)
    # 2
    # >>> round(2.5)
    # 2
    #
    p[0] = round(p[2])

# Error rule for syntax errors
def p_error(p):
    # People start counting from "1", hence "+1" in the error position output.
    print("Syntax error at position {}:".format(p.lexpos+1))
    # Print the malformed string
    print(string_to_parse)
    # Print enough spaces and then the caret sign indicating the position
    print(' ' * p.lexpos + '^')

def p_reserved_word_error(p):
    '''expression : error parenthesised_expression'''
    # This rule matches a special "error" token. Here, erroneously spelled reserved words
    # are followed by a parenthesised expression. For example, instead of "round(5.3)"
    # we may have received "roind(5.3)"
    
    # "start1" is the start of the offending reserved word
    start1, end1 = p.lexspan(1)
    # "start2" is the start of the parenthesised expression
    start2, end2 = p.lexspan(2)
    # The misspelled reserved word:
    word = string_to_parse[start1:start2]
    print("Hint: syntax error in reserved word '{}'".format(word))
    # return "None":
    p[0] = None

################   END OF PARSER DEFINITIONS #################

def evaluate(s, outer_names):
    # The main jewel of this module; the other functions just do the dirty work.
    #
    # This function is passed the string to be parsed and the dictionary of names.
    # The string could, for example, look like this:
    #
    # cost := round(15 * unit_price)
    #
    # The function evaluates the expression in the right-hand side. When it needs to
    # substitute identifier values (such as "unit_price"), it looks them up in the
    # passed dictionary.
    #
    # The function returns the result of the evaluation, together with the dictionary
    # of metrics that were evaluated:
    
    # Don't create local variables with these names, use global variables instead:
    global passed_names, added_names, string_to_parse
    # Assign the contents of the passed dictionary to the global variable
    # so that it is accessible to other functions in this module
    passed_names = outer_names
    # Initialise to an empty value, this will be updated as required
    # by functions such as "p_statement_assign" (called by the parser),
    # and returned as this function's result:
    added_names = {}
    # Same with the string to be parsed
    string_to_parse = s
    # Parse the string. Tracking is enabled to report correct positions of syntax errors.
    result = parser.parse(s, lexer=lexer, tracking=True)
    # "added_names" has been updated by the parser
    return (result, added_names)

####################### INITIALISATION #######################

# Global variable for storing names passed by the outer module
passed_names = {}

# Here we will store values of assignment expressions
added_names = {}

# Global variable for storing the string to be parsed
# (used for error reporting)
string_to_parse = ''

# Build the lexer
lexer = lex.lex()

# Build the parser; "debug=0" instructs not to write the debug output file "parser.out"
parser = yacc.yacc(debug=0)

# If this file is called directly (not imported as a module), act as a calculator:
if __name__ == "__main__":
    while True:
        try:
            # Print variables already defined, if any
            if len(passed_names):
                print("Avalaible variables: ", passed_names)
            # Give some space
            print()
            # Seek input
            s = input('calc > ')
        except EOFError:
            break
        if not s:
            continue
        result, junk = evaluate(s, passed_names)
        print("Result of evaluation: ", result)
