#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import copy
from optparse import OptionParser

EOF = chr(255)

# Enumeration for allowing octals and ignoring junk when converting
# strings to numbers.
#enum ConversionFlags {
NO_FLAGS = 0
ALLOW_HEX = 1
ALLOW_OCTALS = 2
ALLOW_TRAILING_JUNK = 4

def IsInRange(c, lower_limit, higher_limit):
  assert(ord(c) != -1)
  return ord(lower_limit) <= ord(c) and ord(c) <= ord(higher_limit)

def IsDecimalDigit(c):
  # ECMA-262, 3rd, 7.8.3 (p 16)
  return IsInRange(c, '0', '9')

def IsLetter(c):
  return IsInRange(c, 'a', 'z') or IsInRange(c, 'A', 'Z')

def IsIdentifierStart(c):
  if c == '$' or c == '_' or c == '\\':
    return True
  return IsLetter(c)

def IsLineTerminator(c):
  return c == '\n'

def IsIdentifierPart(c):
  return IsIdentifierStart(c) or IsDecimalDigit(c) or c == '_';

def IsAssignmentOp(op):
  iop = IndexOf(op)
  return IndexOf("INIT_VAR") <= iop and iop <= IndexOf("ASSIGN_MOD")

def IsUnaryOp(op):
  iop = IndexOf(op)
  return IndexOf("NOT") <= iop and iop <= IndexOf("VOID") or \
    op == "ADD" or op == "SUB"

def IsCountOp(op):
  return op == "INC" or op == "DEC"

def IsCompareOp(op):
  iop = IndexOf(op)
  return IndexOf("EQ") <= iop and iop <= IndexOf("IN")

def IsBinaryOp(op):
  iop = IndexOf(op)
  return IndexOf("COMMA") <= iop and iop <= IndexOf("MOD")

def Precedence(tok):
  for token in tokens:
    if token[0] == tok:
      return token[2]
  assert(False)

def String(tok):
  for token in tokens:
    if token[0] == tok:
      return token[1]
  assert(False)

tokens = [
  # End of source indicator.
  ("EOS", "EOS", 0),

  # Punctuators (ECMA-262, section 7.7, page 15).
  ("LPAREN", "(", 0),
  ("RPAREN", ")", 0),
  ("LBRACK", "[", 0),
  ("RBRACK", "]", 0),
  ("LBRACE", "{", 0),
  ("RBRACE", "}", 0),
  ("COLON", ":", 0) ,
  ("SEMICOLON", ";", 0),
  ("PERIOD", ".", 0),
  ("CONDITIONAL", "?", 3),
  ("INC", "++", 0),
  ("DEC", "--", 0),

  # Assignment operators.
  # IsAssignmentOp() relies on this block of enum values
  # being contiguous and sorted in the same order!
#  ("INIT_VAR", "=init_var", 2),  # AST-use only.
  ("INIT_VAR", "=", 2),  # AST-use only.
  ("INIT_CONST", "=init_const", 2),  # AST-use only.
  ("ASSIGN", "=", 2),
  ("ASSIGN_BIT_OR", "|=", 2),
  ("ASSIGN_BIT_XOR", "^=", 2),
  ("ASSIGN_BIT_AND", "&=", 2),
  ("ASSIGN_SHL", "<<=", 2),
  ("ASSIGN_SAR", ">>=", 2),
  ("ASSIGN_SHR", ">>>=", 2),
  ("ASSIGN_ADD", "+=", 2),
  ("ASSIGN_SUB", "-=", 2),
  ("ASSIGN_MUL", "*=", 2),
  ("ASSIGN_DIV", "/=", 2),
  ("ASSIGN_MOD", "%=", 2),

  # Binary operators sorted by precedence.
  # IsBinaryOp() relies on this block of enum values
  # being contiguous and sorted in the same order!
  ("COMMA", ",", 1),
  ("OR", "||", 4),
  ("AND", "&&", 5),
  ("BIT_OR", "|", 6),
  ("BIT_XOR", "^", 7),
  ("BIT_AND", "&", 8),
  ("SHL", "<<", 11),
  ("SAR", ">>", 11),
  ("SHR", ">>>", 11),
  ("ADD", "+", 12),
  ("IADD", ".+", 12),
  ("SUB", "-", 12),
  ("MUL", "*", 13),
  ("DIV", "/", 13),
  ("MOD", "%", 13),

  # Binary operators sorted by precedence.
  # IsBinaryOp() relies on this block of enum values
  # being contiguous and sorted in the same order!
  ("COMMA", ",", 1),
  ("OR", "||", 4),
  ("AND", "&&", 5),
  ("BIT_OR", "|", 6),
  ("BIT_XOR", "^", 7),
  ("BIT_AND", "&", 8),
  ("SHL", "<<", 11),
  ("SAR", ">>", 11),
  ("SHR", ">>>", 11),
  ("ADD", "+", 12),
  ("IADD", ".+", 12),
  ("SUB", "-", 12),
  ("MUL", "*", 13),
  ("DIV", "/", 13),
  ("MOD", "%", 13),

  # Compare operators sorted by precedence.
  # IsCompareOp() relies on this block of enum values
  # being contiguous and sorted in the same order!
  ("EQ", "==", 9),
  ("NE", "!=", 9),
  ("EQ_STRICT", "===", 9),
  ("NE_STRICT", "!==", 9),
  ("LT", "<", 10),
  ("GT", ">", 10),
  ("LTE", "<=", 10),
  ("GTE", ">=", 10),
  ("INSTANCEOF", "instanceof", 10),
  ("IN", "in", 10),

  # Unary operators.
  # IsUnaryOp() relies on this block of enum values
  # being contiguous and sorted in the same order!
  ("NOT", "!", 0),
  ("BIT_NOT", "~", 0),
  ("DELETE", "delete", 0),
  ("TYPEOF", "typeof", 0),
  ("VOID", "void", 0),

  # Keywords (ECMA-262, section 7.5.2, page 13).
  ("BREAK", "break", 0),
  ("CASE", "case", 0),
  ("CATCH", "catch", 0),
  ("CONTINUE", "continue", 0),
  ("DEBUGGER", "debugger", 0),
  ("DEFAULT", "default", 0),
  # DELETE
  ("DO", "do", 0),
  ("ELSE", "else", 0),
  ("FINALLY", "finally", 0),
  ("FOR", "for", 0),
  ("FUNCTION", "function", 0),
  ("IF", "if", 0),
  # IN
  # INSTANCEOF
  ("NEW", "new", 0),
  ("RETURN", "return", 0),
  ("SWITCH", "switch", 0),
  ("THIS", "this", 0),
  ("THROW", "throw", 0),
  ("TRY", "try", 0),
  # TYPEOF
  ("VAR", "var", 0),
  # VOID
  ("WHILE", "while", 0),
  ("WITH", "with", 0),

  # Literals (ECMA-262, section 7.8, page 16).
  ("NULL_LITERAL", "null", 0),
  ("TRUE_LITERAL", "true", 0),
  ("FALSE_LITERAL", "false", 0),
  ("NUMBER", None, 0),
  ("STRING", None, 0),

  # Identifiers (not keywords or future reserved words).
  ("IDENTIFIER", None, 0),

  # Illegal token - not able to scan.
  ("ILLEGAL", "ILLEGAL", 0),

  # Scanner-internal use only.
  ("WHITESPACE", None, 0)
]

def IndexOf(op):
  ret = 0
  for token in tokens:
    if token[0] == op:
      return ret
    ret = ret + 1

class KeywordMatcher:
  def __init__(self):
    self.buffer = ""
  def AddChar(self, c):
    self.buffer += c
  def token(self):
    for token in tokens:
      if token[1] == self.buffer:
        return token[0]
    return "IDENTIFIER"

class Scanner:
  class Location:
    def __init__(self, b = 0, e = 0):
      self.beg_pos = b
      self.end_pos = e

  class TokenDesc:
    def __init__(self):
      self.token = "ILLEGAL"
      self.location = Scanner.Location()
      self.literal_buffer = ""

  def __init__(self, source, position = 0):
    self.source_ = source
    self.position_ = position

    # Set c0_ (one character ahead)
    self.Advance()

    self.current_ = Scanner.TokenDesc()
    self.next_ = Scanner.TokenDesc()

    # Skip initial whitespace allowing HTML comment ends just like
    # after a newline and scan first token.
    self.has_line_terminator_before_next_ = True
    self.SkipWhiteSpace()
    self.Scan()

  def peek(self):
    return self.next_.token

  def Next(self):
    self.current_ = copy.copy(self.next_)
    self.Scan()
    return self.current_.token

  def literal_string(self):
    return self.current_.literal_string

  def source_pos(self):
    return self.position_

  def location(self):
    return self.current_.location

  def has_line_terminator_before_next(self):
    return self.has_line_terminator_before_next_

  def Select(self, *args):
    def Select1(tok):
      self.Advance()
      return tok

    def Select3(next, then, else_):
      self.Advance()
      if self.c0_ == next:
        Advance()
        return then
      else:
        return else_

    return (Select1 if len(args) == 1 else Select3)(*args)

  def Scan(self):
    self.next_.literal_buffer = ""
    token = "WHITESPACE"
    self.has_line_terminator_before_next_ = False
    while token == "WHITESPACE":
      # Remember the position of the next token
      self.next_.location.beg_pos = self.source_pos()

      # Continue scanning for tokens as long as we're just skipping
      # whitespace.

      if self.c0_ == ' ' or self.c0_ == '\t':
        self.Advance()
        token = "WHITESPACE"

      elif self.c0_ == '\n':
        self.Advance()
        self.has_line_terminator_before_next_ = True
        token = "WHITESPACE"

      elif self.c0_ == '"' or self.c0_ == '\'':
        token = self.ScanString()

      elif self.c0_ == '<':
        # < <= << <<= <!--
        self.Advance()
        if self.c0_ == '=':
          token = self.Select("LTE")
        elif self.c0_ == '<':
          token = self.Select("=", "ASSIGN_SHL", "SHL")
#        } else if (c0_ == '!') {
#          token = ScanHtmlComment();
        else:
          token = "LT"

      elif self.c0_ == '>':
        # > >= >> >>= >>> >>>=
        self.Advance()
        if self.c0_ == '=':
          token = self.Select("GTE")
        elif self.c0_ == '>':
          # >> >>= >>> >>>=
          self.Advance()
          if self.c0_ == '=':
            token = self.Select("ASSIGN_SAR")
          elif self.c0_ == '>':
            token = self.Select('=', "ASSIGN_SHR", "SHR")
          else:
            token = "SAR";
        else:
          token = "GT"

      elif self.c0_ == '=':
        self.Advance()
        if self.c0_ == '=':
          token = self.Select('=', "EQ_STRICT", "EQ")
        else:
          token = "ASSIGN"

      elif self.c0_ == '!':
        self.Advance()
        if self.c0_ == '=':
          token = self.Select('=', "NE_STRICT", "NE")
        else:
          token = "NOT"

      elif self.c0_ == '+':
        # + ++ +=
        self.Advance()
        if self.c0_ == '+':
          token = self.Select("INC")
        elif self.c0_ == '=':
          token = self.Select("ASSIGN_ADD")
        else:
          token = "ADD"

      elif self.c0_ == '-':
        # - -- --> -=
        self.Advance()
        if self.c0_ == '-':
          token = self.Select("DEC")
        elif self.c0_ == '=':
          token = self.Select("ASSIGN_SUB")
        else:
          token = "SUB"

      elif self.c0_ == '*':
        # * *=
        token = self.Select('=', "ASSIGN_MUL", "MUL")

      elif self.c0_ == '%':
        # % %=
        token = self.Select('=', "ASSIGN_MOD", "MOD")

      elif self.c0_ == '/':
        # /  // /* /=
        self.Advance();
        if self.c0_ == '/':
          token = self.SkipSingleLineComment()
        elif self.c0_ == '*':
          token = self.SkipMultiLineComment()
        elif self.c0_ == '=':
          token = self.Select("ASSIGN_DIV")
        else:
          token = "DIV"

      elif self.c0_ == '&':
        # & && &=
        self.Advance()
        if self.c0_ == '&':
          token = self.Select("AND")
        elif self.c0_ == '=':
          token = self.Select("ASSIGN_BIT_AND")
        else:
          token = "BIT_AND"

      elif self.c0_ == '|':
        # | || |=
        self.Advance()
        if self.c0_ == '|':
          token = self.Select("OR")
        elif self.c0_ == '=':
          token = self.Select("ASSIGN_BIT_OR")
        else:
          token = "BIT_OR"

      elif self.c0_ == '^':
        # ^ ^=
        token = self.Select('=', "ASSIGN_BIT_XOR", "BIT_XOR")

      elif self.c0_ == '.':
        # . Number
        self.Advance();
        if IsDecimalDigit(self.c0_):
          token = self.ScanNumber(True)
        else:
          token = "PERIOD"

      elif self.c0_ == ':':
        token = self.Select("COLON")

      elif self.c0_ == ';':
        token = self.Select("SEMICOLON")

      elif self.c0_ == ',':
        token = self.Select("COMMA")

      elif self.c0_ == '(':
        token = self.Select("LPAREN")

      elif self.c0_ == ')':
        token = self.Select("RPAREN")

      elif self.c0_ == '[':
        token = self.Select("LBRACK")

      elif self.c0_ == ']':
        token = self.Select("RBRACK")

      elif self.c0_ == '{':
        token = self.Select("LBRACE")

      elif self.c0_ == '}':
        token = self.Select("RBRACE")

      elif self.c0_ == '?':
        token = self.Select("CONDITIONAL")

      elif self.c0_ == '~':
        token = self.Select("BIT_NOT")

      else:
        if IsIdentifierStart(self.c0_):
          token = self.ScanIdentifier()
        elif IsDecimalDigit(self.c0_):
          token = self.ScanNumber(False)
        elif self.SkipWhiteSpace():
          token = "WHITESPACE"
        elif self.c0_ == EOF:
          token = "EOS"
        else:
          token = self.Select("ILLEGAL")

    self.next_.location.end_pos = self.source_pos()
    self.next_.token = token

  def ScanDecimalDigits(self):
    while IsDecimalDigit(self.c0_):
      self.AddCharAdvance()

  def ScanNumber(self, seen_period):
    assert(IsDecimalDigit(self.c0_))

    DECIMAL = 0
    HEX = 1
    OCTAL = 2
    kind = DECIMAL

    self.StartLiteral()
    if seen_period:
      # we have already seen a decimal point of the float
      self.AddChar('.')
      self.ScanDecimalDigits()  # we know we have at least one digit
    else:
      # if the first character is '0' we must check for octals and hex
      if self.c0_ == '0':
        self.AddCharAdvance()

        # either 0, 0exxx, 0Exxx, 0.xxx, an octal number, or a hex number
        if self.c0_ == 'x' or self.c0_ == 'X':
          kind = HEX
          self.AddCharAdvance()
          if not IsHexDigit(self.c0_):
            return "ILLEGAL"
          while IsHexDigit(self.c0_):
            self.AddCharAdvance()

        elif ord('0') <= ord(self.c0_) and ord(self.c0_) <= ord('7'):
          # (possible) octal number
          kind = OCTAL
          while True:
            if self.c0_ == '8' or self.c0_ == '9':
              kind = DECIMAL
              break
            if ord(self.c0_) < ord('0') or ord('7') < ord(self.c0_):
              break
            self.AddCharAdvance()

      # Parse decimal digits and allow trailing fractional part.
      if kind == DECIMAL:
        self.ScanDecimalDigits()  # optional
        if self.c0_ == '.':
          self.AddCharAdvance()
          self.ScanDecimalDigits()  # optional

    # scan exponent, if any
    if self.c0_ == 'e' or self.c0_ == 'E':
      assert(kind != HEX)  # 'e'/'E' must be scanned as part of the hex number
      if kind == OCTAL:
        return "ILLEGAL"  # no exponent for octals allowed
      # scan exponent
      self.AddCharAdvance()
      if self.c0_ == '+' or self.c0_ == '-':
        self.AddCharAdvance()
      if not IsDecimalDigit(self.c0_):
        # we must have at least one decimal digit after 'e'/'E'
        return "ILLEGAL"
      self.ScanDecimalDigits()

    # The source character immediately following a numeric literal must
    # not be an identifier start or a decimal digit; see ECMA-262
    # section 7.8.3, page 17 (note that we read only one decimal digit
    # if the value is 0).
    if IsDecimalDigit(self.c0_) or IsIdentifierStart(self.c0_):
      return "ILLEGAL"

    return "NUMBER"

  def ScanString(self):
    quote = self.c0_
    self.Advance()

    self.StartLiteral()
    while self.c0_ != quote and self.c0_ != EOF and not IsLineTerminator(self.c0_):
      c = self.c0_
      self.Advance()
      if c == '\\':
        if self.c0_ == EOF:
          return "ILLEGAL"
        ScanEscape()
      else:
        self.AddChar(c)

    if self.c0_ != quote:
      return "ILLEGAL"

    self.Advance()  # consume quote
    return "STRING"

  def Advance(self):
    if self.position_ == len(self.source_):
      self.c0_ = EOF
    else:
      self.c0_ = self.source_[self.position_]
      self.position_ += 1

  def SkipWhiteSpace(self):
    start_position = self.source_pos()

    while True:
      while self.c0_.isspace():
        # IsWhiteSpace() includes line terminators!
        if IsLineTerminator(self.c0_):
          # Ignore line terminators, but remember them. This is necessary
          # for automatic semicolon insertion.
          self.has_line_terminator_before_next_ = True
        self.Advance()
      # Return whether or not we skipped any characters.
      return self.source_pos() != start_position

  def SkipSingleLineComment(self):
    self.Advance()
    # The line terminator at the end of the line is not considered
    # to be part of the single-line comment; it is recognized
    # separately by the lexical grammar and becomes part of the
    # stream of input elements for the syntactic grammar (see
    # ECMA-262, section 7.4, page 12).
    while self.c0_ != EOF and not IsLineTerminator(self.c0_):
      self.Advance()
    return "WHITESPACE"

  def SkipMultiLineComment(self):
    assert(self.c0_ == '*')
    self.Advance()

    while self.c0_ != EOF:
      ch = self.c0_
      self.Advance()
      # If we have reached the end of the multi-line comment, we
      # consume the '/' and insert a whitespace. This way all
      # multi-line comments are treated as whitespace - even the ones
      # containing line terminators. This contradicts ECMA-262, section
      # 7.4, page 12, that says that multi-line comments containing
      # line terminators should be treated as a line terminator, but it
      # matches the behaviour of SpiderMonkey and KJS.
      if ch == '*' and self.c0_ == '/':
        self.c0_ = ' '
        return "WHITESPACE"

    # Unterminated multi-line comment.
    return "ILLEGAL"

  def StartLiteral(self):
    self.next_.literal_string = ""

  def AddChar(self, c):
    self.next_.literal_string += c

  def AddCharAdvance(self):
    self.AddChar(self.c0_)
    self.Advance()

  def ScanIdentifier(self):
    assert(IsIdentifierStart(self.c0_))

    self.StartLiteral()
    keyword_match = KeywordMatcher()

    if self.c0_ == '\\':
      assert(False)
    else:
      self.AddChar(self.c0_)
      keyword_match.AddChar(self.c0_)
      self.Advance()

    # Scan the rest of the identifier characters.
    while IsIdentifierPart(self.c0_):
      if self.c0_ == '\\':
        assert(False)
      else:
        self.AddChar(self.c0_)
        keyword_match.AddChar(self.c0_)
        self.Advance()

    return keyword_match.token()

##############
class JSObject:
  def __init__(self, kind, value):
    self.kind = kind
    self.value = value
  def IsString(self):
    return self.kind == "string"
  def IsNumber(self):
    return self.kind == "number"
  def Number(self):
    return self.value

JSNULL = JSObject("null", "null")
JSTRUE = JSObject("true", "true")
JSFALSE = JSObject("false", "false")
JSUNDEFINED = JSObject("undefined", "undefined")
##############

class SmiAnalysis:
  #enum Kind {
  UNKNOWN = 0
  LIKELY_SMI = 1

  def __init__(self):
    self.kind = self.UNKNOWN

  def Is(self, kind):
    return self.kind == kind

  def IsKnown(self):
    return not self.Is(self.UNKNOWN)
  def IsUnknown(self):
    return self.Is(self.UNKNOWN)
  def IsLikelySmi(self):
    return self.Is(self.LIKELY_SMI)

  def CopyFrom(self, other):
    self.kind = other.kind

  def Type2String(type):
    if type == self.UNKNOWN:
      return "UNKNOWN"
    elif type == self.LIKELY_SMI:
      return "LIKELY_SMI"
    assert("UNREACHABLE")

  def SetAsLikelySmi(self):
    self.kind = self.LIKELY_SMI

  def SetAsLikelySmiIfUnknown(self):
    if self.IsUnknown():
      self.SetAsLikelySmi()

# The AST refers to variables via VariableProxies - placeholders for the actual
# variables. Variables themselves are never directly referred to from the AST,
# they are maintained by scopes, and referred to from VariableProxies and Slots
# after binding and variable allocation.
class Variable:
  class UseCount:
    def __init__(self):
      self.nreads_ = 0
      self.nwrites_ = 0

    def RecordRead(self, weight):
      assert(weight > 0)
      self.nreads_ += weight
      # We must have a positive nreads_ here. Handle
      # any kind of overflow by setting nreads_ to
      # some large-ish value.
      if self.nreads_ <= 0: self.nreads_ = 1000000
      assert(self.is_read() and self.is_used())

    def RecordWrite(self, weight):
      assert(weight > 0)
      self.nwrites_ += weight
      # We must have a positive nwrites_ here. Handle
      # any kind of overflow by setting nwrites_ to
      # some large-ish value.
      if self.nwrites_ <= 0: self.nwrites_ = 1000000
      assert(self.is_written() and self.is_used())

    def RecordAccess(self, weight):
      self.RecordRead(weight)
      self.RecordWrite(weight)

    def RecordUses(self, uses):
      if uses.nreads() > 0: self.RecordRead(uses.nreads())
      if uses.nwrites() > 0: self.RecordWrite(uses.nwrites())

    def nreads(self): return self.nreads_
    def nwrites(self): return self.nwrites_
    def nuses(self): return self.nreads_ + self.nwrites_

    def is_read(self): return self.nreads() > 0
    def is_written(self): return self.nwrites() > 0
    def is_used(self): return self.nuses() > 0

  #enum Mode {
  # User declared variables:
  VAR = 0             # declared via 'var', and 'function' declarations

  CONST = 1           # declared via 'const' declarations

  # Variables introduced by the compiler:
  DYNAMIC = 2         # always require dynamic lookup (we don't know
            # the declaration)

  DYNAMIC_GLOBAL = 3  # requires dynamic lookup, but we know that the
            # variable is global unless it has been shadowed
            # by an eval-introduced variable

  DYNAMIC_LOCAL = 4   # requires dynamic lookup, but we know that the
            # variable is local and where it is unless it
            # has been shadowed by an eval-introduced
            # variable

  INTERNAL = 5        # like VAR, but not user-visible (may or may not
            # be in a context)

  TEMPORARY = 6       # temporary variables (not user-visible), never
            # in a context

  #enum Kind {
  NORMAL = 0
  THIS = 1
  ARGUMENTS = 2

  def __init__(self, scope, name, mode, is_valid_lhs, kind):
    self.scope = scope
    self.name = name
    self.mode = mode
    self.is_valid_LHS = is_valid_lhs
    self.kind = kind
    self.local_if_not_shadowed = None
    self.is_accessed_from_inner_scope = False
    self.rewrite = None
    self.var_uses_ = Variable.UseCount()
    #ASSERT(name->IsSymbol());

  def Mode2String(mode):
    for t in ((self.VAR, "VAR"),
          (self.CONST,"CONST"),
          (self.DYNAMIC,"DYNAMIC"),
          (self.DYNAMIC_GLOBAL,"DYNAMIC_GLOBAL"),
          (self.DYNAMIC_LOCAL,"DYNAMIC_LOCAL"),
          (self.INTERNAL,"INTERNAL"),
          (self.TEMPORARY,"TEMPORARY")):
      if mode == t[0]:
        return t[1]
    assert(0)

  def AsProperty(self):
    return None if self.rewrite == None else self.rewrite.AsProperty()
  def AsVariable(self):
    if self.rewrite == None or self.rewrite.AsSlot() != None:
      return self
    else:
      return None

  # The source code for an eval() call may refer to a variable that is
  # in an outer scope about which we don't know anything (it may not
  # be the global scope). scope() is NULL in that case. Currently the
  # scope is only used to follow the context chain length.
  def scope(self):
    return self.scope

  def name(self):
    return self.name
  def mode(self):
    return self.mode
  def is_accessed_from_inner_scope(self):
    return self.is_accessed_from_inner_scope
  def var_uses(self):
    return self.var_uses_
  def obj_uses(self):
    return self.obj_uses_

  def IsVariable(self, n):
    return not self.is_this() and self.name.value == n.value

  def is_dynamic(self):
    return self.mode == self.DYNAMIC or \
         self.mode == self.DYNAMIC_GLOBAL or \
         self.mode == self.DYNAMIC_LOCAL

  def is_global(self):
    # Temporaries are never global, they must always be allocated in the
    # activation frame.
    return self.mode != self.TEMPORARY and self.scope != None and \
      self.scope.is_global_scope()
  def is_this(self):
    return self.kind == self.THIS

  def is_possibly_eval(self):
    return self.IsVariable("eval") and \
      (self.mode == self.DYNAMIC or self.mode == self.DYNAMIC_GLOBAL)

  def local_if_not_shadowed(self):
    assert(self.mode == self.DYNAMIC_LOCAL and self.local_if_not_shadowed != None)
    return local_if_not_shadowed

  def set_local_if_not_shadowed(self, local):
    self.set_local_if_not_shadowed = local

  def rewrite(self):
    return self.rewrite
  def slot(self):
    return self.rewite.AsSlot() if self.rewrite != None else None

  def type(self):
    return self.type

  # added by keisuke
  def Accept(self, v):
    v.VisitVariable(self)

class Type:
  INT = 0
  FLOAT = 1
  BOOL = 2
  STRING = 3
  FUNCTION = 4
  NULL = 5
  UNKNOWN = 10

  @staticmethod
  def ToString(type):
    for T in ((Type.INT, "INT"), (Type.FLOAT, "FLOAT"), (Type.BOOL, "BOOL"),
          (Type.STRING, "STRING"), (Type.FUNCTION, "FUNCTION"),
          (Type.NULL, "NULL"), (Type.UNKNOWN, "UNKNOWN")):
      if T[0] == type:
        return T[1]
    assert(False)

class TypeNode:
  def __init__(self):
    self.types = []
    self.constraints = []

  def AddEdge(self, target):
    self.constraints.append(target)

  def Propagate(self):
    ret = False
    for constraint in self.constraints:
      for type in self.types:
        if not type in constraint.types:
          ret = True
          constraint.types.append(type);
          constraint.Propagate()
    return ret

class AstNode:
  def Accept(self): assert(False)

  # Type testing & conversion.
  def AsStatement(self): return None
  def AsExpressionStatement(self): return None
  def AsEmptyStatement(self): return None
  def AsExpression(self): return None
  def AsLiteral(self): return None
  def AsSlot(self): return None
  def AsVariableProxy(self): return None
  def AsProperty(self): return None
  def AsCall(self): return None
  def AsTargetCollector(self): return None
  def AsBreakableStatement(self): return None
  def AsIterationStatement(self): return None
  def AsUnaryOperation(self): return None
  def AsBinaryOperation(self): return None
  def AsAssignment(self): return None
  def AsFunctionLiteral(self): return None
  def AsMaterializedLiteral(self): return None
  def AsObjectLiteral(self): return None
  def AsArrayLiteral(self): return None

class Statement(AstNode):
  def AsStatement(self): return self
  def AsReturnStatement(self): return None
  def IsEmpty(self): return self.AsEmptyStatement() != None

class Expression(AstNode):
  # enum Context
  # Not assigned a context yet, or else will not be visited during
  # code generation.
  kUninitialized = 0
  # Evaluated for its side effects.
  kEffect = 1
  # Evaluated for its value (and side effects).
  kValue = 2
  # Evaluated for control flow (and side effects).
  kTest = 3
  # Evaluated for control flow and side effects.  Value is also
  # needed if true.
  kValueTest = 4
  # Evaluated for control flow and side effects.  Value is also
  # needed if false.
  kTestValue = 5

  def __init__(self):
    self.context = self.kUninitialized

  def AsExpression(self): return self

  def IsValidJSON(self): return False
  def IsValidLeftHandSide(self): return False

  def MarkAsStatement(self): None  # do nothing

  # Static type information for this expression.
  def type(self): return self.type

  def context(self): return self.context
  def set_context(self, context): self.context = context

class BreakableStatement(Statement):
  # enum Type
  TARGET_FOR_ANONYMOUS = 0
  TARGET_FOR_NAMED_ONLY = 1

  # The labels associated with this statement. May be NULL;
  # if it is != NULL, guaranteed to contain at least one entry.
  def labels(self): return self.labels

  def AsBreakableStatement(self): return self

  def break_target(self): return break_target

  def is_target_for_anonymous(self):
    return self.type == self.TARGET_FOR_ANONYMOUS

  def __init__(self, labels, type):
    self.labels = labels
    self.type = type
    assert(labels == None or len(labels) > 0)

class Block(BreakableStatement):
  def __init__(self, labels, capacity, is_initializer_block):
    BreakableStatement.__init__(self, labels, self.TARGET_FOR_NAMED_ONLY)
    self.statements = []
    self.is_initializer_block = is_initializer_block

  def Accept(self, v):
    v.VisitBlock(self)

  def AddStatement(self, statement):
    self.statements.append(statement)

  def statements(self):
    return self.statements

class Declaration(AstNode):
  def __init__(self, proxy, mode, fun):
    self.proxy = proxy
    self.mode = mode
    self.fun = fun
    assert(mode == Variable.VAR or mode == Variable.CONST)
    assert(fun == None or mode == Variable.VAR)

  def Accept(self, v):
    v.VisitDeclaration(self)

  def proxy(self):
    return self.proxy

  def mode(self):
    return self.mode

  def fun(self):
    return self.fun

class ExpressionStatement(Statement):
  def __init__(self, expression):
    self.expression = expression

  def Accept(self, v):
    v.VisitExpressionStatement(self)

  def AsExpressionStatement(self):
    return self

  def set_expression(self, e):
    self.expression = e
  def expression(self):
    return self.expression

class ReturnStatement(Statement):
  def __init__(self, expression):
    self.expression = expression

  def Accept(self, v):
    v.VisitReturnStatement(self)

  def AsReturnStatement(self):
    return self

  def expression(self):
    return self.expression

# If-statements always have non-null references to their then- and
# else-parts. When parsing if-statements with no explicit else-part,
# the parser implicitly creates an empty statement. Use the
# HasThenStatement() and HasElseStatement() functions to check if a
# given if-statement has a then- or an else-part containing code.
class IfStatement(Statement):
  def __init__(self, condition, then_statement, else_statement):
    self.condition = condition
    self.then_statement = then_statement
    self.else_statement = else_statement

  def Accept(self, v):
    v.VisitIfStatement(self)

  def HasThenStatement(self):
    return not self.then_statement.IsEmpty()
  def HasElseStatement(self):
    return not self.else_statement.IsEmpty()

class EmptyStatement(Statement):
  def Accept(self, v):
    v.VisitEmptyStatement(self)

  def AsEmptyStatement(self):
    return self

class Literal(Expression):
  def __init__(self, handle):
    self.handle = handle

  def Accept(self, v):
    v.VisitLiteral(self)

  def AsLiteral(self):
    return self

  def IsIdenticalTo(other):
    return self.handle == other.handle

  def IsValidJSON(self):
    return True

  def IsNull(self):
    return self.handle == JSNULL
  def IsTrue(self):
    return self.handle == JSTRUE
  def IsFalse(self):
    return self.handle == JSFALSE

  def handle(self):
    return self.handle

class VariableProxy(Expression):
  def __init__(self, name, is_this, inside_with):
    self.name = name
    self.var = None
    self.is_this = is_this
    self.inside_with = inside_with
    # names must be canonicalized for fast equality checks
    #ASSERT(name->IsSymbol());

  def Accept(self, v):
    v.VisitVariableProxy(self)

  def AsProperty(self):
    return None if self.var == None else self.var.AsProperty()
  def AsVariableProxy(self):
    return self

  def AsVariable(self):
    return None if self == None or self.var == None else self.var.AsVariable()

  def IsValidLeftHandSide(self):
    return True if self.var == None else self.var.IsValidLeftHandSide()

  def IsVariable(self, n):
    return self.is_this and self.name == n

  def IsArguments(self):
    variable = self.AsVariable()
    return False if variable == None else variable.is_arguments()

  def BindTo(self, var):
    assert(self.var == None)
    assert(var != None)
    #assert((self.is_this and var.is_this) or self.name.is_identical_to(var.name))
    self.var = var

class Slot(Expression):
  # enum Type {
  # A slot in the parameter section on the stack. index() is
  # the parameter index, counting left-to-right, starting at 0.
  PARAMETER = 0

  # A slot in the local section on the stack. index() is
  # the variable index in the stack frame, starting at 0.
  LOCAL = 1

  # An indexed slot in a heap context. index() is the
  # variable index in the context object on the heap,
  # starting at 0. var()->scope() is the corresponding
  # scope.
  CONTEXT = 2

  # A named slot in a heap context. var()->name() is the
  # variable name in the context object on the heap,
  # with lookup starting at the current context. index()
  # is invalid.
  LOOKUP = 3

  def __init__(self, var, type, index):
    self.var = var
    self.type = type
    self.index = index
    assert(var != None)

  def Accept(self, v):
    v.VisitSlot(self)

  def AsSlot(self):
    return self

  def is_arguments(self):
    return self.var.is_arguments()

class Call(Expression):
  sentinel = None

  def __init__(self, expression, arguments):
    self.expression = expression
    self.arguments = arguments

  def Accept(self, v):
    v.VisitCall(self)

  def AsCall(self):
    return self

  def expression(self):
    return self.expression
  def arguments(self):
    return self.arguments
  def position(self):
    return self.pos

  def sentinel(self):
    return self.sentinel

class UnaryOperation(Expression):
  def __init__(self, op, expression):
    self.op = op
    self.expression = expression
    assert(IsUnaryOp(op))

  def Accept(self, v):
    v.VisitUnaryOperation(self)

  def AsUnaryOperation(self):
    return self

  def op(self):
    return self.op
  def expression(self):
    return self.expression

class BinaryOperation(Expression):
  def __init__(self, op, left, right):
    self.op = op
    self.left = left
    self.right = right
    assert(IsBinaryOp(op))

  def Accept(self, v):
    v.VisitBinaryOperation(self)

  def AsBinaryOperation(self):
    return self

  def ResultOverwrittenAllowed(self):
    not_allowed = ("COMMA", "OR", "AND")
    allowed = ("BIT_OR", "BIT_XOR", "BIT_AND", "SHL", "SAR", "SHR", "ADD",
           "SUB", "MUL", "DIV", "MOD")
    for keyword in not_allowed:
      if self.op == keyword:
        return False
    for keyword in allowed:
      if self.op == keyword:
        return True
    assert(false)
    return true;

  def op(self):
    return self.op

  def left(self):
    return self.left

  def right(self):
    return self.right

class CountOperation(Expression):
  def __init__(self, is_prefix, op, expression):
    self.is_prefix = is_prefix
    self.op = op
    self.expression = expression

  def Accept(self, v):
    v.VisitCountOperation(self)

  def is_prefix(self):
    return self.is_prefix
  def is_postfix(self):
    return not self.is_prefix
  def op(self):
    return self.op
  def expression(self):
    return self.expression

  def MarkAsStatement(self):
    self.is_prefix = true

class CompareOperation(Expression):
  def __init__(self, op, left, right):
    self.op = op
    self.left = left
    self.right = right
    assert(IsCompareOp(op))

  def Accept(self, v):
    v.VisitCompareOperation(self)

  def op(self):
    return self.op
  def left(self):
    return self.left
  def right(self):
    return self.right

class Conditional(Expression):
  def __init__(self, condition, then_expression, else_expression):
    self.condition = condition
    self.then_expression = then_expression
    self.else_expression = else_expression

  def Accept(self, v):
    v.VisitConditional(self)

  def condition(self):
    return self.condition
  def then_expression(self):
    return self.then_expression
  def else_expression(self):
    return self.else_expression

class Assignment(Expression):
  def __init__(self, op, target, value):
    self.op = op
    self.target = target
    self.value = value
    self.block_start = False
    self.block_end = False
    assert(IsAssignmentOp(op))

  def Accept(self, v):
    v.VisitAssignment(self)
  def AsAssignment(self):
    return self

  def binary_op(self):
    T = (("ASSIGN_BIT_OR", "BIT_OR"),
       ("ASSIGN_BIT_XOR", "BIT_XOR"),
       ("ASSIGN_BIT_AND", "BIT_AND"),
       ("ASSIGN_SHL", "SHL"),
       ("ASSIGN_SAR", "SAR"),
       ("ASSIGN_SHR", "SHR"),
       ("ASSIGN_ADD", "ADD"),
       ("ASSIGN_SUB", "SUB"),
       ("ASSIGN_MUL", "MUL"),
       ("ASSIGN_DIV", "DIV"),
       ("ASSIGN_MOD", "MOD"))
    for t in T:
      if t[0] == self.op:
        return t[1]
    assert(False)

  # An initialization block is a series of statments of the form
  # x.y.z.a = ...; x.y.z.b = ...; etc. The parser marks the beginning and
  # ending of these blocks to allow for optimizations of initialization
  # blocks.
  def starts_initialization_block(self):
    return self.block_start
  def ends_initialization_block(self):
    return self.block_end
  def mark_block_start(self):
    self.block_start = True
  def mark_block_end(self):
    self.block_end = True

class FunctionLiteral(Expression):
  def __init__(self, name, scope, body, num_parameters, is_expression):
    self.name = name
    self.scope = scope
    self.body = body
    self.num_parameters = num_parameters
    self.is_expression = is_expression
    self.loop_nesting = 0
    self.inferred_name = ""
    self.try_fast_codegen_ = False

  def Accept(self, v):
    v.VisitFunctionLiteral(self)

  def AsFunctionLiteral(self):
    return self

  def name(self):
    return self.name
  def body(self):
    return self.body
  def is_expression(self):
    return self.is_expression

  def num_parameters(self):
    return self.num_parameters

  def loop_nesting(self):
    return self.loop_nesting
  def set_loop_nesting(self, nesting):
    self.loop_nesting = nesting

  def inferred_name(self):
    return self.inferred_name
  def set_inferred_name(self, inferred_name):
    self.inferred_name = inferred_name

class ThisFunction(Expression):
  def Accept(self, v):
    v.VisitThisFunction(self)

class VariableMap(dict):
  def Declare(self, scope, name, mode, is_valid_lhs, kind):
    self[name.value] = Variable(scope, name, mode, is_valid_lhs, kind)
    return self[name.value]

  def Lookup(self, name):
    if name.value in self:
      return self[name.value]
    else:
      return None

class Scope:
  #enum Type
  EVAL_SCOPE = 0      # the top-level scope for an 'eval' source
  FUNCTION_SCOPE = 1  # the top-level scope for a function
  GLOBAL_SCOPE = 2    # the top-level scope for a program or a top-level eval

  def __init__(self, outer_scope, type):
    self.outer_scope = outer_scope
    self.inner_scopes = []
    self.type = type
    self.scope_name = JSObject("string", "")
    self.variables = VariableMap()
    self.temps = []
    self.params = []
    self.dynamics = None
    self.unresolved = []
    self.decls = []
    self.receiver = None
    self.function = None
    self.arguments = None
    self.arguments_shadow = None
    self.scope_inside_with = False
    self.scope_contains_with = False
    self.scope_calls_eval = False
    self.outer_scope_calls_eval = False
    self.inner_scope_calls_eval = False
    self.outer_scope_is_eval_scope = False
    self.force_eager_compilation = False
    self.num_stack_slots = False
    self.num_heap_slots = False
    # At some point we might want to provide outer scopes to
    # eval scopes (by walking the stack and reading the scope info).
    # In that case, the ASSERT below needs to be adjusted.
    assert((type == self.GLOBAL_SCOPE or self.EVAL_SCOPE) == \
           (self.outer_scope == None))

  def SetScopeName(self, scope_name):
    self.scope_name = scope_name

  def Initialize(self, inside_with):
    if not self.outer_scope == None:
      self.outer_scope.inner_scopes.append(self)
      self.scope_inside_with = self.outer_scope.scope_inside_with or \
        inside_with
    else:
      self.scope_inside_with = inside_with

    # Declare convenience variables.
    # Declare and allocate receiver (even for the global scope, and even
    # if naccesses_ == 0).
    # NOTE: When loading parameters in the global scope, we must take
    # care not to access them as properties of the global object, but
    # instead load them directly from the stack. Currently, the only
    # such parameter is 'this' which is passed on the stack when
    # invoking scripts
    var = self.variables.Declare(self, JSObject("string", "this"), Variable.VAR, False, Variable.THIS)
    var.rewrite = Slot(var, Slot.PARAMETER, -1)
    self.receiver = VariableProxy(JSObject("string", "this"), True, False)
    self.receiver.BindTo(var)

    if self.is_function_scope():
      # Declare 'arguments' variable which exists in all functions.
      # Note that it might never be accessed, in which case it won't be
      # allocated during variable allocation.
      self.variables.Declare(self, JSObject("string", "arguments"),
                   Variable.VAR, True, Variable.ARGUMENTS)

  def LocalLookup(self, name):
    return self.variables.Lookup(name)

  def Lookup(self, name):
    scope = self
    while scope != None:
      var = scope.LocalLookup(name)
      if var != None:
        return var
      scope = scope.outer_scope()
    return None

  def DeclareFunctionVar(self, name):
    assert(self.is_function_scope() and self.function == None)
    self.function = Variable(self, name, Variable.CONST, True, Variable.NORMAL)
    return self.function

  def DeclareLocal(self, name, mode):
    assert(mode == Variable.VAR or mode == Variable.CONST)
    return self.variables.Declare(self, name, mode, True, Variable.NORMAL)

  def DeclareGlobal(self, name):
    assert(self.is_global_scope())
    return self.variables.Declare(self, name, Variable.DYNAMIC, True,
                    Variable.NORMAL)

  def AddParameter(self, var):
    assert(self.is_function_scope())
    assert(self.LocalLookup(var.name) == var)
    self.params.append(var)

  def NewUnresolved(self, name, inside_with):
    # Note that we must not share the unresolved variables with
    # the same name because they may be removed selectively via
    # RemoveUnresolved().
    proxy = VariableProxy(name, False, inside_with)
    self.unresolved.append(proxy)
    return proxy

  def AddDeclaration(self, declaration):
    self.decls.append(declaration)
    #assert(False)

  def is_eval_scope(self):
    return self.type == self.EVAL_SCOPE
  def is_function_scope(self):
    return self.type == self.FUNCTION_SCOPE
  def is_global_scope(self):
    return self.type == self.GLOBAL_SCOPE

  def inside_with(self):
    return self.scope_inside_with
  def contains_with(self):
    return self.scope_contains_with

  def outer_scope(self):
    return self.outer_scope

  def receiver(self):
    return self.receiver

  def function(self):
    assert(self.is_function_scope())
    return self.function

  def parameter(self, index):
    assert(self.is_function_scope())
    return self.params[index]

  def num_parameters(self):
    return len(self.params)

  # The local variable 'arguments' if we need to allocate it; NULL otherwise.
  # If arguments() exist, arguments_shadow() exists, too.
  def arguments(self):
    return self.arguments

  # The '.arguments' shadow variable if we need to allocate it; NULL otherwise.
  # If arguments_shadow() exist, arguments() exists, too.
  def arguments_shadow(self):
    return self.arguments_shadow

  def declarations(self):
    return self.decls

  def MustAllocate(self, var):
    # Give var a read/write use if there is a chance it might be accessed
    # via an eval() call.  This is only possible if the variable has a
    # visible name.
    if (var.is_this() or len(var.name.value) > 0) and            \
      (var.is_accessed_from_inner_scope or                     \
       self.scope_calls_eval or self.inner_scope_calls_eval or \
       self.scope_contains_with):
      var.var_uses().RecordAccess(1)
    # Global variables do not need to be allocated.
      return not var.is_global() and var.var_uses().is_used()

  def MustAllocateInContext(self, var):
    # If var is accessed from an inner scope, or if there is a
    # possibility that it might be accessed from the current or an inner
    # scope (through an eval() call), it must be allocated in the
    # context.  Exception: temporary variables are not allocated in the
    # context.
    return \
        var.mode != Variable.TEMPORARY and \
        (var.is_accessed_from_inner_scope or
         self.scope_calls_eval or self.inner_scope_calls_eval or
         self.scope_contains_with or var.is_global())

  def AllocateHeapSlot(self, var):
    var.rewrite = Slot(var, Slot.CONTEXT, self.num_heap_slots)
    self.num_heap_slots += 1

  def AllocateParameterLocals(self):
    assert(self.is_function_scope())
    arguments = self.LocalLookup(JSObject("string", "arguments"))
    assert(arguments != None)  # functions have 'arguments' declared implicitly
    if self.MustAllocate(arguments) and not self.HasArgumentsParameter():
      # 'arguments' is used. Unless there is also a parameter called
      # 'arguments', we must be conservative and access all parameters via
      # the arguments object: The i'th parameter is rewritten into
      # '.arguments[i]' (*). If we have a parameter named 'arguments', a
      # (new) value is always assigned to it via the function
      # invocation. Then 'arguments' denotes that specific parameter value
      # and cannot be used to access the parameters, which is why we don't
      # need to rewrite in that case.
      #
      # (*) Instead of having a parameter called 'arguments', we may have an
      # assignment to 'arguments' in the function body, at some arbitrary
      # point in time (possibly through an 'eval()' call!). After that
      # assignment any re-write of parameters would be invalid (was bug
      # 881452). Thus, we introduce a shadow '.arguments'
      # variable which also points to the arguments object. For rewrites we
      # use '.arguments' which remains valid even if we assign to
      # 'arguments'. To summarize: If we need to rewrite, we allocate an
      # 'arguments' object dynamically upon function invocation. The compiler
      # introduces 2 local variables 'arguments' and '.arguments', both of
      # which originally point to the arguments object that was
      # allocated. All parameters are rewritten into property accesses via
      # the '.arguments' variable. Thus, any changes to properties of
      # 'arguments' are reflected in the variables and vice versa. If the
      # 'arguments' variable is changed, '.arguments' still points to the
      # correct arguments object and the rewrites still work.

      # We are using 'arguments'. Tell the code generator that is needs to
      # allocate the arguments object by setting 'arguments_'.
      self.arguments = VariableProxy(JSObject("string", "arguments", False, False))
      self.arguments.BindTo(arguments)

      # We also need the '.arguments' shadow variable. Declare it and create
      # and bind the corresponding proxy. It's ok to declare it only now
      # because it's a local variable that is allocated after the parameters
      # have been allocated.
      #
      # Note: This is "almost" at temporary variable but we cannot use
      # NewTemporary() because the mode needs to be INTERNAL since this
      # variable may be allocated in the heap-allocated context (temporaries
      # are never allocated in the context).
      arguments_shadow = Variable(self, JSObject("string", ".arguments"),
                    Variable.INTERNAL, True, Variable.ARGUMENTS)
      self.arguments_shadow = VariableProxy(JSObject("string", ".arguments"), False, False)
      self.arguments_shadow.BindTo(arguments_shadow)
      #temps_.Add(arguments_shadow);

      # Allocate the parameters by rewriting them into '.arguments[i]' accesses.
      for i in range(0, len(self.params)):
        var = self.params[i]
        assert(var.scope == self)
        if self.MustAllocate(var):
          assert(False)
#                    if self.MustAllocateInContext(var):
            # It is ok to set this only now, because arguments is a local
            # variable that is allocated after the parameters have been
            # allocated.
#                        arguments_shadow.is_accessed_from_inner_scope = True

#                    var.rewrite = Property(self.arguments_shadow,
#                                           Literal("number", i),
#                                           RelocInfo::kNoPosition,
#                                           Property::SYNTHETIC);
#                    arguments_shadow.var_uses.RecordUses(var->var_uses());

    else:
      # The arguments object is not used, so we can access parameters directly.
      # The same parameter may occur multiple times in the parameters_ list.
      # If it does, and if it is not copied into the context object, it must
      # receive the highest parameter index for that parameter; thus iteration
      # order is relevant!
      for i in range(0, len(self.params)):
        var = self.params[i]
        assert(var.scope == self)
        if self.MustAllocate(var):
          if self.MustAllocateInContext(var):
            assert(var.rewrite == None or \
                 (var.slot != None and var.slot.type == Slot.CONTEXT))
            if var.rewrite == None:
              # Only set the heap allocation if the parameter has not
              # been allocated yet.
              self.AllocateHeapSlot(var)
          else:
            assert(var.rewrite == None or
                 (var.slot != None and
                var.slot.type == Slot.PARAMETER))
            # Set the parameter index always, even if the parameter
            # was seen before! (We need to access the actual parameter
            # supplied for the last occurrence of a multiply declared
            # parameter.)
            var.rewrite = Slot(var, Slot.PARAMETER, i)

  def AllocateNonParameterLocal(self, var):
    assert(var.scope == self)
    assert(var.rewrite == None or \
         (not var.IsVariable(JSObject("string", "result"))) or
         (var.slot == None or var.slot.type != Slot.LOCAL))
    if var.rewrite == None and self.MustAllocate(var):
      if self.MustAllocateInContext(var):
        self.AllocateHeapSlot(var)
      else:
        self.AllocateStackSlot(var)


  def AllocateNonParameterLocals(self):
    # All variables that have no rewrite yet are non-parameter locals.
    for i in range(0, len(self.temps)):
      self.AllocateNonParameterLocal(self.temps[i])

    for key in self.variables:
      var = self.variables[key]
      self.AllocateNonParameterLocal(var)

    # For now, function_ must be allocated at the very end.  If it gets
    # allocated in the context, it must be the last slot in the context,
    # because of the current ScopeInfo implementation (see
    # ScopeInfo::ScopeInfo(FunctionScope* scope) constructor).
    if self.function != None:
      self.AllocateNonParameterLocal(self.function)

  def AllocateVariablesRecursively(self):
    class Context:
      MIN_CONTEXT_SLOTS = 5

    # The number of slots required for variables.
    self.num_stack_slots = 0
    self.num_heap_slots = Context.MIN_CONTEXT_SLOTS

    # Allocate variables for inner scopes.
    for i in range(0, len(self.inner_scopes)):
      self.inner_scopes[i].AllocateVariablesRecursively()

    # Allocate variables for this scope.
    # Parameters must be allocated first, if any.
    if self.is_function_scope():
      self.AllocateParameterLocals()
    self.AllocateNonParameterLocals()

    # Allocate context if necessary.
    must_have_local_context = False
    if self.scope_calls_eval or self.scope_contains_with:
      # The context for the eval() call or 'with' statement in this scope.
      # Unless we are in the global or an eval scope, we need a local
      # context even if we didn't statically allocate any locals in it,
      # and the compiler will access the context variable. If we are
      # not in an inner scope, the scope is provided from the outside.
      must_have_local_context = self.is_function_scope()

    # If we didn't allocate any locals in the local context, then we only
    # need the minimal number of slots if we must have a local context.
    if self.num_heap_slots == Context.MIN_CONTEXT_SLOTS and \
        not must_have_local_context:
      self.num_heap_slots = 0

    # Allocation done.
    assert(self.num_heap_slots == 0 or  \
         self.num_heap_slots >= Context.MIN_CONTEXT_SLOTS)

  def AllocateVariables(self, context):
    assert(self.outer_scope == None)

    # 1) Propagate scope information.
    # If we are in an eval scope, we may have other outer scopes about
    # which we don't know anything at this point. Thus we must be conservative
    # and assume they may invoke eval themselves. Eventually we could capture
    # this information in the ScopeInfo and then use it here (by traversing
    # the call chain stack, at compile time).
    eval_scope = self.is_eval_scope()
    self.PropagateScopeInfo(eval_scope, eval_scope);

    # 2) Resolve variables.
    global_scope = None;
    if self.is_global_scope():
      global_scope = self;
    self.ResolveVariablesRecursively(global_scope, context);

    # 3) Allocate variables.
    self.AllocateVariablesRecursively()

  def PropagateScopeInfo(self, outer_scope_calls_eval, outer_scope_is_eval_scope):
    if outer_scope_calls_eval:
      self.outer_scope_calls = True

    if outer_scope_is_eval_scope:
      self.outer_scope_is_eval_scope = True

    calls_eval = self.scope_calls_eval or self.outer_scope_calls_eval
    is_eval = self.is_eval_scope() or outer_scope_is_eval_scope
    for i in range(0, len(self.inner_scopes)):
      inner_scope = self.inner_scopes[i]
      if inner_scope.PropagateScopeInfo(calls_eval, is_eval):
        self.inner_scope_calls_eval = True
      if inner_scope.force_eager_compilation:
        force_eager_compilation = True

    return self.scope_calls_eval or self.inner_scope_calls_eval

  def ResolveVariablesRecursively(self, global_scope, context):
    assert(global_scope == None or global_scope.is_global_scope())

    # Resolve unresolved variables for this scope.
    for i in range(0, len(self.unresolved)):
      self.ResolveVariable(global_scope, context, self.unresolved[i])

    # Resolve unresolved variables for inner scopes.
    for i in range(0, len(self.inner_scopes)):
      self.inner_scopes[i].ResolveVariablesRecursively(global_scope, context)

  def ResolveVariable(self, global_scope, context, proxy):
    assert(global_scope == None or global_scope.is_global_scope())

    # If the proxy is already resolved there's nothing to do
    # (functions and consts may be resolved by the parser).
    if proxy.var != None:
      return

    # Otherwise, try to resolve the variable.
    invalidated_local = [None]
    # NOTE(keisuke):Smells Bad!!
    var = self.LookupRecursive(proxy.name, False, invalidated_local)

    if proxy.inside_with:
      # If we are inside a local 'with' statement, all bets are off
      # and we cannot resolve the proxy to a local variable even if
      # we found an outer matching variable.
      # Note that we must do a lookup anyway, because if we find one,
      # we must mark that variable as potentially accessed from this
      # inner scope (the property may not be in the 'with' object).
      var = self.NonLocal(proxy.name, Variable.DYNAMIC)
    else:
      # We are not inside a local 'with' statement.

      if var == None:
        # We did not find the variable. We have a global variable
        # if we are in the global scope (we know already that we
        # are outside a 'with' statement) or if there is no way
        # that the variable might be introduced dynamically (through
        # a local or outer eval() call, or an outer 'with' statement),
        # or we don't know about the outer scope (because we are
        # in an eval scope).
        if self.is_global_scope() or \
            not (self.scope_inside_with or self.outer_scope_is_eval_scope or
               self.scope_calls_eval or self.outer_scope_calls_eval):
          # We must have a global variable.
          assert(global_scope != None)
          var = global_scope.DeclareGlobal(proxy.name)

        elif self.scope_inside_with:
          # If we are inside a with statement we give up and look up
          # the variable at runtime.
          var = self.NonLocal(proxy.name, Variable.DYNAMIC);

        elif invalidated_local[0] != None:
          # No with statements are involved and we found a local
          # variable that might be shadowed by eval introduced
          # variables.
          var = self.NonLocal(proxy.name, Variable.DYNAMIC_LOCAL)
          var.set_local_if_not_shadowed(invalidated_local)

        elif self.outer_scope_is_eval_scope:
          # No with statements and we did not find a local and the code
          # is executed with a call to eval.  The context contains
          # scope information that we can use to determine if the
          # variable is global if it is not shadowed by eval-introduced
          # variables.
          if context.GlobalIfNotShadowedByEval(proxy.name):
            var = self.NonLocal(proxy.name, Variable.DYNAMIC_GLOBAL)
          else:
            var = self.NonLocal(proxy.name, Variable.DYNAMIC);
        else:
          # No with statements and we did not find a local and the code
          # is not executed with a call to eval.  We know that this
          # variable is global unless it is shadowed by eval-introduced
          # variables.
          var = self.NonLocal(proxy.name, Variable.DYNAMIC_GLOBAL)
    proxy.BindTo(var)

  # Lookup a variable starting with this scope. The result is either
  # the statically resolved variable belonging to an outer scope, or
  # NULL. It may be NULL because a) we couldn't find a variable, or b)
  # because the variable is just a guess (and may be shadowed by
  # another variable that is introduced dynamically via an 'eval' call
  # or a 'with' statement).
  def LookupRecursive(self, name, inner_lookup, invalidated_local):
    # If we find a variable, but the current scope calls 'eval', the found
    # variable may not be the correct one (the 'eval' may introduce a
    # property with the same name). In that case, remember that the variable
    # found is just a guess.
    guess = self.scope_calls_eval

    # Try to find the variable in this scope.
    var = self.LocalLookup(name)

    if var != None:
      # We found a variable. If this is not an inner lookup, we are done.
      # (Even if there is an 'eval' in this scope which introduces the
      # same variable again, the resulting variable remains the same.
      # Note that enclosing 'with' statements are handled at the call site.)
      if not inner_lookup:
        return var

    else:
      # We did not find a variable locally. Check against the function variable,
      # if any. We can do this for all scopes, since the function variable is
      # only present - if at all - for function scopes.
      #
      # This lookup corresponds to a lookup in the "intermediate" scope sitting
      # between this scope and the outer scope. (ECMA-262, 3rd., requires that
      # the name of named function literal is kept in an intermediate scope
      # in between this scope and the next outer scope.)
      if self.function != None and self.function.name.value == name.value:
        var = self.function

      elif self.outer_scope != None:
        var = self.outer_scope.LookupRecursive(name, True, invalidated_local)
        # We may have found a variable in an outer scope. However, if
        # the current scope is inside a 'with', the actual variable may
        # be a property introduced via the 'with' statement. Then, the
        # variable we may have found is just a guess.
        if self.scope_inside_with:
          guess = true

      # If we did not find a variable, we are done.
      if var == None:
        return None

    assert(var != None)

    # If this is a lookup from an inner scope, mark the variable.
    if inner_lookup:
      var.is_accessed_from_inner_scope = True

    # If the variable we have found is just a guess, invalidate the
    # result. If the found variable is local, record that fact so we
    # can generate fast code to get it if it is not shadowed by eval.
    if guess:
      if not var.is_global():
        invalidated_local[0] = var
      var = None

    return var;

class LexicalScope:
  def __init__(self, parser, scope):
    self.activated = True
    self.parser = parser
    self.prev_scope = parser.top_scope
    parser.top_scope = scope

  def __enter__(self):
    return

  def __exit__(self, *e):
    if self.activated:
      self.activated = False
      self.parser.top_scope = self.prev_scope

class TargetScope:
  def __init__(self, parser):
    self.parser = parser
    self.previous = parser.target_stack
    parser.target_stack = None

  def __enter__(self):
    return

  def __exit__(self, *e):
    self.parser.target_stack = self.previous

class Parser:
  # FunctionLiteralType
  EXPRESSION = 0
  DECLARATION = 1
  NESTED = 2

  # enum Mode {
  PARSE_LAZILY = 0
  PARSE_EAGERLY = 1

  def __init__(self, scanner):
    self.scanner = scanner
    self.top_scope = None
    self.temp_scope = None
    self.target_stack = None
    self.allow_native_syntax = False
    self.extension = False

  def inside_with(self):
    return False

  def Precedence(self, tok, accept_IN):
    if tok == "IN" and not accept_IN:
      assert(False)
      return 0
    return Precedence(tok)

  def peek(self):
    return self.scanner.peek()

  def Next(self):
    return self.scanner.Next()

  def ReportError(self, expected):
    print "%s expected but %s comes." % (expected, self.scanner.current_.token)
#    loc = self.scanner.location().beg_pos
#    for line in self.scanner.source_.split('\n'):
#      if 0 <= loc and loc <= len(line):
#        print(line)
#        print(' ' * (loc - 1) + '^ here is wrong.')
#        loc = -1
#      else:
#        print(line)
#        loc -= len(line)

  def Expect(self, token):
    next = self.Next()
    if next != token:
      self.ReportError(token)
      assert(next == token)

  def ExpectSemicolon(self):
    # Check for automatic semicolon insertion according to
    # the rules given in ECMA-262, section 7.9, page 21.
    tok = self.peek()
    if tok == "SEMICOLON":
      self.Next()
      return
    if self.scanner.has_line_terminator_before_next_ or \
          tok == "RBRACE" or \
          tok == "EOS":
      return
    self.Expect("SEMICOLON")

  def Consume(self, token):
    next = self.Next()
    assert(next == token)

  def NewNumberLiteral(self, number):
    return Literal(JSObject("number", number))

  def NewCall(self, expression, arguments):
    return Call(expression, arguments)

  def NewScope(self, parent, type, inside_with):
    result = Scope(parent, type)
    result.Initialize(inside_with)
    return result

  def GetLiteralUndefined(self):
    return JSUNDEFINED

  def Declare(self, name, mode, fun, resolve):
    var = None
    # If we are inside a function, a declaration of a variable
    # is a truly local variable, and the scope of the variable
    # is always the function scope.

    # If a function scope exists, then we can statically declare this
    # variable and also set its mode. In any case, a Declaration node
    # will be added to the scope so that the declaration can be added
    # to the corresponding activation frame at runtime if necessary.
    # For instance declarations inside an eval scope need to be added
    # to the calling function context.
    if self.top_scope.is_function_scope():
      # Declare the variable in the function scope.
      var = self.top_scope.LocalLookup(name)
      if var == None:
        # Declare the name.
        var = self.top_scope.DeclareLocal(name, mode)
      else:
        assert(False)
        # The name was declared before; check for conflicting
        # re-declarations. If the previous declaration was a const or the
        # current declaration is a const then we have a conflict. There is
        # similar code in runtime.cc in the Declare functions.
#                if mode == Variable.CONST or var.mode == Variable.CONST:
          # We only have vars and consts in declarations.
#                    assert(var.mode == Variable.VAR or \
#                           var.mode == Variable.CONST)
#                    type = "var" if var.mode == Variable.VAR else "const"
#                    type_string = JSObject("string", type)
#                    expression = NewThrowTypeError(Factory::redeclaration_symbol(),
#                                                   type_string, name);
#                    top_scope_->SetIllegalRedeclaration(expression);

    # We add a declaration node for every declaration. The compiler
    # will only generate code if necessary. In particular, declarations
    # for inner local variables that do not represent functions won't
    # result in any generated code.
    #
    # Note that we always add an unresolved proxy even if it's not
    # used, simply because we don't know in this method (w/o extra
    # parameters) if the proxy is needed or not. The proxy will be
    # bound during variable resolution time unless it was pre-bound
    # below.
    #
    # WARNING: This will lead to multiple declaration nodes for the
    # same variable if it is declared several times. This is not a
    # semantic issue as long as we keep the source order, but it may be
    # a performance issue since it may lead to repeated
    # Runtime::DeclareContextSlot() calls.
    proxy = self.top_scope.NewUnresolved(name, self.inside_with())
    self.top_scope.AddDeclaration(Declaration(proxy, mode, fun))

    # For global const variables we bind the proxy to a variable.
    if mode == Variable.CONST and self.top_scope.is_global_scope():
      assert(resolve);  # should be set by all callers
      kind = Variable.NORMAL
      var = Variable(self.top_scope, name, Variable.CONST, True, kind)

    # If requested and we have a local variable, bind the proxy to the variable
    # at parse-time. This is used for functions (and consts) declared inside
    # statements: the corresponding function (or const) variable must be in the
    # function scope and not a statement-local scope, e.g. as provided with a
    # 'with' statement:
    #
    #   with (obj) {
    #     function f() {}
    #   }
    #
    # which is translated into:
    #
    #   with (obj) {
    #     // in this case this is not: 'var f; f = function () {};'
    #     var f = function () {};
    #   }
    #
    # Note that if 'f' is accessed from inside the 'with' statement, it
    # will be allocated in the context (because we must be able to look
    # it up dynamically) but it will also be accessed statically, i.e.,
    # with a context slot index and a context chain length for this
    # initialization code. Thus, inside the 'with' statement, we need
    # both access to the static and the dynamic context chain; the
    # runtime needs to provide both.
    if resolve and var != None:
      proxy.BindTo(var)

    return proxy

  def ParseProgram(self, in_global_context):
    self.mode = self.PARSE_EAGERLY
    type = Scope.GLOBAL_SCOPE if in_global_context else Scope.EVAL_SCOPE
    no_name = JSObject("string", "")

    result = [None]
    scope = self.NewScope(self.top_scope, type, self.inside_with())
    with LexicalScope(self, scope) as lexical_scope:
      body = []
      self.ParseSourceElements(body, "EOS")
      result[0] = FunctionLiteral(no_name, self.top_scope, body, 0, False)

    top = result[0].scope
    top.AllocateVariables(None)

    return result[0]

  def ParseSourceElements(self, processor, end_token):
    # SourceElements ::
    #   (Statement)* <end_token>

    # Allocate a target stack to use for this set of source
    # elements. This way, all scripts and functions get their own
    # target stack thus avoiding illegal breaks and continues across
    # functions.
    with TargetScope(self) as scope:

      assert(processor != None)
      while self.peek() != end_token:
        stat = self.ParseStatement(None)
        if stat == None or stat.IsEmpty():
          continue
        processor.append(stat)

    return 0

  def ParseFunctionLiteral(self, var_name, type):
    # Function ::
    #   '(' FormalParameterList? ')' '{' FunctionBody '}'

    is_named = var_name != None

    # The name associated with this function. If it's a function expression,
    # this is the actual function name, otherwise this is the name of the
    # variable declared and initialized with the function (expression). In
    # that case, we don't have a function name (it's empty).
    name = var_name if is_named else JSObject("string", "")
    # The function name, if any.
    function_name = JSObject("string", "")
    if is_named and (type == self.EXPRESSION or type == self.NESTED):
      function_name = name

    num_parameters = 0
    # Parse function body.
    type = Scope.FUNCTION_SCOPE
    scope = self.NewScope(self.top_scope, type, self.inside_with())
    function_literal = [None]
    with LexicalScope(self, scope) as lexical_scope:
      self.top_scope.SetScopeName(name)

      #  FormalParameterList ::
      #    '(' (Identifier)*[','] ')'
      self.Expect("LPAREN")
      done = (self.peek() == "RPAREN")
      while not done:
        param_name = self.ParseIdentifier()
        self.top_scope.AddParameter(self.top_scope.DeclareLocal(param_name, Variable.VAR))
        num_parameters += 1

        done = (self.peek() == "RPAREN")
        if not done:
          self.Expect("COMMA")

      self.Expect("RPAREN")

      self.Expect("LBRACE")
      body = []

      # If we have a named function expression, we add a local variable
      # declaration to the body of the function with the name of the
      # function and let it refer to the function itself (closure).
      # NOTE: We create a proxy and resolve it here so that in the
      # future we can change the AST to only refer to VariableProxies
      # instead of Variables and Proxis as is the case now.
      if function_name != None and function_name.value != "":
        fvar = self.top_scope.DeclareFunctionVar(function_name)
        fproxy = self.top_scope.NewUnresolved(function_name, inside_with())
        fproxy.BindTo(fvar)
        body.append(ExpressionStatement(Assignment("INIT_VAR",
                                                   fproxy, ThisFunction())))

      self.ParseSourceElements(body, "RBRACE")

      self.Expect("RBRACE")

      function_literal[0] = FunctionLiteral(name, self.top_scope, body,
                          num_parameters, function_name != "")

    return function_literal[0]

  def ParseArguments(self):
    # Arguments ::
    #   '(' (AssignmentExpression)*[','] ')'

    result = []
    self.Expect("LPAREN")
    done = (self.peek() == "RPAREN")
    while not done:
      argument = self.ParseAssignmentExpression(True)
      result.append(argument)
      done = (self.peek() == "RPAREN");
      if not done:
        self.Expect("COMMA")
    self.Expect("RPAREN")
    return result

  def ParseIdentifier(self):
    if self.peek() == "IDENTIFIER":
      self.Expect("IDENTIFIER")
      return JSObject("string", self.scanner.literal_string())
    else:
      return JSObject("string", "")

  def ParseFunctionDeclaration(self):
    self.Expect("FUNCTION")
    name = self.ParseIdentifier()
    fun = self.ParseFunctionLiteral(name, Parser.DECLARATION)
    self.Declare(name, Variable.VAR, fun, True)
    return None

  def ParseVariableStatement(self):
    # VariableStatement ::
    #   VariableDeclarations ';'
    result = self.ParseVariableDeclarations(True, None)
    self.ExpectSemicolon()
    return result

  # If the variable declaration declares exactly one non-const
  # variable, then *var is set to that variable. In all other cases,
  # *var is untouched; in particular, it is the caller's responsibility
  # to initialize it properly. This mechanism is used for the parsing
  # of 'for-in' loops.
  def ParseVariableDeclarations(self, accept_IN, var):
    # VariableDeclarations ::
    #   ('var' | 'const') (Identifier ('=' AssignmentExpression)?)+[',']
    mode = Variable.VAR
    is_const = False
    if self.peek() == "VAR":
      self.Consume("VAR")
    elif self.peek() == "CONST":
      self.Consume("CONST")
      mode = Variable.CONST
      is_const = True
    else:
      assert(False)

    # The scope of a variable/const declared anywhere inside a function
    # is the entire function (ECMA-262, 3rd, 10.1.3, and 12.2). Thus we can
    # transform a source-level variable/const declaration into a (Function)
    # Scope declaration, and rewrite the source-level initialization into an
    # assignment statement. We use a block to collect multiple assignments.
    #
    # We mark the block as initializer block because we don't want the
    # rewriter to add a '.result' assignment to such a block (to get compliant
    # behavior for code such as print(eval('var x = 7')), and for cosmetic
    # reasons when pretty-printing. Also, unless an assignment (initialization)
    # is inside an initializer block, it is ignored.
    #
    # Create new block with one expected declaration.
    block = Block(None, 1, True)
    last_var = None
    nvars = 0
    while True:
      # Parse variable name.
      if nvars > 0:
        self.Consume("COMMA")
      name = self.ParseIdentifier()

      # Declare variable.
      # Note that we *always* must treat the initial value via a separate init
      # assignment for variables and constants because the value must be assigned
      # when the variable is encountered in the source. But the variable/constant
      # is declared (and set to 'undefined') upon entering the function within
      # which the variable or constant is declared. Only function variables have
      # an initial value in the declaration (because they are initialized upon
      # entering the function).
      #
      # If we have a const declaration, in an inner scope, the proxy is always
      # bound to the declared variable (independent of possibly surrounding with
      # statements).
      last_var = self.Declare(name, mode, None,
                  is_const) # always bound for CONST!
      nvars += 1

      # Parse initialization expression if present and/or needed. A
      # declaration of the form:
      #
      #    var v = x;
      #
      # is syntactic sugar for:
      #
      #    var v; v = x;
      #
      # In particular, we need to re-lookup 'v' as it may be a
      # different 'v' than the 'v' in the declaration (if we are inside
      # a 'with' statement that makes a object property with name 'v'
      # visible).
      #
      # However, note that const declarations are different! A const
      # declaration of the form:
      #
      #   const c = x;
      #
      # is *not* syntactic sugar for:
      #
      #   const c; c = x;
      #
      # The "variable" c initialized to x is the same as the declared
      # one - there is no re-lookup (see the last parameter of the
      # Declare() call above).

      value = None
      if self.peek() == "ASSIGN":
        self.Expect("ASSIGN")
        value = self.ParseAssignmentExpression(accept_IN)

      # Make sure that 'const c' actually initializes 'c' to undefined
      # even though it seems like a stupid thing to do.
      if value == None and is_const:
        value = JSUNDEFINED

      # Global variable declarations must be compiled in a specific
      # way. When the script containing the global variable declaration
      # is entered, the global variable must be declared, so that if it
      # doesn't exist (not even in a prototype of the global object) it
      # gets created with an initial undefined value. This is handled
      # by the declarations part of the function representing the
      # top-level global code; see Runtime::DeclareGlobalVariable. If
      # it already exists (in the object or in a prototype), it is
      # *not* touched until the variable declaration statement is
      # executed.
      #
      # Executing the variable declaration statement will always
      # guarantee to give the global object a "local" variable; a
      # variable defined in the global object and not in any
      # prototype. This way, global variable declarations can shadow
      # properties in the prototype chain, but only after the variable
      # declaration statement has been executed. This is important in
      # browsers where the global object (window) has lots of
      # properties defined in prototype objects.

      if self.top_scope.is_global_scope():
        arguments = []
        # Be careful not to assign a value to the global variable if
        # we're in a with. The initialization value should not
        # necessarily be stored in the global object in that case,
        # which is why we need to generate a separate assignment node.
        arguments.append(Literal(name))  # we have at least 1 parameter
        if is_const or (value != None and not self.inside_with()):
          arguments.append(value)
          value = None
        # Construct the call to Runtime::DeclareGlobal{Variable,Const}Locally
        # and add it to the initialization statement block. Note that
        # this function does different things depending on if we have
        # 1 or 2 parameters.
        assert(False and "not implemented yet.")
        initialize = None
#                if is_const:
#                    initialize = CallRuntime(JSObject("string", "InitializeConstGlobal"),
#                                             Runtime.FunctionForId(Runtime.kInitializeConstGlobal),
#                                             arguments)
#                else:
#                    initialize = CallRuntime(JSObject("string", "InitializeVarGlobal"),
#                                             Runtime.FunctionForId(Runtime.kInitializeVarGlobal),
#                                             arguments)
        block.AddStatement(ExpressionStatement(initialize))

      # Add an assignment node to the initialization statement block if
      # we still have a pending initialization value. We must distinguish
      # between variables and constants: Variable initializations are simply
      # assignments (with all the consequences if they are inside a 'with'
      # statement - they may change a 'with' object property). Constant
      # initializations always assign to the declared constant which is
      # always at the function scope level. This is only relevant for
      # dynamically looked-up variables and constants (the start context
      # for constant lookups is always the function context, while it is
      # the top context for variables). Sigh...
      if value != None:
        op = "INIT_CONST" if is_const else "INIT_VAR"
        assignment = Assignment(op, last_var, value)
        if block:
          block.AddStatement(ExpressionStatement(assignment))

      if self.peek() != "COMMA":
        break

    if not is_const and nvars == 1:
      # We have a single, non-const variable.
      assert(last_var != None)
      var = last_var

    return block

  # Precedence = 1
  def ParseExpression(self, accept_IN):
    # Expression ::
    #   AssignmentExpression
    #   Expression ',' AssignmentExpression

    result = self.ParseAssignmentExpression(accept_IN)
    while self.peek() == "COMMA":
      self.Expect("COMMA")
      right = self.ParseAssignmentExpression(accept_IN)
      result = BinaryOperation("COMMA", result, right)

    return result

  # Precedence = 2
  def ParseAssignmentExpression(self, accept_IN):
    # AssignmentExpression ::
    #   ConditionalExpression
    #   LeftHandSideExpression AssignmentOperator AssignmentExpression

    expression = self.ParseConditionalExpression(accept_IN);

    if not IsAssignmentOp(self.peek()):
      # Parsed conditional expression only (no assignment).
      return expression

    # Signal a reference error if the expression is an invalid left-hand
    # side expression.  We could report this as a syntax error here but
    # for compatibility with JSC we choose to report the error at
    # runtime.
    if expression == None or not expression.IsValidLeftHandSide():
      assert(False)

    op = self.Next()  # Get assignment operator.
    right = self.ParseAssignmentExpression(accept_IN)

    # TODO(1231235): We try to estimate the set of properties set by
    # constructors. We define a new property whenever there is an
    # assignment to a property of 'this'. We should probably only add
    # properties if we haven't seen them before. Otherwise we'll
    # probably overestimate the number of properties.

    #Property* property = expression ? expression->AsProperty() : NULL;
    #if (op == Token::ASSIGN &&
    #    property != NULL &&
    #    property->obj()->AsVariableProxy() != NULL &&
    #    property->obj()->AsVariableProxy()->is_this()) {
    #    temp_scope_->AddProperty();
    #    }

    return Assignment(op, expression, right)

  # Precedence = 3
  def ParseConditionalExpression(self, accept_IN):
    # ConditionalExpression ::
    #   LogicalOrExpression
    #   LogicalOrExpression '?' AssignmentExpression ':' AssignmentExpression

    # We start using the binary expression parser for prec >= 4 only!
    expression = self.ParseBinaryExpression(4, accept_IN)
    if self.peek() != "CONDITIONAL":
      return expression
    self.Consume("CONDITIONAL")
    # In parsing the first assignment expression in conditional
    # expressions we always accept the 'in' keyword; see ECMA-262,
    # section 11.12, page 58.
    left = self.ParseAssignmentExpression(True)
    self.Expect("COLON")
    right = self.ParseAssignmentExpression(accept_IN)
    return Conditional(expression, left, right)

  # Precedence >= 4
  def ParseBinaryExpression(self, prec, accept_IN):
    assert(prec >= 4)
    x = self.ParseUnaryExpression()
    #for (int prec1 = Precedence(peek(), accept_IN); prec1 >= prec; prec1--) {
    for prec1 in reversed(range(prec, self.Precedence(self.peek(), accept_IN) + 1)):
      # prec1 >= 4
      while self.Precedence(self.peek(), accept_IN) == prec1:
        op = self.Next()
        y = self.ParseBinaryExpression(prec1 + 1, accept_IN)

        # Compute some expressions involving only number literals.
        if x and isinstance(x, Literal) and x.handle.IsNumber() and \
           y and isinstance(y, Literal) and y.handle.IsNumber():
           x_val = x.AsLiteral().handle.Number()
           y_val = y.AsLiteral().handle.Number()

           if op == "ADD":
             x = self.NewNumberLiteral(x_val + y_val)
             continue
           elif op == "SUB":
             x = self.NewNumberLiteral(x_val - y_val)
             continue
           elif op == "MUL":
             x = self.NewNumberLiteral(x_val * y_val)
             continue
           elif op == "DIV":
             x = self.NewNumberLiteral(x_val / y_val)
             continue
           elif op == "BIT_OR":
             x = self.NewNumberLiteral(DoubleToInt32(x_val) | DoubleToInt32(y_val))
             continue
           elif op == "BIT_AND":
             x = self.NewNumberLiteral(DoubleToInt32(x_val) & DoubleToInt32(y_val))
             continue
           elif op == "BIT_XOR":
             x = self.NewNumberLiteral(DoubleToInt32(x_val) ^ DoubleToInt32(y_val))
             continue
           elif op == "SHL":
             value = DoubleToInt32(x_val) << (DoubleToInt32(y_val) & 0x1f)
             x = self.NewNumberLiteral(value)
             continue
           elif op == "SHR":
             shift = DoubleToInt32(y_val) & 0x1f
             value = DoubleToUint32(x_val) >> shift
             x = self.NewNumberLiteral(value)
             continue
           elif op == "SAR":
             shift = DoubleToInt32(y_val) & 0x1f
             value = ArithmeticShiftRight(DoubleToInt32(x_val), shift)
             x = self.NewNumberLiteral(value)
             continue

        # For now we distinguish between comparisons and other binary
        # operations.  (We could combine the two and get rid of this
        # code an AST node eventually.)
        if IsCompareOp(op):
          # We have a comparison.
          cmp = op
          if op == "NE":
            cmp = "EQ"
          elif op == "NE_STRICT":
            cmp = "EQ_STRICT"
          x = CompareOperation(cmp, x, y)
          if cmp != op:
            # The comparison was negated - add a NOT.
            x = UnaryOperation("NOT", x)
        else:
          # We have a "normal" binary operation.
          x = BinaryOperation(op, x, y);

    return x

  def ParseUnaryExpression(self):
    # UnaryExpression ::
    #   PostfixExpression
    #   'delete' UnaryExpression
    #   'void' UnaryExpression
    #   'typeof' UnaryExpression
    #   '++' UnaryExpression
    #   '--' UnaryExpression
    #   '+' UnaryExpression
    #   '-' UnaryExpression
    #   '~' UnaryExpression
    #   '!' UnaryExpression

    op = self.peek()
    if IsUnaryOp(op):
      op = self.Next()
      expression = self.ParseUnaryExpression()

      # Compute some expressions involving only number literals.
      if expression != None and expression.AsLiteral() and \
         expression.AsLiteral().handle().IsNumber():
        value = expression.AsLiteral().handle().Number()
        if op == "ADD":
          return expression
        elif op == "SUB":
          return NewNumberLiteral(-value)
        elif op == "BIT_NOT":
          return NewNumberLiteral(~DoubleToInt32(value))

      return UnaryOperation(op, expression)

    elif IsCountOp(op):
      op = self.Next()
      expression = self.ParseUnaryExpression()
      # Signal a reference error if the expression is an invalid
      # left-hand side expression.  We could report this as a syntax
      # error here but for compatibility with JSC we choose to report the
      # error at runtime.
      if expression == None or not expression.IsValidLeftHandSide():
        assert(false)
      return CountOperation(true, op, expression)
    else:
      return self.ParsePostfixExpression()

  def ParsePostfixExpression(self):
    # PostfixExpression ::
    #   LeftHandSideExpression ('++' | '--')?

    expression = self.ParseLeftHandSideExpression()
    if not self.scanner.has_line_terminator_before_next() and IsCountOp(self.peek()):
      # Signal a reference error if the expression is an invalid
      # left-hand side expression.  We could report this as a syntax
      # error here but for compatibility with JSC we choose to report the
      # error at runtime.
      if expression == None or not expression.IsValidLeftHandSide():
        assert(False)

      next = self.Next()
      expression = CountOperation(False, next, expression)

    return expression

  def ParseLeftHandSideExpression(self):
    # LeftHandSideExpression ::
    #   (NewExpression | MemberExpression) ...

    result = None
    if self.peek() == "NEW":
      result = self.ParseNewExpression()
    else:
      result = self.ParseMemberExpression()

    while True:
      peek = self.peek()
      if peek == "LBRACK":
        self.Consume("LBRACK")
        index = self.ParseExpression(True)
        result = self.NewProperty(result, index)
        Expect("RBRACK")
      elif peek == "LPAREN":
        args = self.ParseArguments()
        # Keep track of eval() calls since they disable all local variable
        # optimizations.
        # The calls that need special treatment are the
        # direct (i.e. not aliased) eval calls. These calls are all of the
        # form eval(...) with no explicit receiver object where eval is not
        # declared in the current scope chain. These calls are marked as
        # potentially direct eval calls. Whether they are actually direct calls
        # to eval is determined at run time.
        #if (!is_pre_parsing_) {
        callee = result.AsVariableProxy()
        if callee != None and callee.IsVariable("eval"):
          name = callee.name()
          var = self.top_scope.Lookup(name)
          if var == None:
            self.top_scope.RecordEvalCall()
        result = self.NewCall(result, args)

      elif peek == "PERIOD":
        self.Consume("PERIOD")
        name = self.ParseIdentifier()
        result = self.NewProperty(result, Literal("string", name))

      else:
        return result

  def ParseMemberExpression(self):
    return self.ParseMemberWithNewPrefixesExpression(None)

  def ParseMemberWithNewPrefixesExpression(self, stack):
    # MemberExpression ::
    #   (PrimaryExpression | FunctionLiteral)
    #     ('[' Expression ']' | '.' Identifier | Arguments)*

    # Parse the initial primary or function expression.
    result = None
    if self.peek() == "FUNCTION":
      self.Expect("FUNCTION")
      name = JSObject("string", "")
      if self.peek() == "IDENTIFIER":
        name = self.ParseIdentifier()
      result = self.ParseFunctionLiteral(name, self.NESTED)
    else:
      result = self.ParsePrimaryExpression()
    return result

  def ParsePrimaryExpression(self):
    # PrimaryExpression ::
    #   'this'
    #   'null'
    #   'true'
    #   'false'
    #   Identifier
    #   Number
    #   String
    #   ArrayLiteral
    #   ObjectLiteral
    #   RegExpLiteral
    #   '(' Expression ')'

    result = None
    peek = self.peek()
    if peek == "THIS":
      assert(False)
#    case Token::THIS: {
#      Consume(Token::THIS);
#      if (is_pre_parsing_) {
#        result = VariableProxySentinel::this_proxy();
#      } else {
#        VariableProxy* recv = top_scope_->receiver();
#        recv->var_uses()->RecordRead(1);
#        result = recv;
#      }
#      break;
#    }

    elif peek == "NULL_LITERAL":
      self.Consume("NULL_LITERAL")
      result = Literal(JSNULL)

    elif peek == "TRUE_LITERAL":
      self.Consume("TRUE_LITERAL")
      result = Literal(JSTRUE)

    elif peek == "FALSE_LITERAL":
      self.Consume("FALSE_LITERAL")
      result = Literal(JSFALSE)

    elif peek == "IDENTIFIER":
      name = self.ParseIdentifier()
      result = self.top_scope.NewUnresolved(name, self.inside_with())

    elif peek == "NUMBER":
      self.Consume("NUMBER")
      #value = StringToDouble(self.scanner.literal_string(), ALLOW_HEX | ALLOW_OCTALS)
      value = int(self.scanner.literal_string())
      result = self.NewNumberLiteral(value)

    elif peek == "STRING":
      self.Consume("STRING")
      symbol = self.scanner.literal_string()
      result = Literal(JSObject("string", symbol))

    elif peek == "ASSIGN_DIV":
      result = self.ParseRegExpLiteral(True)

    elif peek == "DIV":
      result = self.ParseRegExpLiteral(False)

    elif peek == "LBRACK":
      result = self.ParseArrayLiteral()

    elif peek == "LBRACE":
      result = self.ParseObjectLiteral()

    elif peek == "LPAREN":
      self.Consume("LPAREN")
      result = self.ParseExpression(True)
      self.Expect("RPAREN")

    elif peek == "MOD":
      if self.allow_natives_syntax or self.extension != None:
        result = self.ParseV8Intrinsic()
      else:
        assert(False)

    # If we're not allowing special syntax we fall-through to the
    # default case.
    else:
      print(peek)
      assert(False)
#            tok = peek();
      # Token::Peek returns the value of the next token but
      # location() gives info about the current token.
      # Therefore, we need to read ahead to the next token
#            Next();
#            ReportUnexpectedToken(tok);
#            *ok = false;
#            return NULL;
    return result

  def ParseReturnStatement(self):
    # ReturnStatement ::
    #   'return' Expression? ';'

    # Consume the return token. It is necessary to do the before
    # reporting any errors on it, because of the way errors are
    # reported (underlining).
    self.Expect("RETURN")

    tok = self.peek()
    if tok == "SEMICOLON" or tok == "RBRACE" or tok == "EOS":
      return ReturnStatement(self.GetLiteralUndefined())

    expr = self.ParseExpression(True)
    self.ExpectSemicolon()
    return ReturnStatement(expr)

  def ParseExpressionOrLabelledStatement(self, labels):
    # ExpressionStatement | LabelledStatement ::
    #   Expression ';'
    #   Identifier ':' Statement

    expr = self.ParseExpression(True)
    if self.peek() == "COLON" and expr and \
        expr.AsVariableProxy() != None and \
        not expr.AsVariableProxy().is_this():
      assert(False)  # NOTE(keisuke): short cut

    # Parsed expression statement.
    self.ExpectSemicolon()
    return ExpressionStatement(expr)

  def ParseIfStatement(self, labels):
    # IfStatement ::
    #   'if' '(' Expression ')' Statement ('else' Statement)?

    self.Expect("IF")
    self.Expect("LPAREN")
    condition = self.ParseExpression(True)
    self.Expect("RPAREN")
    then_statement = self.ParseStatement(labels)
    else_statement = None
    if self.peek() == "ELSE":
      self.Next()
      else_statement = self.ParseStatement(labels)
    elif True: #!is_pre_parsing_) {
      else_statement = EmptyStatement()
    return IfStatement(condition, then_statement, else_statement)

  def ParseStatement(self, labels):
    stmt = None
    peek = self.peek()
    if peek == "LBRACE":
      return self.ParseBlock(labels)

    elif peek == "CONST" or peek == "VAR":
      stmt = self.ParseVariableStatement()

    elif peek == "SEMICOLON":
      self.Next()
      return None

    elif peek == "IF":
      stmt = self.ParseIfStatement(labels)

    elif peek == "DO":
      stmt = self.ParseDoWhileStatement(labels)

    elif peek == "WHILTE":
      stmt = self.ParseWhileStatement(labels)

    elif peek == "FOR":
      stmt = self.ParseForStatement(labels)

    elif peek == "CONTINUE":
      stmt = self.ParseContinueStatement(o);

    elif peek == "BREAK":
      stmt = self.ParseBreakStatement(labels);

    elif peek == "RETURN":
      stmt = self.ParseReturnStatement();

    elif peek == "WITH":
      stmt = self.ParseWithStatement(labels);

    elif peek == "SWITCH":
      stmt = self.ParseSwitchStatement(labels);

    elif peek == "THROW":
      stmt = self.ParseThrowStatement();

    elif peek == "TRY":
      # NOTE: It is somewhat complicated to have labels on
      # try-statements. When breaking out of a try-finally statement,
      # one must take great care not to treat it as a
      # fall-through. It is much easier just to wrap the entire
      # try-statement in a statement block and put the labels there
#            Block* result = NEW(Block(labels, 1, false));
#            Target target(this, result);
#            TryStatement* statement = ParseTryStatement(CHECK_OK);
#            if (statement) {
#                statement->set_statement_pos(statement_pos);
#                }
#            if (result) result->AddStatement(statement);
#            return result;
      assert(False)

    elif peek == "FUNCTION":
      return self.ParseFunctionDeclaration()

    elif peek == "NATIVE":
      return self.ParseNativeDeclaration()

    elif peek == "DEBUGGER":
      stmt = self.self.ParseDebuggerStatement()

    else:
      stmt = self.ParseExpressionOrLabelledStatement(labels)

    return stmt

class AstVisitor:
  def Visit(self, node):
    node.Accept(self)

class Printer(AstVisitor):
  def __init__(self, print_types = False):
    self.print_types = print_types
    self.nest = 0
    self.buffer = ""

  def W(self, str):
    self.buffer += str

  def NewlineAndIndent(self):
    self.W("\n")
    self.W(" " * (self.nest * 4))

  def PrintTypes(self, node):
    assert(self.print_types)
    self.W('/*')
    for i in range(0, len(node.__type__.types)):
      if i > 0:
        self.W(',')
      self.W(Type.ToString(node.__type__.types[i]))
    self.W('*/')

  def PrintLiteral(self, value, quote):
    if value.IsString():
      if quote:
        self.W('"')
      self.W(value.value)
      if quote:
        self.W('"')
    elif value == JSNULL:
      self.W("null")
    elif value == JSTRUE:
      self.W("true")
    elif value == JSFALSE:
      self.W("false")
    elif value.IsNumber:
      self.W(str(value.value))
    else:
      self.W("<unknown literal>")

  def VisitLiteral(self, node):
    self.PrintLiteral(node.handle, node.handle.IsString())
    if self.print_types:
      self.PrintTypes(node)

  def PrintParameters(self, scope):
    self.W("(")
    for i in range(0, scope.num_parameters()):
      if i > 0:
        self.W(", ")
      self.PrintLiteral(scope.parameter(i).name, False)
      if self.print_types:
        self.PrintTypes(scope.parameter(i))
    self.W(")")

  def PrintDeclarations(self, declarations):
    for i in range(0, len(declarations)):
#            if i > 0:
#                self.W(" ")
      self.Visit(declarations[i])

  def PrintStatements(self, statements):
    changed = True
    for i in range(0, len(statements)):
      if changed:
        self.NewlineAndIndent()
      old_len = len(self.buffer)
      self.Visit(statements[i])
      changed = old_len < len(self.buffer)
      if changed:
        self.W(";")

  def PrintFunctionLiteral(self, function):
    self.W("function ")
    self.PrintLiteral(function.name, False)
    self.PrintParameters(function.scope)
    self.W(" { ")
    self.nest += 1
    self.PrintDeclarations(function.scope.declarations())
    self.PrintStatements(function.body)
    self.nest -= 1
    self.NewlineAndIndent()
    self.W("}")

  def VisitFunctionLiteral(self, node):
    self.W("(")
    self.PrintFunctionLiteral(node)
    self.W(")")
    if self.print_types:
      self.PrintTypes(node)

  def VisitExpressionStatement(self, node):
    self.Visit(node.expression)

  def VisitCall(self, node):
    self.Visit(node.expression)
    self.W("(")
    self.PrintArguments(node.arguments)
    self.W(")")

  def VisitVariableProxy(self, node):
    self.Visit(node.var)
#        self.PrintLiteral(node.name, False)
#        if self.print_types:
#            self.PrintTypes(node)

  def PrintArguments(self, arguments):
    for i in range(0, len(arguments)):
      if i > 0:
        self.W(",")
      self.Visit(arguments[i])

  def VisitDeclaration(self, node):
    self.NewlineAndIndent()
    self.W("var ")
    self.PrintLiteral(node.proxy.name, False)
    if node.fun != None:
      self.W(" = ")
      self.PrintFunctionLiteral(node.fun)
    self.W(";")

  def VisitReturnStatement(self, node):
    self.W("return ")
    self.Visit(node.expression)

  def VisitConditional(self, node):
    self.Visit(node.condition)
    self.W(" ? ")
    self.Visit(node.then_expression)
    self.W(" : ")
    self.Visit(node.else_expression)

  def VisitCompareOperation(self, node):
    self.W("(")
    self.Visit(node.left)
    self.W(String(node.op))
    self.Visit(node.right)
    self.W(")")

  def VisitIfStatement(self, node):
    self.W("if (")
    self.Visit(node.condition)
    self.W(") ")
    self.Visit(node.then_statement)
    if node.HasElseStatement():
      self.W(" else ")
      self.Visit(node.else_statement)

  def VisitBinaryOperation(self, node):
    self.W("(")
    self.Visit(node.left)
    self.W(String(node.op))
    self.Visit(node.right)
    self.W(")")
    if self.print_types:
      self.PrintTypes(node)

  def VisitBlock(self, node):
    # this method may print no characters, so lonely ";" may appear.
    if not node.is_initializer_block:
      self.W("{ ")
      self.nest += 1
    if len(node.statements) > 0:
      self.PrintStatements(node.statements)
    if not node.is_initializer_block:
      self.nest -= 1
      self.W("}")

  def VisitVariable(self, node):
    self.W(node.name.value)
    if self.print_types:
      self.PrintTypes(node)

  def VisitAssignment(self, node):
    self.Visit(node.target.var)
    self.W(" " + String(node.op) + " ")
    self.Visit(node.value)

  def PrintLn(self, node):
    try:
      self.Visit(node)
      self.W("\n")
      sys.stdout.write(self.buffer)
    except Exception:
      print self.buffer
      raise

#class TemplateRepository:
#  def __init__(self, fun):
#    self.repos = dict()
#    self.fun = fun
#  def Create(self, tuple):
#    if not tuple in self.repos:
#      1
#    return self.repos[tuple]

###############
# 1, 2 Allocate type variables, and seed them
class Seeder(AstVisitor):
  def __init__(self, nodes):
    self.nodes = nodes

  def Allocate(self, node):
    if not '__type__' in dir(node):
      node.__type__ = TypeNode()
      self.nodes.append(node.__type__)

  def Seed(self, node, type):
    node.__type__.types.append(type)

  def VisitFunctionLiteral(self, node):
    self.Allocate(node)
    self.Seed(node, Type.FUNCTION)

    for i in range(0, node.scope.num_parameters()):
      self.Visit(node.scope.parameter(i))

    for decl in node.scope.declarations():
      self.Visit(decl)

    for stmt in node.body:
      self.Visit(stmt)

  def VisitDeclaration(self, node):
    self.Visit(node.proxy)
    if node.fun != None:
      self.Visit(node.fun)
      node.fun.__type__.AddEdge(node.proxy.var.__type__)

  def VisitVariableProxy(self, node):
    self.Visit(node.var)

  def VisitVariable(self, node):
    self.Allocate(node)

  def VisitReturnStatement(self, node):
    self.Visit(node.expression)

  def VisitCall(self, node):
    self.Allocate(node)

    self.Visit(node.expression)

    for arg in node.arguments:
      self.Visit(arg)

  def VisitBinaryOperation(self, node):
    self.Allocate(node)

    self.Visit(node.left)
    self.Visit(node.right)

    (node.left.var if node.left.AsVariableProxy() else node.left).__type__.AddEdge(node.__type__)
    (node.right.var if node.right.AsVariableProxy() else node.right).__type__.AddEdge(node.__type__)

  def VisitExpressionStatement(self, node):
    self.Allocate(node)
    self.Visit(node.expression)

  def VisitLiteral(self, node):
    self.Allocate(node)
    if node.handle.IsString():
      type = Type.STRING
    elif node.IsNull():
      type = Type.NULL
    elif node.IsTrue() or node.IsFalse():
      type = Type.BOOL
    elif node.handle.IsNumber():
      type = Type.INT
    else:
      type = Type.UNKNOWN
    self.Seed(node, type)

  def VisitBlock(self, node):
    for stmt in node.statements:
      self.Visit(stmt)

  def VisitAssignment(self, node):
    self.Visit(node.target.var)
    self.Visit(node.value)
    node.value.__type__.AddEdge(node.target.var.__type__)

  def VisitConditional(self, node):
    self.Allocate(node)

    self.Visit(node.condition)
    self.Visit(node.then_expression)
    self.Visit(node.else_expression)

  def VisitCompareOperation(self, node):
    self.Allocate(node)
    self.Visit(node.left)
    self.Visit(node.right)

  def VisitIfStatement(self, node):
    self.Visit(node.condition)
    self.Visit(node.then_statement)
    if node.HasElseStatement():
      self.Visit(node.else_statement)

def Propagate(nodes):
  while True:
    changed = False
    for node in nodes:
      changed = changed or node.Propagate()
    if not changed:
      break

def main():
  myusage = "%prog [-p] < %file"
  psr = OptionParser(usage = myusage)
  psr.add_option('-p', action='store_true', dest='print_types')
  (opts, args) = psr.parse_args(sys.argv)

  scanner = Scanner(sys.stdin.read())
  parser = Parser(scanner)
  ast = parser.ParseProgram(True)

  nodes = []
  Seeder(nodes).Visit(ast)
  Propagate(nodes)
  Printer(opts.print_types).PrintLn(ast)

if __name__ == "__main__":
  main()
