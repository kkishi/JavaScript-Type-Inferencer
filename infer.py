#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import copy
from optparse import OptionParser

def LOG(*args):
  if not LOG.enabled: return
  print('// ' + str(args))
#  S = '//'
#  for a in args: S += ' ' + str(a)
#  print(S)

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

def DoubleToInt32(c):
  return int(c)

class Token:
  tokens_ = [
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
#   ("INIT_VAR", "=init_var", 2),  # AST-use only.
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

    # Future reserved words (ECMA-262, section 7.5.3, page 14).
    ("ABSTRACT", "abstract", 0),
    ("BOOLEAN", "boolean", 0),
    ("BYTE", "byte", 0),
    ("CHAR", "char", 0),
    ("CLASS", "class", 0),
    ("CONST", "const", 0),
    ("DOUBLE", "double", 0),
    ("ENUM", "enum", 0),
    ("EXPORT", "export", 0),
    ("EXTENDS", "extends", 0),
    ("FINAL", "final", 0),
    ("FLOAT", "float", 0),
    ("GOTO", "goto", 0),
    ("IMPLEMENTS", "implements", 0),
    ("IMPORT", "import", 0),
    ("INT", "int", 0),
    ("INTERFACE", "interface", 0),
    ("LONG", "long", 0),
    ("NATIVE", "native", 0),
    ("PACKAGE", "package", 0),
    ("PRIVATE", "private", 0),
    ("PROTECTED", "protected", 0),
    ("PUBLIC", "public", 0),
    ("SHORT", "short", 0),
    ("STATIC", "static", 0),
    ("SUPER", "super", 0),
    ("SYNCHRONIZED", "synchronized", 0),
    ("THROWS", "throws", 0),
    ("TRANSIENT", "transient", 0),
    ("VOLATILE", "volatile", 0),

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

  @staticmethod
  def Init():
    Token.names_ = [token[0] for token in Token.tokens_]
    Token.strings_ = [token[1] for token in Token.tokens_]
    Token.precedences_ = [token[2] for token in Token.tokens_]
    Token.name2int_ = dict()
    for i in range(0, len(Token.tokens_)):
      Token.name2int_[Token.tokens_[i][1]] = i
      vars(Token)[Token.tokens_[i][0]] = i

  @staticmethod
  def Name(type):
    return Token.names_[type]

  @staticmethod
  def String(tok):
    return Token.strings_[tok]

  @staticmethod
  def Precedence(tok):
    return Token.precedences_[tok]

  @staticmethod
  def IsAssignmentOp(op):
    return Token.INIT_VAR <= op and op <= Token.ASSIGN_MOD

  @staticmethod
  def IsUnaryOp(op):
    return Token.NOT <= op and op <= Token.VOID or \
           op == Token.ADD or op == Token.SUB

  @staticmethod
  def IsCountOp(op):
    return op == Token.INC or op == Token.DEC

  @staticmethod
  def IsCompareOp(op):
    return Token.EQ <= op and op <= Token.IN

  @staticmethod
  def IsBinaryOp(op):
    return Token.COMMA<= op and op <= Token.MOD

Token.Init()

class KeywordMatcher:
  def __init__(self):
    self.buffer = ""
  def AddChar(self, c):
    self.buffer += c
  def token(self):
    if self.buffer in Token.name2int_:
      return Token.name2int_[self.buffer]
    return Token.IDENTIFIER

class Scanner:
  class Location:
    def __init__(self, b = 0, e = 0):
      self.beg_pos = b
      self.end_pos = e

  class TokenDesc:
    def __init__(self):
      self.token = Token.ILLEGAL
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
        self.Advance()
        return then
      else:
        return else_

    return (Select1 if len(args) == 1 else Select3)(*args)

  def Scan(self):
    self.next_.literal_buffer = ""
    token = Token.WHITESPACE
    self.has_line_terminator_before_next_ = False
    while token == Token.WHITESPACE:
      # Remember the position of the next token
      self.next_.location.beg_pos = self.source_pos()

      # Continue scanning for tokens as long as we're just skipping
      # whitespace.

      if self.c0_ == ' ' or self.c0_ == '\t':
        self.Advance()
        token = Token.WHITESPACE

      elif self.c0_ == '\n':
        self.Advance()
        self.has_line_terminator_before_next_ = True
        token = Token.WHITESPACE

      elif self.c0_ == '"' or self.c0_ == '\'':
        token = self.ScanString()

      elif self.c0_ == '<':
        # < <= << <<= <!--
        self.Advance()
        if self.c0_ == '=':
          token = self.Select(Token.LTE)
        elif self.c0_ == '<':
          token = self.Select("=", Token.ASSIGN_SHL, Token.SHL)
#        } else if (c0_ == '!') {
#          token = ScanHtmlComment();
        else:
          token = Token.LT

      elif self.c0_ == '>':
        # > >= >> >>= >>> >>>=
        self.Advance()
        if self.c0_ == '=':
          token = self.Select(Token.GTE)
        elif self.c0_ == '>':
          # >> >>= >>> >>>=
          self.Advance()
          if self.c0_ == '=':
            token = self.Select(Token.ASSIGN_SAR)
          elif self.c0_ == '>':
            token = self.Select('=', Token.ASSIGN_SHR, Token.SHR)
          else:
            token = Token.SAR;
        else:
          token = Token.GT

      elif self.c0_ == '=':
        self.Advance()
        if self.c0_ == '=':
          token = self.Select('=', Token.EQ_STRICT, Token.EQ)
        else:
          token = Token.ASSIGN

      elif self.c0_ == '!':
        self.Advance()
        if self.c0_ == '=':
          token = self.Select('=', Token.NE_STRICT, Token.NE)
        else:
          token = Token.NOT

      elif self.c0_ == '+':
        # + ++ +=
        self.Advance()
        if self.c0_ == '+':
          token = self.Select(Token.INC)
        elif self.c0_ == '=':
          token = self.Select(Token.ASSIGN_ADD)
        else:
          token = Token.ADD

      elif self.c0_ == '-':
        # - -- --> -=
        self.Advance()
        if self.c0_ == '-':
          token = self.Select(Token.DEC)
        elif self.c0_ == '=':
          token = self.Select(Token.ASSIGN_SUB)
        else:
          token = Token.SUB

      elif self.c0_ == '*':
        # * *=
        token = self.Select('=', Token.ASSIGN_MUL, Token.MUL)

      elif self.c0_ == '%':
        # % %=
        token = self.Select('=', Token.ASSIGN_MOD, Token.MOD)

      elif self.c0_ == '/':
        # /  // /* /=
        self.Advance();
        if self.c0_ == '/':
          token = self.SkipSingleLineComment()
        elif self.c0_ == '*':
          token = self.SkipMultiLineComment()
        elif self.c0_ == '=':
          token = self.Select(Token.ASSIGN_DIV)
        else:
          token = Token.DIV

      elif self.c0_ == '&':
        # & && &=
        self.Advance()
        if self.c0_ == '&':
          token = self.Select(Token.AND)
        elif self.c0_ == '=':
          token = self.Select(Token.ASSIGN_BIT_AND)
        else:
          token = Token.BIT_AND

      elif self.c0_ == '|':
        # | || |=
        self.Advance()
        if self.c0_ == '|':
          token = self.Select(Token.OR)
        elif self.c0_ == '=':
          token = self.Select(Token.ASSIGN_BIT_OR)
        else:
          token = Token.BIT_OR

      elif self.c0_ == '^':
        # ^ ^=
        token = self.Select('=', Token.ASSIGN_BIT_XOR, Token.BIT_XOR)

      elif self.c0_ == '.':
        # . Number
        self.Advance();
        if IsDecimalDigit(self.c0_):
          token = self.ScanNumber(True)
        else:
          token = Token.PERIOD

      elif self.c0_ == ':':
        token = self.Select(Token.COLON)

      elif self.c0_ == ';':
        token = self.Select(Token.SEMICOLON)

      elif self.c0_ == ',':
        token = self.Select(Token.COMMA)

      elif self.c0_ == '(':
        token = self.Select(Token.LPAREN)

      elif self.c0_ == ')':
        token = self.Select(Token.RPAREN)

      elif self.c0_ == '[':
        token = self.Select(Token.LBRACK)

      elif self.c0_ == ']':
        token = self.Select(Token.RBRACK)

      elif self.c0_ == '{':
        token = self.Select(Token.LBRACE)

      elif self.c0_ == '}':
        token = self.Select(Token.RBRACE)

      elif self.c0_ == '?':
        token = self.Select(Token.CONDITIONAL)

      elif self.c0_ == '~':
        token = self.Select(Token.BIT_NOT)

      else:
        if IsIdentifierStart(self.c0_):
          token = self.ScanIdentifier()
        elif IsDecimalDigit(self.c0_):
          token = self.ScanNumber(False)
        elif self.SkipWhiteSpace():
          token = Token.WHITESPACE
        elif self.c0_ == EOF:
          token = Token.EOS
        else:
          token = self.Select(Token.ILLEGAL)

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
            return Token.ILLEGAL
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
        return Token.ILLEGAL  # no exponent for octals allowed
      # scan exponent
      self.AddCharAdvance()
      if self.c0_ == '+' or self.c0_ == '-':
        self.AddCharAdvance()
      if not IsDecimalDigit(self.c0_):
        # we must have at least one decimal digit after 'e'/'E'
        return Token.ILLEGAL
      self.ScanDecimalDigits()

    # The source character immediately following a numeric literal must
    # not be an identifier start or a decimal digit; see ECMA-262
    # section 7.8.3, page 17 (note that we read only one decimal digit
    # if the value is 0).
    if IsDecimalDigit(self.c0_) or IsIdentifierStart(self.c0_):
      return Token.ILLEGAL

    return Token.NUMBER

  def ScanString(self):
    quote = self.c0_
    self.Advance()

    self.StartLiteral()
    while self.c0_ != quote and self.c0_ != EOF and not IsLineTerminator(self.c0_):
      c = self.c0_
      self.Advance()
      if c == '\\':
        if self.c0_ == EOF:
          return Token.ILLEGAL
        ScanEscape()
      else:
        self.AddChar(c)

    if self.c0_ != quote:
      return Token.ILLEGAL

    self.Advance()  # consume quote
    return Token.STRING

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
    return Token.WHITESPACE

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
        return Token.WHITESPACE

    # Unterminated multi-line comment.
    return Token.ILLEGAL

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
JSBUILTINS = [JSNULL, JSTRUE, JSFALSE, JSUNDEFINED]
##############

class SmiAnalysis:
  #enum Kind {
  UNKNOWN = 0
  LIKELY_SMI = 1

  def __init__(self):
    self.kind = SmiAnalysis.UNKNOWN

  def Is(self, kind):
    return self.kind == kind

  def IsKnown(self):
    return not self.Is(SmiAnalysis.UNKNOWN)
  def IsUnknown(self):
    return self.Is(SmiAnalysis.UNKNOWN)
  def IsLikelySmi(self):
    return self.Is(SmiAnalysis.LIKELY_SMI)

  def CopyFrom(self, other):
    self.kind = other.kind

  @staticmethod
  def Type2String(type):
    if type == SmiAnalysis.UNKNOWN:
      return "UNKNOWN"
    elif type == SmiAnalysis.LIKELY_SMI:
      return "LIKELY_SMI"
    assert(False)

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
    self.scope_ = scope
    self.name_ = name
    self.mode_ = mode
    self.is_valid_LHS_ = is_valid_lhs
    self.kind_ = kind
    self.local_if_not_shadowed_ = None
    self.is_accessed_from_inner_scope_ = False
    self.rewrite_ = None
    self.var_uses_ = Variable.UseCount()
    #ASSERT(name->IsSymbol());
    self.fun_ = None  # added by keisuke

  @staticmethod
  def Mode2String(mode):
    for t in ((Variable.VAR, "VAR"),
              (Variable.CONST,"CONST"),
              (Variable.DYNAMIC,"DYNAMIC"),
              (Variable.DYNAMIC_GLOBAL,"DYNAMIC_GLOBAL"),
              (Variable.DYNAMIC_LOCAL,"DYNAMIC_LOCAL"),
              (Variable.INTERNAL,"INTERNAL"),
              (Variable.TEMPORARY,"TEMPORARY")):
      if mode == t[0]:
        return t[1]
    assert(0)

  def AsProperty(self):
    return None if self.rewrite == None else self.rewrite.AsProperty()
  def AsVariable(self):
    if self.rewrite_ == None or self.rewrite_.AsSlot() != None:
      return self
    else:
      return None

  # The source code for an eval() call may refer to a variable that is
  # in an outer scope about which we don't know anything (it may not
  # be the global scope). scope() is NULL in that case. Currently the
  # scope is only used to follow the context chain length.
  def scope(self): return self.scope_

  def name(self): return self.name_
  def mode(self): return self.mode_
  def is_accessed_from_inner_scope(self):
    return self.is_accessed_from_inner_scope_
  def var_uses(self): return self.var_uses_
  def obj_uses(self): return self.obj_uses_

  def IsVariable(self, n):
    return not self.is_this() and self.name_.value == n.value

  def is_dynamic(self):
    return self.mode_ == self.DYNAMIC or \
           self.mode_ == self.DYNAMIC_GLOBAL or \
           self.mode_ == self.DYNAMIC_LOCAL

  def is_global(self):
    # Temporaries are never global, they must always be allocated in the
    # activation frame.
    return self.mode_ != self.TEMPORARY and self.scope_ != None and \
           self.scope_.is_global_scope()
  def is_this(self): return self.kind_ == self.THIS

  def is_possibly_eval(self):
    return self.IsVariable("eval") and \
      (self.mode_ == self.DYNAMIC or self.mode_ == self.DYNAMIC_GLOBAL)

  def local_if_not_shadowed(self):
    assert(self.mode_ == self.DYNAMIC_LOCAL and self.local_if_not_shadowed_ != None)
    return self.local_if_not_shadowed_

  def set_local_if_not_shadowed(self, local):
    self.set_local_if_not_shadowed_ = local

  def rewrite(self):
    return self.rewrite_
  def slot(self):
    return self.rewite.AsSlot() if self.rewrite_ != None else None

  def type(self):
    return self.type_

  # added by keisuke
  def Accept(self, v):
    return v.VisitVariable(self)

  # added by keisuke
  def fun(self):
    return self.fun_
  def set_fun(self, fun):
    assert(isinstance(fun, FunctionLiteral))
    self.fun_ = fun

class AstNode:
  def Accept(self, v): assert(False)

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
    self.context_ = self.kUninitialized

  def AsExpression(self): return self

  def IsValidJSON(self): return False
  def IsValidLeftHandSide(self): return False

  def MarkAsStatement(self): pass  # do nothing

  # Static type information for this expression.
  #def type(self): return self.type_
  def type(self): return None

  def context(self): return self.context_
  def set_context(self, context): self.context_ = context

class BreakableStatement(Statement):
  # enum Type
  TARGET_FOR_ANONYMOUS = 0
  TARGET_FOR_NAMED_ONLY = 1

  # The labels associated with this statement. May be NULL;
  # if it is != NULL, guaranteed to contain at least one entry.
  def labels(self): return self.labels_

  def AsBreakableStatement(self): return self

  def break_target(self): return self.break_target_

  def is_target_for_anonymous(self):
    return self.type_ == self.TARGET_FOR_ANONYMOUS

  def __init__(self, labels, type):
    self.labels_ = labels
    self.type_ = type
    assert(labels == None or len(labels) > 0)

class Block(BreakableStatement):
  def __init__(self, labels, capacity, is_initializer_block):
    BreakableStatement.__init__(self, labels, self.TARGET_FOR_NAMED_ONLY)
    self.statements_ = []
    self.is_initializer_block_ = is_initializer_block

  def Accept(self, v):
    return v.VisitBlock(self)

  def AddStatement(self, statement):
    self.statements_.append(statement)

  def statements(self):
    return self.statements_

  def is_initializer_block(self):
    return self.is_initializer_block_

class Declaration(AstNode):
  def __init__(self, proxy, mode, fun):
    self.proxy_ = proxy
    self.mode_ = mode
    self.fun_ = fun
    assert(mode == Variable.VAR or mode == Variable.CONST)
    assert(fun == None or mode == Variable.VAR)

  def Accept(self, v):
    return v.VisitDeclaration(self)

  def proxy(self):
    return self.proxy_

  def mode(self):
    return self.mode_

  def fun(self):
    return self.fun_

class IterationStatement(BreakableStatement):
  # Type testing & conversion.
  def AsIterationStatement(self): return self

  def body(self): return self.body_

  # Code generation
  #BreakTarget* continue_target()  { return &continue_target_; }
  # continue_target_ should be [None] (list with one element)
  def continue_target(self): return self.continue_target_

  def __init__(self, labels):
    BreakableStatement.__init__(self, labels, self.TARGET_FOR_ANONYMOUS)
    self.body_ = None

  def Initialize(self, body):
    self.body_ = body

class WhileStatement(IterationStatement):
  def __init__(self, labels):
    IterationStatement.__init__(self, labels)
    self.cond_ = None
    self.may_have_function_literal_ = True

  def Initialize(self, cond, body):
    IterationStatement.Initialize(self, body)
    self.cond_ = cond

  def Accept(self, v):
    return v.VisitWhileStatement(self)

  def cond(self): return self.cond_
  def may_have_function_literal(self):
    # True if there is a function literal subexpression in the condition.
    return self.may_have_function_literal_

class ForStatement(IterationStatement):
  def __init__(self, labels):
    IterationStatement.__init__(self, labels)
    self.init_ = None
    self.cond_ = None
    self.next_ = None
    self.may_have_function_literal_ = True

  def Initialize(self, init, cond, next, body):
    IterationStatement.Initialize(self, body)
    self.init_ = init
    self.cond_ = cond
    self.next_ = next

  def Accept(self, v):
    return v.VisitForStatement(self)

  def init(self): return self.init_
  def cond(self): return self.cond_
  def next(self): return self.next_
  def may_have_function_literal(self):
    # True if there is a function literal subexpression in the condition.
    return self.may_have_function_literal_

class ExpressionStatement(Statement):
  def __init__(self, expression):
    self.expression_ = expression

  def Accept(self, v):
    return v.VisitExpressionStatement(self)

  def AsExpressionStatement(self):
    return self

  def set_expression(self, e): self.expression_ = e
  def expression(self): return self.expression_

class ReturnStatement(Statement):
  def __init__(self, expression):
    self.expression_ = expression

  def Accept(self, v):
    return v.VisitReturnStatement(self)

  def AsReturnStatement(self):
    return self

  def expression(self):
    return self.expression_

# If-statements always have non-null references to their then- and
# else-parts. When parsing if-statements with no explicit else-part,
# the parser implicitly creates an empty statement. Use the
# HasThenStatement() and HasElseStatement() functions to check if a
# given if-statement has a then- or an else-part containing code.
class IfStatement(Statement):
  def __init__(self, condition, then_statement, else_statement):
    self.condition_ = condition
    self.then_statement_ = then_statement
    self.else_statement_ = else_statement

  def Accept(self, v):
    return v.VisitIfStatement(self)

  def HasThenStatement(self): return not self.then_statement_.IsEmpty()
  def HasElseStatement(self): return not self.else_statement_.IsEmpty()

  def condition(self): return self.condition_
  def then_statement(self): return self.then_statement_
  def else_statement(self): return self.else_statement_

class EmptyStatement(Statement):
  def Accept(self, v):
    return v.VisitEmptyStatement(self)

  def AsEmptyStatement(self):
    return self

class Literal(Expression):
  def __init__(self, handle):
    Expression.__init__(self)
    self.handle_ = handle

  def Accept(self, v):
    return v.VisitLiteral(self)

  def AsLiteral(self):
    return self

  def IsIdenticalTo(self, other):
    return self.handle_ == other.handle_

  def IsValidJSON(self):
    return True

  def IsNull(self):
    return self.handle_ == JSNULL
  def IsTrue(self):
    return self.handle_ == JSTRUE
  def IsFalse(self):
    return self.handle_ == JSFALSE
  def IsUndefined(self):
    return self.handle_ == JSUNDEFINED

  def handle(self):
    return self.handle_

class VariableProxy(Expression):
  def __init__(self, name, is_this, inside_with):
    Expression.__init__(self)
    self.name_ = name
    self.var_ = None
    self.is_this_ = is_this
    self.inside_with_ = inside_with
    # names must be canonicalized for fast equality checks
    #ASSERT(name->IsSymbol());

  def Accept(self, v):
    return v.VisitVariableProxy(self)

  def AsProperty(self):
    return None if self.var_ == None else self.var_.AsProperty()
  def AsVariableProxy(self):
    return self

  def AsVariable(self):
    return None if self == None or self.var_ == None else self.var_.AsVariable()

  def IsValidLeftHandSide(self):
    return True if self.var_ == None else self.var_.IsValidLeftHandSide()

  def IsVariable(self, n):
    return self.is_this_ and self.name_ == n

  def IsArguments(self):
    variable = self.AsVariable()
    return False if variable == None else variable.is_arguments()

  def name(self): return self.name_
  def var(self): return self.var_
  def var_uses(self): return self.var_uses_
  def obj_uses(self): return self.obj_uses_
  def is_this(self): return self.is_this_
  def inside_with(self): return self.inside_with_

  def BindTo(self, var):
    assert(self.var_ == None)
    assert(var != None)
    #assert((self.is_this and var.is_this) or self.name.is_identical_to(var.name))
    self.var_ = var

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
    Expression.__init__(self)
    self.var_ = var
    self.type_ = type
    self.index_ = index
    assert(var != None)

  def Accept(self, v):
    return v.VisitSlot(self)

  def AsSlot(self):
    return self

  def var(self): return self.var_
  def type(self): return self.type_
  def index(self): return self.index_
  def is_arguments(self): return self.var_.is_arguments()

class Call(Expression):
  sentinel_ = None

  def __init__(self, expression, arguments):
    Expression.__init__(self)
    self.expression_ = expression
    self.arguments_ = arguments

  def Accept(self, v):
    return v.VisitCall(self)

  def AsCall(self):
    return self

  def expression(self): return self.expression_
  def arguments(self): return self.arguments_
  def position(self): return self.pos_

  @staticmethod
  def sentinel(): return Call.sentinel_

class UnaryOperation(Expression):
  def __init__(self, op, expression):
    Expression.__init__(self)
    self.op_ = op
    self.expression_ = expression
    assert(Token.IsUnaryOp(op))

  def Accept(self, v):
    return v.VisitUnaryOperation(self)

  def AsUnaryOperation(self):
    return self

  def op(self): return self.op_
  def expression(self): return self.expression_

class BinaryOperation(Expression):
  def __init__(self, op, left, right):
    Expression.__init__(self)
    self.op_ = op
    self.left_ = left
    self.right_ = right
    assert(Token.IsBinaryOp(op))

  def Accept(self, v):
    return v.VisitBinaryOperation(self)

  def AsBinaryOperation(self):
    return self

  def ResultOverwrittenAllowed(self):
    not_allowed = (Token.COMMA, Token.OR, Token.AND)
    allowed = (Token.BIT_OR, Token.BIT_XOR, Token.BIT_AND, Token.SHL, Token.SAR, Token.SHR, Token.ADD,
               Token.SUB, Token.MUL, Token.DIV, Token.MOD)
    if self.op_ in not_allowed: return False
    if self.op_ in allowed: return True
    assert(False)

  def op(self): return self.op_
  def left(self): return self.left_
  def right(self): return self.right_

class CountOperation(Expression):
  def __init__(self, is_prefix, op, expression):
    Expression.__init__(self)
    self.is_prefix_ = is_prefix
    self.op_ = op
    self.expression_ = expression

  def Accept(self, v):
    return v.VisitCountOperation(self)

  def is_prefix(self): return self.is_prefix_
  def is_postfix(self): return not self.is_prefix_
  def op(self): return self.op_
  def expression(self): return self.expression_

  def MarkAsStatement(self):
    self.is_prefix_ = True

class CompareOperation(Expression):
  def __init__(self, op, left, right):
    Expression.__init__(self)
    self.op_ = op
    self.left_ = left
    self.right_ = right
    assert(Token.IsCompareOp(op))

  def Accept(self, v):
    return v.VisitCompareOperation(self)

  def op(self): return self.op_
  def left(self): return self.left_
  def right(self): return self.right_

class Conditional(Expression):
  def __init__(self, condition, then_expression, else_expression):
    Expression.__init__(self)
    self.condition_ = condition
    self.then_expression_ = then_expression
    self.else_expression_ = else_expression

  def Accept(self, v):
    return v.VisitConditional(self)

  def condition(self):
    return self.condition_
  def then_expression(self):
    return self.then_expression_
  def else_expression(self):
    return self.else_expression_

class Assignment(Expression):
  def __init__(self, op, target, value):
    Expression.__init__(self)
    self.op_ = op
    self.target_ = target
    self.value_ = value
    self.block_start_ = False
    self.block_end_ = False
    assert(Token.IsAssignmentOp(op))

  def Accept(self, v):
    return v.VisitAssignment(self)

  def AsAssignment(self):
    return self

  def binary_op(self):
    T = ((Token.ASSIGN_BIT_OR, Token.BIT_OR),
         (Token.ASSIGN_BIT_XOR, Token.BIT_XOR),
         (Token.ASSIGN_BIT_AND, Token.BIT_AND),
         (Token.ASSIGN_SHL, Token.SHL),
         (Token.ASSIGN_SAR, Token.SAR),
         (Token.ASSIGN_SHR, Token.SHR),
         (Token.ASSIGN_ADD, Token.ADD),
         (Token.ASSIGN_SUB, Token.SUB),
         (Token.ASSIGN_MUL, Token.MUL),
         (Token.ASSIGN_DIV, Token.DIV),
         (Token.ASSIGN_MOD, Token.MOD))
    for t in T:
      if t[0] == self.op:
        return t[1]
    assert(False)

  def op(self): return self.op_
  def target(self): return self.target_
  def value(self): return self.value_
  def position(self): return self.pos_

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
  num_of_instances = 0
  def __init__(self, name, scope, body, num_parameters, is_expression):
    Expression.__init__(self)
    self.name_ = name
    self.scope_ = scope
    self.body_ = body
    self.num_parameters_ = num_parameters
    self.is_expression_ = is_expression
    self.loop_nesting_ = 0
    self.inferred_name_ = JSObject("string", "")
    self.try_fast_codegen_ = False
    FunctionLiteral.num_of_instances += 1

  def Accept(self, v):
    return v.VisitFunctionLiteral(self)

  def AsFunctionLiteral(self):
    return self

  def name(self): return self.name_
  def scope(self): return self.scope_;
  def body(self): return self.body_
  def is_expression(self): return self.is_expression_

  def num_parameters(self):
    return self.num_parameters_

  def loop_nesting(self):
    return self.loop_nesting_
  def set_loop_nesting(self, nesting):
    self.loop_nesting_ = nesting

  def inferred_name(self):
    return self.inferred_name_
  def set_inferred_name(self, inferred_name):
    self.inferred_name_ = inferred_name

class ThisFunction(Expression):
  def Accept(self, v):
    return v.VisitThisFunction(self)

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
    self.outer_scope_ = outer_scope
    self.inner_scopes_ = []
    self.type_ = type
    self.scope_name_ = JSObject("string", "")
    self.variables_ = VariableMap()
    self.temps_ = []
    self.params_ = []
    self.dynamics_ = None
    self.unresolved_ = []
    self.decls_ = []
    self.receiver_ = None
    self.function_ = None
    self.arguments_ = None
    self.arguments_shadow_ = None
    self.scope_inside_with_ = False
    self.scope_contains_with_ = False
    self.scope_calls_eval_ = False
    self.outer_scope_calls_eval_ = False
    self.inner_scope_calls_eval_ = False
    self.outer_scope_is_eval_scope_ = False
    self.force_eager_compilation_ = False
    self.num_stack_slots_ = False
    self.num_heap_slots_ = False
    # At some point we might want to provide outer scopes to
    # eval scopes (by walking the stack and reading the scope info).
    # In that case, the ASSERT below needs to be adjusted.
    assert((type == self.GLOBAL_SCOPE or self.EVAL_SCOPE) == \
           (self.outer_scope_ == None))

  def SetScopeName(self, scope_name):
    self.scope_name_ = scope_name

  def Initialize(self, inside_with):
    if not self.outer_scope_ == None:
      self.outer_scope_.inner_scopes_.append(self)
      self.scope_inside_with_ = self.outer_scope_.scope_inside_with_ or \
        inside_with
    else:
      self.scope_inside_with_ = inside_with

    # Declare convenience variables.
    # Declare and allocate receiver (even for the global scope, and even
    # if naccesses_ == 0).
    # NOTE: When loading parameters in the global scope, we must take
    # care not to access them as properties of the global object, but
    # instead load them directly from the stack. Currently, the only
    # such parameter is 'this' which is passed on the stack when
    # invoking scripts
    var = self.variables_.Declare(self, JSObject("string", "this"), Variable.VAR, False, Variable.THIS)
    var.rewrite_ = Slot(var, Slot.PARAMETER, -1)
    self.receiver_ = VariableProxy(JSObject("string", "this"), True, False)
    self.receiver_.BindTo(var)

    if self.is_function_scope():
      # Declare 'arguments' variable which exists in all functions.
      # Note that it might never be accessed, in which case it won't be
      # allocated during variable allocation.
      self.variables_.Declare(self, JSObject("string", "arguments"),
                              Variable.VAR, True, Variable.ARGUMENTS)

  def LocalLookup(self, name):
    return self.variables_.Lookup(name)

  def Lookup(self, name):
    scope = self
    while scope != None:
      var = scope.LocalLookup(name)
      if var != None:
        return var
      scope = scope.outer_scope()
    return None

  def DeclareFunctionVar(self, name):
    assert(self.is_function_scope() and self.function_ == None)
    self.function_ = Variable(self, name, Variable.CONST, True, Variable.NORMAL)
    return self.function_

  def DeclareLocal(self, name, mode):
    assert(mode == Variable.VAR or mode == Variable.CONST)
    return self.variables_.Declare(self, name, mode, True, Variable.NORMAL)

  def DeclareGlobal(self, name):
    assert(self.is_global_scope())
    return self.variables_.Declare(self, name, Variable.DYNAMIC, True,
                                   Variable.NORMAL)

  def AddParameter(self, var):
    assert(self.is_function_scope())
    assert(self.LocalLookup(var.name()) == var)
    self.params_.append(var)

  def NewUnresolved(self, name, inside_with):
    # Note that we must not share the unresolved variables with
    # the same name because they may be removed selectively via
    # RemoveUnresolved().
    proxy = VariableProxy(name, False, inside_with)
    self.unresolved_.append(proxy)
    return proxy

  def AddDeclaration(self, declaration):
    self.decls_.append(declaration)

  def is_eval_scope(self): return self.type_ == self.EVAL_SCOPE
  def is_function_scope(self): return self.type_ == self.FUNCTION_SCOPE
  def is_global_scope(self): return self.type_ == self.GLOBAL_SCOPE

  def inside_with(self): return self.scope_inside_with_
  def contains_with(self): return self.scope_contains_with_

  def outer_scope(self): return self.outer_scope_

  def receiver(self): return self.receiver_

  def function(self):
    assert(self.is_function_scope())
    return self.function()

  def parameter(self, index):
    assert(self.is_function_scope())
    return self.params_[index]

  def num_parameters(self):
    return len(self.params_)

  # The local variable 'arguments' if we need to allocate it; NULL otherwise.
  # If arguments() exist, arguments_shadow() exists, too.
  def arguments(self): return self.arguments_

  # The '.arguments' shadow variable if we need to allocate it; NULL otherwise.
  # If arguments_shadow() exist, arguments() exists, too.
  def arguments_shadow(self): return self.arguments_shadow_

  def declarations(self): return self.decls_

  def MustAllocate(self, var):
    # Give var a read/write use if there is a chance it might be accessed
    # via an eval() call.  This is only possible if the variable has a
    # visible name.
    if (var.is_this() or len(var.name().value) > 0) and           \
       (var.is_accessed_from_inner_scope_ or                      \
        self.scope_calls_eval_ or self.inner_scope_calls_eval_ or \
        self.scope_contains_with_):
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
        var.mode() != Variable.TEMPORARY and \
        (var.is_accessed_from_inner_scope_ or
         self.scope_calls_eval_ or self.inner_scope_calls_eval_ or
         self.scope_contains_with_ or var.is_global())

  def AllocateHeapSlot(self, var):
    var.rewrite_ = Slot(var, Slot.CONTEXT, self.num_heap_slots_)
    self.num_heap_slots_ += 1

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
      self.arguments_ = VariableProxy(JSObject("string", "arguments"), False, False)
      self.arguments_.BindTo(arguments)

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
      self.arguments_shadow_ = VariableProxy(JSObject("string", ".arguments"), False, False)
      self.arguments_shadow_.BindTo(arguments_shadow)
      #temps_.Add(arguments_shadow);

      # Allocate the parameters by rewriting them into '.arguments[i]' accesses.
      for i in range(0, len(self.params_)):
        var = self.params_[i]
        assert(var.scope() == self)
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
      for i in range(0, len(self.params_)):
        var = self.params_[i]
        assert(var.scope() == self)
        if self.MustAllocate(var):
          if self.MustAllocateInContext(var):
            assert(var.rewrite_ == None or \
                   (var.slot() != None and var.slot().type() == Slot.CONTEXT))
            if var.rewrite_ == None:
              # Only set the heap allocation if the parameter has not
              # been allocated yet.
              self.AllocateHeapSlot(var)
          else:
            assert(var.rewrite_ == None or
                   (var.slot() != None and
                    var.slot().type() == Slot.PARAMETER))
            # Set the parameter index always, even if the parameter
            # was seen before! (We need to access the actual parameter
            # supplied for the last occurrence of a multiply declared
            # parameter.)
            var.rewrite_ = Slot(var, Slot.PARAMETER, i)

  def AllocateNonParameterLocal(self, var):
    assert(var.scope() == self)
    assert(var.rewrite_ == None or \
         (not var.IsVariable(JSObject("string", "result"))) or
         (var.slot() == None or var.slot().type() != Slot.LOCAL))
    if var.rewrite_ == None and self.MustAllocate(var):
      if self.MustAllocateInContext(var):
        self.AllocateHeapSlot(var)
      else:
        self.AllocateStackSlot(var)


  def AllocateNonParameterLocals(self):
    # All variables that have no rewrite yet are non-parameter locals.
    for i in range(0, len(self.temps_)):
      self.AllocateNonParameterLocal(self.temps_[i])

    for key in self.variables_:
      var = self.variables_[key]
      self.AllocateNonParameterLocal(var)

    # For now, function_ must be allocated at the very end.  If it gets
    # allocated in the context, it must be the last slot in the context,
    # because of the current ScopeInfo implementation (see
    # ScopeInfo::ScopeInfo(FunctionScope* scope) constructor).
    if self.function_ != None:
      self.AllocateNonParameterLocal(self.function_)

  def AllocateVariablesRecursively(self):
    class Context:
      MIN_CONTEXT_SLOTS = 5

    # The number of slots required for variables.
    self.num_stack_slots_ = 0
    self.num_heap_slots_ = Context.MIN_CONTEXT_SLOTS

    # Allocate variables for inner scopes.
    for i in range(0, len(self.inner_scopes_)):
      self.inner_scopes_[i].AllocateVariablesRecursively()

    # Allocate variables for this scope.
    # Parameters must be allocated first, if any.
    if self.is_function_scope(): self.AllocateParameterLocals()
    self.AllocateNonParameterLocals()

    # Allocate context if necessary.
    must_have_local_context = False
    if self.scope_calls_eval_ or self.scope_contains_with_:
      # The context for the eval() call or 'with' statement in this scope.
      # Unless we are in the global or an eval scope, we need a local
      # context even if we didn't statically allocate any locals in it,
      # and the compiler will access the context variable. If we are
      # not in an inner scope, the scope is provided from the outside.
      must_have_local_context = self.is_function_scope()

    # If we didn't allocate any locals in the local context, then we only
    # need the minimal number of slots if we must have a local context.
    if self.num_heap_slots_ == Context.MIN_CONTEXT_SLOTS and \
        not must_have_local_context:
      self.num_heap_slots_ = 0

    # Allocation done.
    assert(self.num_heap_slots_ == 0 or  \
           self.num_heap_slots_ >= Context.MIN_CONTEXT_SLOTS)

  def AllocateVariables(self, context):
    assert(self.outer_scope_ == None)

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
    if self.is_global_scope(): global_scope = self;
    self.ResolveVariablesRecursively(global_scope, context);

    # 3) Allocate variables.
    self.AllocateVariablesRecursively()

  def PropagateScopeInfo(self, outer_scope_calls_eval, outer_scope_is_eval_scope):
    if outer_scope_calls_eval:
      self.outer_scope_calls_ = True

    if outer_scope_is_eval_scope:
      self.outer_scope_is_eval_scope_ = True

    calls_eval = self.scope_calls_eval_ or self.outer_scope_calls_eval_
    is_eval = self.is_eval_scope() or self.outer_scope_is_eval_scope_
    for i in range(0, len(self.inner_scopes_)):
      inner_scope = self.inner_scopes_[i]
      if inner_scope.PropagateScopeInfo(calls_eval, is_eval):
        self.inner_scope_calls_eval_ = True
      if inner_scope.force_eager_compilation_:
        force_eager_compilation_ = True

    return self.scope_calls_eval_ or self.inner_scope_calls_eval_

  def ResolveVariablesRecursively(self, global_scope, context):
    assert(global_scope == None or global_scope.is_global_scope())

    # Resolve unresolved variables for this scope.
    for i in range(0, len(self.unresolved_)):
      self.ResolveVariable(global_scope, context, self.unresolved_[i])

    # Resolve unresolved variables for inner scopes.
    for i in range(0, len(self.inner_scopes_)):
      self.inner_scopes_[i].ResolveVariablesRecursively(global_scope, context)

  def ResolveVariable(self, global_scope, context, proxy):
    assert(global_scope == None or global_scope.is_global_scope())

    # If the proxy is already resolved there's nothing to do
    # (functions and consts may be resolved by the parser).
    if proxy.var() != None:
      return

    # Otherwise, try to resolve the variable.
    invalidated_local = [None]
    var = self.LookupRecursive(proxy.name(), False, invalidated_local)

    if proxy.inside_with():
      # If we are inside a local 'with' statement, all bets are off
      # and we cannot resolve the proxy to a local variable even if
      # we found an outer matching variable.
      # Note that we must do a lookup anyway, because if we find one,
      # we must mark that variable as potentially accessed from this
      # inner scope (the property may not be in the 'with' object).
      var = self.NonLocal(proxy.name(), Variable.DYNAMIC)
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
            not (self.scope_inside_with_ or self.outer_scope_is_eval_scope_ or
               self.scope_calls_eval_ or self.outer_scope_calls_eval_):
          # We must have a global variable.
          assert(global_scope != None)
          var = global_scope.DeclareGlobal(proxy.name())

        elif self.scope_inside_with_:
          # If we are inside a with statement we give up and look up
          # the variable at runtime.
          var = self.NonLocal(proxy.name(), Variable.DYNAMIC);

        elif invalidated_local[0] != None:
          # No with statements are involved and we found a local
          # variable that might be shadowed by eval introduced
          # variables.
          var = self.NonLocal(proxy.name(), Variable.DYNAMIC_LOCAL)
          var.set_local_if_not_shadowed(invalidated_local)

        elif self.outer_scope_is_eval_scope_:
          # No with statements and we did not find a local and the code
          # is executed with a call to eval.  The context contains
          # scope information that we can use to determine if the
          # variable is global if it is not shadowed by eval-introduced
          # variables.
          if context.GlobalIfNotShadowedByEval(proxy.name()):
            var = self.NonLocal(proxy.name(), Variable.DYNAMIC_GLOBAL)
          else:
            var = self.NonLocal(proxy.name(), Variable.DYNAMIC);
        else:
          # No with statements and we did not find a local and the code
          # is not executed with a call to eval.  We know that this
          # variable is global unless it is shadowed by eval-introduced
          # variables.
          var = self.NonLocal(proxy.name(), Variable.DYNAMIC_GLOBAL)
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
    guess = self.scope_calls_eval_

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
      if self.function_ != None and self.function_.name().value == name.value:
        var = self.function_

      elif self.outer_scope_ != None:
        var = self.outer_scope_.LookupRecursive(name, True, invalidated_local)
        # We may have found a variable in an outer scope. However, if
        # the current scope is inside a 'with', the actual variable may
        # be a property introduced via the 'with' statement. Then, the
        # variable we may have found is just a guess.
        if self.scope_inside_with_:
          guess = True

      # If we did not find a variable, we are done.
      if var == None:
        return None

    assert(var != None)

    # If this is a lookup from an inner scope, mark the variable.
    if inner_lookup:
      var.is_accessed_from_inner_scope_ = True

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

  def __enter__(self): pass

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
    if tok == Token.IN and not accept_IN:
      assert(False)
      return 0
    return Token.Precedence(tok)

  def peek(self):
    return self.scanner.peek()

  def Next(self):
    return self.scanner.Next()

  def ReportError(self, expected):
    print "%s expected but %s comes." % (Token.String(expected),
                                         Token.String(self.scanner.current_.token))
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
    if tok == Token.SEMICOLON:
      self.Next()
      return
    if self.scanner.has_line_terminator_before_next_ or \
          tok == Token.RBRACE or \
          tok == Token.EOS:
      return
    self.Expect(Token.SEMICOLON)

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
    return Literal(JSUNDEFINED)

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
    with LexicalScope(self, scope):
      body = []
      self.ParseSourceElements(body, Token.EOS)
      result[0] = FunctionLiteral(no_name, self.top_scope, body, 0, False)

    top = result[0].scope()
    top.AllocateVariables(None)

    return ExpressionStatement(Call(result[0], []))
    #return result[0]

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
    with LexicalScope(self, scope):
      self.top_scope.SetScopeName(name)

      #  FormalParameterList ::
      #    '(' (Identifier)*[','] ')'
      self.Expect(Token.LPAREN)
      done = (self.peek() == Token.RPAREN)
      while not done:
        param_name = self.ParseIdentifier()
        self.top_scope.AddParameter(self.top_scope.DeclareLocal(param_name, Variable.VAR))
        num_parameters += 1

        done = (self.peek() == Token.RPAREN)
        if not done:
          self.Expect(Token.COMMA)

      self.Expect(Token.RPAREN)

      self.Expect(Token.LBRACE)
      body = []

      # If we have a named function expression, we add a local variable
      # declaration to the body of the function with the name of the
      # function and let it refer to the function itself (closure).
      # NOTE: We create a proxy and resolve it here so that in the
      # future we can change the AST to only refer to VariableProxies
      # instead of Variables and Proxis as is the case now.
      if function_name != None and function_name.value != "":
        fvar = self.top_scope.DeclareFunctionVar(function_name)
        fproxy = self.top_scope.NewUnresolved(function_name, self.inside_with())
        fproxy.BindTo(fvar)
        body.append(ExpressionStatement(Assignment(Token.INIT_VAR,
                                                   fproxy, ThisFunction())))

      self.ParseSourceElements(body, Token.RBRACE)

      self.Expect(Token.RBRACE)

      function_literal[0] = FunctionLiteral(name, self.top_scope, body,
                                            num_parameters, function_name != "")

    return function_literal[0]

  def ParseArguments(self):
    # Arguments ::
    #   '(' (AssignmentExpression)*[','] ')'

    result = []
    self.Expect(Token.LPAREN)
    done = (self.peek() == Token.RPAREN)
    while not done:
      argument = self.ParseAssignmentExpression(True)
      result.append(argument)
      done = (self.peek() == Token.RPAREN);
      if not done:
        self.Expect(Token.COMMA)
    self.Expect(Token.RPAREN)
    return result

  def ParseIdentifier(self):
    if self.peek() == Token.IDENTIFIER:
      self.Expect(Token.IDENTIFIER)
      return JSObject("string", self.scanner.literal_string())
    else:
      return JSObject("string", "")

  def ParseFunctionDeclaration(self):
    self.Expect(Token.FUNCTION)
    name = self.ParseIdentifier()
    fun = self.ParseFunctionLiteral(name, Parser.DECLARATION)
    self.Declare(name, Variable.VAR, fun, True)
    return EmptyStatement()

  def ParseBlock(self, labels):
    # Block ::
    #   '{' Statement* '}'

    # Note that a Block does not introduce a new execution scope!
    # (ECMA-262, 3rd, 12.2)
    #
    # Construct block expecting 16 statements.
    result = Block(labels, 16, False)
    #target = Target(self, result)
    self.Expect(Token.LBRACE)
    while self.peek() != Token.RBRACE:
      stat = self.ParseStatement(None)
      if stat and not stat.IsEmpty(): result.AddStatement(stat)
    self.Expect(Token.RBRACE)
    return result

  def ParseVariableStatement(self):
    # VariableStatement ::
    #   VariableDeclarations ';'

    dummy = [0]
    result = self.ParseVariableDeclarations(True, dummy)
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
    if self.peek() == Token.VAR:
      self.Consume(Token.VAR)
    elif self.peek() == Token.CONST:
      self.Consume(Token.CONST)
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
        self.Consume(Token.COMMA)
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
      position = -1
      if self.peek() == Token.ASSIGN:
        self.Expect(Token.ASSIGN)
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

#      if self.top_scope.is_global_scope():
#        arguments = []
#        # Be careful not to assign a value to the global variable if
#        # we're in a with. The initialization value should not
#        # necessarily be stored in the global object in that case,
#        # which is why we need to generate a separate assignment node.
#        arguments.append(Literal(name))  # we have at least 1 parameter
#        if is_const or (value != None and not self.inside_with()):
#          arguments.append(value)
#          value = None
#        # Construct the call to Runtime::DeclareGlobal{Variable,Const}Locally
#        # and add it to the initialization statement block. Note that
#        # this function does different things depending on if we have
#        # 1 or 2 parameters.
#        assert(False and "not implemented yet.")
#        initialize = None
#        if is_const:
#          initialize = CallRuntime(JSObject("string", "InitializeConstGlobal"),
#                                   Runtime.FunctionForId(Runtime.kInitializeConstGlobal),
#                                   arguments)
#        else:
#          initialize = CallRuntime(JSObject("string", "InitializeVarGlobal"),
#                                   Runtime.FunctionForId(Runtime.kInitializeVarGlobal),
#                                   arguments)
#        block.AddStatement(ExpressionStatement(initialize))

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
        op = Token.INIT_CONST if is_const else Token.INIT_VAR
        assignment = Assignment(op, last_var, value)
        if block:
          block.AddStatement(ExpressionStatement(assignment))

      if self.peek() != Token.COMMA:
        break

    if not is_const and nvars == 1:
      # We have a single, non-const variable.
      if False: #if (is_pre_parsing_) {
        # If we're preparsing then we need to set the var to something
        # in order for for-in loops to parse correctly.
        var[0] = ValidLeftHandSideSentinel.instance()
      else:
        assert(last_var != None)
        var[0] = last_var

    return block

  # Precedence = 1
  def ParseExpression(self, accept_IN):
    # Expression ::
    #   AssignmentExpression
    #   Expression ',' AssignmentExpression

    result = self.ParseAssignmentExpression(accept_IN)
    while self.peek() == Token.COMMA:
      self.Expect(Token.COMMA)
      right = self.ParseAssignmentExpression(accept_IN)
      result = BinaryOperation(Token.COMMA, result, right)

    return result

  # Precedence = 2
  def ParseAssignmentExpression(self, accept_IN):
    # AssignmentExpression ::
    #   ConditionalExpression
    #   LeftHandSideExpression AssignmentOperator AssignmentExpression

    expression = self.ParseConditionalExpression(accept_IN);

    if not Token.IsAssignmentOp(self.peek()):
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
    if self.peek() != Token.CONDITIONAL:
      return expression
    self.Consume(Token.CONDITIONAL)
    # In parsing the first assignment expression in conditional
    # expressions we always accept the 'in' keyword; see ECMA-262,
    # section 11.12, page 58.
    left = self.ParseAssignmentExpression(True)
    self.Expect(Token.COLON)
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
        if x and isinstance(x, Literal) and x.handle().IsNumber() and \
           y and isinstance(y, Literal) and y.handle().IsNumber():
           x_val = x.AsLiteral().handle().Number()
           y_val = y.AsLiteral().handle().Number()

           if op == Token.ADD:
             x = self.NewNumberLiteral(x_val + y_val)
             continue
           elif op == Token.SUB:
             x = self.NewNumberLiteral(x_val - y_val)
             continue
           elif op == Token.MUL:
             x = self.NewNumberLiteral(x_val * y_val)
             continue
           elif op == Token.DIV:
             x = self.NewNumberLiteral(x_val / y_val)
             continue
           elif op == Token.BIT_OR:
             x = self.NewNumberLiteral(DoubleToInt32(x_val) | DoubleToInt32(y_val))
             continue
           elif op == Token.BIT_AND:
             x = self.NewNumberLiteral(DoubleToInt32(x_val) & DoubleToInt32(y_val))
             continue
           elif op == Token.BIT_XOR:
             x = self.NewNumberLiteral(DoubleToInt32(x_val) ^ DoubleToInt32(y_val))
             continue
           elif op == Token.SHL:
             value = DoubleToInt32(x_val) << (DoubleToInt32(y_val) & 0x1f)
             x = self.NewNumberLiteral(value)
             continue
           elif op == Token.SHR:
             shift = DoubleToInt32(y_val) & 0x1f
             value = DoubleToUint32(x_val) >> shift
             x = self.NewNumberLiteral(value)
             continue
           elif op == Token.SAR:
             shift = DoubleToInt32(y_val) & 0x1f
             value = ArithmeticShiftRight(DoubleToInt32(x_val), shift)
             x = self.NewNumberLiteral(value)
             continue

        # For now we distinguish between comparisons and other binary
        # operations.  (We could combine the two and get rid of this
        # code an AST node eventually.)
        if Token.IsCompareOp(op):
          # We have a comparison.
          cmp = op
          if op == Token.NE:
            cmp = Token.EQ
          elif op == Token.NE_STRICT:
            cmp = Token.EQ_STRICT
          x = CompareOperation(cmp, x, y)
          if cmp != op:
            # The comparison was negated - add a NOT.
            x = UnaryOperation(Token.NOT, x)
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
    if Token.IsUnaryOp(op):
      op = self.Next()
      expression = self.ParseUnaryExpression()

      # Compute some expressions involving only number literals.
      if expression != None and expression.AsLiteral() and \
         expression.AsLiteral().handle().IsNumber():
        value = expression.AsLiteral().handle().Number()
        if op == Token.ADD:
          return expression
        elif op == Token.SUB:
          return self.NewNumberLiteral(-value)
        elif op == Token.BIT_NOT:
          return self.NewNumberLiteral(~DoubleToInt32(value))

      return UnaryOperation(op, expression)

    elif Token.IsCountOp(op):
      op = self.Next()
      expression = self.ParseUnaryExpression()
      # Signal a reference error if the expression is an invalid
      # left-hand side expression.  We could report this as a syntax
      # error here but for compatibility with JSC we choose to report the
      # error at runtime.
      if expression == None or not expression.IsValidLeftHandSide():
        assert(False)
      return CountOperation(True, op, expression)
    else:
      return self.ParsePostfixExpression()

  def ParsePostfixExpression(self):
    # PostfixExpression ::
    #   LeftHandSideExpression ('++' | '--')?

    expression = self.ParseLeftHandSideExpression()
    if not self.scanner.has_line_terminator_before_next() and \
          Token.IsCountOp(self.peek()):
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
    if self.peek() == Token.NEW:
      result = self.ParseNewExpression()
    else:
      result = self.ParseMemberExpression()

    while True:
      peek = self.peek()
      if peek == Token.LBRACK:
        self.Consume(Token.LBRACK)
        index = self.ParseExpression(True)
        result = self.NewProperty(result, index)
        self.Expect(Token.RBRACK)
      elif peek == Token.LPAREN:
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

      elif peek == Token.PERIOD:
        self.Consume(Token.PERIOD)
        name = self.ParseIdentifier()
        result = self.NewProperty(result, Literal(JSObject("string", name)))

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
    if self.peek() == Token.FUNCTION:
      self.Expect(Token.FUNCTION)
      name = JSObject("string", "")
      if self.peek() == Token.IDENTIFIER:
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
    if peek == Token.THIS:
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

    elif peek == Token.NULL_LITERAL:
      self.Consume(Token.NULL_LITERAL)
      result = Literal(JSNULL)

    elif peek == Token.TRUE_LITERAL:
      self.Consume(Token.TRUE_LITERAL)
      result = Literal(JSTRUE)

    elif peek == Token.FALSE_LITERAL:
      self.Consume(Token.FALSE_LITERAL)
      result = Literal(JSFALSE)

    elif peek == Token.IDENTIFIER:
      name = self.ParseIdentifier()
      result = self.top_scope.NewUnresolved(name, self.inside_with())

    elif peek == Token.NUMBER:
      self.Consume(Token.NUMBER)
      #value = StringToDouble(self.scanner.literal_string(), ALLOW_HEX | ALLOW_OCTALS)
      value = int(self.scanner.literal_string())
      result = self.NewNumberLiteral(value)

    elif peek == Token.STRING:
      self.Consume(Token.STRING)
      symbol = self.scanner.literal_string()
      result = Literal(JSObject("string", symbol))

    elif peek == Token.ASSIGN_DIV:
      result = self.ParseRegExpLiteral(True)

    elif peek == Token.DIV:
      result = self.ParseRegExpLiteral(False)

    elif peek == Token.LBRACK:
      result = self.ParseArrayLiteral()

    elif peek == Token.LBRACE:
      result = self.ParseObjectLiteral()

    elif peek == Token.LPAREN:
      self.Consume(Token.LPAREN)
      result = self.ParseExpression(True)
      self.Expect(Token.RPAREN)

    elif peek == Token.MOD:
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
    self.Expect(Token.RETURN)

    tok = self.peek()
    if tok == Token.SEMICOLON or tok == Token.RBRACE or tok == Token.EOS:
      return ReturnStatement(self.GetLiteralUndefined())

    expr = self.ParseExpression(True)
    self.ExpectSemicolon()
    return ReturnStatement(expr)

  def ParseWhileStatement(self, labels):
    # WhileStatement ::
    #   'while' '(' Expression ')' Statement

    loop = WhileStatement(labels)
    #target(this, loop)

    self.Expect(Token.WHILE)
    self.Expect(Token.LPAREN)
    cond = self.ParseExpression(True)
    self.Expect(Token.RPAREN)
    body = self.ParseStatement(None)

    if loop != None: loop.Initialize(cond, body)
    return loop

  def ParseForStatement(self, labels):
    # ForStatement ::
    #   'for' '(' Expression? ';' Expression? ';' Expression? ')' Statement

    init = None

    self.Expect(Token.FOR)
    self.Expect(Token.LPAREN)
    if self.peek() != Token.SEMICOLON:
      if self.peek() == Token.VAR or self.peek() == Token.CONST:
        each = [None]
        variable_statement = self.ParseVariableDeclarations(False, each)
        if self.peek() == Token.IN and each[0] != None:
          loop = ForInStatement(labels)
          target = Target(self, loop)

          self.Expect(Token.IN)
          enumerable = self.ParseExpression(True)
          self.Expect(Token.RPAREN)

          body = self.ParseStatement(None)
          #if (is_pre_parsing_) {
          if False:
            return None
          else:
            loop.Initialize(each, enumerable, body)
            result = Block(None, 2, False)
            result.AddStatement(variable_statement)
            result.AddStatement(loop)
            # Parsed for-in loop w/ variable/const declaration.
            return result

        else:
          init = variable_statement

      else:
        expression = self.ParseExpression(False)
        if self.peek() == Token.IN:
          raise Exception('not implemented')
          # Signal a reference error if the expression is an invalid
          # left-hand side expression.  We could report this as a syntax
          # error here but for compatibility with JSC we choose to report
          # the error at runtime.
          if expression == None or not expression.IsValidLeftHandSide():
            assert(False)
            #Handle<String> type = Factory::invalid_lhs_in_for_in_symbol();
            #expression = NewThrowReferenceError(type);

          loop = ForInStatement(labels)
          target(self, loop)

          self.Expect(Token.IN)
          enumerable = self.ParseExpression(True)
          self.Expect(Token.RPAREN)

          body = self.ParseStatement(None)
          if loop: loop.Initialize(expression, enumerable, body)

          # Parsed for-in loop.
          return loop

        else:
          init = ExpressionStatement(expression)

    # Standard 'for' loop
    loop = ForStatement(labels)
    #target = (self, loop)

    # Parsed initializer at this point.
    self.Expect(Token.SEMICOLON)

    cond = None
    if self.peek() != Token.SEMICOLON:
      cond = self.ParseExpression(True)
    self.Expect(Token.SEMICOLON)

    next = None
    if self.peek() != Token.RPAREN:
      exp = self.ParseExpression(True)
      next = ExpressionStatement(exp)
    self.Expect(Token.RPAREN)

    body = self.ParseStatement(None)

    if loop: loop.Initialize(init, cond, next, body)
    return loop

  def ParseExpressionOrLabelledStatement(self, labels):
    # ExpressionStatement | LabelledStatement ::
    #   Expression ';'
    #   Identifier ':' Statement

    expr = self.ParseExpression(True)
    if self.peek() == Token.COLON and expr and \
        expr.AsVariableProxy() != None and \
        not expr.AsVariableProxy().is_this():
      assert(False)  # NOTE(keisuke): short cut

    # Parsed expression statement.
    self.ExpectSemicolon()
    return ExpressionStatement(expr)

  def ParseIfStatement(self, labels):
    # IfStatement ::
    #   'if' '(' Expression ')' Statement ('else' Statement)?

    self.Expect(Token.IF)
    self.Expect(Token.LPAREN)
    condition = self.ParseExpression(True)
    self.Expect(Token.RPAREN)
    then_statement = self.ParseStatement(labels)
    else_statement = None
    if self.peek() == Token.ELSE:
      self.Next()
      else_statement = self.ParseStatement(labels)
    elif True: #!is_pre_parsing_) {
      else_statement = EmptyStatement()
    return IfStatement(condition, then_statement, else_statement)

  def ParseStatement(self, labels):
    stmt = None
    peek = self.peek()
    if peek == Token.LBRACE:
      return self.ParseBlock(labels)

    elif peek == Token.CONST or peek == Token.VAR:
      stmt = self.ParseVariableStatement()

    elif peek == Token.SEMICOLON:
      self.Next()
      return EmptyStatement()

    elif peek == Token.IF:
      stmt = self.ParseIfStatement(labels)

    elif peek == Token.DO:
      stmt = self.ParseDoWhileStatement(labels)

    elif peek == Token.WHILE:
      stmt = self.ParseWhileStatement(labels)

    elif peek == Token.FOR:
      stmt = self.ParseForStatement(labels)

    elif peek == Token.CONTINUE:
      stmt = self.ParseContinueStatement();

    elif peek == Token.BREAK:
      stmt = self.ParseBreakStatement(labels);

    elif peek == Token.RETURN:
      stmt = self.ParseReturnStatement();

    elif peek == Token.WITH:
      stmt = self.ParseWithStatement(labels);

    elif peek == Token.SWITCH:
      stmt = self.ParseSwitchStatement(labels);

    elif peek == Token.THROW:
      stmt = self.ParseThrowStatement();

    elif peek == Token.TRY:
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

    elif peek == Token.FUNCTION:
      return self.ParseFunctionDeclaration()

    elif peek == Token.NATIVE:
      return self.ParseNativeDeclaration()

    elif peek == Token.DEBUGGER:
      stmt = self.self.ParseDebuggerStatement()

    else:
      stmt = self.ParseExpressionOrLabelledStatement(labels)

    return stmt

class AstVisitor:
  def __init__(self):
    self.current_scope_ = None

  def Visit(self, node):
    return node.Accept(self)

class PrettyPrinter(AstVisitor):
  class EnterNormalFunctionLiteral:
    def __init__(self, visitor):
      self.visitor = visitor
      self.prev_state = visitor.in_original_function_literal
      visitor.in_original_function_literal = True
    def __enter__(self): pass
    def __exit__(self, *e):
      self.visitor.in_original_function_literal = self.prev_state

  def __init__(self, print_types = False, print_address = False):
    AstVisitor.__init__(self)
    self.print_types = print_types
    self.print_address = print_address
    # whether printing function literal in original AST or not.
    # if we are printing original one, say, generic version, no type informations
    # are seeded so no need to print the informations.
    self.in_original_function_literal = False
    self.nest = 0
    self.buffer = ""

  def W(self, S):
    self.buffer += S

  # remove characters
  def R(self, I):
    self.buffer = self.buffer[0:len(self.buffer) - I]

  def NewlineAndIndent(self):
    self.W("\n")
    self.W(" " * (self.nest * 4))

  def PrintTypes(self, node):
    if not self.print_types: return
    if self.in_original_function_literal: return
    self.W('/*')
    for i in range(0, len(node.__type__.types)):
      if i > 0:
        self.W(',')
      self.W(Type.ToString(node.__type__.types[i]))
    self.W('*/')

  def PrintAddress(self, node):
    if not self.print_address: return
    self.W('/*id:')
    self.W(str(id(node)))
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
    elif value == JSUNDEFINED:
      self.W("undefined")
    elif value.IsNumber():
      self.W(str(value.value))
    elif value.IsString():
      self.W(value.value)
    else:
      self.W("<unknown literal>")

  def VisitLiteral(self, node):
    self.PrintLiteral(node.handle(), node.handle().IsString())
    self.PrintTypes(node)

  def PrintParameters(self, scope):
    self.W("(")
    for i in range(0, scope.num_parameters()):
      if i > 0:
        self.W(", ")
      self.PrintLiteral(scope.parameter(i).name(), False)
      self.PrintTypes(scope.parameter(i))
      self.PrintAddress(scope.parameter(i))
    self.W(")")

  def PrintDeclarations(self, declarations):
    for i in range(0, len(declarations)):
# if i > 0:
# self.W(" ")
      self.Visit(declarations[i])

  def PrintStatements(self, statements):
    changed = True
    for i in range(0, len(statements)):
      if changed:
        self.NewlineAndIndent()
      old_len = len(self.buffer)
      self.Visit(statements[i])
      changed = old_len < len(self.buffer)
#      if changed:
#        self.W(";")

  def PrintFunctionLiteral(self, function):
    self.W("function ")
    self.PrintLiteral(function.name(), False)
    self.PrintParameters(function.scope())
    self.W(" {")
    self.nest += 1
    self.PrintDeclarations(function.scope().declarations())
    self.PrintStatements(function.body())
    self.nest -= 1
    self.NewlineAndIndent()
    self.W("}")

  def VisitFunctionLiteral(self, node):
    self.W("(")
    self.PrintFunctionLiteral(node)
    self.W(")")
    self.PrintTypes(node)

  def VisitExpressionStatement(self, node):
    self.Visit(node.expression())
    self.W(";")

  def VisitEmptyStatement(self, node):
    self.W(';')

  def VisitCall(self, node):
    if self.in_original_function_literal:
      is_monomorphic_call = False
      arg_types = ()
    else:
      # True if all types of arguments are monomorphic
      is_monomorphic_call = True
      for arg in node.arguments():
        type_node = Seeder.GetTypeNode(arg)
        if len(type_node.types) == 0:
          assert(False)
        elif len(type_node.types) > 1:
          is_monomorphic_call = False

      arg_types = tuple([Seeder.GetTypeNode(a).types[0] for a in node.arguments()])

    expr = node.expression()
    if isinstance(expr, FunctionLiteral):
      if is_monomorphic_call:
        template = expr.__repos__.CreateTemplate(arg_types) # only lookup
        fun = template.fun()
        self.W("(")
        self.PrintFunctionLiteral(fun)
        self.W(")")
        self.PrintTypes(fun)
        self.PrintAddress(fun)
      else:
        with PrettyPrinter.EnterNormalFunctionLiteral(self):
          self.Visit(expr)
    elif isinstance(expr, VariableProxy):
      assert(expr.var() != None)
      if self.in_original_function_literal:
        self.W(expr.var().name().value)
        self.PrintAddress(expr.var())
      else:
        callee = expr.var().fun()
        if is_monomorphic_call:
          # call specialized version
          template = callee.__repos__.CreateTemplate(arg_types)
          fun = template.fun()
          self.W(fun.name().value)
          self.PrintTypes(fun)
        else:
          # call general version
          self.W(callee.name().value)
          self.PrintTypes(callee)
    else:
      assert(False)

    #print(node.expression().__repos__, node.expression().__repos__.CreateTemplate(()).fun())

    #self.Visit(node.expression())
    self.W("(")
    self.PrintArguments(node.arguments())
    self.W(")")
    self.PrintTypes(node)

  def VisitVariableProxy(self, node):
    self.Visit(node.var())
# self.PrintLiteral(node.name, False)
# if self.print_types:
# self.PrintTypes(node)

  def PrintArguments(self, arguments):
    for i in range(0, len(arguments)):
      if i > 0:
        self.W(",")
      self.Visit(arguments[i])

  def VisitDeclaration(self, node):
    if node.fun() != None and not self.in_original_function_literal:
      repos = node.fun().__repos__
      for key in repos.repos_:
        fun = repos.repos_[key].fun()
        self.NewlineAndIndent()
        self.W("var ")
        self.PrintLiteral(fun.name(), False)
        self.PrintTypes(fun)
        self.PrintAddress(fun)
        self.W(" = ")
        self.PrintFunctionLiteral(fun)
        self.W(";")

    self.NewlineAndIndent()
    self.W("var ")
    self.PrintLiteral(node.proxy().name(), False)
    assert(node.proxy().var() != None)
    self.PrintTypes(node.proxy().var())
    self.PrintAddress(node.proxy().var())
    if node.fun() != None:
      self.W(" = ")
      with PrettyPrinter.EnterNormalFunctionLiteral(self):
        self.PrintFunctionLiteral(node.fun())
    self.W(";")

  def VisitWhileStatement(self, node):
    #PrintLabels(node->labels());
    self.NewlineAndIndent()
    self.W('while (')
    self.Visit(node.cond())
    self.W(") ")
    self.Visit(node.body())

  def VisitForStatement(self, node):
    #PrintLabels(node->labels());
    self.NewlineAndIndent()
    self.W('for (')
    if node.init() != None:
      self.Visit(node.init())
      self.W(' ')
    else:
      self.W('; ')
    if node.cond() != None: self.Visit(node.cond())
    self.W('; ')
    if node.next() != None:
      self.Visit(node.next())  # prints extra ';', unfortunately
      self.R(1) # remove extra ';'
    self.W(') ')
    self.Visit(node.body())

  def VisitReturnStatement(self, node):
    self.W("return ")
    self.Visit(node.expression())
    self.W(";")

  def VisitConditional(self, node):
    self.W("(")
    self.Visit(node.condition())
    self.W(" ? ")
    self.Visit(node.then_expression())
    self.W(" : ")
    self.Visit(node.else_expression())
    self.W(")")
    self.PrintTypes(node)

  def VisitCompareOperation(self, node):
    self.W("(")
    self.Visit(node.left())
    self.W(Token.String(node.op()))
    self.Visit(node.right())
    self.W(")")
    self.PrintTypes(node)

  def VisitIfStatement(self, node):
    self.W("if (")
    self.Visit(node.condition())
    self.W(") ")
    self.Visit(node.then_statement())
    if node.HasElseStatement():
      self.W(" else ")
      self.Visit(node.else_statement())

  def VisitUnaryOperation(self, node):
    self.W('(' + Token.String(node.op()))
    self.Visit(node.expression())
    self.W(')')

  def VisitCountOperation(self, node):
    self.W("(")
    if node.is_prefix(): self.W(Token.String(node.op()))
    self.Visit(node.expression())
    if node.is_postfix(): self.W(Token.String(node.op()))
    self.W(")")
    self.PrintTypes(node)

  def MaybePrintSpecializedOperation(self, ltype, rtype, op):
    if len(ltype) == 1 and ltype[0] == Type.SMI and \
       len(rtype) == 1 and rtype[0] == Type.SMI and \
       op == Token.ADD:
      self.W('.+')
    else:
      self.W(Token.String(op))

  def VisitBinaryOperation(self, node):
    self.W("(")
    self.Visit(node.left())

    if not self.in_original_function_literal:
      self.MaybePrintSpecializedOperation(Seeder.GetTypeNode(node.left()).types,
                                          Seeder.GetTypeNode(node.right()).types,
                                          node.op())
    else:
      self.W(Token.String(node.op()))

    self.Visit(node.right())
    self.W(")")
    self.PrintTypes(node)

  def VisitBlock(self, node):
    # this method may print no characters, so lonely ";" may appear.
    if not node.is_initializer_block():
      self.W("{ ")
      self.nest += 1
    if len(node.statements()) > 0:
      self.PrintStatements(node.statements())
    if not node.is_initializer_block():
      self.nest -= 1
      self.NewlineAndIndent()
      self.W("}")

  def VisitVariable(self, node):
    self.W(node.name().value)
    self.PrintTypes(node)
    self.PrintAddress(node)
    #print(node, ' ', node.name().value)

  def VisitAssignment(self, node):
    self.Visit(node.target().var())
    self.W(" " + Token.String(node.op()) + " ")
    self.Visit(node.value())

  def PrintLn(self, node):
    try:
      self.Visit(node)
      self.W("\n")
      sys.stdout.write(self.buffer)
    except:
      print self.buffer
      raise

  def Init(self): pass
  def OutPut(self): pass


class IndentedScope:
  def __init__(self, *args):
    if len(args) == 0:
      IndentedScope.ast_printer_.inc_indent()
    else:
      txt = args[0]
      type = None if len(args) == 1 else args[1]
      IndentedScope.ast_printer_.PrintIndented(txt)
      print(type)
      if type != None and type.IsKnown():
        IndentedScope.ast_printer_.Print(' (type = ')
        IndentedScope.ast_printer_.Print(SmiAnalysis.Type2String(type))
        IndentedScope.ast_printer_.Print(')')
      IndentedScope.ast_printer_.Print('\n')
      IndentedScope.ast_printer_.inc_indent()

  def __enter__(self): pass

  def __exit__(self, *args):
    IndentedScope.ast_printer_.dec_indent()

  @staticmethod
  def SetAstPrinter(a): IndentedScope.ast_printer_ = a

  ast_printer_ = None

class AstPrinter(PrettyPrinter):
  @staticmethod
  def Print(txt): sys.stdout.write(txt)

  indent_ = 0

  def inc_indent(self): AstPrinter.indent_ += 1
  def dec_indent(self): AstPrinter.indent_ -= 1

  def __init__(self):
    assert(self.indent_ == 0)
    PrettyPrinter.__init__(self)
    IndentedScope.SetAstPrinter(self)

  def __del__(self):
    assert(self.indent_ == 0)
    IndentedScope.SetAstPrinter(None)

  def PrintIndented(self, txt):
    self.Print('. ' * self.indent_)
    self.Print(txt)

  def PrintLiteralIndented(self, info, value, quote):
    self.PrintIndented(info)
    self.Print(' ')
    self.PrintLiteral(value, quote)
    self.Print('\n')

  def PrintLiteralWithModeInented(self, info, var, value, type):
    if var == None:
      self.PrintLiteralIndented(info, value, True)
    else:
      if type.IsKnown():
        buf = info + ' ' + '(mode = ' + Variable.Mode2String(var.mode()) + \
            ', type = ' + SmiAnalysis.Type2String(type) + ')'
      else:
        buf = info + ' (mode = ' + Variable.Mode2String(var.mode()) + ')'
      self.PrintLiteralIndented(buf, value, True)

  def PrintIndentedVisit(self, s, node):
    with IndentedScope(s):
      self.Visit(node)

  def PrintProgram(self, program):
    self.Init()
    with IndentedScope('FUNC'):
      self.PrintLiteralIndented('NAME', program.name(), True)
      self.PrintLiteralIndented('INFERRED NAME', program.inferred_name(), True)
      self.PrintParameters(program.scope())
      self.PrintDeclarations(program.scope().declarations())
      self.PrintStatements(program.body())
    #return self.Output()

  def PrintDeclarations(self, declarations):
    if len(declarations) > 0:
      with IndentedScope('DECLS'):
        for decl in declarations:
          self.Visit(decl)

  def PrintParameters(self, scope):
    if scope.num_parameters() > 0:
      with IndentedScope('PARAMS'):
        for i in range(0, scope.num_parameters()):
          self.PrintLiteralWithModeInented('VAR', scope.parameter(i),
                                           scope.parameter(i).name(),
                                           scope.parameter(i).type())

  def PrintStatements(self, statements):
    for stmt in statements:
      self.Visit(stmt)

  def PrintArguments(self, arguments):
    for arg in arguments:
      self.Visit(arg)

  def VisitBlock(self, node):
    block_txt = 'BLOCK INIT' if node.is_initializer_block() else 'BLOCK'
    with IndentedScope(block_txt):
      self.PrintStatements(node.statements())

  def VisitDeclaration(self, node):
    if node.fun() == None:
      # var or const declarations
      self.PrintLiteralWithModeInented(Variable.Mode2String(node.mode()),
                                       node.proxy().AsVariable(),
                                       node.proxy().name(),
                                       node.proxy().AsVariable().type())
    else:
      # function declarations
      self.PrintIndented('FUNCTION ')
      self.PrintLiteral(node.proxy().name(), True)
      self.Print(' = function ')
      self.PrintLiteral(node.fun().name(), False)
      self.Print('\n')

  def VisitExpressionStatement(self, node):
    self.Visit(node.expression())

  def VisitEmptyStatement(self, node):
    self.PrintIndented('EMPTY\n')

  def VisitIfStatement(self, node):
    self.PrintIndentedVisit('IF', node.condition())
    self.PrintIndentedVisit('THEN', node.then_statement())
    if node.HasElseStatement():
      self.PrintIndentedVisit('ELSE', node.else_statement())

  def VisitReturnStatement(self, node):
    self.PrintIndentedVisit('RETURN', node.expression())

  def VisitFunctionLiteral(self, node):
    with IndentedScope('FUNC LITERAL'):
      self.PrintLiteralIndented('NAME', node.name(), False)
      self.PrintLiteralIndented('INFERRED NAME', node.inferred_name(), False)
      self.PrintParameters(node.scope())
      # We don't want to see the function literal in this case: it
      # will be printed via PrintProgram when the code for it is
      # generated.
      self.PrintStatements(node.body())

  def VisitConditional(self, node):
    with IndentedScope('CONDITIONAL'):
      self.PrintIndentedVisit('?', node.condition())
      self.PrintIndentedVisit('THEN', node.then_expression())
      self.PrintIndentedVisit('ELSE', node.else_expression())

  def VisitLiteral(self, node):
    self.PrintLiteralIndented('LITERAL', node.handle(), True)

  def VisitSlot(self, node):
    self.PrintIndented('SLOT ')
    if node.type() == Slot.PARAMETER:
      self.Print('parameter[' + str(node.index()) + ']')
    elif node.type() == Slot.LOCAL:
      self.Print('frame[' + str(node.index()) + ']')
    elif node.type() == Slot.CONTEXT:
      self.Print('.context[' + str(node.index()) + ']')
    elif node.type() == Slot.LOOKUP:
      self.Print('.context[')
      self.PrintLiteral(node.var().name(), False)
      self.Print(']')
    else:
      assert(False)
    self.Print('\n')

  def VisitVariableProxy(self, node):
    self.PrintLiteralWithModeInented('VAR PROXY', node.AsVariable(), node.name(),
                                     node.type())
    var = node.var()
    if var != None and var.rewriter() != None:
      with IndentedScope():
        self.Visit(var.rewite())

  def VisitAssignment(self, node):
    with IndentedScope(Token.Name(node.op()), node.type()):
      self.Visit(node.target())
      self.Visit(node.value())

  def VisitCall(self, node):
    with IndentedScope('CALL'):
      self.Visit(node.expression())
      self.PrintArguments(node.arguments())

  def VisitCallRuntime(self, node):
    self.PrintLiteralIndented('CALL RUNTIME ', node.name(), False)
    # NOTE&TODO(keisuke): Below "IndentedScope('')" should call
    # IndentedScope::IndentedScope()
    # instead of
    # explicit IndentedScope::IndentedScope(const char*, SmiAnalysis*)
    with IndentedScope(''):
      self.PrintArguments(node.arguments())

  def VisitUnaryOperation(self, node):
    self.PrintIndentedVisit(Token.Name(node.op()), node.expression())

  def VisitCountOperation(self, node):
    if node.type().IsKnown():
      buf = ('PRE' if node.is_prefix() else 'POST') + ' ' + \
          Token.Name(node.op()) + ' (type = ' + \
          SmiAnalysis.Type2String(node.type()) + ')'
    else:
      buf = ('PRE' if node.is_prefix() else 'POST') + ' ' + Token.Name(node.op())
    self.PrintIndentedVisit(buf, node.expression())

  def VisitBinaryOperation(self, node):
    with IndentedScope(Token.Name(node.op()), node.type()):
      self.Visit(node.left())
      self.Visit(node.right())

  def VisitCompareOperation(self, node):
    with IndentedScope(Token.Name(node.op()), node.type()):
      self.Visit(node.left())
      self.Visit(node.right())

class AstCopier(AstVisitor):
  def __init__(self):
    AstVisitor.__init__(self)
    self.repos_ = dict()

  def CopyLiteral(self, value):
    if value in JSBUILTINS:
      return value
    else:
      return JSObject(value.kind, value.value)

  def VisitLiteral(self, node):
    return Literal(self.CopyLiteral(node.handle()))

  def CopyScope(self, node):
    if not node in self.repos_:
      s = Scope(node.outer_scope(), node.type_)
      self.repos_[node] = s
      for i in range(0, node.num_parameters()):
        s.params_.append(self.CopyVariable(node.parameter(i)))
      for decl in node.declarations():
        s.decls_.append(self.Visit(decl))

    return self.repos_[node]

  def VisitFunctionLiteral(self, node):
    if not node in self.repos_:
      LOG('>' * 50)
      LOG('AstCopier.VisitFunctionLiteral(enter)',node,node.name().value)
      flit = FunctionLiteral(self.CopyLiteral(node.name()),
                             self.CopyScope(node.scope()),
                             [self.Visit(stmt) for stmt in node.body()],
                             node.num_parameters(),
                             node.is_expression())
      flit.loop_nesting_ = node.loop_nesting()
      flit.inferred_name_ = self.CopyLiteral(node.inferred_name())
      flit.try_fast_codegen_ = node.try_fast_codegen_
      self.repos_[node] = flit
      LOG('AstCopier.VisitFunctionLiteral(exit)',node,flit,flit.name().value)
      LOG('<' * 50)

    return self.repos_[node]

  def VisitExpressionStatement(self, node):
    return ExpressionStatement(self.Visit(node.expression()))

  def VisitEmptyStatement(self, node):
    return EmptyStatement()

  def VisitCall(self, node):
#    return Call(self.Visit(node.expression()),
#                [self.Visit(arg) for arg in node.arguments()])
    expr = node.expression()
    if isinstance(expr, VariableProxy):
      var = expr.var()
      if var in self.repos_:
        return Call(self.Visit(node.expression()),
                    [self.Visit(arg) for arg in node.arguments()])
      else:
        LOG("AstCopier.VisitCall",var.name().value,"was not found in this template")
    LOG("AstCopier.VisitCall",node.expression())
    return Call(node.expression(),
                [self.Visit(arg) for arg in node.arguments()])

  # variable copy should not occur through calling VisitVariableProxy but only
  # through CopyVariableProxy. This method just resolves variable reference.
  def VisitVariableProxy(self, node):
    ret = VariableProxy(self.CopyLiteral(node.name()),
                        node.is_this(),
                        node.inside_with())
    assert(node.var())
    ret.var_ = self.LookupVariable(node.var())
    return ret

  def CopyVariableProxy(self, node):
    ret = VariableProxy(self.CopyLiteral(node.name()),
                        node.is_this(),
                        node.inside_with())
    assert(node.var())
    ret.var_ = self.CopyVariable(node.var())
    return ret

  def VisitDeclaration(self, node):
    return Declaration(self.CopyVariableProxy(node.proxy()),
                       node.mode(),
                       None if node.fun() == None else self.Visit(node.fun()))

  def VisitWhileStatement(self, node):
    def MaybeVisit(N): return self.Visit(N) if N else None
    ret = WhileStatement(None)
    ret.Initialize(MaybeVisit(node.cond()),
                   MaybeVisit(node.body()))
    return ret

  def VisitForStatement(self, node):
    def MaybeVisit(N): return self.Visit(N) if N else None
    ret = ForStatement(None)
    ret.Initialize(MaybeVisit(node.init()),
                   MaybeVisit(node.cond()),
                   MaybeVisit(node.next()),
                   MaybeVisit(node.body()))
    return ret

  def VisitReturnStatement(self, node):
    return ReturnStatement(self.Visit(node.expression()))

  def VisitConditional(self, node):
    return Conditional(self.Visit(node.condition()),
                       self.Visit(node.then_expression()),
                       self.Visit(node.else_expression()))

  def VisitCompareOperation(self, node):
    return CompareOperation(node.op(),
                            self.Visit(node.left()),
                            self.Visit(node.right()))

  def VisitIfStatement(self, node):
    return IfStatement(self.Visit(node.condition()),
                       self.Visit(node.then_statement()),
                       self.Visit(node.else_statement())
                       if node.HasElseStatement() else None)

  def VisitUnaryOperation(self, node):
    return UnaryOperation(node.op(),
                          self.Visit(node.expression()));

  def VisitCountOperation(self, node):
    return CountOperation(node.is_prefix(),
                          node.op(),
                          self.Visit(node.expression()))

  def VisitBinaryOperation(self, node):
    return BinaryOperation(node.op(),
                           self.Visit(node.left()),
                           self.Visit(node.right()))

  def VisitBlock(self, node):
    ret = Block(node.labels(), 1, node.is_initializer_block())
    ret.statements_ = [self.Visit(stmt) for stmt in node.statements()]
    return ret

  def LookupVariable(self, node):
    if node in self.repos_:
      return self.repos_[node]
    else:
      return node

  def CopyVariable(self, node):
#    self.scope_ = scope
#    self.name_ = name
#    self.mode_ = mode
#    self.is_valid_LHS_ = is_valid_lhs
#    self.kind_ = kind
#    self.local_if_not_shadowed_ = None
#    self.is_accessed_from_inner_scope_ = False
#    self.rewrite_ = None
#    self.var_uses_ = Variable.UseCount()
#    self.fun_ = None  # added by keisuke

    assert(not node in self.repos_)
    if not node in self.repos_:
      v = Variable(self.CopyScope(node.scope()),
                   self.CopyLiteral(node.name()),
                   node.mode(),
                   node.is_valid_LHS_,
                   node.kind_)
      v.local_if_not_shadowed_ = node.local_if_not_shadowed_
      v.is_accessed_from_inner_scope_ = node.is_accessed_from_inner_scope_
      v.rewrite_ = node.rewrite_ #?
      v.var_uses_ = copy.copy(node.var_uses())
      v.fun_ = None if node.fun() == None else self.Visit(node.fun())
      self.repos_[node] = v
    return self.repos_[node]

  def VisitAssignment(self, node):
    ret = Assignment(node.op(),
                     self.Visit(node.target()),
                     self.Visit(node.value()))
    ret.block_start_ = node.block_start_
    ret.block_end_ = node.block_end_
    return ret

    #self.Visit(node.target().var())
    #self.W(" " + String(node.op) + " ")
    #self.Visit(node.value())

  def Copy(self, node):
    return self.Visit(node)

class Type:
  SMI = 0
  NUM = 1
  BOOL = 2
  STR = 3
  FUN = 4
  NULL = 5
  UNDEFINED = 6
  NaN = 7
  UNKNOWN = 10

  @staticmethod
  def ToString(type):
    for T in ((Type.SMI, "SMI"),
              (Type.NUM, "NUM"),
              (Type.BOOL, "BOOL"),
              (Type.STR, "STR"),
              (Type.FUN, "FUN"),
              (Type.NULL, "NULL"),
              (Type.UNDEFINED, "UNDEF"),
              (Type.NaN, "NaN"),
              (Type.UNKNOWN, "UNKNOWN")):
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

class BinOpTypeNode(TypeNode):
  def __init__(self, left, right, op):
    TypeNode.__init__(self)
    self.left_ = left
    self.right_ = right
    self.op_ = op

  def GetResultType(self, ltype, rtype):
    lsmi = (ltype in [Type.SMI, Type.BOOL])
    rsmi = (rtype in [Type.SMI, Type.BOOL])

    # ("COMMA", ",", 1),
    # not implemented

    # ("OR", "||", 4),
    # ("AND", "&&", 5),
    # not implemented

    # ("BIT_OR", "|", 6),
    # ("BIT_XOR", "^", 7),
    if self.op_ == Token.BIT_OR or self.op_ == Token.BIT_XOR:
      if lsmi and rsmi:
        return Type.SMI
      else:
        return Type.NUM

    # ("BIT_AND", "&", 8),
    if self.op_ == Token.BIT_AND:
      if lsmi or rsmi:
        return Type.SMI
      else:
        return Type.NUM

    # ("SHL", "<<", 11),
    if self.op_ == Token.SHL:
      return Type.NUM

    # ("SAR", ">>", 11),
    # ("SHR", ">>>", 11),
    if self.op_ == Token.SAR or self.op_ == Token.SHR:
      if lsmi:
        return Type.SMI
      else:
        return Type.NUM

    # ("ADD", "+", 12),
    if self.op_ == Token.ADD:
      if ltype == Type.STR or rtype == Type.STR:
        return Type.STR
      elif lsmi and rsmi:
        return Type.SMI  # optimistic...
      else:
        return Type.NUM

    # ("SUB", "-", 12),
    # ("MUL", "*", 13),
    if self.op_ == Token.SUB or self.op_ == Token.MUL:
      if lsmi and rsmi:
        return Type.SMI  # optimistic...
      else:
        return Type.NUM
    # ("DIV", "/", 13),
    # ("MOD", "%", 13),
    if self.op_ == Token.DIV or self.op_ == Token.MOD:
      return Type.NUM

    assert(False)

  def Propagate(self):
    ret = False
    for ltype in Seeder.GetTypeNode(self.left_).types:
      for rtype in Seeder.GetTypeNode(self.right_).types:
        type = self.GetResultType(ltype, rtype)
        if not type in self.types:
          ret = True
          self.types.append(type)
          for constraint in self.constraints:
            if not type in constraint.types:
              constraint.types.append(type)
              constraint.Propagate()
    return ret

class FunctionTemplate:
  def __init__(self, fun):
    assert(isinstance(fun, FunctionLiteral))
    self.fun_ = AstCopier().Copy(fun)

  def fun(self): return self.fun_

class TemplateRepository:
  num_of_templates = 0
  def __init__(self, fun):
    self.repos_ = dict()
    self.fun_ = fun

  def CreateTemplate(self, types):
    if not types in self.repos_:
      LOG('-' * 50)
      LOG("CreateTemplate(enter)", self.fun_, self.fun_.name().value, types)
      TemplateRepository.num_of_templates += 1
      self.repos_[types] = FunctionTemplate(self.fun_)
      for type in types:
        self.repos_[types].fun().name().value += '_' + Type.ToString(type)
      LOG("CreateTemplate(exit)", self.fun_, self.repos_[types].fun(), self.repos_[types].fun().name().value, types)
      LOG('-' * 50)
    return self.repos_[types]

def BAILOUT(str):
  raise Exception(str)

Seeder_depth = 0

###############
# 1. Allocate type variables
# 2. Seed them
class Seeder(AstVisitor):
  class TemplateScope:
    def __init__(self, visitor, scope):
      self.activated_ = True
      self.visitor_ = visitor
      self.prev_scope_ = visitor.current_scope_
      visitor.current_scope_ = scope
      LOG("TemplateScope.__init__",self.visitor_.current_scope_.fun())

    def __enter__(self): pass

    def __exit__(self, *e):
      if self.activated_:
        LOG("TemplateScope.__exit__",self.visitor_.current_scope_.fun())
        self.activated_ = False
        self.visitor_.current_scope_ = self.prev_scope_

  def __init__(self, nodes):
    AstVisitor.__init__(self)
    self.nodes = nodes
    self.current_scope_ = None  # represents currently analyzed function template.
    self.analyzed = dict()

  def Allocate(self, node):
    if not '__type__' in dir(node):
      node.__type__ = TypeNode()
      if isinstance(node, FunctionLiteral):
        node.__repos__ = TemplateRepository(node)
      self.nodes.append(node.__type__)

  def AllocateBinOpTypeNode(self, node, left, right, op):
    if not '__type__' in dir(node):
      node.__type__ = BinOpTypeNode(left, right, op)
      self.nodes.append(node.__type__)

  def Seed(self, node, type):
    if not type in node.__type__.types:
      node.__type__.types.append(type)

  def VisitFunctionLiteral(self, node):
    self.Allocate(node)
    self.Seed(node, Type.FUN)

#    for i in range(0, node.scope().num_parameters()):
#      self.Visit(node.scope().parameter(i))

#    for decl in node.scope().declarations():
#      self.Visit(decl)

#    for stmt in node.body():
#      self.Visit(stmt)

  def VisitDeclaration(self, node):
    var = node.proxy().var()
    if var == None:
      BAILOUT("unsupported invalid left-hand side")
    self.Visit(var)

    # function x() {...} was converted into var x = function() {...}
    if node.fun() != None:
      # NOTE(keisuke):
      # Here assumes that 'var' keeps to hold same instance of FunctionLiteral.
      # Maybe we can validate this assumption by looking at UseCount of the
      # variable.
      var.set_fun(node.fun())

#      print(var.name().value)
#      print(var)
#      print(var.fun())
#      print("")

      self.Visit(node.fun())
      #print(node.fun(),node.fun().name().value)
      self.Connect(node.fun(), var)

  def VisitVariableProxy(self, node):
    self.Visit(node.var())

  def VisitVariable(self, node):
    self.Allocate(node)

  def VisitWhileStatement(self, node):
    def MaybeVisit(N):
      if N: self.Visit(N)
    MaybeVisit(node.cond())
    MaybeVisit(node.body())

  def VisitForStatement(self, node):
    def MaybeVisit(N):
      if N: self.Visit(N)
    MaybeVisit(node.init())
    MaybeVisit(node.cond())
    MaybeVisit(node.next())
    MaybeVisit(node.body())

  def VisitReturnStatement(self, node):
    assert(self.current_scope_ != None)
    self.Visit(node.expression())
    LOG("Seeder.VisitReturnStatement",self.current_scope_,node.expression())
    if self.current_scope_:
      self.Connect(node.expression(), self.current_scope_)

  def VisitCall(self, node):
    if isinstance(node.expression(), FunctionLiteral):
      LOG('Seeder.VisitCall',node.expression(),node.expression().name().value)
    else:
      LOG('Seeder.VisitCall',node.expression().var(),node.expression().var().name().value)

    def Dfs(depth, callee, concrete_types):
      if (depth == callee.num_parameters()):
        template = callee.__repos__.CreateTemplate(tuple(concrete_types))

        if not template in self.analyzed:
          self.analyzed[template] = True

          self.Allocate(template)  # holds types of return value
          fun = template.fun()

          with Seeder.TemplateScope(self, template):
            self.Visit(fun)

            for i in range(0, fun.scope().num_parameters()):
              self.Visit(fun.scope().parameter(i))

            for decl in fun.scope().declarations():
              self.Visit(decl)

            for stmt in fun.body():
              self.Visit(stmt)

            for i in range(0, depth):
              self.Seed(fun.scope().parameter(i), concrete_types[i])

        self.Connect(template, node)

      else:
        type_node = self.GetTypeNode(node.arguments()[depth])
        for type in type_node.types:
          concrete_types.append(type)
          Dfs(depth + 1, callee, concrete_types)
          concrete_types.pop()

    self.Allocate(node)

    for arg in node.arguments():
      self.Visit(arg)

    expr = node.expression()
    if isinstance(expr, FunctionLiteral):
      # anonymous function call
      self.Visit(expr)
      #expr.__type__.AddEdge(node.__type__)
      assert(expr.num_parameters() == len(node.arguments()))
      Dfs(0, expr, [])
    elif isinstance(expr, VariableProxy):

      if not expr.var(): raise Exception("unbound variable proxy")
      if not expr.var().fun(): raise Exception("unbound variable: " + expr.var().name().value)

      assert(expr.var() and expr.var().fun())
      callee = expr.var().fun()
      # callee must be visited by VisitDeclaration, so no need to visit here.
      #self.Visit(callee)
      #callee.__type__.AddEdge(node.__type__)
      assert(callee.num_parameters() == len(node.arguments()))
      Dfs(0, callee, [])
    else:
      assert(False)

  def VisitUnaryOperation(self, node):
    self.Allocate(node)
    self.Visit(node.expression())  # NOTE(keisuke): maybe no need to do
    if node.op() == Token.NOT or node.op() == Token.DELETE:
      self.Seed(node, Type.BOOL)
    elif node.op() == Token.BIT_NOT:
      self.Seed(node, Type.SMI)
    elif node.op() == Token.VOID:
      self.Seed(node, Type.UNDEFINED)
    elif node.op() == Token.TYPEOF:
      assert(False)  # NOTE(keisuke): not implemented
    else:
      assert(False)
    self.Connect(node.expression(), node)

  def VisitCountOperation(self, node):
    # TODO(keisuke): need to investigate what happens if we do ('foo')++
    # for example...
    # v = 42,    ++v; // v -> 43
    # v = 'str', ++v; // v -> NaN
    # v = '42',  ++v; // v -> 43
    # v = [],    ++v; // v -> 1
    # v = [42],  ++v; // v -> 43
    # v = {},    ++v; // v -> Nan
    self.Allocate(node)
    self.Visit(node.expression())
    #self.Connect(node.expression(), node)

    # if node.expression() has a type which is not Type.SMI, we conservatively
    # seed Type.NaN to node.
    exp_types = self.GetTypeNode(node.expression()).types
    if len(exp_types) == 1 and exp_types[0] == Type.SMI:
      self.Seed(node, Type.SMI)
    elif len(exp_types) >= 1:
      for type in exp_types:
        if type != Type.SMI:
          self.Seed(node, Type.NaN)

  def VisitBinaryOperation(self, node):
    self.Visit(node.left())
    self.Visit(node.right())

    self.AllocateBinOpTypeNode(node, node.left(), node.right(), node.op())

  def VisitExpressionStatement(self, node):
    self.Allocate(node)
    self.Visit(node.expression())

  def VisitEmptyStatement(self, node): pass

  def VisitLiteral(self, node):
    self.Allocate(node)
    if node.handle().IsString():
      type = Type.STR
    elif node.IsNull():
      type = Type.NULL
    elif node.IsTrue() or node.IsFalse():
      type = Type.BOOL
    elif node.IsUndefined():
      type = Type.UNDEFINED
    elif node.handle().IsNumber():
      type = Type.SMI
    else:
      type = Type.UNKNOWN
    self.Seed(node, type)

  def VisitBlock(self, node):
    for stmt in node.statements():
      self.Visit(stmt)

  def VisitAssignment(self, node):
    self.Allocate(node)
    self.Visit(node.target().var())
    self.Visit(node.value())
    self.Connect(node.value(), node.target().var())  # type of variable
    self.Connect(node.value(), node)  # type of this expression

  def VisitConditional(self, node):
    self.Allocate(node)

    self.Visit(node.condition())
    self.Visit(node.then_expression())
    self.Visit(node.else_expression())
    self.Connect(node.then_expression(), node)
    self.Connect(node.else_expression(), node)

  def VisitCompareOperation(self, node):
    self.Allocate(node)
    self.Visit(node.left())
    self.Visit(node.right())
    self.Seed(node, Type.BOOL)

  def VisitIfStatement(self, node):
    self.Visit(node.condition())
    self.Visit(node.then_statement())
    if node.HasElseStatement():
      self.Visit(node.else_statement())

  @staticmethod
  def GetTypeNode(node):
    if isinstance(node, VariableProxy):
      return node.var().__type__
    else:
      return node.__type__

  @staticmethod
  def Connect(u, v):
    Seeder.GetTypeNode(u).AddEdge(Seeder.GetTypeNode(v))

def Propagate(nodes):
  ret = False
  while True:
    changed = False
    for node in nodes:
      changed = changed or node.Propagate()
    if changed:
      ret = True
    if not changed:
      break
  return ret

def ShowFlit(flit, depth = 0):
  def Print(*args):
    if depth:
      sys.stdout.write(' ' * depth * 4)
    print args

  print'-'*50
  Print(flit)
  assert(isinstance(flit, FunctionLiteral))
  Print("name = ", flit.name(), ('"' + flit.name().value + '"'))
  Print("scope = ", flit.scope())
  Print("scope.params = ", flit.scope().params_)
  Print("scope.decls = ", flit.scope().decls_)
  for decl in flit.scope().decls_:
    Print("scope.decls[i].proxy = ",decl.proxy())
    Print("scope.decls[i].mode = ",decl.mode())
    Print("scope.decls[i].fun = ",decl.fun())
    ShowFlit(decl.fun(), depth + 1)
  #Print("scope.outer_scope = ", flit.scope().outer_scope_)  # no need
  #Print("scope.inner_scopes = ", flit.scope().inner_scopes_) # no need
  Print("body = ", flit.body())

  if len(flit.body()) > 0:
    if isinstance(flit.body()[0], ExpressionStatement):
      in_flit = flit.body()[0].expression().expression()
      if (isinstance(in_flit, FunctionLiteral)):
        ShowFlit(in_flit, depth + 1)
      else:
        Print(flit.body()[0])
  #print(in_flit.scope().decls_)

  print'-'*50

# dummy definitions for helping type inference.
BUILTIN_FUNCTIONS = ['function print(a) { return; }']

def main():
  # parse commandline options
  myusage = "%prog [-pl] < %file"
  psr = OptionParser(usage = myusage)
  psr.add_option('-p', action='store_true', dest='print_types')
  psr.add_option('-l', action='store_true', dest='enable_logging')
  psr.add_option('-a', action='store_true', dest='print_address')
  (opts, args) = psr.parse_args(sys.argv)

  LOG.enabled = opts.enable_logging

  # create AST
  source = sys.stdin.read()
  for f in BUILTIN_FUNCTIONS: source = f + source
  scanner = Scanner(source)
  parser = Parser(scanner)
  ast = parser.ParseProgram(True)

  #ShowFlit(ast)
  #tmpl = AstCopier().Copy(ast)
  #ShowFlit(tmpl)

  nodes = []
  iteration = 0
  while True:
    iteration += 1
    LOG("iter", iteration)
    Seeder(nodes).Visit(ast)
    if not Propagate(nodes): break

  # print debug info
  LOG(str(FunctionLiteral.num_of_instances), "function literal")
  LOG(str(TemplateRepository.num_of_templates), "function templates")

  PrettyPrinter(opts.print_types, opts.print_address).PrintLn(ast)
  #template = AstCopier().Copy(ast)
  #AstPrinter().PrintProgram(ast)

if __name__ == "__main__":
  main()
