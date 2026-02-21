/// Token types for the JavaScript lexer.
pub type Token {
  Token(kind: TokenKind, lexeme: String, line: Int, column: Int)
}

pub type TokenKind {
  // Literals
  Number
  String
  TemplateString
  RegExp

  // Identifiers and keywords
  Ident
  // Keywords
  KVar
  KLet
  KConst
  KFunction
  KReturn
  KIf
  KElse
  KWhile
  KDo
  KFor
  KBreak
  KContinue
  KSwitch
  KCase
  KDefault
  KThrow
  KTry
  KCatch
  KFinally
  KNew
  KDelete
  KTypeof
  KVoid
  KIn
  KInstanceof
  KThis
  KClass
  KExtends
  KSuper
  KImport
  KExport
  KFrom
  KAs
  KOf
  KAsync
  KAwait
  KYield
  KNull
  KUndefined
  KTrue
  KFalse
  KDebugger
  KWith

  // Punctuation
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  LeftBracket
  RightBracket
  Semicolon
  Comma
  Dot
  DotDotDot
  QuestionDot
  QuestionQuestion
  Arrow

  // Operators
  Plus
  Minus
  Star
  StarStar
  Slash
  Percent
  Ampersand
  AmpersandAmpersand
  Pipe
  PipePipe
  Caret
  Tilde
  Bang
  Equal
  EqualEqual
  EqualEqualEqual
  BangEqual
  BangEqualEqual
  LessThan
  LessThanEqual
  GreaterThan
  GreaterThanEqual
  LessThanLessThan
  GreaterThanGreaterThan
  GreaterThanGreaterThanGreaterThan
  PlusEqual
  MinusEqual
  StarEqual
  SlashEqual
  PercentEqual
  AmpersandEqual
  PipeEqual
  CaretEqual
  PlusPlus
  MinusMinus
  Colon
  Question

  // Special
  Eof
  Invalid
}
