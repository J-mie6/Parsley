package parsley

import parsley.Char.{digit, hexDigit, octDigit, satisfy}
import parsley.Combinator.{sepBy, sepBy1, between, some, skipSome, notFollowedBy}
import parsley.internal.deepembedding.Sign.{DoubleType, IntType, SignType}
import parsley.Parsley.{void, unit, fail, many, skipMany, attempt, lift2, pure, empty, LazyParsley}
import parsley.TokenParser.TokenSet
import parsley.Implicits.{charLift, stringLift}
import parsley.internal.deepembedding

import scala.language.{higherKinds, implicitConversions}

/**
  * This class is required to construct a TokenParser. It defines the various characteristics of the language to be
  * tokenised. Where a parameter can be either a `Set[Char]` or a `Parsley` object, prefer the `Set` where possible.
  * It will unlock a variety of faster intrinsic versions of the parsers, which will greatly improve tokenisation
  * performance! In addition, the Sets are one time converted to heavily optimised BitSets, though that has up to 8KB
  * memory usage associated but at least doubles the execution speed for that instruction. See `parsley.Impl`.
  *
  * @param commentStart For multi-line comments; how does the comment start? (If this or `commentEnd` is the empty
  *                     string, multi-line comments are disabled)
  * @param commentEnd For multi-line comments; how does the comment end? (If this or `commentEnd` is the empty
  *                   string, multi-line comments are disabled)
  * @param commentLine For single-line comments; how does the comment start? (This this is the empty string, single-line
  *                    comments are disabled)
  * @param nestedComments Are multi-line comments allowed to be nested inside each other? E.g. If `{-` and `-}` are
  *                       opening and closing comments, is the following valid syntax: `{-{-hello -}-}`? Note in C this
  *                       is not the case.
  * @param identStart What characters can an identifier in the language start with?
  * @param identLetter What characters can an identifier in the language consist of after the starting character?
  * @param opStart What characters can an operator in the language start with?
  * @param opLetter What characters can an operator in the language consist of after the starting character?
  * @param keywords What keywords does the language contain?
  * @param operators What operators does the language contain?
  * @param caseSensitive Is the language case-sensitive. I.e. is IF equivalent to if?
  * @param space What characters count as whitespace in the language?
  */
final case class LanguageDef(commentStart: String,
                             commentEnd: String,
                             commentLine: String,
                             nestedComments: Boolean,
                             identStart: Impl,
                             identLetter: Impl,
                             opStart: Impl,
                             opLetter: Impl,
                             keywords: Set[String],
                             operators: Set[String],
                             caseSensitive: Boolean,
                             space: Impl) {
    private [parsley] lazy val supportsComments = {
        val on = (commentStart.nonEmpty && commentEnd.nonEmpty) || commentLine.nonEmpty
        if (on && commentStart.nonEmpty && commentLine.startsWith(commentStart)) {
            throw new IllegalArgumentException(
                "multi-line comments which are a valid prefix of a single-line comment are not allowed as this causes ambiguity in the parser"
            )
        }
        on
    }
}
object LanguageDef
{
    val plain = LanguageDef("", "", "", false, NotRequired, NotRequired, NotRequired, NotRequired, Set.empty, Set.empty, true, NotRequired)
}

/**
  * The Impl trait is used to provide implementation of the parser requirements from `LanguageDef`
  */
sealed trait Impl
/**
  * The implementation provided is a parser which parses the required token.
  * @param p The parser which will parse the token
  */
final case class Parser(p: Parsley[_]) extends Impl
/**
  * The implementation provided is a function which matches on the input streams characters
  * @param f The predicate that input tokens are tested against
  */
final case class Predicate(f: Char => Boolean) extends Impl
/**
  * This implementation states that the required functionality is not required. If it is used it will raise an error
  * at parse-time
  */
case object NotRequired extends Impl
private [parsley] final case class BitSetImpl(cs: TokenSet) extends Impl
/**
  * This implementation uses a set of valid tokens. It is converted to a high-performance BitSet.
  */
object CharSet
{
    /**
      * @param cs The set to convert
      */
    def apply(cs: Set[Char]): Impl = BitSetImpl(new BitSet(Left(cs)))
    def apply(cs: Char*): Impl = apply(Set(cs: _*))
}
/**
  * This implementation uses a predicate to generate a BitSet. This should be preferred over `Predicate` when the
  * function in question is expensive to execute and the parser itself is expected to be used many times. If the
  * predicate is cheap, this is unlikely to provide any performance improvements, but will instead incur heavy space
  * costs
  */
object BitGen
{
    def apply(f: Char => Boolean): Impl = BitSetImpl(new BitSet(Right(f)))
}


/**
  * When provided with a `LanguageDef`, this class will produce a large variety of parsers that can be used for
  * tokenisation of a language. This includes parsing numbers and strings in their various formats and ensuring that
  * all operations consume whitespace after them (so-called lexeme parsers). These are very useful in parsing
  * programming languages. This class also has a large number of hand-optimised intrinsic parsers to improve performance!
  * @param lang The rules that govern the language we are tokenising
  */
final class TokenParser(lang: LanguageDef)
{
    private def keyOrOp(startImpl: Impl, letterImpl: Impl, parser: Parsley[String], predicate: String => Boolean,
                        combinatorName: String, name: String, illegalName: String) = {
        val builder = (start: TokenSet, letter: TokenSet) =>
            new Parsley(new deepembedding.NonSpecific(combinatorName, name, illegalName, start, letter, !predicate(_)))
        lexeme((startImpl, letterImpl) match
        {
            case (BitSetImpl(start), BitSetImpl(letter)) => builder(start, letter)
            case (BitSetImpl(start), Predicate(letter)) => builder(start, letter)
            case (Predicate(start), BitSetImpl(letter)) => builder(start, letter)
            case (Predicate(start), Predicate(letter)) => builder(start, letter)
            case _ => attempt((parser ? name).guard(predicate, s"unexpected $illegalName " + _))
        })
    }

    // Identifiers & Reserved words
    /**This lexeme parser parses a legal identifier. Returns the identifier string. This parser will
     * fail on identifiers that are reserved words (i.e. keywords). Legal identifier characters and
     * keywords are defined in the `LanguageDef` provided to the token parser. An identifier is treated
     * as a single token using `attempt`.*/
    lazy val identifier: Parsley[String] = keyOrOp(lang.identStart, lang.identLetter, ident, !isReservedName(_),  "identifier", "identifier", "keyword")

    /**The lexeme parser `keyword(name)` parses the symbol `name`, but it also checks that the `name`
     * is not a prefix of a valid identifier. A `keyword` is treated as a single token using `attempt`.*/
    def keyword(name: String): Parsley[Unit] = lang.identLetter match
    {
        case BitSetImpl(letter) => lexeme(new Parsley(new deepembedding.Specific("keyword", name, letter, lang.caseSensitive)))
        case Predicate(letter) => lexeme(new Parsley(new deepembedding.Specific("keyword", name, letter, lang.caseSensitive)))
        case _ => lexeme(attempt(caseString(name) *> notFollowedBy(identLetter) ? ("end of " + name)))
    }

    private def caseString(name: String): Parsley[String] =
    {
        def caseChar(c: Char): Parsley[Char] = if (c.isLetter) c.toLower <|> c.toUpper else c
        if (lang.caseSensitive) name
        else name.foldRight(pure(name))((c, p) => caseChar(c) *> p) ? name
    }
    private def isReservedName(name: String): Boolean = theReservedNames.contains(if (lang.caseSensitive) name else name.toLowerCase)
    private val theReservedNames =  if (lang.caseSensitive) lang.keywords else lang.keywords.map(_.toLowerCase)
    private lazy val identStart = toParser(lang.identStart)
    private lazy val identLetter = toParser(lang.identLetter)
    private lazy val ident = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, identStart, many(identLetter))

    // Operators & Reserved ops
    /**This lexeme parser parses a legal operator. Returns the name of the operator. This parser
     * will fail on any operators that are reserved operators. Legal operator characters and
     * reserved operators are defined in the `LanguageDef` provided to the token parser. A
     * `userOp` is treated as a single token using `attempt`.*/
    lazy val userOp: Parsley[String] = keyOrOp(lang.opStart, lang.opLetter, oper, !isReservedOp(_), "userOp", "operator", "reserved operator")

    /**This non-lexeme parser parses a reserved operator. Returns the name of the operator.
     * Legal operator characters and reserved operators are defined in the `LanguageDef`
     * provided to the token parser. A `reservedOp_` is treated as a single token using `attempt`.*/
    lazy val reservedOp_ : Parsley[String] = keyOrOp(lang.opStart, lang.opLetter, oper, isReservedOp(_), "reservedOp", "operator", "non-reserved operator")

    /**This lexeme parser parses a reserved operator. Returns the name of the operator. Legal
     * operator characters and reserved operators are defined in the `LanguageDef` provided
     * to the token parser. A `reservedOp` is treated as a single token using `attempt`.*/
    lazy val reservedOp: Parsley[String] = lexeme(reservedOp_)

    /**The lexeme parser `operator(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    def operator(name: String): Parsley[Unit] = lexeme(operator_(name))

    /**The non-lexeme parser `operator_(name)` parses the symbol `name`, but also checks that the `name`
     * is not the prefix of a valid operator. An `operator` is treated as a single token using
     * `attempt`.*/
    def operator_(name: String): Parsley[Unit] = lang.opLetter match
    {
        case BitSetImpl(letter) => new Parsley(new deepembedding.Specific("operator", name, letter, true))
        case Predicate(letter) => new Parsley(new deepembedding.Specific("operator", name, letter, true))
        case _ => attempt(name *> notFollowedBy(opLetter) ? ("end of " + name))
    }

    /**The lexeme parser `maxOp(name)` parses the symbol `name`, but also checks that the `name`
      * is not part of a larger reserved operator. An `operator` is treated as a single token using
      * `attempt`.*/
    def maxOp(name: String): Parsley[Unit] = lexeme(maxOp_(name))

    /**The non-lexeme parser `maxOp_(name)` parses the symbol `name`, but also checks that the `name`
      * is not part of a larger reserved operator. An `operator` is treated as a single token using
      * `attempt`.*/
    def maxOp_(name: String): Parsley[Unit] = void(new Parsley(new deepembedding.MaxOp(name, lang.operators)))

    private def isReservedOp(op: String): Boolean = lang.operators.contains(op)
    private lazy val opStart = toParser(lang.opStart)
    private lazy val opLetter = toParser(lang.opLetter)
    private lazy val oper = lift2((c: Char, cs: List[Char]) => (c::cs).mkString, opStart, many(opLetter))

    // Chars & Strings
    /**This lexeme parser parses a single literal character. Returns the literal character value.
     * This parser deals correctly with escape sequences. The literal character is parsed according
     * to the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val charLiteral: Parsley[Char] = lexeme(between('\'', '\'' ? "end of character", characterChar)) ? "character"

    /**This lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val stringLiteral: Parsley[String] = lexeme(stringLiteral_)

    /**This non-lexeme parser parses a literal string. Returns the literal string value. This parser
     * deals correctly with escape sequences and gaps. The literal string is parsed according to
     * the grammar rules defined in the Haskell report (which matches most programming languages
     * quite closely).*/
    lazy val stringLiteral_ : Parsley[String] = lang.space match
    {
        case BitSetImpl(ws) => new Parsley(new deepembedding.StringLiteral(ws))
        case Predicate(ws) => new Parsley(new deepembedding.StringLiteral(ws))
        case NotRequired => new Parsley(new deepembedding.StringLiteral(_ => false))
        case _ => between('"' ? "string", '"' ? "end of string", many(stringChar)) <#> (_.flatten.mkString)
    }

    /**This non-lexeme parser parses a string in a raw fashion. The escape characters in the string
     * remain untouched. While escaped quotes do not end the string, they remain as \" in the result
     * instead of becoming a quote character. Does not support string gaps. */
    lazy val rawStringLiteral: Parsley[String] = new Parsley(new deepembedding.RawStringLiteral)

    private def letter(terminal: Char): Parsley[Char] = satisfy(c => c != terminal && c != '\\' && c > '\u0016')

    private lazy val escapeCode = new Parsley(new deepembedding.Escape)
    private lazy val charEscape = '\\' *> escapeCode
    private lazy val charLetter = letter('\'')
    private lazy val characterChar = (charLetter <|> charEscape) ? "literal character"

    private val escapeEmpty = '&'
    private lazy val escapeGap = skipSome(space) *> '\\' ? "end of string gap"
    private lazy val stringLetter = letter('"')
    private lazy val stringEscape: Parsley[Option[Char]] =
    {
        '\\' *> (escapeGap #> None
             <|> escapeEmpty #> None
             <|> (escapeCode <#> (Some(_))))
    }
    private lazy val stringChar: Parsley[Option[Char]] = ((stringLetter <#> (Some(_))) <|> stringEscape) ? "string character"

    // Numbers
    /**This lexeme parser parses a natural number (a positive whole number). Returns the value of
     * the number. The number can specified in `decimal`, `hexadecimal` or `octal`. The number is
     * parsed according to the grammar rules in the Haskell report.*/
    lazy val natural: Parsley[Int] = lexeme(nat)

    /**This lexeme parser parses an integer (a whole number). This parser is like `natural` except
     * that it can be prefixed with a sign (i.e '-' or '+'). Returns the value of the number. The
     * number can be specified in `decimal`, `hexadecimal` or `octal`. The number is parsed
     * according to the grammar rules in the haskell report.*/
    lazy val integer: Parsley[Int] = lexeme(int) ? "integer"

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report.*/
    lazy val unsignedFloat: Parsley[Double] = lexeme(floating)

    /**This lexeme parser parses a floating point value. Returns the value of the number. The number
     * is parsed according to the grammar rules defined in the Haskell report. Accepts an optional
     * '+' or '-' sign.*/
    lazy val float: Parsley[Double] = lexeme(signedFloating) ? "float"

    /**This lexeme parser parses either `integer` or `float`. Returns the value of the number. This
     * parser deals with any overlap in the grammar rules for naturals and floats. The number is
     * parsed according to the grammar rules defined in the Haskell report.*/
    lazy val number: Parsley[Either[Int, Double]] = lexeme(number_) ? "number"

    /**This lexeme parser parses either `natural` or `unsigned float`. Returns the value of the number. This
      * parser deals with any overlap in the grammar rules for naturals and floats. The number is
      * parsed according to the grammar rules defined in the Haskell report.*/
    lazy val naturalOrFloat: Parsley[Either[Int, Double]] = lexeme(natFloat) ? "unsigned number"

    private lazy val decimal_ = number(base = 10, digit)

    private def prefixedNumber(prefix: Char, base: Int, digit: Parsley[Char]) = satisfy(c => c.toLower == prefix.toLower) *> number(base, digit)
    private lazy val hexadecimal_ = prefixedNumber('x', 16, hexDigit)
    private lazy val octal_ = prefixedNumber('o', 8, octDigit)

    // Floats
    private def sign(ty: SignType) = new Parsley(new deepembedding.Sign[ty.resultType](ty))
    private lazy val floating = new Parsley(new deepembedding.Float)
    private lazy val signedFloating = sign(DoubleType) <*> floating
    private lazy val natFloat = attempt(floating.map(Right(_))) <|> nat.map(Left(_))
    private lazy val number_ =
        ('+' *> natFloat
     <|> '-' *> natFloat.map{ case Left(n) => Left(-n); case Right(f) => Right(-f) }
     <|> natFloat)

    // Integers and Naturals
    private lazy val nat = new Parsley(new deepembedding.Natural)
    private lazy val int = sign(IntType) <*> nat

    /**Parses a positive whole number in the decimal system. Returns the value of the number.*/
    lazy val decimal: Parsley[Int] = lexeme(decimal_)

    /**Parses a positive whole number in the hexadecimal system. The number should be prefixed with
     * "0x" or "0X". Returns the value of the number.*/
    lazy val hexadecimal: Parsley[Int] = lexeme('0' *> hexadecimal_)

    /**Parses a positive whole number in the octal system. The number should be prefixed with "0o"
     * or "0O". Returns the value of the number.*/
    lazy val octal: Parsley[Int] = lexeme('0' *> octal_)

    private def number(base: Int, baseDigit: Parsley[Char]): Parsley[Int] = baseDigit.foldLeft(0)((x, d) => base*x + d.asDigit)

    // White space & symbols
    /**Lexeme parser `symbol(s)` parses `string(s)` and skips trailing white space.*/
    def symbol(name: String): Parsley[String] = lexeme[String](name)
    /**Lexeme parser `symbol(c)` parses `char(c)` and skips trailing white space.*/
    def symbol(name: Char): Parsley[Char] = lexeme[Char](name)

    /**Like `symbol`, but treats it as a single token using `attempt`. Only useful for
     * strings, since characters are already single token.*/
    def symbol_(name: String): Parsley[String] = attempt(symbol(name))

    /**`lexeme(p)` first applies parser `p` and then the `whiteSpace` parser, returning the value of
     * `p`. Every lexical token (lexeme) is defined using `lexeme`, this way every parse starts at a
     * point without white space. The only point where the `whiteSpace` parser should be called
     * explicitly is the start of the main parser in order to skip any leading white space.*/
    def lexeme[A](p: =>Parsley[A]): Parsley[A] = p <* whiteSpace

    private lazy val space = toParser(lang.space)

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the `LanguageDef`), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the token parser.*/
    lazy val whiteSpace: Parsley[Unit] = whiteSpace_(lang.space).hide

    /**Parses any white space. White space consists of zero or more occurrences of a `space` (as
     * provided by the parameter), a line comment or a block (multi-line) comment. Block
     * comments may be nested. How comments are started and ended is defined in the `LanguageDef`
     * that is provided to the token parser.*/
    val whiteSpace_ : Impl => Parsley[Unit] =
    {
        case BitSetImpl(ws) =>
            new Parsley(new deepembedding.WhiteSpace(ws, lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments))
        case Predicate(ws) =>
            new Parsley(new deepembedding.WhiteSpace(ws, lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments))
        case Parser(space_) if lang.supportsComments =>
            skipMany(new Parsley(new deepembedding.Comment(lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments)) <\> space_)
        case Parser(space_) => skipMany(space_)
        case NotRequired => skipComments
    }

    /**Parses any comments and skips them, this includes both line comments and block comments.*/
    lazy val skipComments: Parsley[Unit] = {
        if (!lang.supportsComments) unit
        else {
            new Parsley(new deepembedding.SkipComments(lang.commentStart, lang.commentEnd, lang.commentLine, lang.nestedComments))
        }
    }

    private def enclosing[A](p: =>Parsley[A], open: Char, close: Char, singular: String, plural: String) =
        between(symbol(open) ? s"open $singular",
                symbol(close) ? s"matching closing $singular" <|> fail(s"unclosed $plural"),
                p)

    // Bracketing
    /**Lexeme parser `parens(p)` parses `p` enclosed in parenthesis, returning the value of `p`.*/
    def parens[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '(', ')', "parenthesis", "parentheses")

    /**Lexeme parser `braces(p)` parses `p` enclosed in braces ('{', '}'), returning the value of 'p'*/
    def braces[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '{', '}', "brace", "braces")

    /**Lexeme parser `angles(p)` parses `p` enclosed in angle brackets ('<', '>'), returning the
     * value of `p`.*/
    def angles[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '<', '>', "angle bracket", "angle brackets")

    /**Lexeme parser `brackets(p)` parses `p` enclosed in brackets ('[', ']'), returning the value
     * of `p`.*/
    def brackets[A](p: =>Parsley[A]): Parsley[A] = enclosing(p, '[', ']', "square bracket", "square brackets")

    /**Lexeme parser `semi` parses the character ';' and skips any trailing white space. Returns ";"*/
    val semi: Parsley[Char] = symbol(';') ? "semicolon"

    /**Lexeme parser `comma` parses the character ',' and skips any trailing white space. Returns ","*/
    val comma: Parsley[Char] = symbol(',') ? "comma"

    /**Lexeme parser `colon` parses the character ':' and skips any trailing white space. Returns ":"*/
    val colon: Parsley[Char] = symbol(':') ? "colon"

    /**Lexeme parser `dot` parses the character '.' and skips any trailing white space. Returns "."*/
    val dot: Parsley[Char] = symbol('.') ? "dot"

    /**Lexeme parser `semiSep(p)` parses zero or more occurrences of `p` separated by `semi`. Returns
     * a list of values returned by `p`.*/
    def semiSep[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy(p, semi)

    /**Lexeme parser `semiSep1(p)` parses one or more occurrences of `p` separated by `semi`. Returns
     * a list of values returned by `p`.*/
    def semiSep1[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy1(p, semi)

    /**Lexeme parser `commaSep(p)` parses zero or more occurrences of `p` separated by `comma`.
     * Returns a list of values returned by `p`.*/
    def commaSep[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy(p, comma)

    /**Lexeme parser `commaSep1(p)` parses one or more occurrences of `p` separated by `comma`.
     * Returns a list of values returned by `p`.*/
    def commaSep1[A](p: =>Parsley[A]): Parsley[List[A]] = sepBy1(p, comma)

    private def toParser(e: Impl) = e match
    {
        case BitSetImpl(cs) => satisfy(cs(_))
        case Parser(p) => p.asInstanceOf[Parsley[Char]]
        case Predicate(f) => satisfy(f)
        case NotRequired => empty
    }
}

private [parsley] object TokenParser
{
    type TokenSet = Char => Boolean
}