//===--- LiteralSupport.cpp - Code to parse and process literals ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the NumericLiteralParser, CharLiteralParser, and
// StringLiteralParser interfaces.
//
//===----------------------------------------------------------------------===//

#include "vlang/Lex/LiteralSupport.h"
#include "vlang/Basic/CharInfo.h"
#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Lex/Preprocessor.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/ErrorHandling.h"

using namespace vlang;

static unsigned getCharWidth(tok::TokenKind kind) {
  switch (kind) {
  default: llvm_unreachable("Unknown token type!");
  case tok::string_literal:
    return 8;
  }
}

static CharSourceRange MakeCharSourceRange(const LangOptions &Features,
                                           FullSourceLoc TokLoc,
                                           const char *TokBegin,
                                           const char *TokRangeBegin,
                                           const char *TokRangeEnd) {
  SourceLocation Begin =
    Lexer::AdvanceToTokenCharacter(TokLoc, TokRangeBegin - TokBegin,
                                   TokLoc.getManager(), Features);
  SourceLocation End =
    Lexer::AdvanceToTokenCharacter(Begin, TokRangeEnd - TokRangeBegin,
                                   TokLoc.getManager(), Features);
  return CharSourceRange::getCharRange(Begin, End);
}

/// \brief Produce a diagnostic highlighting some portion of a literal.
///
/// Emits the diagnostic \p DiagID, highlighting the range of characters from
/// \p TokRangeBegin (inclusive) to \p TokRangeEnd (exclusive), which must be
/// a substring of a spelling buffer for the token beginning at \p TokBegin.
static DiagnosticBuilder Diag(DiagnosticsEngine *Diags,
                              const LangOptions &Features, FullSourceLoc TokLoc,
                              const char *TokBegin, const char *TokRangeBegin,
                              const char *TokRangeEnd, unsigned DiagID) {
  SourceLocation Begin =
    Lexer::AdvanceToTokenCharacter(TokLoc, TokRangeBegin - TokBegin,
                                   TokLoc.getManager(), Features);
  return Diags->Report(Begin, DiagID) <<
    MakeCharSourceRange(Features, TokLoc, TokBegin, TokRangeBegin, TokRangeEnd);
}

/// ProcessCharEscape - Parse a standard C escape sequence, which can occur in
/// either a character or a string literal.
static unsigned ProcessCharEscape(const char *ThisTokBegin,
                                  const char *&ThisTokBuf,
                                  const char *ThisTokEnd, bool &HadError,
                                  FullSourceLoc Loc, unsigned CharWidth,
                                  DiagnosticsEngine *Diags,
                                  const LangOptions &Features) {
  const char *EscapeBegin = ThisTokBuf;

  // Skip the '\' char.
  ++ThisTokBuf;

  // We know that this character can't be off the end of the buffer, because
  // that would have been \", which would not have been the end of string.
  unsigned ResultChar = *ThisTokBuf++;
  switch (ResultChar) {
  // These map to themselves.
  case '\\': case '\'': case '"': case '?': break;

    // These have fixed mappings.
  case 'a':
    // TODO: K&R: the meaning of '\\a' is different in traditional C
    ResultChar = 7;
    break;
  case 'b':
    ResultChar = 8;
    break;
  case 'e':
    if (Diags)
      Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
           diag::ext_nonstandard_escape) << "e";
    ResultChar = 27;
    break;
  case 'E':
    if (Diags)
      Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
           diag::ext_nonstandard_escape) << "E";
    ResultChar = 27;
    break;
  case 'f':
    ResultChar = 12;
    break;
  case 'n':
    ResultChar = 10;
    break;
  case 'r':
    ResultChar = 13;
    break;
  case 't':
    ResultChar = 9;
    break;
  case 'v':
    ResultChar = 11;
    break;
  case 'x': { // Hex escape.
    ResultChar = 0;
    if (ThisTokBuf == ThisTokEnd || !isHexDigit(*ThisTokBuf)) {
      if (Diags)
        Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
             diag::err_hex_escape_no_digits) << "x";
      HadError = 1;
      break;
    }

    // Hex escapes are a maximal series of hex digits.
    bool Overflow = false;
    for (; ThisTokBuf != ThisTokEnd; ++ThisTokBuf) {
      int CharVal = llvm::hexDigitValue(ThisTokBuf[0]);
      if (CharVal == -1) break;
      // About to shift out a digit?
      Overflow |= (ResultChar & 0xF0000000) ? true : false;
      ResultChar <<= 4;
      ResultChar |= CharVal;
    }

    // See if any bits will be truncated when evaluated as a character.
    if (CharWidth != 32 && (ResultChar >> CharWidth) != 0) {
      Overflow = true;
      ResultChar &= ~0U >> (32-CharWidth);
    }

    // Check for overflow.
    if (Overflow && Diags)   // Too many digits to fit in
      Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
           diag::warn_hex_escape_too_large);
    break;
  }
  case '0': case '1': case '2': case '3':
  case '4': case '5': case '6': case '7': {
    // Octal escapes.
    --ThisTokBuf;
    ResultChar = 0;

    // Octal escapes are a series of octal digits with maximum length 3.
    // "\0123" is a two digit sequence equal to "\012" "3".
    unsigned NumDigits = 0;
    do {
      ResultChar <<= 3;
      ResultChar |= *ThisTokBuf++ - '0';
      ++NumDigits;
    } while (ThisTokBuf != ThisTokEnd && NumDigits < 3 &&
             ThisTokBuf[0] >= '0' && ThisTokBuf[0] <= '7');

    // Check for overflow.  Reject '\777', but not L'\777'.
    if (CharWidth != 32 && (ResultChar >> CharWidth) != 0) {
      if (Diags)
        Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
             diag::warn_octal_escape_too_large);
      ResultChar &= ~0U >> (32-CharWidth);
    }
    break;
  }

    // Otherwise, these are not valid escapes.
  case '(': case '{': case '[': case '%':
    // GCC accepts these as extensions.  We warn about them as such though.
    if (Diags)
      Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
           diag::ext_nonstandard_escape)
        << std::string(1, ResultChar);
    break;
  default:
    if (Diags == 0)
      break;

    if (isPrintable(ResultChar))
      Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
           diag::ext_unknown_escape)
        << std::string(1, ResultChar);
    else
      Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
           diag::ext_unknown_escape)
        << "x" + llvm::utohexstr(ResultChar);
    break;
  }

  return ResultChar;
}

// IEEE-1800 2012 - A.8.7
// number ::= decimal_number
//          | octal_number
//          | binary_number
//          | hex_number
//          | real_number
// decimal_number ::=  unsigned_number
//                  | [ size ] decimal_base unsigned_number
//                            | [ size ] decimal_base x_digit { _ }
//                            | [ size ] decimal_base z_digit { _ }
// binary_number            ::= [ size ] binary_base binary_value
// octal_number             ::= [ size ] octal_base octal_value
// hex_number               ::= [ size ] hex_base hex_value
// sign                     ::= + | -
// size                     ::= non_zero_unsigned_number
// non_zero_unsigned_number ::= non_zero_decimal_digit { _ | decimal_digit}
// real_number              ::= fixed_point_number
//                            | unsigned_number [ . unsigned_number ] exp [ sign ] unsigned_number
// fixed_point_number     ::= unsigned_number . unsigned_number
// exp                    ::= e | E
// unsigned_number        ::= decimal_digit { _ | decimal_digit }
// binary_value           ::= binary_digit { _ | binary_digit }
// octal_value            ::= octal_digit { _ | octal_digit }
// hex_value              ::= hex_digit { _ | hex_digit }
// decimal_base           ::= '[s|S]d | '[s|S]D
// binary_base            ::= '[s|S]b | '[s|S]B
// octal_base             ::= '[s|S]o | '[s|S]O
// hex_base               ::= '[s|S]h | '[s|S]H
// non_zero_decimal_digit ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
// decimal_digit          ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
// binary_digit           ::= x_digit | z_digit | 0 | 1
// octal_digit            ::= x_digit | z_digit | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
// hex_digit              ::= x_digit | z_digit | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | b | c | d | e | f | A | B | C | D | E | F
// x_digit                ::= x | X
// z_digit                ::= z | Z | ?
// unbased_unsized_literal ::= '0 | '1 | 'z_or_x 48
   
NumericLiteralParser::NumericLiteralParser(StringRef TokSpelling,
                                           SourceLocation TokLoc,
                                           Preprocessor &PP)
  : PP(PP), ThisTokBegin(TokSpelling.begin()), ThisTokEnd(TokSpelling.end()) {

  // This routine assumes that the range begin/end matches the regex for integer
  // and FP constants (specifically, the 'pp-number' regex), and assumes that
  // the byte at "*end" is both valid and not part of the regex.  Because of
  // this, it doesn't have to check for 'overscan' in various places.
  assert(!isPreprocessingNumberBody(*ThisTokEnd) && "didn't maximally munch?");

  s = DigitsBegin = ThisTokBegin;
  saw_period = false;
  isUnsigned = false;
  hadError = false;

  radix = 10;
  s = SkipDigits(s);
  if (s == ThisTokEnd) {
     // Done.
  } else if (isHexDigit(*s) && !(*s == 'e' || *s == 'E')) {
     PP.Diag(PP.AdvanceToTokenCharacter(TokLoc, s - ThisTokBegin),
        diag::err_invalid_decimal_digit) << StringRef(s, 1);
     hadError = true;
     return;
  } else if (*s == '.') {
     s++;
     saw_period = true;
     s = SkipDigits(s);
  }
}

static bool alwaysFitsInto64Bits(unsigned Radix, unsigned NumDigits) {
  switch (Radix) {
  case 2:
    return NumDigits <= 64;
  case 8:
    return NumDigits <= 64 / 3; // Digits are groups of 3 bits.
  case 10:
    return NumDigits <= 19; // floor(log10(2^64))
  case 16:
    return NumDigits <= 64 / 4; // Digits are groups of 4 bits.
  default:
    llvm_unreachable("impossible Radix");
  }
}

/// GetIntegerValue - Convert this numeric literal value to an APInt that
/// matches Val's input width.  If there is an overflow, set Val to the low bits
/// of the result and return true.  Otherwise, return false.
bool NumericLiteralParser::GetIntegerValue(llvm::APInt &Val) {
  // Fast path: Compute a conservative bound on the maximum number of
  // bits per digit in this radix. If we can't possibly overflow a
  // uint64 based on that bound then do the simple conversion to
  // integer. This avoids the expensive overflow checking below, and
  // handles the common cases that matter (small decimal integers and
  // hex/octal values which don't overflow).
  const unsigned NumDigits = DigitsEnd - DigitsBegin;
  if (alwaysFitsInto64Bits(radix, NumDigits)) {
    uint64_t N = 0;
    for (const char *Ptr = DigitsBegin; Ptr != DigitsEnd; ++Ptr)
      N = N * radix + llvm::hexDigitValue(*Ptr);

    // This will truncate the value to Val's input width. Simply check
    // for overflow by comparing.
    Val = N;
    return Val.getZExtValue() != N;
  }

  Val = 0;
  const char *Ptr = DigitsBegin;

  llvm::APInt RadixVal(Val.getBitWidth(), radix);
  llvm::APInt CharVal(Val.getBitWidth(), 0);
  llvm::APInt OldVal = Val;

  bool OverflowOccurred = false;
  while (Ptr < DigitsEnd) {
    unsigned C = llvm::hexDigitValue(*Ptr++);

    // If this letter is out of bound for this radix, reject it.
    assert(C < radix && "NumericLiteralParser ctor should have rejected this");

    CharVal = C;

    // Add the digit to the value in the appropriate radix.  If adding in digits
    // made the value smaller, then this overflowed.
    OldVal = Val;

    // Multiply by radix, did overflow occur on the multiply?
    Val *= RadixVal;
    OverflowOccurred |= Val.udiv(RadixVal) != OldVal;

    // Add value, did overflow occur on the value?
    //   (a + b) ult b  <=> overflow
    Val += CharVal;
    OverflowOccurred |= Val.ult(CharVal);
  }
  return OverflowOccurred;
}

llvm::APFloat::opStatus
NumericLiteralParser::GetFloatValue(llvm::APFloat &Result) {
  using llvm::APFloat;

  unsigned n = std::min(DigitsEnd - ThisTokBegin, ThisTokEnd - ThisTokBegin);
  return Result.convertFromString(StringRef(ThisTokBegin, n),
                                  APFloat::rmNearestTiesToEven);
}


/// \verbatim
///       string-literal: [C++0x lex.string]
///         encoding-prefix " [s-char-sequence] "
///         encoding-prefix R raw-string
///       encoding-prefix:
///         u8
///         u
///         U
///         L
///       s-char-sequence:
///         s-char
///         s-char-sequence s-char
///       s-char:
///         any member of the source character set except the double-quote ",
///           backslash \, or new-line character
///         escape-sequence
///         universal-character-name
///       raw-string:
///         " d-char-sequence ( r-char-sequence ) d-char-sequence "
///       r-char-sequence:
///         r-char
///         r-char-sequence r-char
///       r-char:
///         any member of the source character set, except a right parenthesis )
///           followed by the initial d-char-sequence (which may be empty)
///           followed by a double quote ".
///       d-char-sequence:
///         d-char
///         d-char-sequence d-char
///       d-char:
///         any member of the basic source character set except:
///           space, the left parenthesis (, the right parenthesis ),
///           the backslash \, and the control characters representing horizontal
///           tab, vertical tab, form feed, and newline.
///       escape-sequence: [C++0x lex.ccon]
///         simple-escape-sequence
///         octal-escape-sequence
///         hexadecimal-escape-sequence
///       simple-escape-sequence:
///         one of \' \" \? \\ \a \b \f \n \r \t \v
///       octal-escape-sequence:
///         \ octal-digit
///         \ octal-digit octal-digit
///         \ octal-digit octal-digit octal-digit
///       hexadecimal-escape-sequence:
///         \x hexadecimal-digit
///         hexadecimal-escape-sequence hexadecimal-digit
///       universal-character-name:
///         \u hex-quad
///         \U hex-quad hex-quad
///       hex-quad:
///         hex-digit hex-digit hex-digit hex-digit
/// \endverbatim
///
StringLiteralParser::
StringLiteralParser(const Token *StringToks, unsigned NumStringToks,
                    Preprocessor &PP, bool Complain)
  : SM(PP.getSourceManager()), Features(PP.getLangOpts()),
    Diags(Complain ? &PP.getDiagnostics() : 0),
    MaxTokenLength(0), SizeBound(0), CharByteWidth(0), Kind(tok::unknown),
    ResultPtr(ResultBuf.data()), hadError(false){
  init(StringToks, NumStringToks);
}

void StringLiteralParser::init(const Token *StringToks, unsigned NumStringToks){
  // The literal token may have come from an invalid source location (e.g. due
  // to a PCH error), in which case the token length will be 0.
  if (NumStringToks == 0 || StringToks[0].getLength() < 2)
    return DiagnoseLexingError(SourceLocation());

  // Scan all of the string portions, remember the max individual token length,
  // computing a bound on the concatenated string length, and see whether any
  // piece is a wide-string.  If any of the string portions is a wide-string
  // literal, the result is a wide-string literal [C99 6.4.5p4].
  assert(NumStringToks && "expected at least one token");
  MaxTokenLength = StringToks[0].getLength();
  assert(StringToks[0].getLength() >= 2 && "literal token is invalid!");
  SizeBound = StringToks[0].getLength()-2;  // -2 for "".
  Kind = StringToks[0].getKind();

  hadError = false;

  // Implement Translation Phase #6: concatenation of string literals
  /// (C99 5.1.1.2p1).  The common case is only one string fragment.
  for (unsigned i = 1; i != NumStringToks; ++i) {
    if (StringToks[i].getLength() < 2)
      return DiagnoseLexingError(StringToks[i].getLocation());

    // The string could be shorter than this if it needs cleaning, but this is a
    // reasonable bound, which is all we need.
    assert(StringToks[i].getLength() >= 2 && "literal token is invalid!");
    SizeBound += StringToks[i].getLength()-2;  // -2 for "".

    // Remember maximum string piece length.
    if (StringToks[i].getLength() > MaxTokenLength)
      MaxTokenLength = StringToks[i].getLength();

    // Remember if we see any wide or utf-8/16/32 strings.
    // Also check for illegal concatenations.
    if (StringToks[i].isNot(Kind) && StringToks[i].isNot(tok::string_literal)) {
      if (isAscii()) {
        Kind = StringToks[i].getKind();
      } else {
        if (Diags)
          Diags->Report(StringToks[i].getLocation(),
                        diag::err_unsupported_string_concat);
        hadError = true;
      }
    }
  }

  // Include space for the null terminator.
  ++SizeBound;

  // TODO: K&R warning: "traditional C rejects string constant concatenation"

  // Get the width in bytes of char/wchar_t/char16_t/char32_t
  CharByteWidth = getCharWidth(Kind);
  assert((CharByteWidth & 7) == 0 && "Assumes character size is byte multiple");
  CharByteWidth /= 8;

  // The output buffer size needs to be large enough to hold wide characters.
  // This is a worst-case assumption which basically corresponds to L"" "long".
  SizeBound *= CharByteWidth;

  // Size the temporary buffer to hold the result string data.
  ResultBuf.resize(SizeBound);

  // Likewise, but for each string piece.
  SmallString<512> TokenBuf;
  TokenBuf.resize(MaxTokenLength);

  // Loop over all the strings, getting their spelling, and expanding them to
  // wide strings as appropriate.
  ResultPtr = &ResultBuf[0];   // Next byte to fill in.

  for (unsigned i = 0, e = NumStringToks; i != e; ++i) {
    const char *ThisTokBuf = &TokenBuf[0];
    // Get the spelling of the token, which eliminates trigraphs, etc.  We know
    // that ThisTokBuf points to a buffer that is big enough for the whole token
    // and 'spelled' tokens can only shrink.
    bool StringInvalid = false;
    unsigned ThisTokLen = 
      Lexer::getSpelling(StringToks[i], ThisTokBuf, SM, Features,
                         &StringInvalid);
    if (StringInvalid)
      return DiagnoseLexingError(StringToks[i].getLocation());

    const char *ThisTokBegin = ThisTokBuf;
    const char *ThisTokEnd = ThisTokBuf+ThisTokLen;

    // Strip the end quote.
    --ThisTokEnd;

    // TODO: Input character set mapping support.

    if (ThisTokBuf[0] != '"') {
      // The file may have come from PCH and then changed after loading the
      // PCH; Fail gracefully.
      return DiagnoseLexingError(StringToks[i].getLocation());
    }
    ++ThisTokBuf; // skip "

    while (ThisTokBuf != ThisTokEnd) {
       // Is this a span of non-escape characters?
       if (ThisTokBuf[0] != '\\') {
          const char *InStart = ThisTokBuf;
          do {
             ++ThisTokBuf;
          } while (ThisTokBuf != ThisTokEnd && ThisTokBuf[0] != '\\');

          // Copy the character span over.
          if (CopyStringFragment(StringToks[i], ThisTokBegin, StringRef(InStart, ThisTokBuf - InStart)))
             hadError = true;
          continue;
       }
       // Otherwise, this is a non-UCN escape character.  Process it.
       unsigned ResultChar =
          ProcessCharEscape(ThisTokBegin, ThisTokBuf, ThisTokEnd, hadError,
                            FullSourceLoc(StringToks[i].getLocation(), SM),
                             CharByteWidth*8, Diags, Features);

       if (CharByteWidth == 4) {
          // FIXME: Make the type of the result buffer correct instead of
          // using reinterpret_cast.
          UTF32 *ResultWidePtr = reinterpret_cast<UTF32*>(ResultPtr);
          *ResultWidePtr = ResultChar;
          ResultPtr += 4;
       } else if (CharByteWidth == 2) {
          // FIXME: Make the type of the result buffer correct instead of
          // using reinterpret_cast.
          UTF16 *ResultWidePtr = reinterpret_cast<UTF16*>(ResultPtr);
          *ResultWidePtr = ResultChar & 0xFFFF;
          ResultPtr += 2;
       } else {
          assert(CharByteWidth == 1 && "Unexpected char width");
          *ResultPtr++ = ResultChar & 0xFF;
       }
    }
  }
}

static const char *resyncUTF8(const char *Err, const char *End) {
  if (Err == End)
    return End;
  End = Err + std::min<unsigned>(getNumBytesForUTF8(*Err), End-Err);
  while (++Err != End && (*Err & 0xC0) == 0x80)
    ;
  return Err;
}

/// \brief This function copies from Fragment, which is a sequence of bytes
/// within Tok's contents (which begin at TokBegin) into ResultPtr.
/// Performs widening for multi-byte characters.
bool StringLiteralParser::CopyStringFragment(const Token &Tok,
                                             const char *TokBegin,
                                             StringRef Fragment) {
  const UTF8 *ErrorPtrTmp;
  if (ConvertUTF8toWide(CharByteWidth, Fragment, ResultPtr, ErrorPtrTmp))
    return false;

  // If we see bad encoding for unprefixed string literals, warn and
  // simply copy the byte values, for compatibility with gcc and older
  // versions of vlang.
  bool NoErrorOnBadEncoding = isAscii();
  if (NoErrorOnBadEncoding) {
    memcpy(ResultPtr, Fragment.data(), Fragment.size());
    ResultPtr += Fragment.size();
  }

  if (Diags) {
    const char *ErrorPtr = reinterpret_cast<const char *>(ErrorPtrTmp);

    FullSourceLoc SourceLoc(Tok.getLocation(), SM);
    const DiagnosticBuilder &Builder =
      Diag(Diags, Features, SourceLoc, TokBegin,
           ErrorPtr, resyncUTF8(ErrorPtr, Fragment.end()),
           NoErrorOnBadEncoding ? diag::warn_bad_string_encoding
                                : diag::err_bad_string_encoding);

    const char *NextStart = resyncUTF8(ErrorPtr, Fragment.end());
    StringRef NextFragment(NextStart, Fragment.end()-NextStart);

    // Decode into a dummy buffer.
    SmallString<512> Dummy;
    Dummy.reserve(Fragment.size() * CharByteWidth);
    char *Ptr = Dummy.data();

    while (!Builder.hasMaxRanges() &&
           !ConvertUTF8toWide(CharByteWidth, NextFragment, Ptr, ErrorPtrTmp)) {
      const char *ErrorPtr = reinterpret_cast<const char *>(ErrorPtrTmp);
      NextStart = resyncUTF8(ErrorPtr, Fragment.end());
      Builder << MakeCharSourceRange(Features, SourceLoc, TokBegin,
                                     ErrorPtr, NextStart);
      NextFragment = StringRef(NextStart, Fragment.end()-NextStart);
    }
  }
  return !NoErrorOnBadEncoding;
}

void StringLiteralParser::DiagnoseLexingError(SourceLocation Loc) {
  hadError = true;
  if (Diags)
    Diags->Report(Loc, diag::err_lexing_string);
}

/// getOffsetOfStringByte - This function returns the offset of the
/// specified byte of the string data represented by Token.  This handles
/// advancing over escape sequences in the string.
unsigned StringLiteralParser::getOffsetOfStringByte(const Token &Tok,
                                                    unsigned ByteNo) const {
  // Get the spelling of the token.
  SmallString<32> SpellingBuffer;
  SpellingBuffer.resize(Tok.getLength());

  bool StringInvalid = false;
  const char *SpellingPtr = &SpellingBuffer[0];
  unsigned TokLen = Lexer::getSpelling(Tok, SpellingPtr, SM, Features,
                                       &StringInvalid);
  if (StringInvalid)
    return 0;

  const char *SpellingStart = SpellingPtr;
  const char *SpellingEnd = SpellingPtr+TokLen;

  // Handle UTF-8 strings just like narrow strings.
  if (SpellingPtr[0] == 'u' && SpellingPtr[1] == '8')
    SpellingPtr += 2;

  assert(SpellingPtr[0] != 'L' && SpellingPtr[0] != 'u' &&
         SpellingPtr[0] != 'U' && "Doesn't handle wide or utf strings yet");

  // For raw string literals, this is easy.
  if (SpellingPtr[0] == 'R') {
    assert(SpellingPtr[1] == '"' && "Should be a raw string literal!");
    // Skip 'R"'.
    SpellingPtr += 2;
    while (*SpellingPtr != '(') {
      ++SpellingPtr;
      assert(SpellingPtr < SpellingEnd && "Missing ( for raw string literal");
    }
    // Skip '('.
    ++SpellingPtr;
    return SpellingPtr - SpellingStart + ByteNo;
  }

  // Skip over the leading quote
  assert(SpellingPtr[0] == '"' && "Should be a string literal!");
  ++SpellingPtr;

  // Skip over bytes until we find the offset we're looking for.
  while (ByteNo) {
    assert(SpellingPtr < SpellingEnd && "Didn't find byte offset!");

    // Step over non-escapes simply.
    if (*SpellingPtr != '\\') {
      ++SpellingPtr;
      --ByteNo;
      continue;
    }

    // Otherwise, this is an escape character.  Advance over it.
    bool HadError = false;
    ProcessCharEscape(SpellingStart, SpellingPtr, SpellingEnd, HadError,
                      FullSourceLoc(Tok.getLocation(), SM),
                      CharByteWidth*8, Diags, Features);
    --ByteNo;
    assert(!HadError && "This method isn't valid on erroneous strings");
  }

  return SpellingPtr-SpellingStart;
}
