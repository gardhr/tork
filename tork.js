/*
 Rough proof of concept...
*/

 function char(text)
 {
  return text.charCodeAt(0)
 }

 var
  magic = 6,
  glyphs = [], 
  slot = 128,
  type_undefined = slot++,
  type_unexpected_end_of_input = slot++, 
  type_identifier = slot++,
  type_comment = slot++,
  type_backslash = 0x5c, // FIXME
  type_forward_slash = char("/"),
  type_asterisk = char("*"),
  type_quote = slot++,
  type_single_quote = char("'"),
  type_double_quote = 0x22, // FIXME
  type_left_brace = char("{"),
  type_right_brace = char("}"),
  type_left_paren = char("("),
  type_right_paren = char(")"),
  type_left_bracket = char("["),
  type_right_bracket = char("]"),
  type_not = char("!"),
  type_not_equal = slot++,
  type_caret = char("^"),
  type_caret_assign = slot++,
  type_and = char("&"),
  type_and_assign = slot++,
  type_and_logical = slot++,
  type_or = char("|"),
  type_or_assign = slot++,
  type_or_logical = slot++,
  type_plus = char("+"),
  type_plus_plus = slot++,
  type_plus_assign = slot++,
  type_minus = char("-"),
  type_minus_minus = slot++,
  type_minus_assign = slot++,
  type_times = type_asterisk,
  type_times_assign = slot++,
  type_divide = type_forward_slash,
  type_divide_assign = slot++,
  type_modulus = char("%"),
  type_modulus_assign = slot++,
  type_equals = char("="),
  type_equals_equals = slot++,
  type_less = char("<"),
  type_less_or_equal = slot++,
  type_shift_left = slot++,
  type_shift_left_assign = slot++,
  type_greater = char(">"), 
  type_greater_or_equal = slot++,
  type_shift_right = slot++,
  type_shift_right_assign = slot++,
  type_dollar = char("$"),
  type_colon = char(":"),
  type_semicolon = char(";"),
  type_conditional = char("?"),
  type_dot = char("."),
  type_ellipsis = slot++,
  type_comma = char(","),
  type_at = char("@"),
  type_pound = char("#"),
  type_tilde = char("~"),
  type_space = char(" "), 
  type_tab = 0x9, // FIXME
  type_linefeed = 0xa, // FIXME 
  type_carriage_return = 0xd, // FIXME
  type_newline = type_linefeed,
  type_number = slot++,
  type_integer = slot++,
  type_hex = slot++,
  type_octal = slot++,
  type_scientific = slot++,
  type_discardable = slot++,
  type_do = slot++,
  type_while = slot++,
  type_for = slot++,
  type_until = slot++,
  type_if = slot++,
  type_unless = slot++,
  type_else = slot++,
  type_break = slot++,
  type_continue = slot++,
  type_throw = slot++,
  type_catch = slot++,
  type_return = slot++,
  type_end = slot++,
  type_placeholder

 function type_to_text(type)
 {
  function wrong(value)
  {
   return "UNDEFINED_TOKEN(" + value + ")"
  }
  if(!type_to_text.tab)
  {
   var
    tab = []
   for(var idx = 0; idx < 256; ++idx)
    tab[idx] = wrong(idx)
   tab[type_identifier] = "identifier"
   tab[type_integer] = "integer"
   tab[type_octal] = "octal"
   tab[type_hex] = "hex"
   tab[type_number] = "number"
   tab[type_scientific] = "scientific"
   tab[type_comment] = "comment"
   tab[type_quote] = "quote"
   tab[type_unexpected_end_of_input] = "unexpected_end_of_input" 
   tab[type_backslash] = "backslash"
   tab[type_single_quote] = "single_quote"
   tab[type_double_quote] = "double_quote"
   tab[type_left_brace] = "left_brace"
   tab[type_right_brace] = "right_brace"
   tab[type_left_paren] = "left_paren"
   tab[type_right_paren] = "right_paren"
   tab[type_left_bracket] = "left_bracket"
   tab[type_right_bracket] = "right_bracket"
   tab[type_not] = "not"
   tab[type_not_equal] = "not_equal"
   tab[type_caret] = "caret"
   tab[type_caret_assign] = "caret_assign"
   tab[type_and] = "and"
   tab[type_and_assign] = "and_assign"
   tab[type_and_logical] = "and_logical"
   tab[type_or] = "or"
   tab[type_or_assign] = "or_assign"
   tab[type_or_logical] = "or_logical"
   tab[type_plus] = "plus"
   tab[type_plus_plus] = "plus_plus"
   tab[type_plus_assign] = "plus_assign"
   tab[type_minus] = "minus"
   tab[type_minus_assign] = "minus_assign"
   tab[type_times] = "times"
   tab[type_times_assign] = "times_assign"
   tab[type_divide] = "divide"
   tab[type_divide_assign] = "divide_assign"
   tab[type_modulus] = "modulus"
   tab[type_modulus_assign] = "modulus_assign"
   tab[type_equals_equals] = "equals_equals"
   tab[type_equals] = "equals"
   tab[type_less] = "less"
   tab[type_less_or_equal] = "less_or_equal"
   tab[type_shift_left] = "shift_left"
   tab[type_shift_left_assign] = "shift_left_assign"
   tab[type_greater] = "greater" 
   tab[type_greater_or_equal] = "greater_or_equal"
   tab[type_shift_right] = "shift_right"
   tab[type_shift_right_assign] = "shift_right_assign"
   tab[type_dollar] = "dollar"
   tab[type_colon] = "colon"
   tab[type_semicolon] = "semicolon"
   tab[type_conditional] = "conditional"
   tab[type_dot] = "dot"
   tab[type_ellipsis] = "ellipsis"
   tab[type_comma] = "comma"
   tab[type_at] = "at"
   tab[type_pound] = "pound"
   tab[type_tilde] = "tilde"
   tab[type_space] = "spaces"
   tab[type_tab] = "tabs"   
   tab[type_newline] = "newlines"
   tab[type_do] = "do"
   tab[type_while] = "while"
   tab[type_until] = "until"
   tab[type_for] = "for"
   tab[type_if] = "if"
   tab[type_unless] = "unless"
   tab[type_else] = "else"
   tab[type_break] = "break"
   tab[type_continue] = "continue"
   tab[type_throw] = "throw"
   tab[type_catch] = "catch"
   tab[type_return] = "return" 
   tab[type_end] = "end" 
   type_to_text.tab = tab  
  }
  if(type >= 256)
   return wrong(type)
  return type_to_text.tab[type]
 }

/*
 Tokenization
*/
 
 function match_token(type, index)
 {
  return { type: type, index: index }
 }

 function eof(idx)
 {
  return idx >= glyphs.length
 }
  
 function match_quote(quote, idx)
 {
  for(;;)
  {
   if(eof(idx))
    return match_token(type_unexpected_end_of_input, idx)
   var ch = glyphs[++idx]
   if(ch == quote)
    break
   if(ch == type_backslash)
    if(!eof(idx + 2))
     idx += 2
  }
  return match_token(type_quote, idx + 1)
 }

 function match_single_quote(idx)
 {
  return match_quote(type_single_quote, idx)
 }

 function match_double_quote(idx)
 {
  return match_quote(type_double_quote, idx)
 }
 
 function match_equals(idx)
 {
  if(glyphs[++idx] == type_equals)
   return match_token(type_equals_equals, idx + 1)
  return match_token(type_equals, idx)
 }

 function match_not(idx)
 {
  if(glyphs[++idx] == type_equals)
   return match_token(type_not_equal, idx + 1)
  return match_token(type_not, idx)
 }

 function match_modulus(idx)
 {
  if(glyphs[++idx] == type_equals)
   return match_token(type_modulus_assign, idx + 1)
  return match_token(type_modulus, idx)
 }

 function match_caret(idx)
 {
  if(glyphs[++idx] == type_equals)
   return match_token(type_caret_assign, idx + 1)
  return match_token(type_caret, idx)
 }

 function match_and(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_and)
   return match_token(type_and_logical, idx + 1)
  else if(glyph == type_equals)
   return match_token(type_and_assign, idx + 1)    
  return match_token(type_and, idx)
 }

 function match_or(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_or)
   return match_token(type_or_logical, idx + 1)
  else if(glyph == type_equals)
   return match_token(type_or_assign, idx + 1)    
  return match_token(type_or, idx)
 }

 function match_times(idx)
 {
  if(glyphs[++idx] == type_equals)
   return match_token(type_times_assign, idx + 1)
  return match_token(type_times, idx)
 }

 function match_nested_comment(idx)
 {
  if(
   glyphs[idx++] != type_forward_slash ||
   glyphs[idx++] != type_asterisk
  )
   return 0
  for(;;)
  {
   var inner = match_nested_comment(idx)
   if(inner != 0)
    idx = inner 
   if(eof(idx))
    return match_token(type_unexpected_end_of_input, idx)
   if(
    glyphs[idx] == type_asterisk &&
    glyphs[idx + 1] == type_forward_slash
   )  
   {
    idx += 2 
    break
   }
   ++idx
  }
  return match_token(type_comment, idx)
 }

 function match_divide(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_equals)
   return match_token(type_divide_assign, idx + 1)
  else if(glyph == type_forward_slash)
  {
   while(glyphs[++idx] != type_newline)
    continue 
   return match_token(type_comment, idx)
  }
  else if(glyph == type_asterisk)
  {
   return match_nested_comment(idx - 1)
  }
  return match_token(type_divide, idx)
 }

 function match_minus(idx)
 {
  var number = match_number(idx)
  if(number)
   return number
  var
   glyph = glyphs[++idx]
  if(glyph == type_equals)
   return match_token(type_minus_assign, idx + 1)
  else if(glyph == type_minus)
   return match_token(type_minus_minus, idx + 1)
  return match_token(type_minus, idx)
 }

 function match_plus(idx)
 {
  var number = match_number(idx)
  if(number)
   return number
  var
   glyph = glyphs[++idx]
  if(glyph == type_equals)
   return match_token(type_plus_assign, idx + 1)
  else if(glyph == type_plus)
   return match_token(type_plus_plus, idx + 1)    
  return match_token(type_plus, idx)
 }

 function match_less(idx, token)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_equals)
   return match_token(type_less_or_equal, idx + 1)
  else if(glyph == type_less)
  {
   glyph = glyphs[idx + 1]
   if(glyph == type_equals)  
    return match_token(type_shift_left_assign, idx + 2)
   return match_token(type_shift_left, idx + 1)    
  }
  return match_token(type_less, idx)
 }

 function match_greater(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_equals)
   return match_token(type_greater_or_equal, idx + 1)
  else if(glyph == type_greater)
  {
   glyph = glyphs[idx + 1]
   if(glyph == type_equals)  
    return match_token(type_shift_right_assign, idx + 2)    
   return match_token(type_shift_right, idx + 1)    
  }
  return match_token(type_greater, idx)
 }

 function match_dot(idx)
 {
  var number = match_number(idx)
  if(number)
   return number  
  var
   glyph = glyphs[++idx]
  if(
   glyph == type_dot && 
   glyphs[idx + 1] == type_dot
  )
   return match_token(type_ellipsis, idx + 2)
  if(isdigit(glyph))
   return match_number(idx - 1)
  return match_token(type_dot, idx)    
 }

/*
 NOTE: newlines may not be coelesced on some platforms
*/
 
 function match_newlines(idx)
 {
  if(glyphs[idx] != type_newline)
   return 0    
  while(true)
   if(glyphs[++idx] != type_newline)
    break
  return match_token(type_newline, idx)
 }

 function match_spaces(idx)
 {
  while(glyphs[++idx] == type_space)
   continue
  return match_token(type_discardable, idx)
 }

 function match_tabs(idx)
 {
  while(glyphs[++idx] == type_tab)
   continue
  return match_token(type_discardable, idx)
 }

 function match_integer(idx)
 {
  if(!isdigit(glyphs[idx]))
   return null
  while(isdigit(glyphs[++idx]))
   continue
  return match_token(type_integer, idx)
 }

 function match_number(idx)
 { 
  var ise = false, sgn = glyphs[idx]
  if(sgn == type_minus || sgn == type_plus)
   ++idx
  var esc, rsc, isc = match_integer(idx)
  if(isc)
   idx = isc.index
  var dotted = (glyphs[idx] == type_dot)
  if(dotted)
  {
   rsc = match_integer(++idx)
   if(rsc)
    idx = rsc.index
  }
  if(!isc && !rsc)
   return null
  ise = (tolower(glyphs[idx]) == char("e"))
  if(ise)
  {
   sgn = glyphs[++idx]
   if(sgn == type_minus || sgn == type_plus)
    ++idx
   esc = match_integer(idx)
   if(!esc)
    return null
   idx = esc.index
  }
  return match_token(type_number, idx)
 }

 function match_digit_zero(idx)
 {
  var 
   type_X = char("X"),
   type_A = char("A"),
   type_F = char("F"),
   next = glyphs[++idx]
  if(isdigit(next))
  {
   while(isdigit(glyphs[++idx]))
    continue
   return match_token(type_octal, idx)
  }
  if(toupper(next) != type_X)
   return match_number(idx - 1)
  var
   start = idx
  for(;;)
  {
   next = toupper(glyphs[++idx])
   if(!isdigit(next) || next < type_A || next > type_F)
    break
  }
  if(idx == start)
   return match_undefined(idx)  
  return match_token(type_hex, idx)
 }

 function token_at(pos, len)
 {
  var res = ""
  var rmx = pos + len
  for(var rdx = pos; rdx < rmx; ++rdx)
   res += String.fromCharCode(glyphs[rdx])   
  return res
 }

/*
 Functions `signature` and `define` pulled 
 out of match_identifier for efficiency
*/

 function signature(text)
 { 
  var sig = 0, len = text.length
  if(len > magic)
   len = magic
  for(var tdx = 0; tdx < len; ++tdx)   
  {
   sig <<= 8
   sig += text.charCodeAt(tdx)
  }
  return sig
 }

 function define(tag, type)
 {
  var length = tag.length
  if(length > this.longest)
   this.longest = length
  this.push({ 
   tag : tag,
   hash: signature(tag),
   type: type,
   length: length 
  })    
 }

 function lookup(traits)
 {
  var keywords = this
  var tag = traits.tag, 
  hash = traits.hash, 
  length = traits.length
/*
 Simple binary search
*/
  var low = 0, hi = keywords.length - 1
  while(low <= hi)
  {
   var pvt = floor((hi + low) / 2),
    key = keywords[pvt],
    dif = key.hash - hash
   if(dif < 0)
    low = pvt + 1
   else if(dif > 0)
    hi = pvt - 1       
   else
   {
    if(length != key.length || tag != key.tag)
     return null
    return key     
   }
  }
  return null
 }
  
 function match_identifier(idx)
 {
  var start = idx
  var uds = char("_")
  while(true)
  {
   var ch = glyphs[++idx]
   if(!isdigit(ch) && !isalpha(ch) && ch != uds) 
    break
  }
  var keywords = match_identifier.keywords
  if(keywords == null)
  {
   keywords = match_identifier.keywords = []
   keywords.longest = 0 
   keywords.lookup = lookup
   keywords.define = define
   keywords.define("do", type_do)
   keywords.define("while", type_while)
   keywords.define("until", type_until)
   keywords.define("for", type_for)
   keywords.define("if", type_if)
   keywords.define("unless", type_unless)
   keywords.define("else", type_else)
   keywords.define("break", type_break)
   keywords.define("continue", type_continue)
   keywords.define("throw", type_throw)
   keywords.define("catch", type_catch)
   keywords.define("return", type_return)
   keywords.define("end", type_end)
   var comparison = function(left, right)
   { 
    return left.hash - right.hash 
   }
   keywords.sort(comparison) 
  }
/*
 Skip sequences longer than a keyword, 
 otherwise do a quick binary search... 
*/
  var length = idx - start,
   longest = keywords.longest
  if(length > longest) 
   return match_token(type_identifier, idx)
  var tag = token_at(start, length),
   hash = signature(tag), 
   traits = { tag: tag, hash: hash, length: length }
  var found = keywords.lookup(traits)
  if(found)
   return match_token(found.type, idx)
  return match_token(type_identifier, idx)
 }

 function matched(idx)
 {
  return match_token(glyphs[idx], idx + 1)
 }

 function match_undefined(idx)
 {
  return match_token(type_undefined, idx + 1)
 }

 function match_but_discard(idx)
 {
  return match_token(type_discardable, idx + 1)
 }

/*
 TODO: handle utf-8 
*/
 function match_specials(idx)
 {
  return match_undefined(idx)
 }

 var tokenizers = []
 for(var tdx = 0; tdx <= 255; ++tdx)
  tokenizers[tdx] = match_undefined
 tokenizers[type_equals] = match_equals
 tokenizers[type_not] = match_not
 tokenizers[type_at] = matched 
 tokenizers[type_modulus] = match_modulus
 tokenizers[type_caret] = match_caret
 tokenizers[type_and] = match_and
 tokenizers[type_or] = match_or
 tokenizers[type_times] = match_times
 tokenizers[type_minus] = match_minus
 tokenizers[type_plus] = match_plus
 tokenizers[type_divide] = match_divide
 tokenizers[type_less] = match_less
 tokenizers[type_greater] = match_greater
 tokenizers[type_dot] = match_dot
 tokenizers[type_tilde] = matched
 tokenizers[type_pound] = matched
 tokenizers[type_dollar] = matched
 tokenizers[type_left_paren] = matched
 tokenizers[type_right_paren] = matched
 tokenizers[type_left_brace] = matched
 tokenizers[type_right_brace] = matched
 tokenizers[type_left_bracket] = matched
 tokenizers[type_right_bracket] = matched
 tokenizers[type_comma] = matched
 tokenizers[type_colon] = matched
 tokenizers[type_conditional] = matched
 tokenizers[type_semicolon] = matched
 tokenizers[type_newline] = match_newlines
 tokenizers[type_carriage_return] = match_but_discard
 tokenizers[type_tab] = match_tabs
 tokenizers[type_space] = match_spaces
 tokenizers[type_single_quote] = match_single_quote
 tokenizers[type_double_quote] = match_double_quote
 tokenizers[char("0")] = match_digit_zero
 for(var idx = char("1"), imx = char("9"); idx <= imx; ++idx)
  tokenizers[idx] = match_number
 tokenizers[char("_")] = match_identifier
 for(var idx = char("a"), imx = char("z"); idx <= imx; ++idx)
  tokenizers[idx] = match_identifier
 for(var idx = char("A"), imx = char("Z"); idx <= imx; ++idx)
  tokenizers[idx] = match_identifier

/*
 WORKAROUND: cosi's text_to_array is too slow
 ...use non-utf-8 shim until that gets fixed 
*/

 function text_to_array(text)
 {
  var byte, bdx = 0, 
   len = text.length, 
   res = new Array(len),
   bytes = text_to_bytes(text)
  while(true)
  { 
   byte = get_byte(bytes, bdx)
   if(byte == 0)
    break
   res[bdx++] = byte
  }
  free(bytes)
  if(bdx != text.length)
   throw new Error("UTF-8 no yet supported")
  return res
 } 

 function tokenize(text)
 {
  var tokens = [], current = 0
  glyphs = tokens.glyphs = text_to_array(text)
  var count = glyphs.length
  while(current < count)
  {
   var glyph = glyphs[current]
   var scan = (glyph >= 256) ? 
    match_specials : tokenizers[glyph]
   var result = scan(current)
   if(!result)
    result = match_undefined(current)
   var type = result.type 
   var length = result.index - current
   if(type != type_discardable)
    tokens.push({
     type : type,
     index : current, 
     length : length
    })
   current += length
  }
  return tokens
 }

/*
 ...WIP...
*/

function parse(input)
{
 if(input instanceof String)
  return parse(tokenize(input))
 print("Parser")
}

/*
 Test
*/

function process(file)
{
 var text = file_to_text(file)
 if(text == null)
  return print("Error: cannot open file '", file, "'")
 var start = clock()
 var tokens = tokenize(text)
 var elapsed = (clock() - start) / CLOCKS_PER_SEC
 var tokens = parse(tokens)
 for(var tdx = 0, len = tokens.length; tdx < len; ++tdx)
 {
  var token = tokens[tdx],
   type = token.type,
   label = type_to_text(type),
   isn = (type == type_newline), 
   text = isn ? "\\n" : token_at(token.index, token.length)
  print(label, "...", text)
 } 
 print
 (
  "-", text.length, "glyphs processed in", 
  elapsed, "seconds -"
 )
}

contain(function(){
 var  args = script_arguments()
 if(args.length == 0)
  return print("TORK:", script_path(), "[files...]")
 for(var idx in args)
 {
  var arg = args[idx]
  print(arg)
  process(arg)
 }
})
