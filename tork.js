 function char(text)
 {
  return text.charCodeAt(0)
 }

 var
  slot = 128,
  type_undefined = slot++,
  type_end_of_input = slot++,
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
  type_xor = char("^"),
  type_xor_assign = slot++,
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
  type_minus_assign = slot++,
  type_times = type_asterisk,
  type_times_assign = slot++,
  type_divide = type_forward_slash,
  type_divide_assign = slot++,
  type_modulus = char("%"),
  type_modulus_assign = slot++,
  type_equal_assign = char("="),
  type_equal = slot++,
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
  type_octal = slot++,
  type_integer = slot++,
  type_hex = slot++,
  type_real = slot++,
  type_scientific = slot++,
  digit = 0x30,
  type_digit_zero = digit++,
  type_digit_one = digit++,
  type_digit_two = digit++,
  type_digit_three = digit++,
  type_digit_four = digit++,
  type_digit_five = digit++,
  type_digit_six = digit++,
  type_digit_seven = digit++,
  type_digit_eight = digit++,
  type_digit_nine = digit++,
  type_placeholder

 var
  glyphs,
  typed 

 function type_to_text(type)
 {
  if(!type_to_text.tab)
  {
   var
    tab = []
   loop(256, function(idx)
   {
    tab[idx] = "UNDEFINED_TOKEN"
   })
   tab[type_identifier] = "identifier"
   tab[type_integer] = "integer"
   tab[type_octal] = "octal"
   tab[type_hex] = "hex"
   tab[type_real] = "real"
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
   tab[type_xor] = "xor"
   tab[type_xor_assign] = "xor_assign"
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
   tab[type_equal_assign] = "equal_assign"
   tab[type_equal] = "equal"
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
   type_to_text.tab = tab  
  }
  return type_to_text.tab[type]
 }
 
 function match_token(type, index)
 {
  typed = type
  return index
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
 
 function match_equal(idx)
 {
  if(glyphs[++idx] == type_equal_assign)
   return match_token(type_equal, idx + 1)
  return match_token(type_equal_assign, idx)
 }

 function match_not(idx)
 {
  if(glyphs[++idx] == type_equal_assign)
   return match_token(type_not_equal, idx + 1)
  return match_token(type_not, idx)
 }

 function match_modulus(idx)
 {
  if(glyphs[++idx] == type_equal_assign)
   return match_token(type_modulus_assign, idx + 1)
  return match_token(type_modulus, idx)
 }

 function match_xor(idx)
 {
  if(glyphs[++idx] == type_equal_assign)
   return match_token(type_xor_assign, idx + 1)
  return match_token(type_xor, idx)
 }

 function match_and(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_and)
   return match_token(type_and_logical, idx + 1)
  else if(glyph == type_equal_assign)
   return match_token(type_and_assign, idx + 1)    
  return match_token(type_and, idx)
 }

 function match_or(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_or)
   return match_token(type_or_logical, idx + 1)
  else if(glyph == type_equal_assign)
   return match_token(type_or_assign, idx + 1)    
  return match_token(type_or, idx)
 }

 function match_times(idx)
 {
  if(glyphs[++idx] == type_equal_assign)
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
  if(glyph == type_equal_assign)
   return match_token(type_divide_assign, idx + 1)
  else if(glyph == type_forward_slash)
  {
   while(!crlf(++idx))
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
  var
   glyph = glyphs[++idx]
  if(glyph == type_equal_assign)
   return match_token(type_minus_assign, idx + 1)
  else if(glyph == type_minus)
   return match_token(type_minus_minus, idx + 1)
  return match_token(type_minus, idx)
 }

 function match_plus(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_equal_assign)
   return match_token(type_plus_assign, idx + 1)
  else if(glyph == type_plus)
   return match_token(type_plus_plus, idx + 1)    
  return match_token(type_plus, idx)
 }

 function match_less(idx, token)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_equal_assign)
   return match_token(type_less_or_equal, idx + 1)
  else if(glyph == type_less)
  {
   glyph = glyphs[idx + 1]
   if(glyph == type_equal_assign)  
    return match_token(type_shift_left_assign, idx + 2)
   return match_token(type_shift_left, idx + 1)    
  }
  return match_token(type_less, idx)
 }

 function match_greater(idx)
 {
  var
   glyph = glyphs[++idx]
  if(glyph == type_equal_assign)
   return match_token(type_greater_or_equal, idx + 1)
  else if(glyph == type_greater)
  {
   glyph = glyphs[idx + 1]
   if(glyph == type_equal_assign)  
    return match_token(type_shift_right_assign, idx + 2)    
   return match_token(type_shift_right, idx + 1)    
  }
  return match_token(type_greater, idx)
 }

 function match_dot(idx)
 {
  var
   glyph = glyphs[++idx]
  if(
   glyph != type_dot || 
   glyphs[idx + 1] != type_dot
  )
   return match_token(type_dot, idx)
  return match_token(type_ellipsis, idx + 2)    
 }

 function crlf(idx)
 {
  var glyph = glyphs[idx]
  return (glyph == type_newline) ||
   (glyph == type_linefeed)
 }

 function match_newlines(idx)
 {
  if(!crlf(idx))
   return 0    
  while(true)
   if(!crlf(++idx))
    break
  return match_token(type_newline, idx)
 }

 function match_spaces(idx)
 {
  while(isspace(glyphs[++idx]))
   continue
  return match_token(type_space, idx)
 }

 function match_tabs(idx)
 {
  while(glyphs[++idx] == type_tab)
   continue
  return match_token(type_tab, idx)
 }

 function match_integer(idx)
 {
  while(isdigit(glyphs[++idx]))
   continue
  return match_token(type_integer, idx)
 }

 function match_digit_zero(idx)
 {
  var 
   type_X = 0x58,
   type_A = 0x41,
   type_F = 0x46,
   next = glyphs[++idx]
  if(isdigit(next))
  {
   while(isdigit(glyphs[++idx]))
    continue
   return match_token(type_octal, idx)
  }
  else if(toupper(next) != type_X)
   return match_integer(idx - 1)
  var
   start = idx
  for(;;)
  {
   next = toupper(glyphs[++idx])
   if(!isdigit(next) && (next < type_A || next > type_F))
    break
  }
  if(idx == start)
   return 0  
  return match_token(type_hex, idx)
 }

 function matched(idx)
 {
  return match_token(glyphs[idx], idx + 1)
 }

 var
  tokenizers = []
 tokenizers[type_digit_zero] = match_digit_zero
 tokenizers[type_digit_one] = match_integer
 tokenizers[type_digit_two] = match_integer
 tokenizers[type_digit_three] = match_integer
 tokenizers[type_digit_four] = match_integer
 tokenizers[type_digit_five] = match_integer
 tokenizers[type_digit_six] = match_integer
 tokenizers[type_digit_seven] = match_integer
 tokenizers[type_digit_eight] = match_integer
 tokenizers[type_digit_nine] = match_integer
 tokenizers[type_equal_assign] = match_equal
 tokenizers[type_not] = match_not
 tokenizers[type_at] = matched // TODO
 tokenizers[type_modulus] = match_modulus
 tokenizers[type_xor] = match_xor
 tokenizers[type_and] = match_and
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
 tokenizers[type_tab] = match_tabs
 tokenizers[type_space] = match_spaces
 tokenizers[type_single_quote] = match_single_quote
 tokenizers[type_double_quote] = match_double_quote

 function tokenize(text)
 {
  var tokens = [],
   current = 0,
   unidentified = -1,
   identifier_index = unidentified
  glyphs = text_to_array(text)
  glyphs.push(type_newline)
  var count = glyphs.length
  tokens.glyphs = glyphs
  while(current < count)
  {
   var glyph = glyphs[current],
    scan = tokenizers[glyph],
    marked = (identifier_index != unidentified)
   if(!scan)
   {
    if(!marked)
      identifier_index = current
    ++current
    continue
   }
   var
    length = scan(current) - current
   if(marked)
   {
    tokens.push({
     type : type_identifier,
     index : identifier_index, 
     length : current - identifier_index
    })
    identifier_index = unidentified 
   } 
   if(typed != type_space)
   {
    tokens.push({
     type : typed,
     index : current, 
     length : length
    })
   }
   current += length
  }
  if(identifier_index != unidentified)
   tokens.push({
    type : type_identifier,
    index : identifier_index, 
    length : count - identifier_index
   })
  return tokens
 }

function process(file)
{
 var text = file_to_text(file)
 if(text == null)
 {  
  display("Error: cannot open file '", file, "'")
  return
 }
 var tokens = tokenize(text), 
  len = tokens.length
 for(var tdx = 0; tdx < len; ++tdx)
 {
  var token = tokens[tdx],
   type = token.type,
   text =
    array_to_text(tokens.glyphs.slice
    (
     token.index, 
     token.index + token.length 
    ))
  display(type_to_text(type), "(", token.index, token.length, ") :", text)
 } 
}

escape(function(){
 display("Tork!")
 display("Usage:", script_path(), "[files...]")
 var 
  set = {},
  args = script_arguments()
 loop(args, function(idx){
  var arg = args[idx]
  display(arg)
  process(arg)
 })
 display("Done!")
})
