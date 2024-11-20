//
// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// <<Areesh Nadeem>>
// CS341, Fall 2024
// netID: anade2
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  
  // Checks to see if the literal string starts with the given pattern
  let private beginsWith (pattern: string) (literal: string) = 
    literal.StartsWith (pattern)

  

  // If the next token begins with the given pattern,
  // then it is valid to match and <tokens> can be updated
  // so we just return the tail
  let rec private matchTokenSpecial pattern tokens =
    let next_token = List.head tokens

    if beginsWith pattern next_token then  
      List.tail tokens
    else
      failwith ("expecting " + pattern + ", but found " + next_token)



  // Matches a ';' character and returns updated <tokens>
  let private empty tokens = 
    matchToken ";" tokens
     


  // Matches a "int identifier;" case and returns updated <tokens>
  let private vardecl tokens = 
    let T2 = matchToken "int" tokens

    // special case: the token must contain "identifier"
    let T3 = matchTokenSpecial "identifier" T2
    matchToken ";" T3



  // Matches a "cin >> identifier" case and returns updated <tokens>
  let private input tokens = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2

    // special case: the token must contain "identifier"
    let T4 = matchTokenSpecial "identifier" T3
    matchToken ";" T4



  // Matches a "cout << <output-value>" case and returns updated <tokens>
  let rec private output tokens = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    matchToken ";" T4



  // Matches a "<expr-value> || endl" case, returns updated <tokens>
  and private output_value tokens = 
    let next_token = List.head tokens

    // endl case
    if next_token = "endl" then matchToken "endl" tokens

    // <expr-value> case
    else expr_value next_token tokens



  // Matches the given token to one of the expr value patterns
  // and updates <tokens> accordingly
  and private expr_value token tokens = 
    if beginsWith "identifier" token then
      matchTokenSpecial "identifier" tokens

    elif beginsWith "int_literal" token then
      matchTokenSpecial "int_literal" tokens

    elif beginsWith "str_literal" token then
      matchTokenSpecial "str_literal" tokens

    elif token = "true" then
      matchToken "true" tokens

    elif token = "false" then
      matchToken "false" tokens
    
    // none match, error
    else failwith ("expecting identifier or literal, but found " + token)


  
  // Matches <expr> case
  // returns updated <tokens>
  and private condition tokens = 
    expr tokens


  // Matches the <expr-value> <expr-op> <expr-value> || <expr-value> case
  // returns updated <tokens>
  and private expr tokens = 
    // match the expr value
    let T2 = expr_value (List.head tokens) tokens

    // if there is a expr-op, then match it and the next expr-value
    if check_op T2 = true then 
      let T3 = expr_op T2
      expr_value (List.head T3) T3

    else T2 // we are at the only <expr-value> case



  // Matches a expr operator
  // return the updated <tokens>
  and private expr_op tokens = 
    let next_token = List.head tokens

    match next_token with
    | "+" -> matchToken "+" tokens
    | "-" -> matchToken "-" tokens
    | "*" -> matchToken "*" tokens
    | "/" -> matchToken "/" tokens
    | "^" -> matchToken "^" tokens
    | "<" -> matchToken "<" tokens
    | "<=" -> matchToken "<=" tokens
    | ">" -> matchToken ">" tokens
    | ">=" -> matchToken ">=" tokens
    | "==" -> matchToken "==" tokens
    | "!=" -> matchToken "!=" tokens
    | _ -> failwith ("expecting expression operator, but found " + next_token)



  // Checks to see if the next token is possibly a operator
  // returns true if it is an expr op, else false
  and private check_op tokens = 
    let next_token = List.head tokens

    // token is a valid op
    if next_token = "+" || next_token = "-" || next_token = "*" || next_token = "/" || 
       next_token = "^" || next_token = "<" || next_token = "<=" || next_token = ">" || 
       next_token = ">=" || next_token = "==" || next_token = "!=" then
        true

    else false // not a valid op


  
  // Matches a "identifier = <expr>" case
  // returns updated <tokens>
  let private assignment tokens = 
    // special case: the token must contain "identifier" 
    let T2 = matchTokenSpecial "identifier" tokens

    let T3 = matchToken "=" T2

    // match the <expr> part
    let T4 = expr T3

    matchToken ";" T4



  // Logic for <stmt>
  // Chooses a path depending on the next token
  let rec private stmt tokens = 
    let next_token = List.head tokens

    // find matching path
    match next_token with
      | ";" -> empty tokens            // <empty>
      | "int" -> vardecl tokens        // <vardecl>
      | "cin" -> input tokens          // <input>
      | "cout" -> output tokens        // <output>
      | "if" -> ifstmt tokens          // <ifstmt>
      | _ -> 
        if beginsWith "identifier" next_token then 
          assignment tokens  // <assignment>

        else failwith ("expecting statement, but found " + next_token)




  // Matches a <stmt> for the then-part
  // returns updated <tokens>
  and private then_part tokens = 
    stmt tokens



  // Matches "else <stmt>" case || epsilon case
  // returns updated <tokens>
  and private else_part tokens = 
    let next_token = List.head tokens

    // if the next token is "else"
    // then match it, and match a <stmt>
    if next_token = "else" then 
      let T2 = matchToken "else" tokens
      stmt T2

    else tokens // epsilon case



  // Matches an <ifstmt> and returns updated <tokens>
  and private ifstmt tokens = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2

    // match the <condition>
    let T4 = condition T3

    let T5 = matchToken ")" T4

    // match the <then-part>
    let T6 = then_part T5

    // match the <else-part>
    else_part T6



  // Sees of an additional <stmt> is possible
  // If not, can just return <tokens> as it is optional
  let rec private morestmts tokens = 
    let next_token = List.head tokens

    // if next_token is possibly a <stmt> then call <stmt>
    if next_token = ";" || next_token =  "int" || next_token =  "cin" ||
       next_token = "cout" || next_token = "if" || beginsWith "identifier" next_token then
        let T2 = stmt tokens
        morestmts T2 // check for more stmts

    else tokens // epsilon case



  // Recurse through <stmts>
  // stmts can lead to a <stmt> or <morestmts>
  let private stmts tokens = 
      let T2 = stmt tokens
      let T3 = morestmts T2
      T3



  //
  // simpleC
  //
  let private simpleC tokens = 

    // match all the required basic syntax for valid simpleC
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5

    // Start parsing and consuming all the stmts
    let T7 = stmts T6

    // match the end of the program
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message
