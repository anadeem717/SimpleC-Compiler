# SimpleC Compiler  

## Overview  
This project is a parser for SimpleC programs, implemented in F#. The parser checks the syntax of input programs to ensure they conform to the rules of the SimpleC language. It processes tokens and validates their order and structure according to a defined grammar.  

### Features:  
- **Syntax Validation**: The parser verifies if the input tokens adhere to SimpleC's syntax.  
- **Error Reporting**: On syntax errors, it returns descriptive messages indicating the issue.  
- **Token Matching**: Supports token pattern recognition for keywords, identifiers, literals, operators, and statements.  

### Components:  

1. **Core Functions**:  
    - `matchToken`: Matches an exact token with the expected one.  
    - `matchTokenSpecial`: Matches tokens based on specific patterns (e.g., identifiers).  
    - `beginsWith`: Utility function to check if a token starts with a given string.  

2. **Statements and Expressions**:  
    - `stmt`, `vardecl`, `input`, `output`, `assignment`, and others handle specific statement types.  
    - Recursive functions like `expr`, `expr_value`, and `condition` evaluate expressions.  

3. **Control Flow**:  
    - Supports `if` statements with optional `else` parts using `ifstmt`, `then_part`, and `else_part`.  
    - Parses sequences of statements via `stmts` and `morestmts`.  

4. **Entry Point**:  
    - `simpleC`: Validates the structure of a complete SimpleC program, ensuring proper start (`void main()`) and end (`}` with `$`).  
    - `parse`: Parses a list of tokens and returns either `"Success!"` or a detailed syntax error.  

---

## Getting Started  

### Prerequisites:  
- **F# Compiler**: Ensure you have an F# environment installed. For example, [dotnet SDK](https://dotnet.microsoft.com/download).  

### File Structure:  
- `parser.fs`: Contains the parser module implementation.  

--- 

Created by:  
**Areesh Nadeem**  
**University of Illinois Chicago, Fall 2024**  
**CS 341: Programming Language Design**
