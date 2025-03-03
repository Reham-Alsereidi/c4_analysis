// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>    // Standard I/O functions
#include <stdlib.h>   // Memory allocation, exit function
#include <memory.h>   // Memory operations like memset, memcmp
#include <unistd.h>   // POSIX API for file operations
#include <fcntl.h>    // File control options
#define int long long // Using 64-bit integers for all int types

// Global variables for compiler state
char *p, *lp,  // p: current position in source code, lp: line position for error reporting
     *data;    // Pointer to data/bss section in memory

int *e, *le,   // e: current position in emitted code, le: last emitted instruction
    *id,       // Currently parsed identifier
    *sym,      // Symbol table - a simple list of identifiers
    tk,        // Current token
    ival,      // Current token value (for numbers, strings)
    ty,        // Current expression type
    loc,       // Local variable offset from base pointer
    line,      // Current line number for error reporting
    src,       // Flag to print source and assembly output
    debug;     // Flag to print executed VM instructions

// Token types and classes (enum values chosen to simplify parsing)
// Tokens above 128 are multi-character tokens
enum {
  // Values for single character tokens are their ASCII values
  // Complex tokens start at 128
  Num = 128, Fun, Sys, Glo, Loc, Id,  // Value types
  Char, Else, Enum, If, Int, Return, Sizeof, While,  // Keywords
  // Operators in order of precedence (lowest to highest)
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// Virtual machine instruction set
// These are the operations for the VM that will run the compiled code
enum { 
  LEA,IMM,JMP,JSR,BZ,BNZ,ENT,ADJ,LEV,LI,LC,SI,SC,PSH,  // VM control instructions
  OR,XOR,AND,EQ,NE,LT,GT,LE,GE,SHL,SHR,ADD,SUB,MUL,DIV,MOD,  // Arithmetic and logic
  OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT  // System calls
};

// C type system - extremely minimal
enum { CHAR, INT, PTR };  // Type indicators

// Symbol table entry structure
// Since we can't create a struct, we use offsets in an array
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };
// Tk: token type
// Hash: hash value for quick symbol lookup
// Name: pointer to identifier name in source
// Class: variable class (global, local, etc)
// Type: data type
// Val: value or address
// HClass, HType, HVal: used to save local symbol info when parsing function parameters

/**
 * next() - lexical analyzer (tokenizer)
 * 
 * Reads the next token from source code into the global variable tk.
 * Also updates other globals like ival for numeric values.
 */
void next()
{
  char *pp;  // Temporary pointer for token processing

  while (tk = *p) {  // Assign current character to tk and check if not 0
    ++p;  // Move to next character
    
    if (tk == '\n') {    // Handle newlines - for error reporting and source listing
      if (src) {  // If source listing is enabled
        printf("%d: %.*s", line, p - lp, lp);    // Print source line with line number
        lp = p;
        // Print any VM instructions generated for this line
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line;  // Increment line counter
    }
    // Skip preprocessor directives (#include, #define, etc.)
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;  // Skip until end of line
    }
    // Parse identifiers and keywords
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;  // Point to start of identifier
      
      // Read the complete identifier
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || 
             (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;  // Simple hash calculation
      
      // Calculate final hash and identifier length
      tk = (tk << 6) + (p - pp);
      
      // Look up the identifier in symbol table
      id = sym;
      while (id[Tk]) {  // Walk through symbol table
        // If tokens match (by hash and string comparison)
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { 
          tk = id[Tk];  // Use the existing token type
          return; 
        }
        id = id + Idsz;  // Move to next symbol entry
      }
      
      // If not found, create new symbol table entry
      id[Name] = (int)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;  // Token type is Id (identifier)
      return;
    }
    // Parse numeric literals (decimal, hex, octal)
    else if (tk >= '0' && tk <= '9') {
      // Decimal numbers
      if (ival = tk - '0') {  // Non-zero starting digit (decimal)
        while (*p >= '0' && *p <= '9') 
          ival = ival * 10 + *p++ - '0';  // Parse decimal number
      } 
      // Hex numbers (0x...) or octal numbers (0...)
      else if (*p == 'x' || *p == 'X') {  // Hex number
        while ((tk = *++p) && 
              ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);  // Parse hex digits
      }
      else {  // Octal number
        while (*p >= '0' && *p <= '7') 
          ival = ival * 8 + *p++ - '0';  // Parse octal digits
      }
      tk = Num;  // Set token type to number
      return;
    }
    // Handle division operator or comments
    else if (tk == '/') {
      if (*p == '/') {  // Line comment
        ++p;
        while (*p != 0 && *p != '\n') ++p;  // Skip until end of line
      }
      else {  // Division operator
        tk = Div;
        return;
      }
    }
    // Parse string and character literals
    else if (tk == '\'' || tk == '"') {
      pp = data;  // Store string in data segment
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {  // Handle escape sequences
          if ((ival = *p++) == 'n') ival = '\n';  // Only \n is supported
        }
        if (tk == '"') *data++ = ival;  // Store string characters
      }
      ++p;  // Skip closing quote
      if (tk == '"') ival = (int)pp;  // For strings, value is pointer to data
      else tk = Num;  // For char literals, value is the character's ASCII value
      return;
    }
    // Parse multi-character operators
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    
    // Single-character operators return their token value directly
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    
    // Other single-character tokens return as themselves
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

/**
 * expr() - expression parser and code generator
 * Parses expressions using recursive descent with precedence climbing.
 * Generates VM code for the expressions as it parses.
 * @param lev The operator precedence level to start parsing from
 */
void expr(int lev)
{
  int t, *d;  // t: temporary type storage, d: symbol pointer or address for jumps

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }    // Handle unexpected EOF
  // Parse numeric literals
  else if (tk == Num) { 
    *++e = IMM; *++e = ival;  // Generate immediate load instruction
    next();  // Move to next token
    ty = INT;  // Type is integer
  }
  // Parse string literals
  else if (tk == '"') {
    *++e = IMM; *++e = ival;  // Store string pointer
    next();
    while (tk == '"') next();  // Handle multiple adjacent strings (concat)
    // Align data pointer to int boundary
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); 
    ty = PTR;  // Type is pointer (to string)
  }
  // Parse sizeof operator
  else if (tk == Sizeof) {
    next(); 
    if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT;  // Default type is INT
    if (tk == Int) next(); 
    else if (tk == Char) { next(); ty = CHAR; }
    // Handle pointer types with multiple stars
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    
    // Generate immediate with size of type
    *++e = IMM; 
    *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
    ty = INT;  // sizeof always returns an integer
  }
  // Parse identifiers (variables, functions, etc.)
  else if (tk == Id) {
    d = id;  // Save the identifier symbol pointer
    next();
    
    // Function call
    if (tk == '(') {
      next();
      t = 0;  // Count arguments
      
      // Parse arguments
      while (tk != ')') { 
        expr(Assign);  // Each argument is an expression
        *++e = PSH;    // Push argument onto stack
        ++t;           // Increment argument count
        if (tk == ',') next();  // Move past comma between arguments
      }
      next();  // Move past closing paren
      
      // System call
      if (d[Class] == Sys) *++e = d[Val];
      // User-defined function call
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      else { printf("%d: bad function call\n", line); exit(-1); }
      
      // Clean up stack after call
      if (t) { *++e = ADJ; *++e = t; }
      ty = d[Type];  // Function return type
    }
    // Enumeration constant 
    else if (d[Class] == Num) { 
      *++e = IMM; *++e = d[Val];  // Immediate value
      ty = INT;
    }
    // Variable reference
    else {
      // Local variable - calculate address relative to base pointer
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      // Global variable - use absolute address
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
      else { printf("%d: undefined variable\n", line); exit(-1); }
      
      // Load variable value based on its type
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  // Type cast or parenthesized expression
  else if (tk == '(') {
    next();
    // Type cast
    if (tk == Int || tk == Char) {
      t = (tk == Int) ? INT : CHAR;  // Set target type
      next();
      // Handle pointer types
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);  // Parse expression after cast
      ty = t;     // Set type to cast type
    }
    // Regular parenthesized expression
    else {
      expr(Assign);  // Parse expression
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  // Pointer dereference
  else if (tk == Mul) {
    next(); 
    expr(Inc);  // Parse expression (address to dereference)
    
    // Can only dereference pointers
    if (ty > INT) ty = ty - PTR; 
    else { printf("%d: bad dereference\n", line); exit(-1); }
    
    // Load value from address based on type
    *++e = (ty == CHAR) ? LC : LI;
  }
  // Address-of operator
  else if (tk == And) {
    next();
    expr(Inc);  // Parse expression
    
    // If the expression is already loading a value, convert to just the address
    if (*e == LC || *e == LI) --e; 
    else { printf("%d: bad address-of\n", line); exit(-1); }
    
    ty = ty + PTR;  // Type is pointer to original type
  }
  // Logical not operator
  else if (tk == '!') { 
    next(); 
    expr(Inc);  // Parse expression
    // Generate code to compare with 0
    *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; 
    ty = INT;  // Result is always integer
  }
  // Bitwise not operator
  else if (tk == '~') { 
    next(); 
    expr(Inc);  // Parse expression
    // Generate code to XOR with -1 (all bits set)
    *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; 
    ty = INT;  // Result is always integer
  }
  // Unary plus (no effect)
  else if (tk == Add) { 
    next(); 
    expr(Inc);  // Parse expression
    ty = INT;   // Force type to INT
  }
  // Unary minus
  else if (tk == Sub) {
    next();
    *++e = IMM;
    // Special case for negating a literal number
    if (tk == Num) { 
      *++e = -ival; 
      next(); 
    } 
    // Otherwise, multiply by -1
    else { 
      *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; 
    }
    ty = INT;  // Result is always integer
  }
  // Pre-increment/decrement
  else if (tk == Inc || tk == Dec) {
    t = tk;  // Save whether Inc or Dec
    next();
    expr(Inc);  // Parse expression (variable to modify)
    
    // Must be an lvalue (something we can store to)
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    
    // Generate code to add/subtract 1 and store back
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);  // Increment by type size for pointers
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;  // Store based on type
  }
  // Error for unrecognized expression start
  else { printf("%d: bad expression\n", line); exit(-1); }

  // "Precedence climbing" or "Top Down Operator Precedence" method
  // Continue parsing operators as long as their precedence is >= the current level
  while (tk >= lev) { 
    t = ty;  // Save expression type
    
    // Assignment operator
    if (tk == Assign) {
      next();
      // Must have an lvalue on the left
      if (*e == LC || *e == LI) *e = PSH; 
      else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      
      expr(Assign);  // Parse right side expression
      // Store result based on variable type
      *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    // Conditional/ternary operator (x ? y : z)
    else if (tk == Cond) {
      next();
      // Generate branch if zero to skip true branch
      *++e = BZ; d = ++e;
      expr(Assign);  // Parse true branch
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      // Jump past false branch after executing true branch
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);  // Parse false branch
      *d = (int)(e + 1);  // Patch jump address
    }
    // Logical OR
    else if (tk == Lor) { 
      next(); 
      // Short-circuit: if left is true, jump to end
      *++e = BNZ; d = ++e; 
      expr(Lan);  // Parse right operand
      *d = (int)(e + 1);  // Patch jump address
      ty = INT;  // Result is integer
    }
    // Logical AND
    else if (tk == Lan) { 
      next(); 
      // Short-circuit: if left is false, jump to end
      *++e = BZ; d = ++e; 
      expr(Or);  // Parse right operand
      *d = (int)(e + 1);  // Patch jump address
      ty = INT;  // Result is integer
    }
    // Bitwise operations and comparisons
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    
    // Addition (with special handling for pointer arithmetic)
    else if (tk == Add) {
      next();
      *++e = PSH; 
      expr(Mul);  // Parse right operand
      
      // If left is a pointer, scale the right side by the pointed type size
      if ((ty = t) > PTR) { 
        *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;
      }
      *++e = ADD;
    }
    // Subtraction (with special handling for pointer arithmetic)
    else if (tk == Sub) {
      next();
      *++e = PSH; 
      expr(Mul);  // Parse right operand
      
      // Pointer subtraction: divide by type size to get count
      if (t > PTR && t == ty) { 
        *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; 
        ty = INT;
      }
      // Pointer arithmetic: scale the right side
      else if ((ty = t) > PTR) { 
        *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB;
      }
      else *++e = SUB;  // Regular subtraction
    }
    // Multiplication, division, and modulo
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    
    // Post-increment/decrement (++x, --x)
    else if (tk == Inc || tk == Dec) {
      // Must be an lvalue
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      
      // Push original value, then add/subtract and store back
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      
      // Add/subtract from value on stack to undo for return value
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      
      next();
    }
    // Array subscript [index]
    else if (tk == Brak) {
      next();
      *++e = PSH;     // Save array address
      expr(Assign);  // Parse index expression
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      
      // Handle pointer arithmetic - scale index by element size
      if (t > PTR) { 
        *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;
      }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      
      *++e = ADD;  // Add index to array base
      // Load value at the calculated address
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

/**
 * stmt() - statement parser and code generator
 * 
 * Parses and generates code for C statements.
 * Handles if, while, return, blocks, and expression statements.
 */
void stmt()
{
  int *a, *b;  // Jump addresses for patching

  // If statement
  if (tk == If) {
    next();
    // Parse condition in parentheses
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);  // Parse condition expression
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    
    // Generate branch if zero to skip if-block
    *++e = BZ; b = ++e;
    stmt();  // Parse if block
    
    // Handle else block
    if (tk == Else) {
      // Add jump at end of if-block to skip else-block
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();  // Parse else block
    }
    
    // Patch jump target with current address
    *b = (int)(e + 1);
  }
  // While loop
  else if (tk == While) {
    next();
    a = e + 1;  // Save loop start address for conditional jump back
    
    // Parse condition in parentheses
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);  // Parse condition expression
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    
    // Generate branch if zero to exit loop
    *++e = BZ; b = ++e;
    stmt();  // Parse loop body
    
    // Jump back to condition
    *++e = JMP; *++e = (int)a;
    // Patch exit address
    *b = (int)(e + 1);
  }
  // Return statement
  else if (tk == Return) {
    next();
    // Optional expression to return a value
    if (tk != ';') expr(Assign);
    // Generate function exit code
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  // Block of statements
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();  // Parse statements until closing brace
    next();
  }
  // Empty statement
  else if (tk == ';') {
    next();
  }
  // Expression statement
  else {
    expr(Assign);  // Parse expression
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

/**
 * main() - compiler driver and VM interpreter
 * Handles initialization, compilation, and runs the virtual machine.
 * @param argc Command line argument count
 * @param argv Command line arguments
 * @return Exit status code
 */
int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  // Process command line arguments
  --argc; ++argv;
  // Check for source printing flag (-s)
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  // Check for debug flag (-d)
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  // Ensure we have a file to compile
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  // Open the source file
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  // Allocate memory for various compiler components
  poolsz = 256*1024; // 256KB - arbitrary size for memory pools
  // Allocate symbol table
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  // Allocate text segment (for compiled VM instructions)
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  // Allocate data segment (for variables and constants)
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  // Allocate stack for VM execution
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  // Initialize memory areas to zero
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // Initialize symbol table with keywords, library functions, and types
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  
  // Add keywords to symbol table (Char through While)
  i = Char; while (i <= While) { next(); id[Tk] = i++; }
  
  // Add system calls to symbol table (OPEN through EXIT)
  // These are built-in functions the generated code can call
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; }
  
  // Handle special types
  next(); id[Tk] = Char; // Add void type
  next(); idmain = id;   // Keep track of main function for entry point

  // Allocate and read source code into memory
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0; // Null-terminate the source code
  close(fd);

  // Begin compilation process
  line = 1;
  next(); // Get first token
  
  // Parse declarations (global variables and functions)
  while (tk) {
    bt = INT; // Default base type is int
    
    // Parse type specifiers
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0; // Initialize enum value
        
        // Process enum members
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          
          // Handle explicit enum values
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival; // Set enum value to explicit value
            next();
          }
          
          // Add enum member to symbol table
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    
    // Parse variable and function declarations
    while (tk != ';' && tk != '}') {
      ty = bt; // Start with base type
      
      // Handle pointers (each * adds one PTR level to type)
      while (tk == Mul) { next(); ty = ty + PTR; }
      
      // Expect identifier
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      
      // Save type information
      id[Type] = ty;
      
      // Function definition
      if (tk == '(') {
        id[Class] = Fun;       // Mark as function
        id[Val] = (int)(e + 1); // Store address in VM code
        next(); i = 0;         // Reset parameter count
        
        // Parse function parameters
        while (tk != ')') {
          ty = INT; // Default parameter type
          
          // Parse parameter type
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          
          // Handle pointer parameters
          while (tk == Mul) { next(); ty = ty + PTR; }
          
          // Expect parameter name
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          
          // Save old identifier information and update
          id[HClass] = id[Class]; id[Class] = Loc;  // Parameters are local variables
          id[HType]  = id[Type];  id[Type] = ty;    // Save parameter type
          id[HVal]   = id[Val];   id[Val] = i++;    // Assign parameter index
          
          next();
          if (tk == ',') next();
        }
        next();
        
        // Expect function body
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i; // First local variable will be at index i
        next();
        
        // Parse local variable declarations
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          
          // Process variables of this type
          while (tk != ';') {
            ty = bt;
            
            // Handle pointer local variables
            while (tk == Mul) { next(); ty = ty + PTR; }
            
            // Expect local variable name
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            
            // Save old identifier information and update
            id[HClass] = id[Class]; id[Class] = Loc;      // Mark as local
            id[HType]  = id[Type];  id[Type] = ty;        // Set variable type
            id[HVal]   = id[Val];   id[Val] = ++i;        // Assign stack offset
            
            next();
            if (tk == ',') next();
          }
          next();
        }
        
        // Emit function entry code (allocate stack space for locals)
        *++e = ENT; *++e = i - loc;
        
        // Parse function body statements
        while (tk != '}') stmt();
        
        // Emit function exit code
        *++e = LEV;
        
        // Restore global symbol table (remove locals)
        id = sym; // Start at beginning of symbol table
        while (id[Tk]) {
          if (id[Class] == Loc) {
            // Restore saved global values
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz; // Move to next symbol
        }
      }
      else {
        // Global variable definition
        id[Class] = Glo;       // Mark as global
        id[Val] = (int)data;   // Assign address in data segment
        data = data + sizeof(int); // Allocate space (always int-sized for simplicity)
      }
      
      if (tk == ',') next(); // Handle multiple declarations (int a, b, c;)
    }
    next();
  }

  // Compilation complete, locate main() function
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0; // Exit if we're only printing source

  // Setup VM execution
  // Initialize stack pointer at the top of stack area
  bp = sp = (int *)((int)sp + poolsz);
  
  // Setup call frame for main():
  *--sp = EXIT; // Return address (exit when main returns)
  *--sp = PSH; t = sp; // Save the "argc, argv" pointer for clean-up
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // Start the virtual machine execution
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle; // Fetch instruction and increment instruction counter
    
    // Debug output if -d flag was specified
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    
    // Execute the instruction (VM instruction set)
    if      (i == LEA) a = (int)(bp + *pc++);                             // Load local address: computes address relative to base pointer
    else if (i == IMM) a = *pc++;                                         // Load immediate value: loads the next word as a value
    else if (i == JMP) pc = (int *)*pc;                                   // Jump: unconditional jump to address
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // Jump to subroutine: push return address and jump
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // Branch if zero: conditional jump if a is zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // Branch if not zero: conditional jump if a is not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // Enter subroutine: setup new stack frame
    else if (i == ADJ) sp = sp + *pc++;                                   // Adjust stack: remove n items from stack
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // Leave subroutine: restore previous stack frame and return
    else if (i == LI)  a = *(int *)a;                                     // Load int: load int value from address in a
    else if (i == LC)  a = *(char *)a;                                    // Load char: load char value from address in a
    else if (i == SI)  *(int *)*sp++ = a;                                 // Store int: store a into address pointed by stack top
    else if (i == SC)  a = *(char *)*sp++ = a;                            // Store char: store a (truncated) into address pointed by stack top
    else if (i == PSH) *--sp = a;                                         // Push: push value of a onto stack

    // Arithmetic and logical operations (binary operations pop stack top as operand)
    else if (i == OR)  a = *sp++ |  a;                                    // Bitwise OR
    else if (i == XOR) a = *sp++ ^  a;                                    // Bitwise XOR
    else if (i == AND) a = *sp++ &  a;                                    // Bitwise AND
    else if (i == EQ)  a = *sp++ == a;                                    // Equal comparison
    else if (i == NE)  a = *sp++ != a;                                    // Not equal comparison
    else if (i == LT)  a = *sp++ <  a;                                    // Less than comparison
    else if (i == GT)  a = *sp++ >  a;                                    // Greater than comparison
    else if (i == LE)  a = *sp++ <= a;                                    // Less than or equal comparison
    else if (i == GE)  a = *sp++ >= a;                                    // Greater than or equal comparison
    else if (i == SHL) a = *sp++ << a;                                    // Shift left
    else if (i == SHR) a = *sp++ >> a;                                    // Shift right
    else if (i == ADD) a = *sp++ +  a;                                    // Addition
    else if (i == SUB) a = *sp++ -  a;                                    // Subtraction
    else if (i == MUL) a = *sp++ *  a;                                    // Multiplication
    else if (i == DIV) a = *sp++ /  a;                                    // Division
    else if (i == MOD) a = *sp++ %  a;                                    // Modulo

    // System calls (essentially standard library functions)
    else if (i == OPEN) a = open((char *)sp[1], *sp);                     // File open
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);              // File read
    else if (i == CLOS) a = close(*sp);                                   // File close
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); } // Printf (supports up to 6 args)
    else if (i == MALC) a = (int)malloc(*sp);                             // Memory allocation
    else if (i == FREE) free((void *)*sp);                                // Memory free
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);       // Memory set
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);    // Memory compare
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; } // Exit program
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }  // Unknown instruction error
  }
}
