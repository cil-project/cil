/* 
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * flatten all toplevel forms to single lines.
 * very heuristic... */

%{
#include <stdlib.h>     // atoi

// emit yytext as-is
void emit();

// debugging diagnostic, emitted when enabled
void diag(char const *str);

// add a newline if nesting <= threshold
void possibleNewline();

// keep track of brace nesting (0 means not inside any pair)
int nesting = 0;

// nesting threshold; when nesting is greater than threshold,
// newlines are suppressed
int threshold = 0;

%}

/* don't try to call yywrap() */
%option noyywrap

/* start condition for strings */
%x STRING
%x CHARLIT


%%

";"           { emit(); possibleNewline(); }

"/\n"         { if (nesting <= threshold) {    /* end of C comment */
                  emit();
                }
                else {
                  printf("%c", yytext[0]);
                }
              }

"{"           { nesting++;
                emit();
                possibleNewline();      // so the header is separated from the components
              }

"}"(";"?)     { nesting--;
                emit();
                possibleNewline();
              }

  /* a hash, then some non-newlines.  then, possibly, an escaped
   * newline followed by more non-newlines (repeat as needed).
   * finally, a newline */
"#".*("\\\n".*)*"\n" {
                printf("\n");      /* make sure starts on own line */
                emit();            /* preprocessor */
              }

"\n"          { printf(" "); }     /* not any above case, eat it*/

"//".*"\n"    { emit(); }          /* C++ comment */

"\""          { diag("<STR>"); emit(); BEGIN(STRING); }     /* start quote */

<STRING>{
  "\\"(.|\n)  { emit(); }                                   /* escaped character */
  "\""        { emit(); diag("</STR>"); BEGIN(INITIAL); }   /* close quote */
  (.|\n)      { emit(); }                                   /* ordinary char */
}

"\'"          { diag("<CHAR>"); emit(); BEGIN(CHARLIT); }   /* start tick */

<CHARLIT>{
  "\\"(.|\n)  { emit(); }                                   /* escaped character */
  "\'"        { emit(); diag("</CHAR>"); BEGIN(INITIAL); }  /* close tick */
  (.|\n)      { emit(); }                                   /* ordinary char */
}

.             { emit(); }

%%

void emit()
{
  printf("%.*s", yyleng, yytext);
}

void diag(char const *str)
{
  //printf("%s", str);
}

void possibleNewline()
{
  if (nesting <= threshold) {
    printf("\n");
  }
}

int main(int argc, char *argv[])
{
  if (isatty(0)) {
    printf("usage: %s [threshold] <input.c >output.c\n", argv[0]);
    printf("  The threshold (default: 0) specifies at what nesting level\n"
           "  of braces will line breaks be allowed (or inserted).  By\n"
           "  starting with 0, you get all top-level forms, one per line\n"
           "  (roughly).  Increasing the threshold leads to finer-grained\n"
           "  structure on each line.  The intent is to use the delta\n"
           "  minimizer on each level of granularity.\n");
    return 0;
  }

  if (argc >= 2) {
    threshold = atoi(argv[1]);    // user-specified threshold
  }  

  yyin = stdin;
  yylex();
  return 0;
}
