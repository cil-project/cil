#include <testharness.h>
#include <stdio.h>

void BuildWord(char * pchWord) {
    int i;
    char * pch = pchWord;

    /* original code: 
     * while ((i = *pch++) != '\0') { } 
     */

    do {
      i = *pch;
      pch++;
    } while (i != '\0'); 

    printf("%s\n",pchWord); 
}

int main() {
  char *test = "foo"; 

  BuildWord(test);

  SUCCESS; 
} 
