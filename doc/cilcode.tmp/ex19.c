int main(void) {
# 1
   int x, y;
   for(int i = 0; i<5; i++) {
      if(i == 5) continue;
      if(i == 4) break;
      i += 2;
   } 
   while(x < 5) {
     if(x == 3) continue;
     x ++;
   }
}
