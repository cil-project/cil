int main() {
  struct foo { 
        int x; } foo; 
  {
     struct foo { 
        double d;
     };
     return foo.x;
  }      
}
