union u { 
   int i; 
   struct s { 
      int i1, i2;
   } s;
};

union u x = (union u)6;

int main() {
  struct s y = {1, 2};
  union u  z = (union u)y;
}
