  struct foo { 
     int x, y; 
     int a[5];
     struct inner {
        int z;
     } inner;
  } s = { 0, .inner.z = 3, .a[1 ... 2] = 5, 4, y : 8 };
