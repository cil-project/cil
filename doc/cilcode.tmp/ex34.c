struct { 
   int x; 
   struct { 
       int y, z; 
   } nested;
} i = { .nested.y = 5, 6, .x = 1, 2 };               
