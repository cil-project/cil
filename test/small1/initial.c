

typedef struct {
  int a, b;
} DATA;

typedef struct {
  int tag[5];
  int x;
  DATA d1;
  DATA d2;
} TDATA;


TDATA x = { {0,0,0},
            5 };

TDATA x1 = { .x = 7,
             .d1 = { .b = 5 },
             .d2 = { 9 } };

TDATA x2[] = { [5] = { 8 }} ;


    
