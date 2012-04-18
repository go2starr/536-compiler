int x;
int y;
double d;

void f(double a, int c, int d) {
  printf("Expected output:\n22.5 55 65\n");
  printf("%f", a);
  printf(" ");
  printf("%d", c);
  printf(" ");
  printf("%d", d);
  printf("\n");
  return;
}

int g(int x) { 
  return x;
}

double gg(double dd) {
  return d+dd;
}

void foo() {
  int a;
  int b;
  int c;
  int B;
  
  printf("Expected output:\n6 7 8 9\n");
  a = 6;
  while (a < 10) {
    printf("%d", a);
    printf(" ");
    a++;
  }
  printf("\n");

  printf("\nExpected output:\n6 5 4 3 2 1 0 -1\n");
  a = 6;
  while (a >= -1) {
    printf("%d", a);
    printf(" ");
    a--;
  }
  printf("\n");
  a = 10;

  printf("\nExpected output:\n1 32 10 220 10 9 10\n");
  B = 1 < 3;
  printf("%d", B);
  printf(" ");
  b = 22;
  a = a + b;  /* a was 10 */
  printf("%d", a);
  printf(" ");
  a = a - b;
  printf("%d", a);
  printf(" ");
  a = a * b;
  printf("%d", a);
  printf(" ");
  a = a / b;
  printf("%d", a);
  printf(" ");
  b = a--;
  printf("%d", a);
  printf(" ");
  b = a++;
  printf("%d", a);
  printf("\n");
  a = 10;
  b = 22;

  /* a is 10, b is 22 */
  printf("\nExpected output:\n0 0 1 0 1 1\n");
  c = 1;
  c = ! c;
  printf("%d", c);
  printf(" ");
  c = c && 1;
  printf("%d", c);
  printf(" ");
  c = 0 || (!c);
  printf("%d", c);
  printf(" ");
  c = a == b;
  printf("%d", c);
  printf(" ");
  c = a != b;
  printf("%d", c);
  printf(" ");
  c = a < b;
  printf("%d", c);
  printf("\n");

  /* a is 10, b is 22 */
  printf("\nExpected output:\n0 1 0 1 1\n");
  c = a > b;
  printf("%d", c);
  printf(" ");
  c = a <= b;
  printf("%d", c);
  printf(" ");
  c = a >= b;
  printf("%d", c);
  printf(" ");
  printf("%d", b <= b);
  printf(" ");
  printf("%d", b >= b);
  printf("\n");

  printf("\nExpected output:\n-50 3 2 1 0 -1 -2\n");
  a = - 50;
  printf("%d", a);
  printf(" ");
  if (a <= 10) {
    int a;
    a = 3;
    while (a >= -2) {
      printf("%d", a);
      printf(" ");
      a = a - 1;
    }
  }
  printf("\n");

  printf("\nExpected output:\n-5 -4 -3 -2 -1\n");
  b = -(1*1);
  if (b == -1) {
    x = -5;
    while (x<=b) {
      printf("%d", x);
      printf(" ");
      x = x + 1;
    }
  }
  else {
    printf("ERROR\n");
  }
  printf("\n");
} 

void bar() {
  double a;
  double b;
  int c;
  double B;
  int aa;
  int bb;
  
  printf("\nExpected output:\n6.5 7.5 8.5 9.5\n");
  a = 6.5;
  aa = 6;
  while (aa < 10) {
    printf("%f", a);
    printf(" ");
    a = a + 1.0;
    aa = aa + 1;
  }
  printf("\n");

  a = 10.5;
  printf("\nExpected output:\n33 10.5 236.25 10.5\n");
  b = 22.5;
  a = a + b;
  printf("%f", a);
  printf(" ");
  a = a - b;
  printf("%f", a);
  printf(" ");
  a = a * b;
  printf("%f", a);
  printf(" ");
  a = a / b;
  printf("%f", a);
  printf("\n");


  printf("\nExpected output:\n66 66\n");
  a = b = 66.0;
  printf("%f", a);
  printf(" ");
  printf("%f", b);
  printf("\n");

  printf("\nExpected output:\n-50 3 2 1 0 -1 -2\n");
  a = - 50.0;
  aa = -50;
  printf("%f", a);
  printf(" ");
  if (aa <= 10) {
    double a;
    a = 3.0;
    aa = 3;
    while (aa >= -2) {
      printf("%f", a);
      printf(" ");
      a = a - 1.0;
      aa = aa - 1;
    }
  }
  printf("\n");

  printf("\nExpected output:\n-5 -4 -3 -2\n");
  b = -(1.0*1.0);
  bb = -(1 * 1);
  if (bb == -1) {
    a = -5.0;
    aa = -5;
    while (aa<bb) {
      printf("%f", a);
      printf(" ");
      a = a + 1.0;
      aa = aa + 1;
    }
  }
  else {
    printf("ERROR\n");
  }
  printf("\n");
} 

int b;

void main() {
  double myd;

  d = 22.5;
  x = 55;
  y = 65;
  printf("Expected output:\n56 66 55 65\n");
  ++x;
  ++y;
  printf("%d", x);
  printf(" ");
  printf("%d", y);
  printf(" ");
  --x;
  --y;
  printf("%d", x);
  printf(" ");
  printf("%d", y);
  printf("\n");

  printf("\ncalling f\n");
  f(d, x, y);
  
  printf("\ncalling g\n");
  x = g(x);
  printf("Expected output:\n55\n");
  printf("%d", x);
  printf("\n\n");
  
  printf("calling gg\n");
  myd = gg(d);
  printf("Expected output:\n45 22.5\n");
  printf("%f", myd);
  printf(" ");
  printf("%f", d);
  printf("\n\n");
  
  printf("calling foo\n");
  foo();

  printf("\ncalling bar\n");
  bar();

  printf("\nExpected output:\n2 5 2 5 2\n");
  b = 1;
  x = 2;
  if (b || (x=5)) {
     printf("%d", x);
     printf(" ");
  }
  x = 2;
  if ((x=5) || (b=2)) {
    printf("%d", x);
    printf(" ");
  }
  b = 1;
  x = 2;
  if (!b && (x=5)) {
  } else {
     printf("%d", x);
     printf(" ");
  }
  b = 0;
  x = 2;
  if ((x=5) && (b=2)) {
    printf("%d", x);
    printf(" ");
    printf("%d", b);
    printf("\n");
  }
}
