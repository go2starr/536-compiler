int gint;
double gdbl;

double f(double d1, double d2) {
  printf("%f", d1);
  printf("  ");
  printf("%f", d2);
  printf("  ");
  return gint;  /* dbl fn returing an int */
}

void main() {
  int k;
  double d;
  
  gint = 5;
  gdbl = 6.5;
  
  k = 10;

  printf("Calling f, Expected output: 10 5 5\n");
  d = f(k, gint);  /* int actual passed to dbl formal */
  printf("%f", d);
  printf("\n");

  printf("\nExpected output: 5\n");
  d = 5;  /* int assigned to dbl */
  printf("%f", d);
  printf("\n");

  /*** All kinds of dbl/int operands to binary operators **/
  printf("\nExpected output:\n15 -5 50 0.5 15 5 50 2\n");
  printf("%f", d+k);
  printf(" ");
  printf("%f", d-k);
  printf(" ");
  printf("%f", d*k);
  printf(" ");
  printf("%f", d/k);
  printf(" ");
  printf("%f", k+d);
  printf(" ");
  printf("%f", k-d);
  printf(" ");
  printf("%f", k*d);
  printf(" ");
  printf("%f", k/d);
  printf("\n");
  

  if (d < 6) {
  } else {
    printf("ERROR d < 6 is false\n");
  }

  if (d <= 6.0) {
  } else {
    printf("ERROR d <= 6 is false\n");
  }

  if (d > 6) {
    printf("ERROR d > 6 is true\n");
  } else {
  }

  if (d >= 6) {
    printf("ERROR d >= 6 is true\n");
  } else {
  }

  if (d == 6) {
    printf("ERROR d == 6 is true\n");
  } else {
  }

  if (d != 6) {
  } else {
    printf("ERROR d != 6 is false\n");
  }
}
