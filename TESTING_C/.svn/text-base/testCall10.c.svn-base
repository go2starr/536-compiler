/* TEST INT AND DOUBLE ACTUALS THAT ARE EXPRESSIONS */

double returnD() {
  return 5.5;
}

void f(double a, int b, double c, int d) {
  printf("Expected output:\n3.75 3 5.5 1\n");
  printf("%f", a);
  printf(" ");
  printf("%d", b);
  printf(" ");
  printf("%f", c);
  printf(" ");
  printf("%d", d);
  printf("\n");
  return;
}

int g;
void main() {
  double x;
  int y;
  double z;
  x = 1.5;
  y = 2;
  z = 3.5;
  g = 4;
  f(x*2.5, y+1, returnD(), g/3);
}
