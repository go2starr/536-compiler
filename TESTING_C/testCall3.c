double g;

double f() {
  return g;
}

void main() {
  double p;

  g = 10.5;
  p = f();
  printf("g: ");
  printf("%f", g);
  printf("\np: ");
  printf("%f", p);
  printf("\n");
}
