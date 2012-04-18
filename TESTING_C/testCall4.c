/* TEST INT PARAMETERS */
void f(int a, int b, int c) {
  printf("Expected output:\n1 2 3\n");
  printf("%d", a);
  printf(" ");
  printf("%d", b);
  printf(" ");
  printf("%d", c);
  printf("\n");
  return;
}

void main() {
  f(1, 2, 3);
}
