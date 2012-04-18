void main() {
  int x;
  int y;
  y = x = 0;
  while (y<10) {
    printf("x: ");
    printf("%d", x);
    printf("\n");
    printf("y: ");
    printf("%d", y);
    printf("\n");
    x = x + 2;
    y++;
  }
  printf("x: ");
  printf("%d", x);
  printf("\n");
  printf("y: ");
  printf("%d", y);
  printf("\n");
}
