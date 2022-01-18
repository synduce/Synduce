
int mysum(int *a, int n)
{
  int i = 0;
  int sum = a[0];
  int amax = a[0];

  for (i = 1; i < n + 10; i++)
  {
    if (a[i] > 0)
    {
      sum += a[i];
    }
    amax = max(a[i], amax);
  }
  return sum;
}
