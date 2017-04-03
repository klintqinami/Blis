window w;

@gpuonly void foo()
{
  window_should_close(w);
}

int main()
{
  return 0;
}
