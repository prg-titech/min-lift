// {"ChunkSize":5,"InputSize":1,"KernelCount":2,"InputTypes":["float*"],"ResultType":"float*"}

kernel void KERNEL(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  global int* bitmap,
  int N) {
    int i1 = get_global_id(0);

    float x = xs[i1];

    float temp1 = 0.5f;
    bitmap[i1] = x > temp1;
}

kernel void KERNEL2(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  global int* bitmap,
  global int* indices,
  int N) {

    int i1 = get_global_id(0);
    if (bitmap[i1]) {
        result[indices[i1] - 1] = xs[i1] * 2.0f;
    }
    int l14 = indices[N - 1];

    *result_size = l14;
}

kernel void prefix_sum(global int *xs, global int *ys, int i) {
  global int *in = NULL, *out = NULL;
  if (i % 2 == 0) {
    in = xs; out = ys;
  }
  else {
    in = ys; out = xs;
  }

  int j = get_global_id(0);

  int powiof2 = pown(2.0f, i);
  if (j < powiof2) {
    out[j] = in[j];
  }
  else {
    out[j] = in[j] + in[j - powiof2];
  }
}


