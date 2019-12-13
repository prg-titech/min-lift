// {"ChunkSize":5,"InputSize":1,"KernelCount":1}

kernel void KERNEL(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  int N) {


    int idx = 0;
    for (int i = 0; i < N; i++) {
        if (xs[i] > 0.5f) {
            result[idx++] = xs[i] * 2.0f;
        }
    }

    *result_size = idx;
}

