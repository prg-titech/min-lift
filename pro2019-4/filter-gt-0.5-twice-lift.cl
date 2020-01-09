// {"ChunkSize":5,"InputSize":1,"KernelCount":3,"InputTypes":["float*"],"ResultType":"float*"}

kernel void KERNEL(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  global int* bitmap,
  int N) {
     // int ary_size = 0;






int i0 = get_global_id(0);


 float v0 = i0;

private bool v1;


v1 = xs[i0] > 0.5f;


bitmap[i0] = v1;


}

kernel void KERNEL2(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  global int* bitmap,
  global int* indices,
  int N) {


int id = get_global_id(0);
if (bitmap[id]) {
  result[indices[id] - 1] = xs[id];
}
int len = indices[N - 1];


     *result_size = len;
}

kernel void KERNEL3(
  const global float* restrict temp0,
  global float* result,
  global int* result_size,
  int t4) {
     int ary_size = 0;

// { "InputArray": "const global float* restrict temp0", "InputLength": "int t4" }


{
  int i1 = get_global_id(0);


 float v2 = i1;

private float v3;


v3 = temp0[i1] * 2.0f;


  result[i1] = v3;
}



*result_size = t4;
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


