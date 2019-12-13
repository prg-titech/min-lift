// {"ChunkSize":5,"InputSize":1,"KernelCount":3}

kernel void KERNEL(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  global int* bitmap,
  int N) {
     // int ary_size = 0;

     

int i1 = get_global_id(0);

 float x = xs[i1];

private bool temp2;


private float temp1;
temp1 = 0.5f;
     
temp2 = x > temp1;
            
     
bitmap[i1] = temp2;


}

kernel void KERNEL2(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  global int* bitmap,
  global int* indices,
  int N) {
     int ary_size = 0;
     

int i1 = get_global_id(0);
if (bitmap[i1]) {
  result[indices[i1] - 1] = xs[i1];
}
int l14 = indices[N - 1];


     
*result_size = l14;
}

kernel void KERNEL3(
  const global float* restrict xs,
  global float* result,
  global int* result_size,
  int N) {
     int ary_size = 0;
     
             
global float* ys = result;



{
  int i2 = get_global_id(0);
  
 float x = ys[i2];

private float temp4;


private float temp3;
temp3 = 2.0f;
     
temp4 = x * temp3;
            
     
  result[i2] = temp4;
}
                 
     
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
    
      